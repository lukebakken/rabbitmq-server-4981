-module(lrb_cluster_links).

-compile(export_all).
-compile(nowarn_export_all).

ets_search() ->
    Match = {connection, '$1', '_', '_', '_', '$2', '_', '$3', '_', '$4', '_', '_', '_'},
    Conns = ets:match(sys_dist, Match),
    io:format("Conns: ~p~n", [Conns]).

search() ->
    {ok, Items} = net_kernel:nodes_info(),
    {ok, Data} = search_items(Items, []),
    ok = io:format("Data: ~p~n", [Data]),
    ok.

search_items([], Acc) ->
    {ok, Acc};
search_items([{Node, Item} | Rest], Acc0) ->
    OwnerPid = proplists:get_value(owner, Item),
    true = is_pid(OwnerPid),
    Pids0 = maps:new(),
    K = {pid, {Node, Item}},
    Pids1 = maps:put(K, true, Pids0),
    Acc1 = case catch process_info(OwnerPid, links) of
               {links, Links} ->
                   {ok, Ports, _Pids} = search_for_ports(Node, Links, maps:new(), Pids1),
                   io:format("Node: ~p~nPorts: ~p~n", [Node, Ports]),
                   Acc0;
               %% {ok, Data} = search_for_ports(Node, Links),
               %% case maps:get(port, Data, not_found) of
               %%     not_found ->
               %%         Acc0;
               %%     Value ->
               %%         [Value | Acc0]
               %% end;
               _ ->
                   Acc0
           end,
    search_items(Rest, Acc1).

search_for_ports(_Node, [], Ports, Pids) ->
    {ok, Ports, Pids};
search_for_ports(Node, [Item | Rest], Ports0, Pids) when is_port(Item) ->
    Ports1 = case erlang:port_info(Item, name) of
                 {name, "tcp_inet"} ->
                     K = {port, {Node, recon:port_info(Item)}},
                     false = maps:is_key(K, Ports0),
                     maps:put(K, true, Ports0);
                 _ ->
                     Ports0

             end,
    search_for_ports(Node, Rest, Ports1, Pids);
search_for_ports(Node, [Item | Rest], Ports0, Pids0) when is_pid(Item) ->
    K = {pid, {Node, Item}},
    {ok, Ports1, Pids1} = case maps:is_key(K, Pids0) of
                      true ->
                          {ok, Ports0, Pids0};
                      false ->
                          Pids2 = maps:put(K, true, Pids0),
                          case catch process_info(Item, links) of
                              {links, Links} ->
                                  search_for_ports(Node, Links, Ports0, Pids2);
                              _ ->
                                  {ok, Ports0, Pids2}
                          end
                  end,
    search_for_ports(Node, Rest, Ports1, Pids1);
search_for_ports(Node, [Item | Rest], Ports, Pids) ->
    io:format("Node: ~p Unknown Item: ~p~n", [Node, Item]),
    search_for_ports(Node, Rest, Ports, Pids).

%% search_for_ports(_Node, [], Data) ->
%%     {ok, Data};
%% search_for_ports(Node, [Port | _Rest], Data0) when is_port(Port) ->
%%     PortInfo = erlang:port_info(Port),
%%     OwnerPid = proplists:get_value(connected, PortInfo),
%%     true = is_pid(OwnerPid),
%%     Key = port,
%%     Value = {Node, OwnerPid, format_port(Port)},
%%     false = maps:is_key(Key, Data0),
%%     Data1 = maps:put(Key, Value, Data0),
%%     {ok, Data1};
%% search_for_ports(Node, [OwnerPid | Rest], Data0) when is_pid(OwnerPid) ->
%%     Key = {pid, OwnerPid},
%%     case maps:get(Key, Data0, not_found) of
%%         not_found ->
%%             io:format("OwnerPid not_found: ~p~n", [OwnerPid]),
%%             Data1 = maps:put(Key, true, Data0),
%%             case catch process_info(OwnerPid, links) of
%%                 {links, Links} ->
%%                     search_for_ports(Node, Links, Data1);
%%                 _ ->
%%                     search_for_ports(Node, Rest, Data1)
%%             end;
%%         true ->
%%             io:format("OwnerPid found: ~p~n", [OwnerPid]),
%%             search_for_ports(Node, Rest, Data0)
%%     end;
%% search_for_ports(Node, [Link | Rest], Data) ->
%%     io:format("Unknown link: ~p~n", [Link]),
%%     search_for_ports(Node, Rest, Data).

format_port(Port) ->
    case {rabbit_net:socket_ends(Port, inbound),
          rabbit_net:getstat(Port, [recv_oct, send_oct])} of
        {{ok, {PeerAddr, PeerPort, SockAddr, SockPort}}, {ok, Stats}} ->
            [{peer_addr, maybe_ntoab(PeerAddr)},
             {peer_port, PeerPort},
             {sock_addr, maybe_ntoab(SockAddr)},
             {sock_port, SockPort},
             {recv_bytes, pget(recv_oct, Stats)},
             {send_bytes, pget(send_oct, Stats)}];
        Unexpected ->
            io:format("Unexpected: ~p~n", [Unexpected]),
            []
    end.

maybe_ntoab(A) when is_tuple(A) -> list_to_binary(rabbit_misc:ntoab(A));
maybe_ntoab(H)                  -> H.

-spec pget(term(), list() | map()) -> term().
pget(K, M) when is_map(M) ->
    case maps:find(K, M) of
        {ok, V} ->
            V;
        _ ->
            undefined
    end;

pget(K, P) ->
    case lists:keyfind(K, 1, P) of
        {K, V} ->
            V;
        _ ->
            undefined
    end.
