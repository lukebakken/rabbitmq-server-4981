-module(lrb_cluster_links).

-compile(export_all).
-compile(nowarn_export_all).

search() ->
    {ok, Items} = net_kernel:nodes_info(),
    {ok, Data} = search_items(Items, []),
    ok = io:format("Data: ~p~n", [Data]),
    ok.

search_items([], Acc) ->
    {ok, Acc};
search_items([{Node, Item} | Rest], Acc0) ->
    OwnerPid = proplists:get_value(owner, Item),
    Acc1 = case catch process_info(OwnerPid, links) of
        {links, Links} ->
            {ok, Data} = search_for_ports(Node, OwnerPid, Links, maps:new()),
            case maps:get(port, Data, not_found) of
                not_found ->
                    Acc0;
                Value ->
                    [Value | Acc0]
            end;
        _ ->
            undefined
    end,
    search_items(Rest, Acc1).

search_for_ports(_Node, _OwnerPid, [], Data) ->
    {ok, Data};
search_for_ports(Node, OwnerPid, [Item | _Rest], Data0) when is_port(Item) ->
    Key = port,
    Value = {Node, OwnerPid, format_port(Item)},
    false = maps:is_key(Key, Data0),
    Data1 = maps:put(Key, Value, Data0),
    {ok, Data1};
search_for_ports(Node, OwnerPid0, [OwnerPid | Rest], Data0) when is_pid(OwnerPid) ->
    Key = {pid, OwnerPid},
    case maps:get(Key, Data0, not_found) of
        not_found ->
            Data1 = maps:put(Key, true, Data0),
            case catch process_info(OwnerPid, links) of
                {links, Links} ->
                    search_for_ports(Node, OwnerPid, Links, Data1);
                _ ->
                    search_for_ports(Node, OwnerPid0, Rest, Data1)
            end;
        true ->
            search_for_ports(Node, OwnerPid0, Rest, Data0)
    end;
search_for_ports(Node, OwnerPid, [_Link | Rest], Data) ->
    search_for_ports(Node, OwnerPid, Rest, Data).

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
        _ ->
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
