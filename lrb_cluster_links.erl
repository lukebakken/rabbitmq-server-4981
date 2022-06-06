-module(lrb_cluster_links).

-compile(export_all).
-compile(nowarn_export_all).

search() ->
    {ok, Items} = net_kernel:nodes_info(),
    ok = search_items(Items).

search_items([]) ->
    ok;
search_items([{Node, Item} | Rest]) ->
    Pid = proplists:get_value(owner, Item),
    case catch process_info(Pid, links) of
        {links, Links} ->
            {ok, Data} = search_for_ports(Links, maps:new()),
            io:format("NODE: ~p~nDATA ~p~n", [Node, Data]);
        _ ->
            undefined
    end,
    search_items(Rest).

search_for_ports([], Data) ->
    {ok, Data};
search_for_ports([Item | Rest], Data0) when is_port(Item) ->
    Key = {port, Item},
    Value = {{recon_port_info, recon:port_info(Item)}, {formatted_nodes_info, format_nodes_info1(Item)}},
    false = maps:is_key(Key, Data0),
    Data1 = maps:put(Key, Value, Data0),
    search_for_ports(Rest, Data1);
search_for_ports([Item | Rest], Data0) when is_pid(Item) ->
    Key = {pid, Item},
    case maps:get(Key, Data0, not_found) of
        not_found ->
            Data1 = maps:put(Key, true, Data0),
            case catch process_info(Item, links) of
                {links, Links} ->
                    search_for_ports(Links, Data1);
                _ ->
                    search_for_ports(Rest, Data1)
            end;
        true ->
            search_for_ports(Rest, Data0)
    end;
search_for_ports([_Link | Rest], Data) ->
    search_for_ports(Rest, Data).

cluster_links() ->
    {ok, Items} = net_kernel:nodes_info(),
    [Link || Item <- Items,
             Link <- [format_nodes_info(Item)], Link =/= undefined].

format_nodes_info({Node, Info}) ->
    Pid = proplists:get_value(owner, Info),
    case catch process_info(Pid, links) of
        {links, Links} ->
            case [Link || Link <- Links, is_port(Link)] of
                [Port] ->
                    {Node, Pid, format_nodes_info1(Port)};
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

format_nodes_info1(Port) ->
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
