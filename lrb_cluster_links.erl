-module(lrb_cluster_links).

-compile(export_all).
-compile(nowarn_export_all).

ets_search() ->
    Match = {'connection', '$1', '_', '_', '_', '$2', '_', '_', '_', 'normal', '_', '_', '_'},
    ets_search(ets:match(sys_dist, Match)).

%% ['1GO38EPSV1UWL@shostakovich',#Port<9118.64>, {net_address,{{127,0,0,1},43162},"shostakovich",tcp,inet}, hidden]
ets_search([]) ->
    ok;
ets_search([[Node, Port] | Rest]) ->
    io:format("Node: ~p~n", [Node]),
    io:format("Port: ~p~n", [format_port(Port)]),
    ets_search(Rest).

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
