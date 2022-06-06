cluster_links() ->
    {ok, Items} = net_kernel:nodes_info(),
    [Link || Item <- Items,
             Link <- [format_nodes_info(Item)], Link =/= undefined].

format_nodes_info({Node, Info}) ->
    Owner = proplists:get_value(owner, Info),
    case catch process_info(Owner, links) of
        {links, Links} ->
            case [Link || Link <- Links, is_port(Link)] of
                [Port] ->
                    {Node, Owner, format_nodes_info1(Port)};
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
