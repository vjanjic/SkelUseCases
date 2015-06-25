-module(apsp_cpu).

-compile(export_all).

apsp_seq(NrNodes, DistMatrix) ->
    Time = timer:tc(fun() ->
                            lists:map(fun(X) ->
                                              sssp_cpu:dijkstra(NrNodes, X, DistMatrix) end,
                                      lists:seq(1,NrNodes)) end),
    io:fwrite("Time ~p~n", [element(1,Time)]),
    element(2,Time).
