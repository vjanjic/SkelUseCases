-module(main).

-compile(export_all).

open_file(Fname) ->
    case file:open(Fname, [read, raw]) of
        {ok, Fd} ->
            Fd;
        {error, Reason} ->
            Reason
    end.  

get_data(Fd) ->
    case file:read_line(Fd) of 
	{ok, Data} ->
	    [lists:map(fun(X) -> {Num, _} = string:to_integer(X), Num end ,
		      string:tokens(Data, " ")) |
	     get_data(Fd)];
	{error, Report} ->
	    Report;
	eof ->
	    []
    end.

init(Fname) ->
    Fd = open_file(Fname),
    apsp_binary:get_data(Fd).
    %get_data(Fd).

start([NN]) ->
    NrNodes = list_to_integer(atom_to_list(NN)),
    DistMatrix = init("input_data"),
    Chunk = apsp_binary:make_chunk(0,NrNodes-1),
    Time = timer:tc(fun() -> sssp_gpu:dijkstra_gpu(Chunk, DistMatrix, NrNodes) end),
    %Time = timer:tc(fun() -> sssp_cpu:dijkstra(NrNodes, 1, DistMatrix) end),
    io:fwrite("Time is ~p~n", [element(1,Time)]).
    %io:fwrite("Results are ~p~n", [element(2,Time)]).

