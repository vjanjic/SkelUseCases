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
    get_data(Fd).

start([NN]) ->
    NrNodes = list_to_integer(atom_to_list(NN)),
    ReadingTime = timer:tc(fun() -> init("input_data") end),
    io:fwrite("Reading input time ~p~n", [element(1,ReadingTime)]),
    DistMatrix = element (2, ReadingTime),
    apsp_cpu:apsp_seq(NrNodes, DistMatrix).
    %Time = timer:tc(fun() -> sssp_cpu:dijkstra(NrNodes, 1, DistMatrix) end),
    %io:fwrite("Time is ~p~n", [element(1,Time)]).
    %io:fwrite("Results are ~p~n", [element(2,Time)]).

