-module(sssp_list).

-compile(export_all).

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

get_nr_nodes(DistMatrix) ->
    length(DistMatrix).

make_chunk(Start,End) ->
    lists:seq(Start,End).

get_distance(Target,Distances) ->
    lists:nth(Target+1, Distances).

get_distances_from_source(Source, Distances, _NrNodes) ->
    lists:nth(Source+1, Distances).

collect_results(FinalDs) ->
    gb_sets:to_list(FinalDs).

print_result(_NrNodes, Result) ->
    io:fwrite("~p~n", [Result]).

apply_to_chunk(Fun, Chunk) ->
    lists:map(Fun, Chunk).
                     
