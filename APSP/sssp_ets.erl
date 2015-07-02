-module(sssp_ets).

-compile(export_all).

-define(table, distances).

create() ->
    ?table = ets:new(?table, 
                     [ordered_set, public, named_table,
                      {write_concurrency, true}, {read_concurrency, true}]).

build_target_dist_pairs(Data) ->
    Tokens = string:tokens(Data, " "),
    lists:zip(lists:seq(0,length(Tokens)-1), Tokens).

get_data_worker(Fd, Index) ->
    case file:read_line(Fd) of 
        {ok, Data} ->
            lists:map(fun({Target, X}) -> 
                              {Dist, _} = string:to_integer(X), 
                              true = ets:insert_new(?table, {{Index, Target}, Dist})
                      end ,
                      build_target_dist_pairs(Data)),
            get_data_worker(Fd, Index+1);
        {error, Report} ->
            Report;
        eof ->
            true = ets:insert_new(?table, {{-1, 0}, Index}),
            ?table
    end.

get_data(Fd) ->
    create(),
    get_data_worker(Fd, 0).

get_nr_nodes(DistMatrix) ->
    [{{-1,0},NrNodes}] = ets:lookup(DistMatrix, {-1,0}),
    NrNodes.

make_chunk(Start,End) ->
    lists:seq(Start,End).

get_distance(Target,{Source,Distances}) ->
    [{{Source, Target}, Dist}] = ets:lookup(Distances, {Source, Target}),
    Dist.

get_distances_from_source(Source, Distances, _NrNodes) ->
    {Source, Distances}.

collect_results(FinalDs) ->
    gb_sets:to_list(FinalDs).

print_result(_NrNodes, Result) ->
    io:fwrite("~p~n", [Result]).

apply_to_chunk(Fun, Chunk) ->
    lists:map(Fun, Chunk).
                     
