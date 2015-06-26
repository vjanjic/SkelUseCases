-module(sssp_binary).

-compile(export_all).

make_bin([]) ->
    <<>>;
make_bin(Data) ->
    {IntFromStr, _} = string:to_integer(hd(Data)),
    Bin = <<IntFromStr:4/little-unsigned-integer-unit:8>>,
    Rest = make_bin(tl(Data)),
    <<Bin/binary,Rest/binary>>.

get_data(Fd) ->
    case file:read_line(Fd) of 
        {ok, Data} ->
            Bin = make_bin(string:tokens(Data," ")),
            Rest = get_data(Fd),
            <<Bin/binary, Rest/binary>>;
        {error, Report} ->
            Report;
        eof ->
            <<>>
    end.

make_chunk(Start,End) -> 
    lists:foldl(fun(X,Acc) -> <<Acc/binary, X:4/little-unsigned-integer-unit:8>> end,
                <<>>,
                lists:seq(Start,End)).

    
get_distances_from_source(Source, Distances, NrNodes) ->
    binary:part(Distances, {Source*NrNodes*4, NrNodes*4}).

get_distance(Target, Distances) ->
    <<D:4/little-unsigned-integer-unit:8>> = binary_part(Distances, {Target*4, 4}),
    D.

collect_results(FinalDs) ->
    make_bin(gb_sets:to_list(FinalDs)).
