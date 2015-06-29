-module(sssp_binary).

-compile(export_all).

make_bin_from_pairs([]) ->
    <<>>;
make_bin_from_pairs([{_X,Y}|Tl]) ->
    Bin = <<Y:4/little-unsigned-integer-unit:8>>,
    Rest = make_bin_from_pairs(Tl),
    <<Bin/binary, Rest/binary>>.

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

get_nr_nodes(DistMatrix) ->
    round(math:sqrt(byte_size(DistMatrix)/4)).

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
    Data = gb_sets:to_list(FinalDs),
    make_bin_from_pairs(Data).

print_part(Binary, Pos) when Pos>=byte_size(Binary) ->
    io:fwrite("~n");
print_part(Binary, Pos) ->
    io:fwrite("{~p,", [Pos div 4]),
    <<X:4/little-unsigned-integer-unit:8>> = binary:part(Binary, {Pos, 4}),
    io:fwrite("~p} ", [X]),
    print_part(Binary, Pos+4).

print_part(Binary) ->
    print_part(Binary, 0).

print_result(NrNodes, Results) ->
    lists:map(fun(X) ->
                      Part = binary:part(Results, {X*NrNodes*4, NrNodes*4}),
                      print_part(Part)
              end,
              lists:seq(0,(byte_size(Results) div (4*NrNodes))-1)).

apply_to_chunk(_Fun, Chunk, Pos, Acc) when Pos>=byte_size(Chunk) ->
    Acc;
apply_to_chunk(Fun, Chunk, Pos, Acc) ->
    <<X:4/little-unsigned-integer-unit:8>> = binary:part(Chunk, {Pos, 4}),
    Part = Fun(X),
    apply_to_chunk(Fun, Chunk, Pos+4, <<Acc/binary, Part/binary>>).
                                                         

apply_to_chunk(Fun, Chunk) ->
    apply_to_chunk(Fun, Chunk, 0, <<>>).
