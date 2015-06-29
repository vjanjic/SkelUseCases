-module(main).

-compile(export_all).

get_module(Mode) ->
    case Mode of 
        0 ->
            sssp_list;
        1 ->
            sssp_binary
    end.

open_file(Fname) ->
    case file:open(Fname, [read, raw]) of
        {ok, Fd} ->
            Fd;
        {error, Reason} ->
            Reason
    end.  

init(Fname,Module) ->
    Fd = open_file(Fname),
    Module:get_data(Fd).

create_chunks([], _Ind, _Module) ->
    [];
create_chunks([HChunkSize|TChunkSizes], Ind, Module) ->
    ChunkSize = element(1,HChunkSize),
    Chunk = Module:make_chunk(Ind, Ind+ChunkSize-1),
    [Chunk | create_chunks(TChunkSizes, Ind+ChunkSize, Module)].

calculate_chunk_size(0, _NrWorkers) ->
    [];
calculate_chunk_size(_NrTasks, 0) ->
    [];
calculate_chunk_size(NrTasks, NrWorkers) ->
    ChunkSize = NrTasks div NrWorkers,
    Remainder = NrTasks rem NrWorkers,
    ChunkSizes = lists:duplicate(Remainder, {ChunkSize+1}) ++ lists:duplicate(NrWorkers-Remainder, {ChunkSize}),
    ChunkSizes.

seq_apsp(NrNodes, DistMatrix, Module) ->
    Chunk = Module:make_chunk(0,NrNodes-1),
    sssp_cpu:dijkstra_chunk(NrNodes, DistMatrix, Chunk, Module).
    %Module:print_result(NrNodes,Result).

%% Mode determines what version of the CPU code are we using
%%      0 - list
%%      1 - binary
start_seq([NN, ArgMode]) ->
    NrNodes = list_to_integer(atom_to_list(NN)),
    Mode = list_to_integer(atom_to_list(ArgMode)),
    Module = get_module(Mode),
    DistMatrix = init("input_data",Module),
    Time = timer:tc(fun() -> seq_apsp(NrNodes, DistMatrix, Module) end),
    io:fwrite("Time is ~p~n", [element(1,Time)]).


start_gpu([NN]) ->
    NrNodes = list_to_integer(atom_to_list(NN)),
    Module = sssp_binary,
    DistMatrix = init("input_data",Module),
    Chunk = Module:make_chunk(0,NrNodes-1),
    Time = timer:tc(fun() -> sssp_gpu:dijkstra_gpu(Chunk, DistMatrix, NrNodes) end),
    io:fwrite("Time is ~p~n", [element(1,Time)]).

start_skel_cpu([NN,NW,ArgMode]) ->
    NrNodes = list_to_integer(atom_to_list(NN)),
    NrWorkers = list_to_integer(atom_to_list(NW)),
    Mode = list_to_integer(atom_to_list(ArgMode)),
    Module = get_module(Mode),
    DistMatrix = init("input_data",Module),
    ChunkSizes = calculate_chunk_size(NrNodes, NrWorkers),
    Chunks = create_chunks(ChunkSizes, 0, Module),
    Map = {map, [{seq, fun(X) -> sssp_cpu:dijkstra_chunk(NrNodes, DistMatrix, X, Module) end}],
           fun(X) -> X end,
           fun(X) -> X end},
    Time = timer:tc(fun() -> skel:do([Map],[Chunks]) end),
    io:fwrite("Time is ~p~n", [element(1,Time)]).
    
