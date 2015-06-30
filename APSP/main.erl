-module(main).

-compile(export_all).

floor(X) -> 
    T = erlang:trunc(X),
        case (X - T) of
        Neg when Neg < 0 ->
		T - 1;
	            Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

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

calculate_ratio(TimeRatio, NTasks, NCPUW) ->
    TasksCPU = lists:seq(0, NTasks),
    Time = fun(CPUTasks, GPUTasks) ->
		   Est = max (ceiling(CPUTasks/NCPUW)*TimeRatio, GPUTasks),
		   Est
	   end,
    Ratio = lists:foldl(fun(Elem,Acc) -> FooBar = Time(Elem, NTasks-Elem),
					 if
					     (FooBar < element(1,Acc)) or (element(1,Acc) == -1) 
					     -> {FooBar, Elem};
					     true -> Acc
					 end end,
			{-1,0}, TasksCPU),
    {NTasks-element(2,Ratio), element(2,Ratio)}.

seq_apsp(NrNodes, DistMatrix, Module) ->
    Chunk = Module:make_chunk(0,NrNodes-1),
    sssp_cpu:dijkstra_chunk(NrNodes, DistMatrix, Chunk, Module).
    %Module:print_result(NrNodes,Result).

hyb_sssp(NrNodes, DistMatrix, {gpu, Chunk}) ->
    %Gonada = timer:tc(fun() ->
    sssp_gpu:dijkstra_gpu(Chunk, DistMatrix, NrNodes);
    %io:fwrite("GPU time ~p, tasks ~p, per task ~p~n", [element(1,Gonada), byte_size(Chunk)/4,
%						       element(1,Gonada)/byte_size(Chunk)/4]),
%    element(2, Gonada);
hyb_sssp(NrNodes, DistMatrix, {cpu, Chunk}) ->
    %Gonada = timer:tc(fun() ->
    sssp_cpu:dijkstra_chunk(NrNodes, DistMatrix, Chunk, sssp_binary).
    %io:fwrite("CPU time ~p, tasks ~p, per task ~p~n", [element(1,Gonada), byte_size(Chunk)/4,
%						       element(1,Gonada)/byte_size(Chunk)/4]),
 %   element(2, Gonada).
    

%% Mode determines what version of the CPU code are we using
%%      0 - list
%%      1 - binary
start_seq([ArgMode]) ->
    Mode = list_to_integer(atom_to_list(ArgMode)),
    Module = get_module(Mode),
    DistMatrix = init("input_data",Module),
    NrNodes = Module:get_nr_nodes(DistMatrix),
    Time = timer:tc(fun() -> seq_apsp(NrNodes, DistMatrix, Module) end),
    io:fwrite("Time is ~p~n", [element(1,Time)]).


start_gpu() ->
    Module = sssp_binary,
    DistMatrix = init("input_data",Module),
    NrNodes = Module:get_nr_nodes(DistMatrix),
    Chunk = Module:make_chunk(0,NrNodes-1),
    Time = timer:tc(fun() -> sssp_gpu:dijkstra_gpu(Chunk, DistMatrix, NrNodes) end),
    io:fwrite("Time is ~p~n", [element(1,Time)]).

start_skel_cpu([NW,ArgMode]) ->
    NrWorkers = list_to_integer(atom_to_list(NW)),
    Mode = list_to_integer(atom_to_list(ArgMode)),
    Module = get_module(Mode),
    DistMatrix = init("input_data",Module),
    NrNodes = Module:get_nr_nodes(DistMatrix),
    ChunkSizes = calculate_chunk_size(NrNodes, NrWorkers),
    Chunks = create_chunks(ChunkSizes, 0, Module),
    Map = {map, [{seq, fun(X) -> sssp_cpu:dijkstra_chunk(NrNodes, DistMatrix, X, Module) end}],
           fun(X) -> X end,
           fun(X) -> X end},
    Time = timer:tc(fun() -> skel:do([Map],[Chunks]) end),
    io:fwrite("Time is ~p~n", [element(1,Time)]).

start_skel_hybrid([NCPUW,NGPUW]) ->
    NrCPUWs = list_to_integer(atom_to_list(NCPUW)),
    NrGPUWs = list_to_integer(atom_to_list(NGPUW)),
    DistMatrix = init("input_data",sssp_binary),
    NrNodes = sssp_binary:get_nr_nodes(DistMatrix),
    TimeRatio = 10,
    Ratio = calculate_ratio(TimeRatio,NrNodes,NrCPUWs),
    %io:fwrite("Ratio is ~p~n", [Ratio]),
    ChunkSizesCPU = calculate_chunk_size(element(2,Ratio), NrCPUWs),
    %io:fwrite("Grgec je ~p~n", [[[element(1,Ratio) | ChunkSizesCPU]]]),
    Chunks = create_chunks([{element(1,Ratio)} | ChunkSizesCPU], 0, sssp_binary),
    GPUChunk = {gpu, hd(Chunks)},
    CPUChunks = lists:map(fun(X) -> {cpu, X} end, tl(Chunks)),
    Work = [GPUChunk | CPUChunks],
    Map = {map, [{seq, fun(X) -> hyb_sssp(NrNodes, DistMatrix, X) end}],
           fun(X) -> X end,
           fun(X) -> X end},
    Time = timer:tc(fun() -> skel:do([Map],[Work]) end),
    io:fwrite("CPU ~p GPU ~p Time ~p~n", [NrCPUWs, NrGPUWs, element(1,Time)]).
    
