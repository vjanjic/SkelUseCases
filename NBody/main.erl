-module(main).

-compile(export_all).

get_module(Mode) ->
    case Mode of 
        0 ->
            nbody_list;
        1 ->
            nbody_binary;
        2 -> 
            nbody_ets
    end.

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

do_nbody_hybrid({cpu, Chunk} ,Particles, Dt) ->
    Gormenghast = timer:tc(fun() -> 
    %seq_nbody_binary:nbody(Chunk, Particles, Dt)
    nbody_binary:nbody_chunk(Chunk, Particles, Dt)				   
			      end),			      
    io:fwrite("CPU chunk in time ~p, length is ~p, per unit is ~p~n", [element(1,Gormenghast), byte_size(Chunk), element(1,Gormenghast)/byte_size(Chunk)]),
    element(2,Gormenghast);
do_nbody_hybrid({gpu, Chunk}, Particles, Dt) ->
    Gormenghast = timer:tc(fun() -> 
    nbody_gpu:nbody_gpu(Chunk, Particles, Dt)
			      end),			      
    io:fwrite("GPU chunk in time ~p, length is ~p, per unit is ~p~n", [element(1,Gormenghast), byte_size(Chunk), element(1,Gormenghast)/byte_size(Chunk)]),
    %io:fwrite("GPU chunk in time ~p, length is ~p~n", [element(1,Gormenghast), byte_size(Chunk)]),
    element(2,Gormenghast).

open_file(Fname) ->
    case file:open(Fname, [read, raw]) of
        {ok, Fd} ->
            Fd;
        {error, Reason} ->
            Reason
    end.  

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

combine(X) -> X.

create_input_list(_TaskChunkGPU, [], 0, _ChunkSize, _RemainingParticles) ->
    [];
create_input_list(TaskChunkGPU, [HTaskChunksCPU|TTaskChunksCPU], 0, ChunkSize, RemainingParticles) ->
    ThisChunkSizeInBytes = 64*ChunkSize*element(1,HTaskChunksCPU),
    [ {cpu, binary:part(RemainingParticles, {0,ThisChunkSizeInBytes})} |
      create_input_list(TaskChunkGPU, TTaskChunksCPU, 0, ChunkSize, binary:part(RemainingParticles, {ThisChunkSizeInBytes, byte_size(RemainingParticles) - ThisChunkSizeInBytes})) ];
create_input_list(TaskChunkGPU, TaskChunksCPU, NGPUWs, ChunkSize, RemainingParticles) ->
    GPUChunkSizeInBytes = 64*ChunkSize*TaskChunkGPU,
    %io:fwrite("GPUChunkSizeInBytes is ~p~n", [GPUChunkSizeInBytes]),
    [ {gpu, binary:part(RemainingParticles, {0, GPUChunkSizeInBytes})} |
      create_input_list(TaskChunkGPU, TaskChunksCPU, NGPUWs-1, ChunkSize, binary:part(RemainingParticles, {GPUChunkSizeInBytes, byte_size(RemainingParticles) - GPUChunkSizeInBytes})) ].

calculate_chunk_size(0, _NrWorkers) ->
    [];
calculate_chunk_size(_NrTasks, 0) ->
    [];
calculate_chunk_size(NrTasks, NrWorkers) ->
    ChunkSize = NrTasks div NrWorkers,
    Remainder = NrTasks rem NrWorkers,
    ChunkSizes = lists:duplicate(Remainder, {ChunkSize+1}) ++ lists:duplicate(NrWorkers-Remainder, {ChunkSize}),
    ChunkSizes.

start_seq([Mode]) ->
    Module = get_module(list_to_integer(atom_to_list(Mode))),
    Fd = open_file("input_data"),
    Particles = Module:init(Fd),
    Dt = 0.000001,
    Time = timer:tc(fun() -> Module:nbody_chunk(Particles, Particles, Dt) end),
    io:fwrite("SEQ ~p~n", [element(1,Time)]).
    

start_skel_cpu([NW,Mode]) ->
    NrCPUWs = list_to_integer(atom_to_list(NW)),
    Module = get_module(list_to_integer(atom_to_list(Mode))),
    Fd = open_file("input_data"),
    Particles = Module:init(Fd),
    NrParticles = Module:get_nr_particles(Particles),
    Dt = 0.000001,
    ChunkSizes = calculate_chunk_size(NrParticles, NrCPUWs),
    Work = Module:create_work(Particles, ChunkSizes),
    Map = {map, [{seq, fun(X) ->
                              Module:nbody_chunk(X, Particles, Dt)
                      end}],
           fun(X) -> X end,
           fun(X) -> X end},
    Time = timer:tc(fun() -> skel:do([Map],[Work]) end),
    io:fwrite("CPU ~p Time ~p~n", [NrCPUWs, element(1, Time)]).
    

start_skel_hyb([NC,NG]) ->      
    Fd = open_file("input_data"),
    Particles = nbody_binary:init(Fd),
    Dt = 0.000001,
    NrCPUWs = list_to_integer(atom_to_list(NC)),
    NrGPUWs = list_to_integer(atom_to_list(NG)),
    ChunkSize = 10,
    %OneChunk = binary_part(Particles, {0, 64*ChunkSize}),
    %GPUTimes = lists:map (fun(_X) -> element (1, timer:tc( fun() -> nbody_gpu:nbody_gpu(OneChunk, Particles, Dt) end)) end, [1,1,1,1,1,1,1,1,1,1]),
    %CPUTimes = lists:map (fun(_X) -> element (1, timer:tc( fun() -> seq_nbody_binary:nbody(OneChunk, Particles, Dt) end)) end, [1]),
    %TimeRatio = (lists:sum(CPUTimes) / length(CPUTimes)) /  (lists:sum(GPUTimes) / length(GPUTimes)),
    %io:fwrite("Time ratio is ~p~n", [TimeRatio]),
    TimeRatio = 150,
    NTasks = byte_size(Particles) div (ChunkSize*64),
    {GPUTasks, CPUTasks} = calculate_ratio(TimeRatio, NTasks, NrCPUWs),
    io:fwrite("Ratio is ~p~n", [{GPUTasks,CPUTasks}]),
    Map = {map, [{seq, fun(X) -> do_nbody_hybrid(X,Particles, Dt) end}],
           fun(X) -> create_input_list(GPUTasks, calculate_chunk_size(CPUTasks, NrCPUWs), NrGPUWs, ChunkSize, X) end,
           fun(X) -> X end},
    Time = timer:tc(fun() -> skel:do([Map],[Particles]) end),
    io:fwrite("CPU ~p GPU ~p Time ~p~n", [NrCPUWs, NrGPUWs, element(1, Time)]).

