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

seq_apsp(NrNodes, DistMatrix, Module) ->
    Chunk = Module:make_chunk(0,NrNodes-1),
    Result = lists:map(fun(X) ->
                               sssp_cpu:dijkstra(NrNodes, X, DistMatrix, Module) end,
                       Chunk),
    io:fwrite("Krklec~n"),
    Module:print_result(NrNodes,Result).

%% Mode determines what version of the CPU code are we using
%%      0 - list
%%      1 - binary
start([NN, ArgMode]) ->
    NrNodes = list_to_integer(atom_to_list(NN)),
    Mode = list_to_integer(atom_to_list(ArgMode)),
    Module = get_module(Mode),
    DistMatrix = init("input_data",Module),
    %Chunk = Module:make_chunk(0,NrNodes-1),
    %Time = timer:tc(fun() -> sssp_gpu:dijkstra_gpu(Chunk, DistMatrix, NrNodes) end),
    %Time = timer:tc(fun() -> sssp_cpu:dijkstra(NrNodes, 1, DistMatrix, Module) end),
    %io:fwrite("Time is ~p~n", [element(1,Time)]),
    %Module:print_result(element(2,Time)).
    seq_apsp(NrNodes, DistMatrix, Module).

