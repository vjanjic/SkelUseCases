-module(nbody_ets).

-compile(export_all).

-define(table, particles).

create() ->
    ?table = ets:new(?table, 
		     [set, public, named_table,
		      {write_concurrency, true}, {read_concurrency, true}]).

restore(X) ->
    true = ets:delete(?table),
    X.

get_tuple_list([]) ->
    [];
get_tuple_list(Data) ->
    {Raw, Rest} = lists:split(8, Data),
    Particle = list_to_tuple(Raw),
    [Particle|get_tuple_list(Rest)].

init(Fd) ->
    case file:read_line(Fd) of
        {ok,Data} ->
            AllData = lists:map(fun(X) -> {Float, _} = string:to_float(X), Float end,
                                string:tokens(Data," ")),
            Particles = get_tuple_list(AllData),
            Particles;
        {error, Report} ->
            Report
    end.

create_work_worker(_Particles, []) ->
    [];
create_work_worker(Particles, [HChunkSize|TChunkSizes]) ->
    {Chunk, Rest} = lists:split(element(1,HChunkSize), Particles),
    true = ets:insert_new(?table, {I, Chunk}),
    [ I | create_work_worker(Rest, TChunkSizes) ].

create_work(Particles, ChunkSizes) ->
    create(),
    true = ets:insert_new(?table, {0, Particles}),
    create_work_worker(Particles, ChunkSizes).

add_to_acc (OrigPart, Sum, Part) ->
    {Sum1,Sum2,Sum3} = Sum,
    {X1,Y1,Z1,_,_,_,_,_} = OrigPart,
    {X2,Y2,Z2,M,_,_,_,_} = Part,
    Diff = {X2-X1,Y2-Y1,Z2-Z1},
    {Dx,Dy,Dz} = Diff,
    Invr = 1 / math:sqrt(Dx*Dx+Dy*Dy*+Dz*Dz+0.0001),
    Invr3 = M*Invr*Invr*Invr,
    {Sum1+Invr3*Dx, Sum2+Invr3*Dy, Sum3+Invr3*Dz}.

calc_acc_vector (Part,Particles) ->
    lists:foldl(fun(X,Sum) -> add_to_acc(X,Sum,Part) end, {0,0,0}, Particles).

calc_nbody(Part, Particles,Dt) -> 
    {Ax,Ay,Az} = calc_acc_vector (Part, Particles),
    {X,Y,Z,M,Vx,Vy,Vz,Rest} = Part,
    Xnew = X + Dt*Vx + 0.5*Dt*Dt*Ax,
    Ynew = Y + Dt*Vy + 0.5*Dt*Dt*Ay,
    Znew = Z + Dt*Vz + 0.5*Dt*Dt*Az,
    Vxnew = Vx + Dt*Ax,
    Vynew = Vy + Dt*Ay,
    Vznew = Vz + Dt*Az,
    {Xnew,Ynew,Znew,M,Vxnew,Vynew,Vznew,Rest}.

nbody_chunk(I, Dt) ->
    [{0, Particles}] = ets:lookup(?table, 0), 
    [{I, Chunk}] = ets:lookup(?table, I),
    R = lists:map (fun(X) -> calc_nbody (X,Particles,Dt) end, Chunk),
    ets:insert(?table, {I, R}),
    I.

