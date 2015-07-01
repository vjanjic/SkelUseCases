-module(nbody_binary).

-compile(export_all).

init(Fd) ->
    case file:read_line(Fd) of
        {ok,Data} ->
            Particles = lists:foldl(fun(X,Acc) -> {Str, _} = string:to_float(X), FloatBin = <<Str/little-float-unsigned>>, 
                                                  <<Acc/binary,FloatBin/binary>>  end, <<>>, string:tokens(Data," ")),
            Particles;
        {error, Report} ->
            Report
    end.

get_nr_particles(Particles) ->
    byte_size(Particles) div 64.

create_work(Particles, ChunkSizes) ->
    create_work_worker(Particles, ChunkSizes, 0).

create_work_worker(_Particles, [], _Ind) ->
    [];
create_work_worker(Particles, [HChunkSize|TChunkSizes], Ind) ->
    io:fwrite("Element is ~p~n", [HChunkSize]),
    ThisChunkSizeInBytes = 64*element(1,HChunkSize),
    [ binary:part(Particles, {Ind, ThisChunkSizeInBytes}) |
      create_work_worker(Particles, TChunkSizes, Ind+ThisChunkSizeInBytes) ].


decode_particle(BinPart) ->
    <<X/little-float-unsigned, Y/little-float-unsigned,
      Z/little-float-unsigned, M/little-float-unsigned,
      Vx/little-float-unsigned, Vy/little-float-unsigned,
      Vz/little-float-unsigned >> = binary:part(BinPart,{0,56}),
    {X,Y,Z,M,Vx,Vy,Vz,0}.

calc_acc_vector_list_bin(_Coor, Particles, _Dt, Acc, Ind) when Ind >= byte_size(Particles) ->
    Acc;
calc_acc_vector_list_bin({X,Y,Z}, Particles, Dt, {Ax,Ay,Az}, Ind) ->
    <<X1/little-float-unsigned, Y1/little-float-unsigned, Z1/little-float-unsigned, M/little-float-unsigned>> = binary:part(Particles, {Ind,32}),
    {Dx,Dy,Dz} = {X1-X,Y1-Y,Z1-Z},
    Invr = 1 / math:sqrt(Dx*Dx+Dy*Dy*+Dz*Dz+0.0001),
    Invr3 = M*Invr*Invr*Invr,
    calc_acc_vector_list_bin({X,Y,Z}, Particles, Dt, {Ax+Invr3*Dx, Ay+Invr3*Dy, Az+Invr3*Dz}, Ind+64).
    
calc_nbody_particle(Chunk, Particles, Dt, Ind) ->
    {X,Y,Z,M,Vx,Vy,Vz,_} = decode_particle(binary:part(Chunk, {Ind, 64})),
    {Ax,Ay,Az} = calc_acc_vector_list_bin({X,Y,Z}, Particles, Dt, {0,0,0}, 0),
    Xnew = X + Dt*Vx + 0.5*Dt*Dt*Ax,
    Ynew = Y + Dt*Vy + 0.5*Dt*Dt*Ay,
    Znew = Z + Dt*Vz + 0.5*Dt*Dt*Az,
    Vxnew = Vx + Dt*Ax,
    Vynew = Vy + Dt*Ay,
    Vznew = Vz + Dt*Az,
    {Xnew,Ynew,Znew,M,Vxnew,Vynew,Vznew,0}.
    
nbody_cpu_worker(Chunk, _Particles, _Dt, Ind) when Ind >= byte_size(Chunk) ->
    [];
nbody_cpu_worker(Chunk, Particles, Dt, Ind) ->
    NewParticle = calc_nbody_particle(Chunk, Particles, Dt, Ind),
    [NewParticle | nbody_cpu_worker(Chunk, Particles, Dt, Ind+64)].

nbody_chunk(Chunk, Particles, Dt) ->
    nbody_cpu_worker(Chunk, Particles, Dt, 0).

