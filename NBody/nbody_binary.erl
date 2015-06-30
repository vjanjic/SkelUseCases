-module(nbody_binary).

-compile(export_all).

init(Fname) ->
    Fd = open_file(Fname),
    case file:read_line(Fd) of
        {ok,Data} ->
            Particles = lists:foldl(fun(X,Acc) -> {Str, _} = string:to_float(X), FloatBin = <<Str/little-float-unsigned>>, 
                                                  <<Acc/binary,FloatBin/binary>>  end, <<>>, string:tokens(Data," ")),
            Particles;
        {error, Report} ->
            Report
    end.

decode_particle(BinPart) ->
    <<X/little-float-unsigned>> = binary:part(BinPart,{0,8}),
    <<Y/little-float-unsigned>> = binary:part(BinPart,{8,8}),
    <<Z/little-float-unsigned>> = binary:part(BinPart,{16,8}),
    <<M/little-float-unsigned>> = binary:part(BinPart,{24,8}),
    <<Vx/little-float-unsigned>> = binary:part(BinPart,{32,8}),
    <<Vy/little-float-unsigned>> = binary:part(BinPart,{40,8}),
    <<Vz/little-float-unsigned>> = binary:part(BinPart,{48,8}),
    {X,Y,Z,M,Vx,Vy,Vz,0}.

calc_acc_vector_list_bin(_Coor, Particles, _Dt, Acc, Ind) when Ind >= byte_size(Particles) ->
    Acc;
calc_acc_vector_list_bin({X,Y,Z}, Particles, Dt, {Ax,Ay,Az}, Ind) ->
    <<X1/little-float-unsigned, Y1/little-float-unsigned, Z1/little-float-unsigned, M/little-float-unsigned>> = binary:part(BinParticles, {Ind,32}),
    {Dx,Dy,Dz} = {X1-X,Y1-Y,Z1-Z},
    Invr = 1 / math:sqrt(Dx*Dx+Dy*Dy*+Dz*Dz+0.0001),
    Invr3 = M*Invr*Invr*Invr,
    calc_acc_vector_list_bin({X,Y,Z}, Particles, Dt, {Ax+Invr3*Dx, Ay+Invr3*Dy, Az+Invr3*Dz}, Ind+64).
    
calc_nbody_particle(Chunk, Particles, Dt, Ind) ->
    {X,Y,Z,M,Vx,Vy,Vz,_} = decode_particle(binary:part(Chunk, {Ind, 64}),
    {Ax,Ay,Az} = calc_acc_vector_list_bin({X,Y,Z}, BinParticles, Dt, {0,0,0}, 0),
    %{Ax,Ay,Az} = {1,1,1},
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

nbody_cpu(Chunk, Particles, Dt) ->
    nbody_cpu_worker(Chunk, Particles, Dt, 0).

