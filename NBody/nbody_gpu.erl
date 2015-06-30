-module(nbody_gpu).

-include_lib("../include/cl.hrl").

-compile(export_all).

-import(lists, [map/2, foreach/2, foldl/3]).

-record(kwork,
	{
          program,
	  kernel,
          queue,   %% the queue
	  local,   %% kernel work_group_size
	  freq,    %% device max_clock_frequenct
	  units,   %% device max_compute_units
	  weight,  %% weight [0..1]
	  e1,e2,e3, %% events (fixme)
	  imem,    %% input memory object
	  omem,    %% output memory object
	  isize,   %% item size
	  idata,    %% input data
	  chunkInBuffer,
	  particlesInBuffer,
	  chunkOutBuffer,
	  result
	 }).

get_biggest_local_size(_Size, Limit, Acc, Best) when Acc==Limit ->
    Best;
get_biggest_local_size(Size, Limit, Acc, Best) ->
    NewBest = case Size/Acc == Size div Acc of
		  true -> Acc;
		  false -> Best
	      end,
    get_biggest_local_size(Size, Limit, Acc+1, NewBest).
    
output_first_n(_Binary, 0) ->
    io:fwrite("~n");
output_first_n(Binary, N) ->
    <<X/little-float-unsigned>> = binary:part(Binary,{0,8}),
    <<Y/little-float-unsigned>> = binary:part(Binary,{8,8}),
    <<Z/little-float-unsigned>> = binary:part(Binary,{16,8}),
    <<M/little-float-unsigned>> = binary:part(Binary,{24,8}),
    <<Vx/little-float-unsigned>> = binary:part(Binary,{32,8}),
    <<Vy/little-float-unsigned>> = binary:part(Binary,{40,8}),
    <<Vz/little-float-unsigned>> = binary:part(Binary,{48,8}),
    io:fwrite("{~p,~p,~p,~p,~p,~p,~p,0},~n", [X,Y,Z,M,Vx,Vy,Vz]),
    output_first_n(binary:part(Binary, {64, byte_size(Binary)-64}), N-1).
    
    
do_nbody_gpu(Chunk, Particles, Dt, ChunkSize)	 ->
    E = clu:setup(all),
    {ok,Program} = clu:build_source(E, program(ok)),
    {ok,Kernel} = cl:create_kernel(Program, "nbody_kern"),
    %io:fwrite("Chunk is ~p~n", [Chunk]),
    Kws = map(
	    fun(Device) ->
		    {ok,Queue} = cl:create_queue(E#cl.context,Device,[]),
		    {ok,Local} = cl:get_kernel_workgroup_info(Kernel,Device,
							      work_group_size),
		    {ok,Freq} = cl:get_device_info(Device,max_clock_frequency),
		    {ok,K} = cl:get_device_info(Device, max_compute_units),
		    #kwork{ queue=Queue, local=Local, freq=Freq, units=K, kernel=Kernel, program=Program}
	    end, E#cl.devices),
    Kws3 = map(
	     fun(K) ->
		     {ok,ChunkInBuffer} = cl:create_buffer(E#cl.context,[read_only],byte_size(Chunk)),
		     {ok,ParticlesBuffer}  = cl:create_buffer(E#cl.context,[read_only],byte_size(Particles)),
		     {ok,ChunkOutBuffer}  = cl:create_buffer(E#cl.context,[write_only],byte_size(Chunk)),
		     K#kwork {chunkInBuffer=ChunkInBuffer, particlesInBuffer=ParticlesBuffer, 
			      chunkOutBuffer = ChunkOutBuffer }
	     end, Kws),

    Global = ChunkSize,
    LocalSize = get_biggest_local_size(Global, 768, 1, 1),
    io:fwrite("Global size ~p, local size ~p~n", [Global,LocalSize]),
    %% Enque input data
    Kws4 = map(
	     fun(K) ->
		     {ok,E1} = cl:enqueue_write_buffer(K#kwork.queue,
						       K#kwork.chunkInBuffer,
						       0, byte_size(Chunk),
						       Chunk, []),
		     {ok,E2} = cl:enqueue_write_buffer(K#kwork.queue,
						       K#kwork.particlesInBuffer,
						       0, byte_size(Particles),
						       Particles, []),
		     
		     %% Set kernel arguments
		     ok = cl:set_kernel_arg(K#kwork.kernel, 0, K#kwork.chunkInBuffer),
		     ok = cl:set_kernel_arg(K#kwork.kernel, 1, K#kwork.particlesInBuffer),
		     ok = cl:set_kernel_arg(K#kwork.kernel, 2, K#kwork.chunkOutBuffer),
		     %ok = cl:set_kernel_arg_size(K#kwork.kernel, 3, LocalSize*64+100),
		     %ok = cl:set_kernel_arg(K#kwork.kernel, 4, Dt),
                     ok = cl:set_kernel_arg(K#kwork.kernel, 3, Dt),
		     
		     %% Enqueue the kernel
		     {ok,E6} = cl:enqueue_nd_range_kernel(K#kwork.queue,
							  K#kwork.kernel,
							  [Global], [LocalSize],
						     [E1,E2]),
		     case cl:wait(E6,3000) of
			 {ok, Blah} ->
			     Blah;
			 Whatever -> 
			     Whatever
		     end,
		%% Enqueue the read from device memory (wait for kernel to finish)
		     {ok,E7} = cl:enqueue_read_buffer(K#kwork.queue,
						      K#kwork.chunkOutBuffer,
						      0, byte_size(Chunk), [E6]),
		     %% Now flush the queue to make things happend 
		     ok = cl:flush(K#kwork.queue),
		     Result = case cl:wait(E7,3000) of
				  {ok, Data} -> 
				      Data;
				  Res3 ->
				      Res3
			      end,
						%io:fwrite("Result is ~p~n", [Result]),
		     K2 = K#kwork { result=Result},
		     %% FIXME: here we should release E1,E2
		K2#kwork { e1=E1,e2=E2 }
	     end, Kws3),
    %% Wait for Result buffer to be written
    Bs = map(
	   fun(K) ->
		   cl:release_mem_object(K#kwork.particlesInBuffer),
		   cl:release_mem_object(K#kwork.chunkInBuffer),
		   cl:release_mem_object(K#kwork.chunkOutBuffer),
		   cl:release_queue(K#kwork.queue),
                   cl:release_kernel(K#kwork.kernel),
                   cl:release_program(K#kwork.program),
                   K#kwork.result
	   end, Kws4),
    
    clu:teardown(E),
    hd(Bs).

program(ok) -> "
#pragma OPENCL EXTENSION cl_khr_fp64: enable
__kernel void nbody_kern(
       __global double4* chunk_old,
       __global double4* particles,
       __global double* chunk_new,
       const float time
    ) {
  const double eps = 0.0001;
  int gti = get_global_id(0);
  int n = get_global_size(0);
 

  double4 my_part = chunk_old[2*gti];
  double4 my_v = chunk_old[2*gti+1];
  double4 other_part;
  double4 d;
  double4 a = (0,0,0,0); 
  double invr, f;

  
  for (int i=0; i < n; i++) {
    if (i!=gti) {
      other_part = (double4) chunk_old[2*i];
      d = other_part - my_part;
      invr = rsqrt(d.x*d.x + d.y*d.y + d.z*d.z + eps);
      f = other_part.w*invr*invr*invr;
      a += f * d;
    }
  }
  
  barrier (CLK_LOCAL_MEM_FENCE);
  chunk_new[8*gti] = my_part.x + time * my_v.x + 0.5 * time * time * a.x;
  chunk_new[8*gti+1] = my_part.y + time * my_v.y + 0.5 * time * time * a.y;
  chunk_new[8*gti+2] = my_part.z + time * my_v.z + 0.5 * time * time * a.z;
  chunk_new[8*gti+3] = 0; //should be my_part.w, but it gives an error
  chunk_new[8*gti+4] = my_v.x + time * a.x;
  chunk_new[8*gti+5] = my_v.y + time * a.y;
  chunk_new[8*gti+6] = my_v.z + time * a.z;
  chunk_new[8*gti+7] = 0;
}
".

nbody_gpu(Chunk, Particles, Dt) ->
    do_nbody_gpu(Chunk, Particles, Dt, byte_size(Chunk) div 64).

