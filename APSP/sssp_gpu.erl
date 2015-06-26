-module(sssp_gpu).

-compile(export_all).

-include_lib("cl.hrl").

get_biggest_local_size(_Size, Limit, Acc, Best) when Acc==Limit ->
    Best;
get_biggest_local_size(Size, Limit, Acc, Best) ->
    NewBest = case Size/Acc == Size div Acc of
		  true -> Acc;
		  false -> Best
	      end,
    get_biggest_local_size(Size, Limit, Acc+1, NewBest).

dijkstra_gpu(Chunk, DistMatrix, NrNodes) ->
    %% Set up cl environment, build kernel
    E = clu:setup(all),
    {ok,Program} = clu:build_source(E, program(ok)),
    {ok,Kernel} = cl:create_kernel(Program, "dijkstra"),
    %% Get gpu device
    GPUs = lists:filter(fun(X) -> {ok, Type} = cl:get_device_info(X, type), hd(Type) == gpu end,
                        E#cl.devices),
    Device = hd(GPUs),
    %% Get maximum local size, so that we can compute a good local size
    {ok,MaxLocalSize} = cl:get_kernel_workgroup_info(Kernel,Device,
                                              work_group_size),
    %% Create a queue on the GPU and all the necessary buffers
    {ok,Queue} = cl:create_queue(E#cl.context,Device,[]),
    {ok,DistMatrixInBuffer} = cl:create_buffer(E#cl.context,[read_only],byte_size(DistMatrix)),
    {ok,IndicesInBuffer}  = cl:create_buffer(E#cl.context,[read_only], byte_size(Chunk)),
    {ok,OutBuffer}  = cl:create_buffer(E#cl.context,[write_only], byte_size(Chunk)*NrNodes),
    {ok,MarkedOutBuffer}  = cl:create_buffer(E#cl.context,[write_only],byte_size(Chunk)*NrNodes),
    %% Set local and global size
    Global = byte_size(Chunk) div 4,
    LocalSize = get_biggest_local_size(Global, MaxLocalSize, 1, 1),
    %% Enque input data
    {ok,E1} = cl:enqueue_write_buffer(Queue,
                                      DistMatrixInBuffer,
                                      0, byte_size(DistMatrix),
                                      DistMatrix, []),
    {ok,E2} = cl:enqueue_write_buffer(Queue,
                                      IndicesInBuffer,
                                      0, byte_size(Chunk),
                                      Chunk, []),
    %% Set kernel arguments
    ok = cl:set_kernel_arg(Kernel, 0, DistMatrixInBuffer),
    ok = cl:set_kernel_arg(Kernel, 1, IndicesInBuffer),
    ok = cl:set_kernel_arg(Kernel, 2, NrNodes),
    ok = cl:set_kernel_arg(Kernel, 3, OutBuffer),
    ok = cl:set_kernel_arg(Kernel, 4, MarkedOutBuffer),
    %% Enqueue the kernel
    {ok,E3} = cl:enqueue_nd_range_kernel(Queue,
                                         Kernel,
                                         [Global], [LocalSize],
                                         [E1,E2]),
		%% Enqueue the read from device memory (wait for kernel to finish)
    {ok,E4} = cl:enqueue_read_buffer(Queue,
                                     OutBuffer,
                                     0, byte_size(Chunk)*NrNodes, [E3]),
    %% Now flush the queue to make things happend 
    ok = cl:flush(Queue),
    Result = case cl:wait(E4,3000000) of
                 {ok, Data} -> 
                     Data;
                 Res3 ->
                     Res3
             end,
    cl:release_mem_object(DistMatrixInBuffer),
    cl:release_mem_object(IndicesInBuffer),
    cl:release_mem_object(OutBuffer),
    cl:release_mem_object(MarkedOutBuffer),
    cl:release_queue(Queue),
    cl:release_kernel(Kernel),
    cl:release_program(Program),
    clu:teardown(E),
    Result.
    


program(ok) -> "
__kernel void dijkstra(
                         __global uint *dist_mat,
                         __global uint *indices,
                                  uint nr_nodes,
                         __global uint *out,
                         __global uint *marked
                        )
{
  uint gtid = get_global_id(0);
  uint my_ind = indices[gtid];
  uint i, j, min_dist, min_ind;

  barrier(CLK_LOCAL_MEM_FENCE);
  for (i=0; i<nr_nodes; i++) {
    if (i!=my_ind) 
      out[gtid*nr_nodes+i] = (uint) 4294967295;
    else 
      out[gtid*nr_nodes+i] = 0;
    marked[gtid*nr_nodes+i] = 0;
  }
  
  for (i=0; i<nr_nodes; i++) {
    min_dist = (uint) 4294967295;
    for (j=0; j<nr_nodes; j++) 
      if (!marked[j] && out[gtid*nr_nodes+j] < min_dist) {
        min_dist = out[gtid*nr_nodes+j];
        min_ind = j;
      }
    marked[gtid*nr_nodes+min_ind] = 1;
    for (j=0; j<nr_nodes; j++) 
      if (!marked[gtid*nr_nodes+j] && min_dist + dist_mat[min_ind*nr_nodes+j] < out[gtid*nr_nodes+j]) 
        out[gtid*nr_nodes+j] = min_dist + dist_mat[min_ind*nr_nodes+j];
  }
}
".
