-module(sssp_gpu).

-compile(export_all).

program(ok) -> "
__kernel void nbody_kern(
                         __global uint *dist_mat,
                         __global uint *indices
                         __global uint *out
                        )
{
  int gtid = get_global_id(0);
  int my_ind = out[gtid];
  
}

".
