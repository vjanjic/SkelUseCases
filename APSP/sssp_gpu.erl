-module(sssp_gpu).

-compile(export_all).

program(ok) -> "
__kernel void nbody_kern(
                         __global int *dist_mat,
                         __global uint *indices,
                                  uint nr_nodes,
                         __global uint *out
                         __global uint *marked
                        )
{
  uint gtid = get_global_id(0);
  uint my_ind = out[gtid];
  uint i, j, min_dist = (uint) 4294967296, min_ind;

  for (i=0; i<nr_nodes; i++) {
    if (i!=my_ind) 
      out[i] = 4294967296;
    else 
      out[i] = 0;
  marked[i] = 0;
  
  for (i=0; i<nr_nodes; i++) {
    for (j=0; j<nr_nodes; j++) 
      if (!marked[j] && out[j] < min_dist) {
        min_dst = out[j];
        min_ind = j;
      }
    
  }

}

".
