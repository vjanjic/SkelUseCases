CL_PATH=../../cl
SKEL_PATH=../../skel-master-working

all: hybrid

hybrid: 
	  erlc main.erl
	  erlc apsp_cpu.erl
	  erlc sssp_binary.erl
	  erlc sssp_list.erl
		erlc sssp_cpu.erl
	  erlc -I $(CL_PATH)/include sssp_gpu.erl

run-seq-list:
		erl -pa $(CL_PATH)/ebin -pa $(SKEL_PATH)/ebin -pa $(CL_PATH)/priv -s main start_seq 0 -noshell -s init stop
run-seq-bin:
		erl -pa $(CL_PATH)/ebin -pa $(SKEL_PATH)/ebin -pa $(CL_PATH)/priv -s main start_seq 1 -noshell -s init stop
run-gpu:
		erl -pa $(CL_PATH)/ebin -pa $(SKEL_PATH)/ebin -pa $(CL_PATH)/priv -s main start_gpu -noshell -s init stop
