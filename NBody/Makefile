all:
		erlc nbody_list.erl
		erlc nbody_binary.erl
		erlc nbody_ets.erl
	  erlc -I ../../cl/include nbody_gpu.erl
		erlc main.erl

run-seq-list:
		erl -s main start_seq 0 -noshell -s init stop

run-seq-bin:
		erl -s main start_seq 1 -noshell -s init stop


