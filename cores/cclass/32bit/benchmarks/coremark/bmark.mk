#=======================================================================
# UCB CS250 Makefile fragment for benchmarks
#-----------------------------------------------------------------------
#
# Each benchmark directory should have its own fragment which
# essentially lists what the source files are and how to link them
# into an riscv and/or host executable. All variables should include
# the benchmark name as a prefix so that they are unique.
#

coremark_c_src = \
	core_list_join.c \
	core_main.c \
	core_matrix.c \
	core_state.c \
	core_util.c \
	core_portme.c \
	ee_printf.c \
	syscalls.c \

coremark_riscv_src = \
	crt.S \

coremark_c_objs     = $(patsubst %.c, %.o, $(coremark_c_src))
coremark_riscv_objs = $(patsubst %.S, %.o, $(coremark_riscv_src))

coremark_host_bin = coremark.host
$(coremark_host_bin): $(coremark_c_src)
	$(HOST_COMP) $^ -o $(coremark_host_bin)

bmarks_defs = $(bmarks_defs) -DPERFORMANCE_RUN=1 -DITERATIONS=1000

coremark_riscv_bin = coremark.riscv
$(coremark_riscv_bin): $(coremark_c_objs) $(coremark_riscv_objs)
	$(RISCV_LINK) $(coremark_c_objs) $(coremark_riscv_objs) \
    -o $(coremark_riscv_bin) $(RISCV_LINK_OPTS)

junk += $(coremark_c_objs) $(coremark_riscv_objs) \
        $(coremark_host_bin) $(coremark_riscv_bin)
