#=======================================================================
# UCB VLSI FLOW: Makefile for riscv-bmarks
#-----------------------------------------------------------------------
# Yunsup Lee (yunsup@cs.berkeley.edu)
#

XLEN ?= 32

default: all

src_dir = .

instname = shakti-bmarks
instbasedir = ./outputfiles
base_address = 2147483648



#--------------------------------------------------------------------
# Sources
#--------------------------------------------------------------------

bmarks =	coremark \
	dhrystone \
	median \
	qsort \
	rsort \
	towers \
	vvadd \
	multiply \
	spmv \
	mt-vvadd \
	mt-matmul 

bmarks_host = coremark \
	dhrystone \
	median \
	qsort \
	towers \
	vvadd \
	multiply \
	spmv \
	vec-vvadd \
	vec-cmplxmult \
	vec-matmul \

#--------------------------------------------------------------------
# Build rules
#--------------------------------------------------------------------

HOST_OPTS = -std=gnu99 -DPREALLOCATE=0 -DHOST_DEBUG=1
HOST_COMP = gcc $(HOST_OPTS)

RISCV_PREFIX ?= riscv$(XLEN)-unknown-elf-
RISCV_GCC ?= $(RISCV_PREFIX)gcc
RISCV_GCC_OPTS ?= -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf
RISCV_LINK ?= $(RISCV_GCC) -T $(src_dir)/common/test.ld $(incs)
RISCV_LINK_MT ?= $(RISCV_GCC) -T $(src_dir)/common/test-mt.ld
RISCV_LINK_OPTS ?= -static -nostartfiles -lgcc -lm
RISCV_OBJDUMP ?= $(RISCV_PREFIX)objdump --disassemble-all --disassemble-zeroes --section=.text --section=.text.startup --section=.data
RISCV_SIM ?= spike --isa=rv$(XLEN)gc
RISCV_HEX = elf2hex 4 32768

VPATH += $(addprefix $(src_dir)/, $(bmarks))
VPATH += $(src_dir)/common

incs  += -I$(src_dir)/../env -I$(src_dir)/common $(addprefix -I$(src_dir)/, $(bmarks))
objs  :=

include $(patsubst %, $(src_dir)/%/bmark.mk, $(bmarks))

#------------------------------------------------------------
# Build and run benchmarks on riscv simulator

copy_shakti:
	cp common/syscalls.c.shakti common/syscalls.c
copy_spike:
	cp common/syscalls.c.spike common/syscalls.c

bmarks_riscv_bin  = $(addsuffix .riscv,  $(bmarks))
bmarks_riscv_dump = $(addsuffix .riscv.dump, $(bmarks))
bmarks_riscv_out  = $(addsuffix .riscv.out,  $(bmarks))
bmarks_riscv_hex =  $(addsuffix .riscv.hex, $(bmarks))

bmarks_defs   = -DPREALLOCATE=1 -DHOST_DEBUG=0 -DITERATIONS=10000 -DPERFORMANCE_RUN=1
bmarks_cycles = 80000

$(bmarks_riscv_dump): %.riscv.dump: %.riscv
	$(RISCV_OBJDUMP) $< > $@

$(bmarks_riscv_out): %.riscv.out: %.riscv
	$(RISCV_SIM) $< > $@

$(bmarks_riscv_hex): %.riscv.hex: %.riscv
	$(RISCV_HEX) $< $(base_address) > $@

%.o: %.c
	$(RISCV_GCC) $(RISCV_GCC_OPTS) $(bmarks_defs) \
	             -c $(incs) $< -o $@

%.o: %.S
	$(RISCV_GCC) $(RISCV_GCC_OPTS) $(bmarks_defs) -D__ASSEMBLY__=1 \
	             -c $(incs) $< -o $@

riscv: $(bmarks_riscv_dump) $(bmarks_riscv_hex)
run-riscv: $(bmarks_riscv_out)
	echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' \
	       $(bmarks_riscv_out); echo;

junk += $(bmarks_riscv_bin) $(bmarks_riscv_dump) $(bmarks_riscv_hex) $(bmarks_riscv_out)

#------------------------------------------------------------
# Build and run benchmarks on host machine

bmarks_host_bin = $(addsuffix .host, $(bmarks_host))
bmarks_host_out = $(addsuffix .host.out, $(bmarks_host))

$(bmarks_host_out): %.host.out: %.host
	./$< > $@

host: $(bmarks_host_bin)
run-host: $(bmarks_host_out)
	echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' \
	       $(bmarks_host_out); echo;

junk += $(bmarks_host_bin) $(bmarks_host_out)

#------------------------------------------------------------
# Default

all: shakti
shakti: copy_shakti riscv install
spike: copy_spike riscv install
#------------------------------------------------------------
# Install

date_suffix = $(shell date +%Y-%m-%d_%H-%M)
install_dir = $(instbasedir)/$(instname)-$(date_suffix)
latest_install = $(shell ls -1 -d $(instbasedir)/$(instname)* | tail -n 1)

install:
	mkdir -p $(instbasedir)
	mv *.o $(bmarks_riscv_bin) $(bmarks_riscv_dump) $(bmarks_riscv_hex) $(instbasedir)

install-link:
	rm -rf $(instbasedir)/$(instname)
	ln -s $(latest_install) $(instbasedir)/$(instname)

#------------------------------------------------------------
# Clean up

clean:
	rm -rf $(objs) $(junk) $(instbasedir)
