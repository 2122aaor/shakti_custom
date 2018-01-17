
default: project

base_dir = $(abspath ../)
proj_dir = $(abspath .)
#mem_gen = $(base_dir)/fpga/common/fpga_mem_gen
generated_dir = $(abspath ./generated-src)

glip_dir = $(base_dir)/opensocdebug/glip/src
osd_dir = $(base_dir)/opensocdebug/hardware
example_dir = $(base_dir)/fpga/bare_metal/examples

project_name = shakti-nexys4
BACKEND ?= shakti_chip.LowRISCBackend
CONFIG ?= Nexys4DebugConfig

VIVADO = vivado
boot_mem = constraints/code.mem

#include $(base_dir)/Makefrag

.PHONY: default

#--------------------------------------------------------------------
# Sources
#--------------------------------------------------------------------


verilog_srcs = \
	$(base_dir)/nexys4_ddr/Top.v \
	$(base_dir)/verilog_fpga/BRAM2.v \
	$(base_dir)/verilog_fpga/BRAM2Load.v \
	$(base_dir)/verilog_fpga/BRAM2BELoad.v \
	$(base_dir)/verilog_fpga/Counter.v \
	$(base_dir)/verilog_fpga/FIFO1.v \
	$(base_dir)/verilog_fpga/FIFO2.v \
	$(base_dir)/verilog_fpga/FIFOL1.v \
	$(base_dir)/verilog_fpga/RevertReg.v \
	$(base_dir)/verilog_fpga/SizedFIFO.v \
	$(base_dir)/verilog_fpga/mkproc.v \
	$(base_dir)/verilog_fpga/mkcore.v \
	$(base_dir)/verilog_fpga/mkcsr.v \
	$(base_dir)/verilog_fpga/mkdcache.v \
	$(base_dir)/verilog_fpga/mkexecution_unit.v \
	$(base_dir)/verilog_fpga/mkfpu.v \
	$(base_dir)/verilog_fpga/mkicache.v \
	$(base_dir)/verilog_fpga/mkmemory_unit.v \
	$(base_dir)/verilog_fpga/mkmuldiv.v \
	$(base_dir)/verilog_fpga/mkregisterfile.v \
	$(base_dir)/verilog_fpga/mkbpu_bimodal.v \
	$(base_dir)/verilog_fpga/mkriscv.v \
	$(base_dir)/verilog_fpga/module_fn_branch.v \
	$(base_dir)/verilog_fpga/module_fn_decoder.v \

#--------------------------------------------------------------------
# Project generation
#--------------------------------------------------------------------


project = $(project_name)/$(project_name).xpr
project: $(project)
$(project):  
	ln -f -s $(base_dir)/nexys4_ddr/constraints/code.mem $(base_dir)/verilog_fpga/code.mem
	ln -f -s $(base_dir)/nexys4_ddr/constraints/config_string.hex $(base_dir)/verilog_fpga/config_string.hex
	$(VIVADO) -mode batch -source make_project.tcl -tclargs $(project_name) $(CONFIG)

vivado: $(project)
	$(VIVADO) $(project) &

bitstream = $(project_name)/$(project_name).runs/impl_1/Top.bit
bitstream: $(bitstream)
$(bitstream): $(shakti_srcs)  $(shakti_headers) $(verilog_srcs) $(verilog_headers) | $(project)
	ln -f -s $(base_dir)/nexys4_ddr/constraints/code.mem $(base_dir)/verilog_fpga/code.mem
	ln -f -s $(base_dir)/nexys4_ddr/constraints/config_string.hex $(base_dir)/verilog_fpga/config_string.hex
	$(VIVADO) -mode batch -source ./make_bitstream.tcl -tclargs $(project_name)

program:
	$(VIVADO) -mode batch -source ./program.tcl -tclargs "xc7a100t_0" $(bitstream)

.PHONY: project vivado bitstream program

search-ramb: constraints/boot.bmm
constraints/boot.bmm:
	  $(VIVADO) -mode batch -source ./search_bram.tcl -tclargs $(project_name) > search-ramb.log 
		python gen_bmm.py search-ramb.log constraints/code.bmm 32 131072

bit-update: $(project_name)/$(project_name).runs/impl_1/Top.new.bit
$(project_name)/$(project_name).runs/impl_1/Top.new.bit: $(boot_mem) constraints/code.bmm
	data2mem -u -bm $< -bd $(boot_mem) -bt $(bitstream) -o b $@
#--------------------------------------------------------------------
# Clean up
#--------------------------------------------------------------------

clean:
	$(info To clean everything, including the Vivado project, use 'make cleanall')
	-rm -rf *.log *.jou $(junk)

cleanall: clean
	-rm -fr $(project)
	-rm -fr $(project_name)
	-rm -fr constraints/boot.bmm

.PHONY: clean cleanall
