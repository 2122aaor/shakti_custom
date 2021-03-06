### Makefile for the eclass project
### Generated by Bluespec Workstation on Thu Nov 12 19:54:06 IST 2015

ifndef isa
  isa=RV32IMAF
endif

ifneq (,$(findstring M,$(isa)))
  define_macros += -D muldiv=True
endif
ifneq (,$(findstring F,$(isa)))
  define_macros += -D spfpu=True
endif
ifneq (,$(findstring D,$(isa)))
  define_macros += -D dpfpu=True
endif
ifneq (,$(findstring A,$(isa)))
  define_macros += -D atomic=True
endif

ifeq ($(bpu),enable)
  define_macros += -D bpu=True
endif

ifeq ($(axi),enable)
  define_macros += -D AXI=True
endif

ifeq ($(perf),enable)
  define_macros	+= -D perf=True
endif

RUNS:=10

default: check-blue compile_bluesim link_bluesim

AAPG_bsim: check-blue compile_bluesim	link_bluesim	
	@echo "Running Torture Tests now...."
	@cd verification/AAPG; ./make.py clean
	@cd verification/AAPG; ./make.py

AAPG_ncverilog: link_ncverilog
	@echo "Running Torture Tests now...."
	@cd verification/AAPG; ./make.py clean
	@cd verification/AAPG; ./make.py

AAPG_iverilog: link_iverilog
	@echo "Running Torture Tests now...."
	@cd verification/AAPG; ./make.py clean
	@cd verification/AAPG; ./make.py

AAPG_msim: link_msim
	@echo "Running Torture Tests now...."
	@cd verification/AAPG; ./make.py clean
	@cd verification/AAPG; ./make.py

csmith_bsim: compile_bluesim link_bluesim
	@echo "Running CSMITH on BLUESIM....."
	@cd verification/csmith_run; make clean;
	@cd verification/csmith_run; make RUNS=$(RUNS);
  
csmith_ncverilog: link_ncverilog
	@echo "Running CSMITH on NCVERILOG....."
	@cd verification/csmith_run; make clean;
	@cd verification/csmith_run; make RUNS=$(RUNS);
  
csmith_msim: link_msim
	@echo "Running CSMITH on MODELSIM....."
	@cd verification/csmith_run; make clean;
	@cd verification/csmith_run; make RUNS=$(RUNS);

csmith_iverilog: link_iverilog
	@echo "Running CSMITH on IVERILOG....."
	@cd verification/csmith_run; make clean;
	@cd verification/csmith_run; make RUNS=$(RUNS);

torture_bsim: compile_bluesim link_bluesim
	@echo "Running RISC-V TORTURE on BLUESIM....."
	@cd verification/riscv-torture-32; make clean;
	@cd verification/riscv-torture-32; make RUNS=$(RUNS);

torture_ncverilog: link_ncverilog
	@echo "Running RISC-V TORTURE on NCVERILOG....."
	@cd verification/riscv-torture-32; make clean;
	@cd verification/riscv-torture-32; make RUNS=$(RUNS);

torture_msim: link_msim
	@echo "Running RISC-V TORTURE on MODELSIM....."
	@cd verification/riscv-torture-32; make clean;
	@cd verification/riscv-torture-32; make RUNS=$(RUNS);

torture_iverilog: link_iverilog
	@echo "Running RISC-V TORTURE on IVERILOG....."
	@cd verification/riscv-torture-32; make clean;
	@cd verification/riscv-torture-32; make RUNS=$(RUNS);


check-blue:
	@if test -z "$$BLUESPECDIR"; then echo "BLUESPECDIR variable not set"; exit 1; fi; 

check-py:
	@if ! [ -a /usr/bin/python3 ] ; then echo "Python3 is required in /usr/bin to run AAPG" ; exit 1; fi;

.PHONY:  compile_bluesim
compile_bluesim:check-blue
	@echo "Compiling mkhost in Bluesim..."
	@mkdir -p BSV_src/build_bsim; 
	bsc -u -sim -check-assert -simdir BSV_src/build_bsim -bdir BSV_src/build_bsim -info-dir BSV_src/build_bsim $(define_macros) -D simulate=True -suppress-warnings G0020 -keep-fires -opt-undetermined-vals -remove-false-rules -remove-empty-rules -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC:./BSV_src/CPU_src:./BSV_src/UNCORE_src:./BSV_src/FPU_src/SerialFPU:./BSV_src/TLM_src:./BSV_src/AHB_src:./BSV_src/AXI_src -g mkhost BSV_src/UNCORE_src/host.bsv
	@echo "Compilation finished"

.PHONY: generate_verilog
generate_verilog:check-blue
	@echo Compiling mkhost in Verilog for simulations ...
	@mkdir -p BSV_src/build_bsim; 
	@mkdir -p verilog; 
	bsc -u -verilog -elab -vdir verilog -bdir BSV_src/build_bsim -info-dir BSV_src/build_bsim $(define_macros) -D simulate=True  -keep-fires -suppress-warnings G0020 -opt-undetermined-vals -remove-empty-rules -remove-false-rules -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC:./BSV_src/CPU_src:./BSV_src/UNCORE_src:./BSV_src/FPU_src/SerialFPU:./BSV_src/TLM_src:./BSV_src/AHB_src:./BSV_src/AXI_src -g mkhost BSV_src/UNCORE_src/host.bsv
	@cp ${BLUESPECDIR}/Verilog/SizedFIFO.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog/FIFOL1.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog/Counter.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog/RevertReg.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog/FIFO2.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog/FIFO1.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog/main.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM2Load.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM2.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM2BELoad.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM1BELoad.v ./verilog/
	@echo Compilation finished

.PHONY: generate_verilog_fpga
generate_verilog_fpga:check-blue
	@echo Compiling mkhost in Verilog for fpga synthesis...
	@mkdir -p BSV_src/build_bsim; 
	@mkdir -p verilog_fpga; 
	bsc -u -verilog -elab -vdir verilog_fpga -bdir BSV_src/build_bsim -info-dir BSV_src/build_bsim $(define_macros) -keep-fires -suppress-warnings G0020 -opt-undetermined-vals -remove-empty-rules -remove-false-rules -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC:./BSV_src/CPU_src:./BSV_src/UNCORE_src:./BSV_src/FPU_src/SerialFPU:./BSV_src/TLM_src:./BSV_src/AHB_src:./BSV_src/AXI_src -g mkproc BSV_src/UNCORE_src/axi.bsv
	@cp ${BLUESPECDIR}/Verilog/SizedFIFO.v ./verilog_fpga/
	@cp ${BLUESPECDIR}/Verilog/FIFOL1.v ./verilog_fpga/
	@cp ${BLUESPECDIR}/Verilog/Counter.v ./verilog_fpga/
	@cp ${BLUESPECDIR}/Verilog/RevertReg.v ./verilog_fpga/
	@cp ${BLUESPECDIR}/Verilog/FIFO2.v ./verilog_fpga/
	@cp ${BLUESPECDIR}/Verilog/FIFO1.v ./verilog_fpga/
	@cp ${BLUESPECDIR}/Verilog/main.v ./verilog_fpga/
	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM2Load.v ./verilog_fpga/
	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM2.v ./verilog_fpga/
	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM2BELoad.v ./verilog_fpga/
	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM1BELoad.v ./verilog_fpga/
	@echo Compilation finished

.PHONY: link_bluesim
link_bluesim:check-blue
	@echo "Linking mkhost in Bluesim..."
	@mkdir -p bin
	bsc -e mkhost -sim -o bin/out -parallel-sim-link 4 -simdir BSV_src/build_bsim -D simulate=True -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC:./BSV_src/CPU_src:./BSV_src/UNCORE_src:./BSV_src/FPU_src/SerialFPU:./BSV_src/TLM_src:./BSV_src/AHB_src:./BSV_src/AXI_src -bdir BSV_src/build_bsim;
	@cp config_string.hex bin/
	@echo Linking finished

.PHONY: link_ncverilog
link_ncverilog:
	@echo "Linking mkhost using ncverilog..."
	@rm -rf INCA* bin/*
	@mkdir -p bin 
	@ncverilog -q +nc64bit -c +access+r +name+snap_mkhost +nclibdirname+INCA_libs_mkhost +libext+.v -y ./verilog  +define+TOP=mkhost +nowarn+LIBNOU ./verilog/main.v ./verilog/mkhost.v
	@echo 'ncverilog -q -R +nc64bit +name+snap_mkhost +nclibdirname+INCA_libs_mkhost' > bin/out
	@mv INCA* bin/
	@mv ncverilog.* bin/
	@chmod +x bin/out
	@cp config_string.hex bin/
	@echo Linking finished

.PHONY: link_msim
link_msim:
	@echo "Linking mkhost using modelsim..."
	@rm -rf work* bin/*
	@mkdir -p bin 
	vlib work
	vlog -work work +libext+.v+.vqm -y ./verilog +define+TOP=mkhost ./verilog/main.v ./verilog/mkhost.v  > compile_log
	mv compile_log ./bin/
	mv work ./bin/
	echo 'vsim -quiet -novopt -lib work -do "run -all; quit" -c main' > bin/out
	@chmod +x bin/out
	@cp config_string.hex bin/
	@echo Linking finished

.PHONY: link_iverilog
link_iverilog:
	@echo "Linking mkhost using iverilog..."
	@mkdir -p bin 
	@iverilog -v -o bin/out -Wall -y ./verilog/ -DTOP=mkhost ./verilog/main.v ./verilog/mkhost.v
	@cp config_string.hex bin/
	@echo Linking finished

.PHONY: simulate
simulate:
	@echo Simulation...
	@exec ./bin/out
	@echo Simulation finished

.PHONY: clean_bsim
clean_bsim:
	rm -rf BSV_src/build_bsim 
	rm -rf bin/*

clean_verilog: clean_bsim 
	rm -f verilog/*.v
	rm -rf verilog_fpga/*.v

restore: clean_verilog
	cd verification/csmith_run; make clean
	cd verification/riscv-torture-32; make clean
	cd verification/AAPG; ./make.py clean
	cd benchmarks; make clean
	cd apps/FreeRTOS-RISCV-v191/Demo/riscv-spike/; make clean
