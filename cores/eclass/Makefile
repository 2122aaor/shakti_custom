### Makefile for the eclass project
### Generated by Bluespec Workstation on Thu Nov 12 19:54:06 IST 2015

isa_support=RV32IMF
define_macros += -D simulate=True

ifneq (,$(findstring M,$(isa_support)))
  define_macros += -D muldiv=True
endif
ifneq (,$(findstring F,$(isa_support)))
  define_macros += -D spfpu=True
endif
ifneq (,$(findstring D,$(isa_support)))
  define_macros += -D dpfpu=True
endif


default: check_blue compile_bluesim link_bluesim

AAPG_bsim: check-blue compile_bluesim	link_bluesim	
	@echo "Running Torture Tests now...."
	@cd AAPG; ./make.py clean
	@cd AAPG; ./make.py

AAPG_ncverilog: link_ncverilog
	@echo "Running Torture Tests now...."
	@mv INCA* ./bin/
	@cd AAPG; ./make.py clean
	@cd AAPG; ./make.py

AAPG_iverilog: link_iverilog
	@echo "Running Torture Tests now...."
	@cd AAPG; ./make.py clean
	@cd AAPG; ./make.py

AAPG_msim: link_msim
	@echo "Running Torture Tests now...."
	@cd AAPG; ./make.py clean
	@cd AAPG; ./make.py
  
check-blue:
	@if test -z "$$BLUESPECDIR"; then echo "BLUESPECDIR variable not set"; exit 1; fi; 

check-py:
	@if ! [ -a /usr/bin/python3 ] ; then echo "Python3 is required in /usr/bin to run AAPG" ; exit 1; fi;

.PHONY: compile_bluesim
compile_bluesim:check-blue
	@echo Compiling...
	@mkdir -p intermediate_folder
	bsc -u -sim -simdir intermediate_folder -bdir intermediate_folder -info-dir intermediate_folder -D simulate=True $(define_macros) -keep-fires -opt-undetermined-vals -remove-false-rules -remove-empty-rules -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC:./BSV_src/Common:./BSV_src/CPU_src:./BSV_src/TLM_src:./BSV_src/AHB_src -g mkTestbench  BSV_src/CPU_src/Testbench.bsv 
	@echo Compilation finished

#.PHONY:  compile_bluesim
#compile_bluesim:check-blue
#	@echo "Compiling mkahb in Bluesim..."
#	@mkdir -p BSV_src/build_bsim; 
#	bsc -u -sim -simdir BSV_src/build_bsim -bdir BSV_src/build_bsim -info-dir BSV_src/build_bsim -D simulate=True $(define_macros) -suppress-warnings G0020 -keep-fires -opt-undetermined-vals -remove-false-rules -remove-empty-rules -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC:./BSV_src/CPU_src:./BSV_src/FPU_src/SerialFPU:./BSV_src/TLM_src:./BSV_src/AHB_src -g mkahb BSV_src/CPU_src/ahb.bsv
#	@echo "Compilation finished"

.PHONY: generate_verilog
generate_verilog:check-blue
	@echo Compiling...
	@mkdir -p intermediate_folder
	bsc -u -verilog -elab -vdir verilog -bdir intermediate_folder -info-dir intermediate_folder -D simulate=True $(define_macros) -keep-fires -opt-undetermined-vals -remove-empty-rules -remove-false-rules -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC:./BSV_src/Common:./BSV_src/CPU_src:./BSV_src/TLM_src:./BSV_src/AHB_src -g mkTestbench  BSV_src/CPU_src/Testbench.bsv
	@cp ${BLUESPECDIR}/Verilog/SizedFIFO.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog/FIFOL1.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog/RegFileLoad.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog/Counter.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog/RevertReg.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog/FIFO2.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog/FIFO1.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog/main.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM2Load.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM2.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog.Vivado/RegFile.v ./verilog/
	@echo Compilation finished

#.PHONY: generate_verilog
#generate_verilog:check-blue
#	@echo Compiling Testbench_uart in Verilog ...
#	@mkdir -p BSV_src/build_bsim; 
#	@mkdir -p verilog; 
#	bsc -u -verilog -elab -vdir verilog -bdir BSV_src/build_bsim -info-dir BSV_src/build_bsim -D simulate=True $(define_macros) -keep-fires -suppress-warnings G0020 -opt-undetermined-vals -remove-empty-rules -remove-false-rules -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC:./BSV_src/CPU_src:./BSV_src/FPU_src/SerialFPU:./BSV_src/TLM_src:./BSV_src/AHB_src -g mkahb BSV_src/CPU_src/ahb.bsv
#	@cp ${BLUESPECDIR}/Verilog/SizedFIFO.v ./verilog/
#	@cp ${BLUESPECDIR}/Verilog/FIFOL1.v ./verilog/
#	@cp ${BLUESPECDIR}/Verilog/RegFileLoad.v ./verilog/
#	@cp ${BLUESPECDIR}/Verilog/Counter.v ./verilog/
#	@cp ${BLUESPECDIR}/Verilog/RevertReg.v ./verilog/
#	@cp ${BLUESPECDIR}/Verilog/FIFO2.v ./verilog/
#	@cp ${BLUESPECDIR}/Verilog/FIFO1.v ./verilog/
#	@cp ${BLUESPECDIR}/Verilog/main.v ./verilog/
#	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM2Load.v ./verilog/
#	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM2.v ./verilog/
#	@cp ${BLUESPECDIR}/Verilog.Vivado/RegFile.v ./verilog/
#	@echo Compilation finished

.PHONY: link_bluesim
link_bluesim:check-blue
	@echo Linking...
	@mkdir -p bin
	bsc -e mkTestbench -sim -o bin/out -parallel-sim-link 4 -simdir intermediate_folder -D simulate=True -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC:./BSV_src/Common:./BSV_src/CPU_src:./BSV_src/TLM_src:./BSV_src/AHB_src -bdir intermediate_folder -keep-fires ./BSV_src/Common/C_imports.c 
	@echo Linking finished

#.PHONY: link_bluesim
#link_bluesim:check-blue
#	@echo "Linking mkahb in Bluesim..."
#	@mkdir -p bin
#	bsc -e mkahb -sim -o bin/out -parallel-sim-link 4 -simdir BSV_src/build_bsim -D simulate=True -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC:./BSV_src/CPU_src:./BSV_src/FPU_src/SerialFPU:./BSV_src/TLM_src:./BSV_src/AHB_src -bdir BSV_src/build_bsim;
#	@echo Linking finished

.PHONY: link_ncverilog
link_ncverilog:
	@echo "Linking using ncverilog..."
	@rm -rf INCA* bin/*
	@mkdir -p bin 
	@ncverilog -q +nc64bit -c +access+r +name+snap_mkTestbench +nclibdirname+INCA_libs_mkTestbench +libext+.v -y ./verilog  +define+TOP=mkTestbench +nowarn+LIBNOU ./verilog/main.v ./verilog/mkTestbench.v
	@echo 'ncverilog -q -R +nc64bit +name+snap_mkTestbench +nclibdirname+INCA_libs_mkTestbench' > bin/out
	@mv INCA* bin/
	@mv ncverilog.* bin/
	@chmod +x bin/out
	@echo Linking finished

.PHONY: link_msim
link_msim:
	@echo "Linking using modelsim..."
	@rm -rf work* bin/*
	@mkdir -p bin 
	vlib work
	vlog -work work +libext+.v+.vqm -y ./verilog +define+TOP=mkTestbench ./verilog/main.v ./verilog/mkTestbench.v  > compile_log
	mv compile_log ./bin/
	mv work ./bin/
	echo 'vsim -quiet -novopt -lib work -do "run -all; quit" -c main' > bin/out
	@chmod +x bin/out
	@echo Linking finished

.PHONY: link_iverilog
link_iverilog:
	@echo "Linking mkahb using iverilog..."
	@mkdir -p bin 
	@iverilog -v -o bin/out -Wall -y ./verilog/ -DTOP=mkTestbench ./verilog/main.v ./verilog/mkTestbench.v
	@echo Linking finished

.PHONY: simulate
simulate:
	@echo Simulation...
	@exec ./bin/out
	@echo Simulation finished

.PHONY: clean
clean:
	rm -rf ./intermediate_folder
	rm -f verilog/*.v
	rm -rf bin/*
	rm -rf INCA*

