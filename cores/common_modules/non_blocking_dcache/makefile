# Makefile to compile either in Bluesim for simulation purpose or Verilog for synthesis purpose.

default: full_clean bluesim

.PHONY: bluesim
bluesim: 
	bsc -u -sim -simdir intermediate_files -bdir intermediate_files -info-dir intermediate_files -keep-fires -D SIM -D UART -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC -g mkTb_nbdcache Tb_nbdcache.bsv
	bsc -e mkTb_nbdcache -sim -o ./out -simdir intermediate_files -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC -bdir intermediate_files -keep-fires

.PHONY: blue_compile
blue_compile: 
	bsc -u -sim -simdir intermediate_files -bdir intermediate_files -info-dir intermediate_files -keep-fires -D SIM -D UART -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC -g mkTb_nbdcache Tb_nbdcache.bsv

.PHONY: verilog
verilog:
	#bsc -u -verilog -elab -vdir ../Verilog -bdir intermediate_files -info-dir intermediate_files -keep-fires -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC -g mk_riscv_master riscv_master.bsv
	bsc -u -verilog -elab -vdir ../Verilog -bdir intermediate_files -info-dir intermediate_files -keep-fires -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC -g mkTb_nbdcache Tb_nbdcache.bsv
	bsc -e mkTb_core -verilog -o ./out -vdir ../Verilog -vsim modelsim -keep-fires ../Verilog/mkTb_core.v

.PHONY: full_clean
full_clean: 
	rm -f ./intermediate_files/*
	rm -f ../Verilog/mk*.v ../Verilog/module*.v
	rm -f ./out ./out.so

.PHONY: clean_verilog
clean_verilog:
	rm -f ../Verilog/mk*.v ../Verilog/module*.v

.PHONY: clean_bluesim
clean_bluesim:
	rm -f ./intermediate_files/*
	rm -f ./out ./out.so
