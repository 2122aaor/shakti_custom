### Makefile for the Processor project
### Generated by Bluespec Workstation on Wed Aug 17 11:13:13 IST 2016

default: full_clean compile link simulate

.PHONY: compile
compile:
	@echo Compiling...
	bsc -u -sim -simdir intermediate_files -bdir intermediate_files -info-dir intermediate_files -keep-fires -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC:./../Axi:./../TLM -g mkTb_Processor  Tb_Processor.bsv 
	@echo Compilation finished

.PHONY: link
link:
	@echo Linking...
	bsc -e mkTb_Processor -sim -o ./out -simdir intermediate_files -p .:%/Prelude:%/Libraries:%/Libraries/BlueNoC:./../Axi:./../TLM -bdir intermediate_files -keep-fires 
	@echo Linking finished

.PHONY: simulate
simulate:
	@echo Simulation...
	./out 
	@echo Simulation finished

.PHONY: clean
clean:
	exec rm -f intermediate_files/regFile.bo intermediate_files/AxiMaster.bo intermediate_files/Utils.bo intermediate_files/TLM2Defines.bo intermediate_files/Processor.bo intermediate_files/Memory.bo intermediate_files/commit.bo intermediate_files/integer_multiplier_riscv.bo intermediate_files/AxiDefines.bo intermediate_files/TLM2Ram.bo intermediate_files/Interrupt_controller.bo intermediate_files/nbdcache.bo intermediate_files/map.bo intermediate_files/decode.bo intermediate_files/AxiMonitor.bo intermediate_files/fetch.bo intermediate_files/TLM2Reduce.bo intermediate_files/set_associative_cache.bo intermediate_files/wakeup.bo intermediate_files/rRAM.bo intermediate_files/RB_NB.bo intermediate_files/riscv_types.bo intermediate_files/BusSwitch.bo intermediate_files/set_associative_dcache.bo intermediate_files/IQ.bo intermediate_files/fRAM.bo intermediate_files/Booth2_pp_gen.bo intermediate_files/riscv_arithmetic_unit.bo intermediate_files/RBA.bo intermediate_files/LoadBuffer_d.bo intermediate_files/bpu.bo intermediate_files/frq.bo intermediate_files/TLMReqRsp.bo intermediate_files/decoder.bo intermediate_files/nbicache.bo intermediate_files/mul_div.bo intermediate_files/Tb_Processor.bo intermediate_files/alu.bo intermediate_files/TLM2CBusAdapter.bo intermediate_files/select_grant.bo intermediate_files/CBuff.bo intermediate_files/LoadBuffer_i.bo intermediate_files/CFFIFO.bo intermediate_files/All_types_d.bo intermediate_files/AxiWrBus.bo intermediate_files/Encoder_tree.bo intermediate_files/AxiRdBus.bo intermediate_files/LS_unit.bo intermediate_files/arithmetic_functions.bo intermediate_files/TLM2ReadWriteRam.bo intermediate_files/integer_divider_riscv.bo intermediate_files/AxiSlave.bo intermediate_files/All_types.bo intermediate_files/AxiPC.bo intermediate_files/TLM2BRAM.bo intermediate_files/branch_unit.bo intermediate_files/TLM2Utils.bo intermediate_files/TLM2.bo intermediate_files/Axi.bo
	exec rm -f intermediate_files/module_c_z_mux.ba intermediate_files/mkriscv_arithmetic_unit.ba intermediate_files/mkProcessor.ba intermediate_files/module_wallace_rba_final.ba intermediate_files/module_rba.ba intermediate_files/mkPrf_frq.ba intermediate_files/mkPrf_wakeup.ba intermediate_files/module_priority_encoder.ba intermediate_files/mkPrf_decode.ba intermediate_files/mkinteger_multiplier_riscv.ba intermediate_files/mkPrf_Map.ba intermediate_files/mkPrf_fRAM.ba intermediate_files/module_rbAdder.ba intermediate_files/module_fn_shiftright.ba intermediate_files/module_wallace_rba.ba intermediate_files/module_fn_comparator.ba intermediate_files/module_fn_slt.ba intermediate_files/module_fn_addsub.ba intermediate_files/mkAxiWrSlaveSynthStd.ba intermediate_files/mkPrf_select_grant.ba intermediate_files/module_fn_sltu.ba intermediate_files/mkPrf_commit.ba intermediate_files/mkAxiRdSlaveSynthStd.ba intermediate_files/module_fn_sll.ba intermediate_files/module_rb_nb.ba intermediate_files/module_fn_shiftleft.ba intermediate_files/module_fn_sra_srl.ba intermediate_files/mkPrf_IQ.ba intermediate_files/mkTLM2Source.ba intermediate_files/mkinteger_divider_riscv.ba intermediate_files/mkmul_div.ba intermediate_files/module_fn_decoder.ba intermediate_files/mkAxiRdMasterStd.ba intermediate_files/mkLS_unit.ba intermediate_files/mkPrf_rRAM.ba intermediate_files/mkTb_Processor.ba intermediate_files/mkAxiWrMasterStd.ba intermediate_files/mkalu.ba intermediate_files/mkPrf_fetch.ba intermediate_files/mkInterrupt_controller.ba
	exec rm -f intermediate_files/mkPrf_fetch.o intermediate_files/module_fn_shiftleft.o intermediate_files/module_priority_encoder.o intermediate_files/module_fn_sll.o intermediate_files/mkLS_unit.o intermediate_files/module_fn_comparator.o intermediate_files/model_mkTb_Processor.o intermediate_files/mkPrf_wakeup.o intermediate_files/mkPrf_select_grant.o intermediate_files/module_fn_addsub.o intermediate_files/mkProcessor.o intermediate_files/module_wallace_rba_final.o intermediate_files/mkPrf_frq.o intermediate_files/module_rba.o intermediate_files/module_fn_sra_srl.o intermediate_files/module_fn_slt.o intermediate_files/mkinteger_multiplier_riscv.o intermediate_files/mkPrf_fRAM.o intermediate_files/module_fn_sltu.o intermediate_files/module_wallace_rba.o intermediate_files/mkinteger_divider_riscv.o intermediate_files/mkriscv_arithmetic_unit.o intermediate_files/mkInterrupt_controller.o intermediate_files/mkalu.o intermediate_files/mkTb_Processor.o intermediate_files/mkPrf_rRAM.o intermediate_files/mkmul_div.o intermediate_files/mkPrf_Map.o intermediate_files/module_rbAdder.o intermediate_files/mkPrf_decode.o intermediate_files/module_fn_shiftright.o intermediate_files/mkPrf_IQ.o intermediate_files/mkPrf_commit.o intermediate_files/module_fn_decoder.o

.PHONY: full_clean
full_clean:
	rm -f intermediate_files/regFile.bo intermediate_files/AxiMaster.bo intermediate_files/Utils.bo intermediate_files/TLM2Defines.bo intermediate_files/Processor.bo intermediate_files/Memory.bo intermediate_files/commit.bo intermediate_files/integer_multiplier_riscv.bo intermediate_files/AxiDefines.bo intermediate_files/TLM2Ram.bo intermediate_files/Interrupt_controller.bo intermediate_files/nbdcache.bo intermediate_files/map.bo intermediate_files/decode.bo intermediate_files/AxiMonitor.bo intermediate_files/fetch.bo intermediate_files/TLM2Reduce.bo intermediate_files/set_associative_cache.bo intermediate_files/wakeup.bo intermediate_files/rRAM.bo intermediate_files/RB_NB.bo intermediate_files/riscv_types.bo intermediate_files/BusSwitch.bo intermediate_files/set_associative_dcache.bo intermediate_files/IQ.bo intermediate_files/fRAM.bo intermediate_files/Booth2_pp_gen.bo intermediate_files/riscv_arithmetic_unit.bo intermediate_files/RBA.bo intermediate_files/LoadBuffer_d.bo intermediate_files/bpu.bo intermediate_files/frq.bo intermediate_files/TLMReqRsp.bo intermediate_files/decoder.bo intermediate_files/nbicache.bo intermediate_files/mul_div.bo intermediate_files/Tb_Processor.bo intermediate_files/alu.bo intermediate_files/TLM2CBusAdapter.bo intermediate_files/select_grant.bo intermediate_files/CBuff.bo intermediate_files/LoadBuffer_i.bo intermediate_files/CFFIFO.bo intermediate_files/All_types_d.bo intermediate_files/AxiWrBus.bo intermediate_files/Encoder_tree.bo intermediate_files/AxiRdBus.bo intermediate_files/LS_unit.bo intermediate_files/arithmetic_functions.bo intermediate_files/TLM2ReadWriteRam.bo intermediate_files/integer_divider_riscv.bo intermediate_files/AxiSlave.bo intermediate_files/All_types.bo intermediate_files/AxiPC.bo intermediate_files/TLM2BRAM.bo intermediate_files/branch_unit.bo intermediate_files/TLM2Utils.bo intermediate_files/TLM2.bo intermediate_files/Axi.bo
	rm -f intermediate_files/module_c_z_mux.ba intermediate_files/mkriscv_arithmetic_unit.ba intermediate_files/mkProcessor.ba intermediate_files/module_wallace_rba_final.ba intermediate_files/module_rba.ba intermediate_files/mkPrf_frq.ba intermediate_files/mkPrf_wakeup.ba intermediate_files/module_priority_encoder.ba intermediate_files/mkPrf_decode.ba intermediate_files/mkinteger_multiplier_riscv.ba intermediate_files/mkPrf_Map.ba intermediate_files/mkPrf_fRAM.ba intermediate_files/module_rbAdder.ba intermediate_files/module_fn_shiftright.ba intermediate_files/module_wallace_rba.ba intermediate_files/module_fn_comparator.ba intermediate_files/module_fn_slt.ba intermediate_files/module_fn_addsub.ba intermediate_files/mkAxiWrSlaveSynthStd.ba intermediate_files/mkPrf_select_grant.ba intermediate_files/module_fn_sltu.ba intermediate_files/mkPrf_commit.ba intermediate_files/mkAxiRdSlaveSynthStd.ba intermediate_files/module_fn_sll.ba intermediate_files/module_rb_nb.ba intermediate_files/module_fn_shiftleft.ba intermediate_files/module_fn_sra_srl.ba intermediate_files/mkPrf_IQ.ba intermediate_files/mkTLM2Source.ba intermediate_files/mkinteger_divider_riscv.ba intermediate_files/mkmul_div.ba intermediate_files/module_fn_decoder.ba intermediate_files/mkAxiRdMasterStd.ba intermediate_files/mkLS_unit.ba intermediate_files/mkPrf_rRAM.ba intermediate_files/mkTb_Processor.ba intermediate_files/mkAxiWrMasterStd.ba intermediate_files/mkalu.ba intermediate_files/mkPrf_fetch.ba intermediate_files/mkInterrupt_controller.ba
	rm -f intermediate_files/mkPrf_fetch.o intermediate_files/module_fn_shiftleft.o intermediate_files/module_priority_encoder.o intermediate_files/module_fn_sll.o intermediate_files/mkLS_unit.o intermediate_files/module_fn_comparator.o intermediate_files/model_mkTb_Processor.o intermediate_files/mkPrf_wakeup.o intermediate_files/mkPrf_select_grant.o intermediate_files/module_fn_addsub.o intermediate_files/mkProcessor.o intermediate_files/module_wallace_rba_final.o intermediate_files/mkPrf_frq.o intermediate_files/module_rba.o intermediate_files/module_fn_sra_srl.o intermediate_files/module_fn_slt.o intermediate_files/mkinteger_multiplier_riscv.o intermediate_files/mkPrf_fRAM.o intermediate_files/module_fn_sltu.o intermediate_files/module_wallace_rba.o intermediate_files/mkinteger_divider_riscv.o intermediate_files/mkriscv_arithmetic_unit.o intermediate_files/mkInterrupt_controller.o intermediate_files/mkalu.o intermediate_files/mkTb_Processor.o intermediate_files/mkPrf_rRAM.o intermediate_files/mkmul_div.o intermediate_files/mkPrf_Map.o intermediate_files/module_rbAdder.o intermediate_files/mkPrf_decode.o intermediate_files/module_fn_shiftright.o intermediate_files/mkPrf_IQ.o intermediate_files/mkPrf_commit.o intermediate_files/module_fn_decoder.o
	rm -f intermediate_files/mkriscv_arithmetic_unit.h
	rm -f intermediate_files/mkriscv_arithmetic_unit.cxx
	rm -f intermediate_files/mkProcessor.h
	rm -f intermediate_files/mkProcessor.cxx
	rm -f intermediate_files/module_wallace_rba_final.h
	rm -f intermediate_files/module_wallace_rba_final.cxx
	rm -f intermediate_files/module_rba.h
	rm -f intermediate_files/module_rba.cxx
	rm -f intermediate_files/mkPrf_frq.h
	rm -f intermediate_files/mkPrf_frq.cxx
	rm -f intermediate_files/mkPrf_wakeup.h
	rm -f intermediate_files/mkPrf_wakeup.cxx
	rm -f intermediate_files/module_priority_encoder.h
	rm -f intermediate_files/module_priority_encoder.cxx
	rm -f intermediate_files/mkPrf_decode.h
	rm -f intermediate_files/mkPrf_decode.cxx
	rm -f intermediate_files/mkinteger_multiplier_riscv.h
	rm -f intermediate_files/mkinteger_multiplier_riscv.cxx
	rm -f intermediate_files/mkPrf_Map.h
	rm -f intermediate_files/mkPrf_Map.cxx
	rm -f intermediate_files/mkPrf_fRAM.h
	rm -f intermediate_files/mkPrf_fRAM.cxx
	rm -f intermediate_files/module_rbAdder.h
	rm -f intermediate_files/module_rbAdder.cxx
	rm -f intermediate_files/module_fn_shiftright.h
	rm -f intermediate_files/module_fn_shiftright.cxx
	rm -f intermediate_files/module_wallace_rba.h
	rm -f intermediate_files/module_wallace_rba.cxx
	rm -f intermediate_files/module_fn_comparator.h
	rm -f intermediate_files/module_fn_comparator.cxx
	rm -f intermediate_files/module_fn_slt.h
	rm -f intermediate_files/module_fn_slt.cxx
	rm -f intermediate_files/module_fn_addsub.h
	rm -f intermediate_files/module_fn_addsub.cxx
	rm -f intermediate_files/mkPrf_select_grant.h
	rm -f intermediate_files/mkPrf_select_grant.cxx
	rm -f intermediate_files/module_fn_sltu.h
	rm -f intermediate_files/module_fn_sltu.cxx
	rm -f intermediate_files/mkPrf_commit.h
	rm -f intermediate_files/mkPrf_commit.cxx
	rm -f intermediate_files/module_fn_sll.h
	rm -f intermediate_files/module_fn_sll.cxx
	rm -f intermediate_files/module_fn_shiftleft.h
	rm -f intermediate_files/module_fn_shiftleft.cxx
	rm -f intermediate_files/module_fn_sra_srl.h
	rm -f intermediate_files/module_fn_sra_srl.cxx
	rm -f intermediate_files/mkPrf_IQ.h
	rm -f intermediate_files/mkPrf_IQ.cxx
	rm -f intermediate_files/mkinteger_divider_riscv.h
	rm -f intermediate_files/mkinteger_divider_riscv.cxx
	rm -f intermediate_files/mkmul_div.h
	rm -f intermediate_files/mkmul_div.cxx
	rm -f intermediate_files/module_fn_decoder.h
	rm -f intermediate_files/module_fn_decoder.cxx
	rm -f intermediate_files/mkLS_unit.h
	rm -f intermediate_files/mkLS_unit.cxx
	rm -f intermediate_files/mkPrf_rRAM.h
	rm -f intermediate_files/mkPrf_rRAM.cxx
	rm -f intermediate_files/mkTb_Processor.h intermediate_files/model_mkTb_Processor.h
	rm -f intermediate_files/mkTb_Processor.cxx intermediate_files/model_mkTb_Processor.cxx
	rm -f intermediate_files/mkalu.h
	rm -f intermediate_files/mkalu.cxx
	rm -f intermediate_files/mkPrf_fetch.h
	rm -f intermediate_files/mkPrf_fetch.cxx
	rm -f intermediate_files/mkInterrupt_controller.h
	rm -f intermediate_files/mkInterrupt_controller.cxx
