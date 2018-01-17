package ahb;
	import Vector       :: *;
	import ClientServer :: *;	
	import core::*;
  `ifdef simulate
  	import TLM_Memory_RegFile ::*;
  `else
    import TLM_Memory::*;
  `endif
	import Connectable::*;
	import RegFile::*;
	import defined_types::*;
  	import BRAMCore :: *;
	import Assert::*;
	import TLM2::*;
	import DefaultValue :: *;
	import FIFOF::*;
	import SpecialFIFOs::*;
	import Utils::*;
	import GetPut::*;
	import AHB :: *;	
	import Fabric :: *;
	`include "TLM.defines"
	
    import riscv::*;
    import defined_types::*;
    import DReg::*;
	`include "defined_parameters.bsv"
`define bus_mem_port 0
`define other_port 1
`define cpu_port 0
`define other_master_port 1


	interface Intfc_top;
	      method    Action      sin(Bit#(1) in);
	      method    Bit#(1)     sout();
	endinterface

	module mkahb(Intfc_top);
	

	Wire#(Bool) wr_flush<-mkDWire(False);
	Ifc_core_copy proc <-mkcore_copy();
	Ifc_BRAM bram_mod <- mkTLM_Memory();
	//mkConnection(proc.intfc,bram_mod.intfc_rcv);

	rule read_flush_signal_from_cpu;
  		bram_mod.flush_from_proc(proc.flush);
    	endrule
	
	function Bool addr_match_mem (AHBAddr #(`TLM_PRM_RSP_STD) value);
		return True;
	endfunction
	function Bool addr_mismatch (AHBAddr #(`TLM_PRM_RSP_STD) value);
		return False;
	endfunction
	function AHBMasterFabric funct1(AHBMasterActor a);
		return a.fabric;
	endfunction
	
	function AHBSlaveFabric funct2(AHBSlaveActor a);
		return a.fabric;
	endfunction
	
	// Instantaite transactors for TLM Masters and Slaves
	Vector #(2, AHBMasterActor) masterX <- replicateM (mkAHBMaster);
	Vector #(2, AHBSlaveActor) slaveX = newVector;
	// Make Slaves
	AHBSlaveActor slave_mem <- mkAHBSlave(addr_match_mem);
	AHBSlaveActor slave_other  <- mkAHBSlave(addr_mismatch);
	slaveX[`bus_mem_port] = slave_mem;
	slaveX[`other_port]  = slave_other;


	// Instantiate bus interface
	Empty ahb_bus <- mkAHBBus (map(funct1, masterX), map(funct2, slaveX));
   
	// Establish connectivity between Bus, Masters and Slaves
	mkConnection(proc.intfc, masterX[`cpu_port].tlm);	        // bus_cpu_instr_port = 0
   	//mkConnection(proc.intfc,bram_mod.intfc_rcv);
	mkConnection(slaveX[`bus_mem_port].tlm, bram_mod.intfc_rcv);	// bus_mem_instr_port = 0

	method    Action      sin(Bit#(1) in);
        proc.sin(in);
      	endmethod
    	method    Bit#(1)     sout();
        return proc.sout;
    	endmethod
	endmodule
endpackage
