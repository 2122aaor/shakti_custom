package ahb;
	import Vector       :: *;
	import ClientServer :: *;	
	import core_AHB::*;
  import TLM_Memory_AHB ::*;
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
	import Fabric_AHB :: *;
	`include "TLM.defines"
	import RTC_AHB					:: *;
	`include "defined_parameters.bsv"

	`define main_mem_port 0
	`define rtc_port 1
	`define configmem_port 2
	`define cpu_port 0
	`define other_master_port 1


	interface Ifc_proc;
	      method    Action      sin(Bit#(1) in);
	      method    Bit#(1)     sout();
	endinterface

	(*synthesize*)
 	module mkproc(Ifc_proc);
	
	Ifc_BRAM#(32'h80000000,`Addr_space) main_memory <- mkTLM_Memory("code.mem","MainMEM");
	Ifc_BRAM#(32'h1000,12) config_memory <-mkTLM_Memory("config_string.hex","CONFIGMEM");
	RTC#(1) rtc <- mkRTC_RV32;
	Ifc_core proc <-mkcore();

	rule connect_timer;
		proc.mtip(pack(rtc.timerInterrupt));
	endrule
//	Ifc_BRAM bram_mod <- mkTLM_Memory();
	//mkConnection(proc.intfc,bram_mod.intfc_rcv);

	function Bool addr_match_mem (AHBAddr #(`TLM_PRM_RSP_STD) value);
		return (value>=32'h80000000);
	endfunction
	function Bool addr_match_rtc (AHBAddr #(`TLM_PRM_RSP_STD) value);
		return (value>=32'h40000000 && value<=32'h4000000f);
	endfunction
	function Bool addr_match_configmem (AHBAddr #(`TLM_PRM_RSP_STD) value);
		return (value>=32'h0001000 && value<=32'h4000);
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
	Vector #(1, AHBMasterActor) masterX <- replicateM (mkAHBMaster);
	Vector #(3, AHBSlaveActor) slaveX = newVector;
	// Make Slaves
	AHBSlaveActor slave_main_mem <- mkAHBSlave(addr_match_mem);
	AHBSlaveActor slave_rtc  <- mkAHBSlave(addr_match_rtc);
	AHBSlaveActor slave_configmem  <- mkAHBSlave(addr_match_configmem);
	slaveX[`main_mem_port] = slave_main_mem;
	slaveX[`rtc_port]  = slave_rtc;
	slaveX[`configmem_port]  = slave_configmem;


	// Instantiate bus interface
	Empty ahb_bus <- mkAHBBus (map(funct1, masterX), map(funct2, slaveX));
   
	// Establish connectivity between Bus, Masters and Slaves
	mkConnection(proc.intfc, masterX[`cpu_port].tlm);	        // bus_cpu_instr_port = 0
   	//mkConnection(proc.intfc,bram_mod.intfc_rcv);
	mkConnection(slaveX[`main_mem_port].tlm, main_memory.intfc_rcv);	// bus_mem_instr_port = 0
	mkConnection(slaveX[`configmem_port].tlm, config_memory.intfc_rcv);	// bus_mem_instr_port = 0
	mkConnection(slaveX[`rtc_port].tlm, rtc.bus_ifc);	// bus_mem_instr_port = 0

	method    Action      sin(Bit#(1) in);
        proc.sin(in);
      	endmethod
    	method    Bit#(1)     sout();
        return proc.sout;
    	endmethod
	endmodule
endpackage
