package axi;
	import core_AXI::*;
	import TLM2::*;
	import Axi::*;
	import GetPut::*;
	import Utils::*;
	import Connectable::*;
	import RTC_AXI:: *;
	import TLM_Memory_AXI::*;
	import TLMReqRsp::*;
	import Vector::*;
	`include "TLM.defines"
	`include "defined_parameters.bsv"

	interface Ifc_proc;
		method    Action      sin(Bit#(1) in);
		method    Bit#(1)     sout();
	endinterface

	(*synthesize*)
	module mkproc(Ifc_proc);
		function Bool addr_match_mem (AxiAddr #(`TLM_TYPES) value);
			return (value>='h80000000);
		endfunction

		function Bool addr_match_rtc (AxiAddr #(`TLM_TYPES) value);
			return (value>='h40000000 && value<='h4000000f);
		endfunction

		function Bool addr_match_configmem (AxiAddr #(`TLM_TYPES) value);
			return (value>='h0001000 && value<='h4000);
		endfunction

		function AxiRdFabricMaster #(`AXI_PRM_CPU) fn_map_RM (AxiRdMasterXActorIFC #(`AXI_XTR_CPU) xifc);
	  	return xifc.fabric;
   	endfunction
	
   	function AxiWrFabricMaster #(`AXI_PRM_CPU) fn_map_WM (AxiWrMasterXActorIFC #(`AXI_XTR_CPU) xifc);
	  	return xifc.fabric;
   	endfunction

   	function AxiRdFabricSlave #(`AXI_PRM_MEM) fn_map_RS (AxiRdSlaveXActorIFC #(`AXI_XTR_MEM) xifc);
	  	return xifc.fabric;
   	endfunction

   	function AxiWrFabricSlave #(`AXI_PRM_MEM) fn_map_WS (AxiWrSlaveXActorIFC #(`AXI_XTR_MEM) xifc);
	  	return xifc.fabric;
   	endfunction

		Ifc_core core <-mkcore();
		Memory_IFC#(32'h80000000,`Addr_space) main_memory <- mkMemory("code.mem","MainMEM");
		Memory_IFC#(32'h1000,12) config_memory <-mkMemory("config_string.hex","CONFIGMEM");
		RTC#(1) rtc <- mkRTC_RV32;
		rule connect_timer;
			core.mtip(pack(rtc.timerInterrupt));
		endrule

	 	// +++ Read
		Vector #(1, AxiRdMasterXActorIFC #(`AXI_XTR_CPU)) rd_masters <- replicateM(mkAxiRdMaster);
   	mkConnection (core.bus_rd_ifc, rd_masters[0].tlm);

   	// +++ Write
   	Vector #(1, AxiWrMasterXActorIFC #(`AXI_XTR_CPU)) wr_masters <- replicateM(mkAxiWrMaster);
   	mkConnection (core.bus_wr_ifc, wr_masters[0].tlm);

//   		// +++ Read Bus interfaces
		Vector #(3, AxiRdSlaveXActorIFC #(`AXI_XTR_MEM)) rd_slaves = newVector;
		AxiRdSlaveXActorIFC #(`AXI_XTR_MEM) slave_main_mem_rd <-mkAxiRdSlave(4,addr_match_mem);
		AxiRdSlaveXActorIFC #(`AXI_XTR_MEM)	slave_config_mem_rd <-mkAxiRdSlave(4,addr_match_configmem);
		AxiRdSlaveXActorIFC #(`AXI_XTR_MEM)	slave_rtc_rd <-mkAxiRdSlave(1,addr_match_rtc);
		rd_slaves[0]=slave_main_mem_rd;
		rd_slaves[1]=slave_config_mem_rd;
		rd_slaves[2]=slave_rtc_rd;
 		mkConnection (main_memory.bus_rd_ifc, rd_slaves[0].tlm);
 		mkConnection (config_memory.bus_rd_ifc, rd_slaves[1].tlm);
 		mkConnection (rtc.bus_rd_ifc, rd_slaves[2].tlm);

//   		// +++ Write Bus interfaces
		Vector #(3, AxiWrSlaveXActorIFC #(`AXI_XTR_MEM)) wr_slaves = newVector;
		AxiWrSlaveXActorIFC #(`AXI_XTR_MEM) slave_main_mem_wr <-mkAxiWrSlave(4,addr_match_mem);
		AxiWrSlaveXActorIFC #(`AXI_XTR_MEM)	slave_config_mem_wr <-mkAxiWrSlave(4,addr_match_configmem);
		AxiWrSlaveXActorIFC #(`AXI_XTR_MEM)	slave_rtc_wr <-mkAxiWrSlave(1,addr_match_rtc);
		wr_slaves[0]=slave_main_mem_wr;
		wr_slaves[1]=slave_config_mem_wr;
		wr_slaves[2]=slave_rtc_wr;
 		mkConnection (main_memory.bus_wr_ifc, wr_slaves[0].tlm);
 		mkConnection (config_memory.bus_wr_ifc, wr_slaves[1].tlm);
 		mkConnection (rtc.bus_wr_ifc, wr_slaves[2].tlm);

	// Instantiating read bus (channel)
   mkAxiRdBus(map(fn_map_RM, rd_masters), map(fn_map_RS, rd_slaves));

//   // Instantiating write bus (channel)
   mkAxiWrBus(map(fn_map_WM, wr_masters), map(fn_map_WS, wr_slaves));

		
		method    Action      sin(Bit#(1) in);
  		core.sin(in);
  	endmethod
  	method    Bit#(1)     sout();
  		return core.sout;
  	endmethod
	endmodule
endpackage
