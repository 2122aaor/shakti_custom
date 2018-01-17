/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 


Author Names : Rahul Bodduna
Email ID : rahul.bodduna@gmail.com
*/

/*
	1. Add two client interfaces for CPU: a. For instructions b. For data
	2. Add a file like ICLASS.defines and define parameters like `TLM_PRM_ICLASS.
	   Add a file like ICLASS_Sys_Config and add typedefs like Req_CPU, Rsp_CPU, Req_Mem, Rsp_Mem.
	3. Add corresponding request and response fifos for both
	4. Add rules to accept requests from cache and send them to memory through instruction client interface
	   Add rules to accept requests from cache and send them to memory through data client interface
    5. Add UTILS file to add support for zip functions

	Issues:
	1. Adding additional fields to TLM packet
	2. Use of transaction id
	3. Length of queues for request to and from memory
	4. Multiple clients on Axi4
	5. Address space for different devices
*/


package  Processor;

//==================BLUESPEC LIBRARIES========================//
import Vector		:: *;
import ConfigReg	:: *;
import Connectable	:: *;
import FIFO			:: *;
import FIFOF		:: *;
import SpecialFIFOs	:: *;
import DefaultValue	:: *;
//==================BLUESPEC LIBRARIES========================//

//==================PROJECT IMPORTS===========================//

// ================= LIBRARIES ===============================//
import TLM2 :: *;
import Axi  :: *;
`include "TLM2Defs.defines"
`include "AxiDefs.defines"
// ================= LIBRARIES ===============================//


    //==============TYPES=============================//
`include "defined_parameters.bsv"
import riscv_types	:: *;
import All_types	:: *;
import All_types_d	:: *;
import TLMReqRsp 	:: *;
import Utils		:: *;
    //==============TYPES=============================//

    //==========PIPELINE FLOW=====================//
import bpu			:: *;
import fetch		:: *;
import decode		:: *;
import map			:: *;
import wakeup		:: *;
import select_grant	:: *;
import alu			:: *;
import branch_unit	:: *;
import LS_unit		:: *;
import mul_div		:: *;
import commit		:: *;
//import Interrupt_controller	::*;
import CsrFile :: *;
//import MMCsrs :: *;
    //=========PIPELINE FLOW======================//

    //=========STORAGE ELEMENTS===================//
import regFile		::*;
import fRAM			::*;
import rRAM			::*;
import frq			::*;
import IQ			::*;
import nbicache		::*;
import nbdcache		::*;
import MMCsrs     	::*;


    //=========STORAGE ELEMENTS===================//

//interface of cache with test bench
interface Ifc_cache_to_memory;
	
// --------------------------------------------------------------------------------------------
	// Client interfaces for interaction with memory.
	// Request are put through these clients on the Bus in form of TLM packets
	interface TLMSendIFC #(Req_CPU, Rsp_CPU) bus_rd_ifc;
	interface TLMSendIFC #(Req_CPU, Rsp_CPU) bus_wr_ifc;
// --------------------------------------------------------------------------------------------

/*    method ActionValue#(To_Memory#(`ADDRESS_WIDTH,16)) request_to_memory_from_icache; // send request to memory
    method Action response_from_memory_to_icache(From_Memory#(`ADDRESS_WIDTH,8,`BLOCK_SIZE,16) resp); // recieve response from the memory.
    method ActionValue#(To_Memory_d#(`ADDRESS_WIDTH,8,4)) request_to_memory_from_dcache; // send request to memory
    method Action response_from_memory_to_dcache(From_Memory_d#(`ADDRESS_WIDTH,8,4) resp); // recieve response from the memory.*/
	method Bool flush_icache;
	method Bool flush_dcache;
   // method ActionValue#(WriteBack_structure_d#(`ADDRESS_WIDTH,8,4)) write_back_data();
endinterface

typedef Bit#(64) PAddr;
PAddr baseaddress = 64'b1000000000000001;
PAddr program_counter = 'h80000000;

//==================PROJECT IMPORTS==========================//

(*synthesize, descending_urgency="rl_request_to_memory_from_dcache, rl_request_to_memory_from_icache"*)
module mkProcessor(Ifc_cache_to_memory);
//let rg_icache <- mkReg(InvalidFile);
//let rg_dcache <- mkReg(InvalidFile);
//Branch predictor
Ifcbpu bpu <- mkBPU();
//Fetch Unit
IfcPrf_fetch fetch <- mkPrf_fetch();
//Decode Unit
IfcPrf_decode decode <- mkPrf_decode();
//Map Unit
IfcPrf_Map map <- mkPrf_Map();
//Wakeup
IfcPrf_wakeup wakeup <- mkPrf_wakeup();
//Select Grant and Drive
IfcPrf_select_grant select_grant <- mkPrf_select_grant(); 
//Commit
IfcPrf_commit commit <- mkPrf_commit();
//fRAM
IfcPrf_fRAM fRAM <- mkPrf_fRAM();
//rRAM
IfcPrf_rRAM rRAM <- mkPrf_rRAM();
//Free Register Queue
IfcPrf_frq  frq <- mkPrf_frq();
//Instruction Queue
IfcPrf_IQ inst_Q <- mkPrf_IQ();
//Load Store Unit
Ifc_LS_unit ls_unit <- mkLS_unit();
Ifc_mul_div mul_div <- mkmul_div();
//Instantiate 2 ALUs
Vector#(`ALU_UNITS, Ifc_alu) alu <- replicateM(mkalu());
//Instantiate branch unit
Branch_unit branch_unit <- mk_branch_unit();
//Create physical register file
Ifc_regFile ifc_regFile <- mkRegFile();
// Instruction Cache
Ifc_nbicache#(`ADDRESS_WIDTH,`IWays,`Word_size,`ICACHE_BLOCK_SIZE,512,16) icache <- mknbicache();
// Data Cache 
Ifc_nbdcache#(`ADDRESS_WIDTH,`DWays,`Word_size,`DCACHE_BLOCK_SIZE,512,16,`PRF_SIZE) dcache <- mknbdcache();
//Memory mapped CSRs module to hangle time and time compare registers
IFC_MemoryMappedCSRs mmcsrs <- mkMemoryMappedCSRs(baseaddress);
//Interrupt controller
Ifc_CsrFile  csrFile <- mkCsrFile(0, mmcsrs.timerValue, False, False, False);

// --------------------------------------------------------------------------------------------
// Queues to manage request and response for this CPU module.
// Currently, CPU interacts through bus only with Instruction and Data Memory.
// Cache modules sit inside the CPU core.

// Queues for interaction with memory
// Read channel
FIFOF #(Req_CPU) ff_request_to_memory_rd 	<- mkFIFOF;
FIFOF #(Rsp_CPU) ff_response_from_memory_rd  <-mkFIFOF;

// Write channel
FIFOF #(Req_CPU) ff_request_to_memory_wr 	<- mkFIFOF;
FIFOF #(Rsp_CPU) ff_response_from_memory_wr  <- mkFIFOF;
// --------------------------------------------------------------------------------------------

//Register to implement flush when there is load mis-speculative execution or branch mis-prediction
Reg#(Bool) rg_revert_map <- mkConfigReg(False);

//Register to flush the bus
Reg#(Bool) rg_flush_bus <- mkConfigReg(False);
 
//Register to store the bad address from load store unit
Reg#(Maybe#(Bit#(`REG_WIDTH))) rg_badaddr <- mkReg(tagged Invalid);
//Wire to transfer increment value in erob tail
Wire#(Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)))  wr_update_rob_tail <- mkDWire(0);

//This wire is asserted when pipeline flush is required
Wire#(Bit#(`REG_WIDTH)) wr_squash_pc <- mkWire();

//Wire to update respect slots in erob if there is flush
Wire#(Bool) dwr_squash <- mkDWire(False);

//Wire to transfer PC value to erob if there is flush during execution 
Wire#(Bit#(`REG_WIDTH)) dwr_squash_pc <- mkDWire(0);

//Wires to store load_q and store_q tails
Wire#(Bit#(TLog#(`MEMQ_SIZE))) wr_load_q_tail <- mkWire();
Wire#(Bit#(TLog#(`MEMQ_SIZE))) wr_store_q_tail <- mkWire();

//Wires to transfer rRAM entries
Wire#(RAT_entry) wr_rRAM_entry_1 <- mkDWire(0);
Wire#(RAT_entry) wr_rRAM_entry_2 <- mkDWire(0);

//Wire to carry inputs to CSR File
Wire#(Maybe#(Interrupt_payload_type)) wr_system_payload <- mkDWire(defaultValue); 

//Create broadcast wires, one from each ALU and LS unit
Wire#(Broadcast_type) wr_broadcast_ls <- mkDWire(Broadcast_type{          //TODO: change this
								valid: False,
     	 							dest_tag: 0
     	 							});
Wire#(Broadcast_type) wr_mul_broadcast <- mkDWire(Broadcast_type{          //TODO: change this
								valid: False,
     	 							dest_tag: 0
     	 							});

Wire#(Broadcast_type) wr_div_broadcast <- mkDWire(Broadcast_type{          //TODO: change this
								valid: False,
     	 							dest_tag: 0
     	 							});

Wire#(Broadcast_type) wr_system_broadcast <- mkDWire(Broadcast_type{          //TODO: change this
								valid: False,
     	 							dest_tag: 0
     	 							});

//Broadcast wire separately for load store unit
Vector#(`FUN_UNITS, Wire#(Broadcast_type)) wr_broadcast <- replicateM(mkDWire(Broadcast_type{
  valid: False,
  dest_tag: 0
  }));

Vector#(`FUN_UNITS, Wire#(Maybe#(Result_bypass_type))) wr_result_broadcast <- replicateM(mkDWire(tagged Invalid));
FIFO#(Result_bypass_type) ff_result_bypass_div <- mkBypassFIFO;

//FIFOs to maintain inelastic pipeline between Fetch, Decode and Map stages
FIFOF#(Fetched_instruction) ff_fetch_to_decode_1 <- mkSizedFIFOF(4);
FIFO#(Fetched_instruction_2) ff_fetch_to_decode_2 <- mkSizedFIFO(4); 
FIFO#(Decode_packet) ff_decode_to_map <- mkPipelineFIFO();


//////////////////////////////////////////////////////////////////////
///VERIFICATION FRAMEWORK
//////////////////////////////////////////////////////////////////////

   /* Open the files to dump the logs of different buffer contents */

   Reg#(int) rg_instr_count <- mkReg(0);
   Reg#(int) rg_squash_count <- mkReg(0);
   Vector#(`PRF_SIZE, Wire#(Bit#(`REG_WIDTH))) wr_ifc_regFile <- replicateM(mkDWire(0));
   Vector#(`REGFILE_SIZE, Wire#(RAT_entry)) wr_rRAM <- replicateM(mkDWire(0));
   
   // rl_open_dump_file fires when this reg is true.
   // asserted only once on reset
   Reg#(Bool) rg_open_dump_file <- mkReg(True);
   // holds pointer to the dump file
   let rg_iq_dump_file <- mkReg(InvalidFile);
   let rg_fram_dump_file <- mkReg(InvalidFile);
   let rg_frq_dump_file <- mkReg(InvalidFile);
   let rg_prf_dump_file <- mkReg(InvalidFile);

   // rg_open_dump_file is asserted once on reset
   rule rl_open_dump_file (rg_open_dump_file);
	  String fram_dumpFile = "fram_status.txt";
	  String frq_dumpFile = "frq_status.txt";
	  String prf_dumpFile = "prf_status.txt";
	  String print_icache = "icache.txt";
	  String print_dcache = "dcache.txt";
	  String iq_dumpFile = "iq_status.txt";
   	  File fl_iq_dump <- $fopen( iq_dumpFile, "w");
	  File fl_fram_dump <- $fopen( fram_dumpFile, "w");
	  File fl_frq_dump <- $fopen(frq_dumpFile, "w");
	  File fl_prf_dump <- $fopen(prf_dumpFile, "w");
	  File fl_icache <- $fopen(print_icache, "w");
	  File fl_dcache <- $fopen(print_dcache, "w");
      if (fl_iq_dump == InvalidFile || 
		 fl_fram_dump == InvalidFile || fl_frq_dump == InvalidFile || 
		 fl_prf_dump == InvalidFile)
		 begin
			$finish();
		 end
      rg_open_dump_file <= False;
	  rg_iq_dump_file <= fl_iq_dump;
	  rg_fram_dump_file <= fl_fram_dump;
	  rg_frq_dump_file <= fl_frq_dump;
	  rg_prf_dump_file <= fl_prf_dump;
	  //rg_icache <= fl_icache;
	  //rg_dcache <= fl_dcache;

   endrule   

   
//////////////////////////////////////////////////////////////////////
///VERIFICATION FRAMEWORK
//////////////////////////////////////////////////////////////////////


/* Connects I-cache and processor */
rule rl_connect_icache_processor_1(!rg_revert_map && !decode.abandone_cache);
       
       Bpu_packet lv_bpu_packet_1 <- bpu._incoming_pc_1(fetch.return_pc);     //Predicted pc sent by branch predictor unit 
       Bpu_packet lv_bpu_packet_2 <- bpu._incoming_pc_2(fetch.return_pc_2);
       let lv_icache_addr <- fetch.fetch_enq(lv_bpu_packet_1, lv_bpu_packet_2);
	   let lv_transize = 2'b11;
	   let lv_info_to_cpu = From_Cpu {	address 		: lv_icache_addr,               // Address being sent to cache
									  	transfer_size 	: lv_transize,
										cache_enable 	: 1'b1,
										prediction1 : lv_bpu_packet_1.predict_taken_or_not,  //Prediction if branch taken or not being sent-
									 	prediction2 : lv_bpu_packet_2.predict_taken_or_not}; //to cache only to be returned along with instruction
       icache.request_from_cpu(lv_info_to_cpu);
	   $display("data_requested_frm_icache for PC %h %d", lv_icache_addr, $time);
       
endrule

//Cache response is equeued in FIFO which in turn is read in Decode stage.
rule response_from_cache(ff_fetch_to_decode_1.notFull && !rg_revert_map && !decode.abandone_cache);
		let cache_response = icache.response_to_cpu;                                //response obtained from the cache
		icache.response_deqResult(); 
		$display("data_obtained_frm_icache %h at pc %h at time %d", cache_response.data_word, cache_response.pc, $time);
		TrapCause lv_exception = No_trap;
		Bit#(`INSTR_WIDTH) lv_instr_1 = cache_response.data_word[31:0], lv_instr_2 = cache_response.data_word[63:32];
		Bit#(`ADDRESS_WIDTH) lv_pc = cache_response.pc;
		Bool lv_valid = True;
		if(cache_response.pc[2] == 1) begin
			lv_instr_1 = cache_response.data_word[63:32];
		end
		if(cache_response.pc[2] == 1 || cache_response.prediction1 == Predict_taken) begin //if predicted jump after the first instructioni-
			lv_instr_2 = 32'h00000013;													  //or address is misaligned with 64bits then second instruction is invalidated
			lv_valid = False;
		end
		
		if(cache_response.mis_aligned_error == 1)
		lv_exception = tagged Exception InstAddrMisaligned;
		let fetch_0 = Fetched_instruction {	program_counter : lv_pc, //response from the cache is directly enqueued into FIFO
		 						instruction 	: lv_instr_1,
		 						prediction 	: cache_response.prediction1,  
		 						exception  	: lv_exception};

		let fetch_1 = Fetched_instruction_2 {fetched_instruction : Fetched_instruction 
										{	program_counter : lv_pc + 4, 
											instruction 	: lv_instr_2,
											prediction 	: cache_response.prediction2,  
											exception  	: lv_exception},
											valid 		: lv_valid};
		ff_fetch_to_decode_1.enq(fetch_0);
		ff_fetch_to_decode_2.enq(fetch_1);

		$display("\n \t \t \t \t \t ********************* FETCH ************************\n");
		$display(fshow(fetch_0));
		$display(fshow(fetch_1));
		$display("\n \t \t \t \t \t ****************************************************\n\n\n");
		
endrule

//This stalls the cache in times of flush or misprediction 
rule abandone_instr_in_cache(rg_revert_map || decode.abandone_cache);
		icache.abandon_cycle();
endrule

//This rule dequeues info from cache, executes decode and enqueues for map stage
rule rl_connect_fetch_decode(!rg_revert_map);

	   let lv_d_to_m <- decode.decode_enq(ff_fetch_to_decode_1.first, ff_fetch_to_decode_2.first);
	   ff_fetch_to_decode_1.deq;
	   ff_fetch_to_decode_2.deq;
	   ff_decode_to_map.enq(lv_d_to_m);
endrule

//This rule dequeues info from decode stage and maps the destination registers to new ones and enqueues into erob 
rule rl_connect_decode_map(!rg_revert_map && !inst_Q.if_erob_full && !frq.if_frq_empty && 
		 !((isLD(ff_decode_to_map.first().decode_packet[0].instruction_decoded.inst_op) ||
					isLD(ff_decode_to_map.first().decode_packet[1].instruction_decoded.inst_op)) && ls_unit.is_load_q_full) &&
		 !((isSTR(ff_decode_to_map.first().decode_packet[0].instruction_decoded.inst_op) ||
					isSTR(ff_decode_to_map.first().decode_packet[1].instruction_decoded.inst_op)) && ls_unit.is_store_q_full) &&
		 !((ff_decode_to_map.first().decode_packet[0].instruction_decoded.imm_valid  || 
					ff_decode_to_map.first().decode_packet[1].instruction_decoded.imm_valid) && inst_Q.if_imm_buf_full));  //TODO more to be added here);

		map.map_and_rename(ff_decode_to_map.first());
		ff_decode_to_map.deq;
endrule

	   
/****************** PC from Decode to Fetch ***************/   
rule rl_decode_to_fetch(!rg_revert_map); //Send back pc incase of JAL instructions
	fetch.pc_frm_decode(decode.send_pc);	
endrule

/****************** From Instruction Queue to Map ******************/
mkConnection(inst_Q.to_map, map.frm_inst_Q);

/****************** FRQ to map *****************/
mkConnection(frq.to_map, map.frm_FRQ);

/****************** fRAM to map *****************/
mkConnection(fRAM.to_map, map.frm_fRAM);

//This rule always fires - port mapping between several modules
rule rl_always_fire;
    
	//get load buffer tail and store buffer tail from load store unit
    map.get_load_q_tail(wr_load_q_tail);
    map.get_store_q_tail(wr_store_q_tail);

    let erob_entries = inst_Q.rob_entries;
    let immediate_entries = inst_Q.imm_entries;
    let selected_for_exec = inst_Q.selected_for_execution;
    let op1_entries = inst_Q.op1_ready_info;
    let op2_entries = inst_Q.op2_ready_info;
	$display("op1_entries %d", op1_entries[10]);

    //port mapping of wakeup
    wakeup.get_erob_entries(erob_entries);

    //port mapping of select_and_grant    
    select_grant.get_revert_front_end(rg_revert_map);
    select_grant.get_rob_details(erob_entries);
		select_grant.get_rob_head(inst_Q.send_entry_rob_head);
		select_grant.if_erob_empty(inst_Q.if_erob_empty);
    select_grant.get_imm_value(immediate_entries);
    select_grant.get_if_selected_for_exec(selected_for_exec);
    select_grant.get_if_operand1_ready(op1_entries);
    select_grant.get_if_operand2_ready(op2_entries);
    Vector#(`PRF_SIZE, Bit#(`REG_WIDTH)) prf_values;
    for(Integer i = 0; i < `PRF_SIZE; i=i+1) 
		prf_values[i] = ifc_regFile.read(fromInteger(i));
    select_grant.get_regfile_values(prf_values);

    //port mapping of commit
    commit.flush_signals(rg_revert_map);
    commit.erob_head_entry(inst_Q.send_entry_rob_head_entries);
		commit.imm_head_entry(inst_Q.send_imm_buf_head_entries);
    commit.entry_rob_execute_done(inst_Q.send_heads_execute_done);
		commit.entry_rob_exceptions(inst_Q.send_heads_exception);
    commit.squash_buf_entry(inst_Q.send_heads_squash_value);
    commit.squash_buf_status(inst_Q.send_heads_rob_squash);
	//commit.to_stall(interrupt_controller.if_commit_stall);
endrule

//This rule always fires - port mappings that cannot be incorporated in a single rule are separated
rule rl_always_fire_1;

    wr_rRAM_entry_1 <= rRAM.return_val_in_rRAM_1(inst_Q.send_entry_rob_head_entries[0].dest_arch);
    wr_rRAM_entry_2 <= rRAM.return_val_in_rRAM_2(inst_Q.send_entry_rob_head_entries[1].dest_arch);

	//Port mapping of wake up.
    wakeup.get_squash_pc(dwr_squash_pc);
    wakeup.get_if_squash(dwr_squash);
	let squash_load = ls_unit.if_load_aliased; 
    commit.commit_load(squash_load);
endrule

//This rule always fires - port mappings that cannot be incorporated in a single rule are separated
rule rl_always_fire_2;
    commit.get_rRAM_entry_1(wr_rRAM_entry_1);
    commit.get_rRAM_entry_2(wr_rRAM_entry_2);
endrule

rule rl_always_fire_3;
    Vector#(`FETCH_WIDTH, Bool) if_inst_load;
	if_inst_load = commit.is_inst_load;
    ls_unit.commit_load(if_inst_load);
endrule

//Rule to update tail of erob
rule rl_update_head_and_tail(!rg_revert_map);

    wr_update_rob_tail <= map.send_update_rob_tail;
    inst_Q.update_rob_tail(map.send_update_rob_tail);

endrule

//Rule to update tail of immediate buffer
rule rl_update_imm_tail_frm_map(!rg_revert_map &&& map.update_imm_buf_tail matches tagged Valid .tail);
    inst_Q.update_imm_tail(tail);
endrule

//Send broadcast information from several functional units to map stage and wakeup stage    
rule rl_send_broadcast_information(!rg_revert_map);
  Vector#(TSub#(`FUN_UNITS,1), Broadcast_type) lv_broadcast;
	lv_broadcast =  select_grant.send_pre_exe_broadcast;
	Vector#(`FUN_UNITS, Broadcast_type) lv_all_func_broadcast;
    for(Integer i = 0; i < `FUN_UNITS-1; i = i+1) begin
			wr_broadcast[i] <= lv_broadcast[i];
			lv_all_func_broadcast[i] = lv_broadcast[i];
    end
	if(wr_broadcast_ls.valid == True)
	$display("broadcast from the load store");
	lv_all_func_broadcast[2] = wr_broadcast_ls;
	if(wr_system_payload matches tagged Valid .payload)
		lv_all_func_broadcast[4] = Broadcast_type { valid : True, dest_tag : payload.dest_op};
	else if(wr_div_broadcast.valid == True)
		lv_all_func_broadcast[4] = wr_div_broadcast;
	else 
		lv_all_func_broadcast[4] = wr_mul_broadcast;
	wr_broadcast[4] <= lv_all_func_broadcast[4];
  map.broadcast_frm_fununits(lv_all_func_broadcast);
  wakeup.broadcast_of_functional_units(lv_all_func_broadcast);
endrule

rule rl_send_broadcast_frm_mul_div_csr(!rg_revert_map && !isValid(wr_system_payload) && !wr_div_broadcast.valid);
		let lv_mul_broadcast <- select_grant.send_pre_exe_broadcast_mul;
		wr_mul_broadcast <= lv_mul_broadcast;
endrule

for(Integer i = 0; i <`ALU_UNITS; i = i + 1) begin 
		rule rl_update_prf_valid_alu(!rg_revert_map && wr_broadcast[i].valid);
	  		//update prf_valid
	  		inst_Q.update_broadcast_ifc[i].update_Prf_valid(wr_broadcast[i].dest_tag);
		endrule
end

rule rl_update_prf_valid_branch(!rg_revert_map && wr_broadcast[3].valid);
	//update prf_valid
	inst_Q.update_Prf_valid_2(wr_broadcast[3].dest_tag);
endrule
    
rule rl_update_prf_valid_mul_div(!rg_revert_map && wr_broadcast[4].valid);
	//update prf_valid
	inst_Q.update_Prf_valid_3(wr_broadcast[4].dest_tag);
endrule


//result broadcast to select stage and wakeup stage.
rule rl_result_broadcast_to_wake_up;
     Vector#(`FUN_UNITS, Maybe#(Result_bypass_type)) lv_result_broadcast;
     for(Integer i = 0; i < `FUN_UNITS; i = i + 1)
          lv_result_broadcast[i] = wr_result_broadcast[i];
     wakeup.result_from_fununits(lv_result_broadcast);
     select_grant.bypass_forwarding(lv_result_broadcast);
endrule

//When flushed due to branch misprediction or false speculated load instructions    
rule rl_squash_pc_frm_commit(!rg_revert_map &&& commit.squash_pc matches tagged Valid .pc);
	wr_squash_pc <= pc;
endrule

//Training information from branch unit to Branch Predictor Unit
rule rl_branch_training;

       let lv_training_packet <- branch_unit._training_packet();
       bpu._training(lv_training_packet.pc,lv_training_packet.jump_pc,lv_training_packet.taken_or_not);

endrule

Rules re_fill_slot_1_in_map = (rules 
//Port mapping of map stage with several Register structure modules
    rule rl_fill_slot_1_in_map(!rg_revert_map); 
   		if(map.imm_buf_entry_1.valid == True) 
    	inst_Q.fill_imm_entries_1(map.imm_buf_entry_1.imm);	
    
        let fRAM_slot = map.send_fRAM_slot_1; 
        if(fRAM_slot matches tagged Valid .slot)
    	fRAM.update_fRAM_1(slot, map.send_fRAM_entry_1);	
		frq.update_head(map.update_frq_head);
		if(map.invalidate_prf_valid_1 matches tagged Valid .slot)
			inst_Q.invalidate_prf_valid_1(slot);
  		//port mapping from map stage to erob module      

        if(wr_update_rob_tail > 0) begin
    	inst_Q.fill_entry_rob_1(map.send_entry_rob_1);	   
    	inst_Q.fill_entry_rob_op_1_ready_1(map.send_entry_rob_op_1_ready_1);
    	inst_Q.fill_entry_rob_op_2_ready_1(map.send_entry_rob_op_2_ready_1);
    	inst_Q.fill_entry_rob_execute_done_1(map.send_entry_rob_execute_done_1);
    	inst_Q.fill_entry_rob_squash_1(map.send_entry_rob_squash_1);
    	inst_Q.fill_squash_buf_1(map.send_squash_buf_1);
    	inst_Q.fill_selected_for_exec_1(map.send_selected_for_exec_1);
		inst_Q.fill_entry_rob_execution_1(map.send_if_exception_1);	
        end
    endrule
endrules);
    
Rules re_fill_slot_2_in_map = (rules
//Port mapping of map stage with several Register structure modules
//This rule is for the second instruction being dispatched into map stage
    rule rl_fill_slot_2_in_map(!rg_revert_map); 
   		if(map.imm_buf_entry_2.valid == True) 
    	inst_Q.fill_imm_entries_2(map.imm_buf_entry_2.imm);	
    
        let fRAM_slot = map.send_fRAM_slot_2; 
        if(fRAM_slot matches tagged Valid .slot)
    		fRAM.update_fRAM_2(slot, map.send_fRAM_entry_2);	
		if(map.invalidate_prf_valid_2 matches tagged Valid .slot)
			inst_Q.invalidate_prf_valid_2(slot);
		if(map.allot_mem_q matches tagged Valid .mem_type)
			ls_unit.allot_mem_q(mem_type);
    
  		//port mapping from map stage to erob module      
        if(wr_update_rob_tail > 1) begin
    	inst_Q.fill_entry_rob_2(map.send_entry_rob_2);	   
    	inst_Q.fill_entry_rob_op_1_ready_2(map.send_entry_rob_op_1_ready_2);
    	inst_Q.fill_entry_rob_op_2_ready_2(map.send_entry_rob_op_2_ready_2);
    	inst_Q.fill_entry_rob_execute_done_2(map.send_entry_rob_execute_done_2);
    	inst_Q.fill_entry_rob_squash_2(map.send_entry_rob_squash_2);
    	inst_Q.fill_squash_buf_2(map.send_squash_buf_2);
    	inst_Q.fill_selected_for_exec_2(map.send_selected_for_exec_2);
		inst_Q.fill_entry_rob_execution_2(map.send_if_exception_2);	
        end
    endrule    
endrules);

(*conflict_free = "rl_update_csr_registers,rl_squash_pc_frm_commit"*)

//This rule handles csr registers at the time of exception or trap at commit stage
rule rl_update_csr_registers(!rg_revert_map);
	if(!isNoTrap(commit.return_exception)) begin
		$display("Exception occured at PC %h", commit.return_pc);
		let lv_trap	 <- csrFile.wr(commit.return_pc, 
																tagged Invalid,
																unpack(12'h000),
																0,
																validValue(rg_badaddr),
																tagged Valid commit.return_exception,
																0,
																False,
																False);
			if(lv_trap matches tagged Exception .trap) begin
				$display("New trap handler PC is PC %h", trap.trapHandlerPC);
				wr_squash_pc <= trap.trapHandlerPC;
			end
	end
							
	else if(wr_system_payload matches tagged Valid .system_input) begin
		let lv_system_broadcast <- csrFile.wr(0, 
																			tagged Valid system_input.system_inst,
																			unpack(system_input.csr_address),
																			system_input.src_1,
																			0,
																			tagged Invalid,
																			0,
																			False,
																			False);
		if(lv_system_broadcast matches tagged CsrData .data) begin
			wr_result_broadcast[4] <= tagged Valid Result_bypass_type{ dest_tag : system_input.dest_op,
																	_result		: data};
		end
		else if(lv_system_broadcast matches tagged RedirectPC .pc) begin
			$display("PC redirected to %h due return instructions", pc);
			wr_squash_pc <= pc;
		end
	end				

	//If the instructions at the head of the erob are machine mode instructions, then they are executed during commit stage.
	//let lv_erob_head_entries = inst_Q.send_entry_rob_head_entries;
	//Bit#(`REG_WIDTH) lv_operand = 0;
	//Maybe#(Bit#(`REG_WIDTH)) lv_regfile_update_1 = tagged Invalid, lv_regfile_update_2 = tagged Invalid;
	////The operand for the instruction may be immediate value encoded in rs1 field or present in rs1 register
	//if(lv_erob_head_entries[0].imm_valid)
	//	lv_operand = zeroExtend(lv_erob_head_entries[0].op_1);
	//else 
	//	lv_operand = ifc_regFile.read(unpack(lv_erob_head_entries[0].op_1));

	////The value in the immediate buffer represents CSR address register value
	//let lv_csr_addr_1 = inst_Q.imm_entries[lv_erob_head_entries[0].imm_index];

	////The values in op1 and CSR registers are exchanged as instruction proposes.
	//if(commit.update_csr_registers[0]) 
	//	lv_regfile_update_1 <- interrupt_controller.update_register_1(lv_erob_head_entries[0].csr_inst_type,
	//		 inst_Q.imm_entries[lv_erob_head_entries[0].imm_index].imm, lv_erob_head_entries[0].csr_valid, lv_operand);  
	//if(lv_erob_head_entries[0].dest_op != lv_erob_head_entries[1].dest_op) begin
	//	if(lv_regfile_update_1 matches tagged Valid .res)
	//		ifc_regFile._write(unpack(lv_erob_head_entries[0].dest_op), res);
	//end

	////If both the instructions at the head are machine mode instructions then same is done for second instructions after checking dependencies
	//if(lv_erob_head_entries[1].imm_valid)
	//	lv_operand = zeroExtend(lv_erob_head_entries[1].op_1);
	//else 
	//	lv_operand = ifc_regFile.read(unpack(lv_erob_head_entries[1].op_1));

	//let lv_csr_addr_2 = inst_Q.imm_entries[lv_erob_head_entries[1].imm_index];

	//if(commit.update_csr_registers[1]) 
	//	lv_regfile_update_2 <- interrupt_controller.update_register_2(lv_erob_head_entries[1].csr_inst_type,
	//		 inst_Q.imm_entries[lv_erob_head_entries[0].imm_index].imm, lv_erob_head_entries[1].csr_valid, lv_operand);
	//if(lv_regfile_update_2 matches tagged Valid .res)
	//   ifc_regFile._write(unpack(lv_erob_head_entries[1].dest_op), res);

 endrule
		
Rules re_wakeup = emptyRules;
Rules re_execute_done = emptyRules;
Rules re_op1 = emptyRules;
Rules re_op2 = emptyRules;
Rules re_rob_squash = emptyRules;
Rules re_squash_value = emptyRules;
Rules re_entry_rob_exception = emptyRules;
Rules re_selected_for_execution = emptyRules;

for(Integer i = 0; i < `ENTRY_ROB_SIZE; i=i+1) begin 

    Rules rs_execute_done = (rules  
		//update the slots in erob if they are done with execution
        rule rl_update_execute_done_in_inst_Q(wakeup.if_execute_done()[i] == True);
    		inst_Q.map_to_IQ_ifc[i].update_entry_rob_execute_done(True);
        endrule

    endrules);
    
    Rules rs_op1 = (rules
		//Tag source operands ready in erob
        rule rl_update_op1_in_inst_Q(wakeup.if_op1_ready_in_erob()[i] == True);
    		inst_Q.map_to_IQ_ifc[i].update_if_op1_ready_in_erob(True);
        endrule

    endrules);
    
    Rules rs_op2 = (rules
		//Tag source operands ready in erob
        rule rl_update_op2_in_inst_Q(wakeup.if_op2_ready_in_erob()[i] == True);
    		inst_Q.map_to_IQ_ifc[i].update_if_op2_ready_in_erob(True);
        endrule

    endrules);
    
    Rules rs_rob_squash = (rules 
		//Fill the squash value is true from the broadcasted value in branch
        rule rl_update_entry_rob_squash_in_inst_Q(wakeup.if_entry_rob_squash()[i] == True);
    		inst_Q.map_to_IQ_ifc[i].update_if_entry_rob_squash(True);
        endrule
    
    endrules);

    Rules rs_squash_value = (rules
		//Fill the squash buffer with pc from the broadcasted value in branch
        rule rl_update_squash_value_in_inst_Q(wakeup.send_squash_value()[i] matches tagged Valid .value);
    		inst_Q.map_to_IQ_ifc[i].update_squash_value(value);
        endrule
        
    endrules);

	Rules rs_entry_rob_exception = (rules
		//Update the exception field in entry rob, if exception encountered in execution units
		rule rl_update_erob_exception_in_inst_Q(wakeup.if_entry_rob_exception()[i] matches tagged No_trap);
    		inst_Q.map_to_IQ_ifc[i].update_entry_rob_exception(wakeup.if_entry_rob_exception()[i]);
		endrule

	endrules);

    Rules rs_selected_for_execution = (rules 
		//Update the entry rob slots that are selected for execution based on logic in select and grant stage
        rule rl_update_selected_for_execution(select_grant.send_selected_slots_in_ROB[i] == True);
    	inst_Q.map_to_IQ_ifc[i].update_selected_for_execution(True);
        endrule
    
    endrules);
    
	//Syntax to apply rule attributes to rules in the same group
    re_execute_done = rJoinConflictFree(re_execute_done, rs_execute_done);
    re_op1 = rJoinConflictFree(re_op1, rs_op1);
    re_op2 = rJoinConflictFree(re_op2, rs_op2);
    re_rob_squash = rJoinMutuallyExclusive(re_rob_squash, rs_rob_squash);
    re_squash_value = rJoinMutuallyExclusive(re_squash_value, rs_squash_value);
	re_entry_rob_exception = rJoinMutuallyExclusive(re_entry_rob_exception, rs_entry_rob_exception);
    re_selected_for_execution = rJoinConflictFree(re_selected_for_execution, rs_selected_for_execution);

end

//Applying rule attributes to rules in other groups
re_wakeup = rJoinConflictFree(re_execute_done, re_wakeup);
re_wakeup = rJoinConflictFree(re_op1, re_wakeup);
re_wakeup = rJoinConflictFree(re_op2, re_wakeup);
re_wakeup = rJoinConflictFree(re_rob_squash, re_wakeup);
re_wakeup = rJoinConflictFree(re_squash_value, re_wakeup);
re_wakeup = rJoinConflictFree(re_selected_for_execution, re_wakeup);
re_wakeup = rJoinConflictFree(re_fill_slot_1_in_map, re_wakeup);
re_wakeup = rJoinConflictFree(re_fill_slot_2_in_map, re_wakeup);

addRules(re_wakeup);


//sending inputs to ALU unit that are enqueued into payload FIFO in select and grant stage 
rule rl_send_inputs_to_ALU_0;
   let lv_payload <- select_grant.send_ALU_0_inputs;
   alu[0]._inputs(lv_payload.alu_type, lv_payload.alu_op, lv_payload.word_flag, lv_payload.src_1, lv_payload.src_2, lv_payload.dest_op, lv_payload.pc);
endrule

//sending inputs to ALU unit that are enqueued into payload FIFO in select and grant stage 
rule rl_send_inputs_to_ALU_1;
   let lv_payload <- select_grant.send_ALU_1_inputs;
   alu[1]._inputs(lv_payload.alu_type, lv_payload.alu_op, lv_payload.word_flag, lv_payload.src_1, lv_payload.src_2, lv_payload.dest_op, lv_payload.pc);
endrule

(*conflict_free = "rl_send_inputs_to_MUL, rl_send_inputs_to_DIV"*)
//sending inputs to Multiplier unit that are enqueued into payload FIFO in select and grant stage 
rule rl_send_inputs_to_MUL(select_grant.send_mul_div_system_inputs matches tagged Mul_div_inst .lv_md_payload); 
	if(lv_md_payload.alu_type == MUL) begin 
		mul_div.mul_inputs(lv_md_payload.alu_op, lv_md_payload.word_flag, lv_md_payload.src_1, lv_md_payload.src_2, lv_md_payload.dest_op);	
		select_grant.release_mul_div_payload;
	end
endrule

//sending inputs to Divider unit that are enqueued into payload FIFO in select and grant stage 
rule rl_send_inputs_to_DIV(select_grant.send_mul_div_system_inputs matches tagged Mul_div_inst .lv_md_payload); 
	if(lv_md_payload.alu_type == DIV) begin 
		mul_div.div_inputs(lv_md_payload.alu_op, lv_md_payload.word_flag, lv_md_payload.src_1, lv_md_payload.src_2, lv_md_payload.dest_op);	
		select_grant.release_mul_div_payload;
	end
endrule

//sending inputs to Multiplier and Divider unit that are enqueued into payload FIFO in select and grant stage 
rule rl_send_inputs_to_CSR_file(select_grant.send_mul_div_system_inputs matches tagged System_inst .lv_system_payload);
		wr_system_payload <= tagged Valid lv_system_payload;
		select_grant.release_mul_div_payload;
endrule

//sending inputs to Load Store unit that are enqueued into payload FIFO in select and grant stage 
rule rl_send_inputs_to_LS(!rg_revert_map);
	let lv_send_LS_inputs <- select_grant.send_LS_inputs;
    ls_unit.inputs(lv_send_LS_inputs.base, lv_send_LS_inputs.offset, lv_send_LS_inputs.mem_q_index, lv_send_LS_inputs.dest_op, lv_send_LS_inputs.str_data, lv_send_LS_inputs.mem_size);
endrule

//sending inputs to branch unit that are enqueued into payload FIFO in select and grant stage 
rule rl_send_inputs_to_branch;
	let lv_payload <- select_grant.send_Branch_input;
    branch_unit.inputs(lv_payload.branch_op, lv_payload.src_1, lv_payload.src_2, lv_payload.dest_op, lv_payload.imm, lv_payload.program_counter, lv_payload.prediction);
endrule
   
//Signal from commit stage to invalidate immediate buffer slot 
(*conflict_free = "rl_invalidate_imm_1, rl_invalidate_imm_2"*)
rule rl_invalidate_imm_1(!rg_revert_map &&& commit.invalidate_imm_slot_1 matches tagged Valid .slot);
    inst_Q.invalidate_imm_1(slot);
    $display("commit update head %d %d", commit.update_imm_head, $time);
endrule

//Signal from commit stage to invalidate immediate buffer slot 
rule rl_invalidate_imm_2(!rg_revert_map &&& commit.invalidate_imm_slot_2 matches tagged Valid .slot);
    inst_Q.invalidate_imm_2(slot);
    $display("commit update head %d %d", commit.update_imm_head+1, $time);
endrule

//Signal from commit stage to invalidate entry rob slot 
rule rl_invalidate_rob_1(commit.erob_invalidate_1 == True);
    inst_Q.invalidate_erob_1;
endrule

(*conflict_free = "rl_invalidate_rob_1, rl_invalidate_rob_2"*)
//Signal from commit stage to invalidate entry rob slot 
rule rl_invalidate_rob_2(commit.erob_invalidate_2 == True);
    inst_Q.invalidate_erob_2;
endrule

//Signal from commit stage to update head in entry rob and immediate buffer
rule rl_update_entry_rob_head(!rg_revert_map);
    inst_Q.update_entry_rob_head(commit.update_erob_head);
	inst_Q.update_imm_buf_head(commit.update_imm_head);
	$display("imm_head is %d %d", commit.update_imm_head, $time);
endrule

//Execute store and dequeue from store buffer upon reaching the head of entry rob
(*conflict_free = "rl_commit_store_1, rl_commit_store_2"*)
rule rl_commit_store_1(commit.commit_store_1 == True);
    ls_unit.commit_store();
endrule

/*In case of speculated load execution failure, flush and execute load again 
dequeue from load buffer upon reaching the head of entry rob*/
rule rl_commit_store_2(commit.commit_store_2 == True);
    ls_unit.commit_store();
endrule

//update the mapping of registers of committed instructions during commit stage
(*conflict_free = "rl_update_rRAM_1, rl_update_rRAM_2"*)
rule rl_update_rRAM_1(commit.update_rRAM_1 == True);
    rRAM.update_rRAM_1(inst_Q.send_entry_rob_head_entries[0].dest_arch, inst_Q.send_entry_rob_head_entries[0].dest_op);
endrule

//update the mapping of registers of committed instructions during commit stage
rule rl_update_rRAM_2(commit.update_rRAM_2 == True);
    rRAM.update_rRAM_2(inst_Q.send_entry_rob_head_entries[1].dest_arch, inst_Q.send_entry_rob_head_entries[1].dest_op);
endrule

//Add the architectural registers of the instructions to Free Register Queue during commit stage
rule rl_update_frq_1(!rg_revert_map && commit.update_frq_tail > 0);
    frq.update_frq_1(commit.frq_update_1);
    frq.update_tail(commit.update_frq_tail);
endrule

(*conflict_free = "rl_update_frq_1, rl_update_frq_2"*)
//Add the architectural registers of the instructions to Free Register Queue during commit stage
rule rl_update_frq_2(!rg_revert_map && commit.update_frq_tail > 1);
    frq.update_frq_2(commit.frq_update_2);
endrule

   //(* descending_urgency = "rl_broadcast_frm_ls_unit_load, rl_broadcast_frm_ls_unit_store" *)
   
   /* Broadcast the LS unit result. Fires only if the LS unit has a valid
	  load broadcast */
   
   rule rl_broadcast_frm_ls_unit_load;
      
      let lv_broadcast_from_ls_unit_load <- ls_unit.get_load_broadcast_packet();
      wr_result_broadcast[2] <= tagged Valid Result_bypass_type { dest_tag : lv_broadcast_from_ls_unit_load.dest_tag,
    															 _result : lv_broadcast_from_ls_unit_load.result};
      wr_broadcast_ls <= Broadcast_type {
    	 valid: lv_broadcast_from_ls_unit_load.valid,
    	 dest_tag: lv_broadcast_from_ls_unit_load.dest_tag
    	 };
      
      $display("Time:%d\nvalid broadcast from LS unit LOAD", $time);
      $display(fshow(lv_broadcast_from_ls_unit_load));

   endrule: rl_broadcast_frm_ls_unit_load
   

   /* Broadcast the LS unit result. Fires only if the LS unit has a valid
      store broadcast */
   
   rule rl_broadcast_frm_ls_unit_store;
      
      let lv_broadcast_from_ls_unit_store <- ls_unit.get_store_broadcast_packet();
      wr_result_broadcast[2] <= tagged Valid Result_bypass_type { dest_tag : lv_broadcast_from_ls_unit_store.dest_tag,
    															 	_result : 0};
      
      wr_broadcast_ls <= lv_broadcast_from_ls_unit_store;

      $display("Time:%d\nvalid broadcast from LS unit STORE", $time);
      $display(fshow(lv_broadcast_from_ls_unit_store));
      
   endrule: rl_broadcast_frm_ls_unit_store   
   

//Broadcast pc and assert flush from branch unit to erob. These values are used when flushing in commit stage.
   rule rl_broadcast_squash_pc;
      
      let lv_squash_pc <- branch_unit.next_pc();
      dwr_squash_pc <= lv_squash_pc;

   endrule: rl_broadcast_squash_pc
   

   rule rl_broadcast_squash;
	  
	  let lv_squash <- branch_unit.get_squash();
	  dwr_squash <= lv_squash;

   endrule	  

//Update the mtvec CSR register with corresponding exception that is received from commit stage.
   //rule rl_send_exception_to_interrupt_controller(commit.return_exception matches tagged Valid .exception);
	 // interrupt_controller.take_exception(exception);
	 // if(commit.return_badaddr matches tagged Valid .badaddr)
	 // interrupt_controller.take_address_exception(badaddr); 
	 // else if(exception == 4 || exception == 5)
	 // interrupt_controller.take_address_exception(ls_unit.return_head_load_address);
	 // else if(exception == 6 || exception == 7)
	 // interrupt_controller.take_address_exception(ls_unit.return_head_store_address);
   //endrule

   //The flush signal is received from commit stage and sent to fetch to update PC.
   rule rl_enable_squash_reg(!rg_revert_map);
          
          $display("Time:%d\nCOMMENCE SQUASH",$time);
          fetch.squash_pc(wr_squash_pc);
          rg_revert_map <= True;
          
   endrule:rl_enable_squash_reg

(*execution_order = "rl_connect_dcache_processor_read, rl_processor_to_dcache_read"*)
 //D-cache read for load instructions
   rule rl_connect_dcache_processor_read;

	  let lv_read_req <- ls_unit.get_dcache_read_req();
	  dcache.request_from_cpu(From_Cpu_d {address : lv_read_req.address, transfer_size : lv_read_req.ld_size[1:0], u_signed : lv_read_req.ld_size[2], cache_enable : 1'b1, ld_st : lv_read_req.ld_st,write_data : lv_read_req.data, destination_tag : lv_read_req.dest_reg });
	  $display("address to cache is %d and transfer size is %d", lv_read_req.address, lv_read_req.ld_size[1:0]);
	  $display("Time:%d\nRead request to d-cache is", $time);
	  $display(fshow(lv_read_req));
	  
   endrule

   rule rl_processor_to_dcache_read;

      let lv_load_data = dcache.response_to_cpu; 
	  dcache.response_deqResult;
	  let data = lv_load_data.data_word;
      $display("Response from d-cache is %h with destination %h", lv_load_data.data_word, lv_load_data.destination_tag);
	  if(lv_load_data.u_signed == 0) begin
		$display("the data is signed");
		if(lv_load_data.transfer_size == 0)
			data  = signExtend(data[7:0]);
		if(lv_load_data.transfer_size == 1)
			data  = signExtend(data[15:0]);
		if(lv_load_data.transfer_size == 2)
			data  = signExtend(data[31:0]);
	  end
	  if(lv_load_data.mis_aligned_error == 1)
	  $finish(0);
      ls_unit.put_load_data(Load_Broadcast_type { valid : True, dest_tag : lv_load_data.destination_tag, result : data});

   endrule

   rule rl_abandone_dcache(rg_revert_map);
	 dcache.abandon_cycle;
   endrule 

   //Updating Register file and operand valid registers with broadcast from ALU.
   for(Integer i=0;i<`ALU_UNITS;i=i+1)
	  begin
		 
		 rule rl_update_prf_alu;
			
			let lv_result_bypass = alu[i].get_broadcast_packet();
			$display("Time:%d\nwriting data %h to address %d alu %d",$time, lv_result_bypass._result, unpack(lv_result_bypass.dest_tag), i);
			wr_result_broadcast[i] <= tagged Valid lv_result_bypass;
			
			//update PRF
			ifc_regFile._write(unpack(lv_result_bypass.dest_tag), lv_result_bypass._result);
			
		 endrule: rl_update_prf_alu
		 
	  end
   
   (* conflict_free = "rl_update_csr_registers, rl_update_prf_ls_unit, rl_update_prf_branch_unit, rl_update_prf_alu, rl_update_prf_alu_1, rl_update_prf_mul_div"*) 
      
   /* Update the PRF by LS unit */
   /* Updates :  PRF and prf_valid */

   rule rl_update_prf_mul_div(!rg_revert_map);
	  
		if(!isValid(wr_system_payload)) begin
			let lv_result_bypass = mul_div.get_broadcast_packet;
	  		wr_result_broadcast[4] <= tagged Valid lv_result_bypass;
			if(mul_div.result_from == Div)
				wr_div_broadcast <= Broadcast_type { valid : True, dest_tag : lv_result_bypass.dest_tag};
	  	
		end

   endrule

   rule rl_update_prf_mul_div_system(wr_result_broadcast[4] matches tagged Valid .broadcast_value); 
	  //update prf
	  ifc_regFile._write(unpack(broadcast_value.dest_tag), broadcast_value._result);
	  $display("Time:%d\nwriting data mul div %h to address %h", $time, broadcast_value._result, unpack(broadcast_value.dest_tag));
   endrule

   rule rl_update_prf_ls_unit(!rg_revert_map &&& wr_result_broadcast[2] matches tagged Valid .broadcast_value);
	  
	  //update prf
	  ifc_regFile._write(unpack(broadcast_value.dest_tag), broadcast_value._result);
	  
	  //update prf_valid
	  inst_Q.update_Prf_valid_1(broadcast_value.dest_tag);
	  
	  $display("Time:%d\nwriting data LS unit %h to address %h", $time, broadcast_value._result, unpack(broadcast_value.dest_tag));

   endrule: rl_update_prf_ls_unit
  

(*conflict_free = "rl_update_prf_ls_unit, rl_update_prf_valid_branch, rl_update_prf_valid_alu, rl_update_prf_valid_alu_1, rl_update_prf_valid_mul_div"*)

  rule rl_update_prf_branch_unit;
	  
	 let lv_result_bypass <- branch_unit.get_broadcast_packet();
	 wr_result_broadcast[3] <= tagged Valid lv_result_bypass;

	 ifc_regFile._write(unpack(lv_result_bypass.dest_tag), lv_result_bypass._result);
	  
	 $display("Time:%d\nwriting data from branch %h to address %h", $time, lv_result_bypass._result, unpack(lv_result_bypass.dest_tag));
	 
  endrule: rl_update_prf_branch_unit  			//TODO correct the display statements
   

   /* This rule adds back the mapped destination registers 
      of wrongly speculated instructions into FRQ till it
	  becomes empty */
   

   /* squash front end clears all fifos and buffers except IQ
	  and copies RRAM to FRAM
	   */

   rule rl_revert_map(rg_revert_map);

	  $display("Time:%d\nSquash Frontend rule is called",$time);
	  //Clear the fifos that connect Fetch, Decode and Map stages
	  ff_fetch_to_decode_1.clear();
	  ff_fetch_to_decode_2.clear();
	  ff_decode_to_map.clear();
	  ls_unit.clear_mem_queues();
	  // Update head and tail values in entry rob and immediate buffers to 0
	  for(Integer i=0;i<`ALU_UNITS;i=i+1)
	         alu[i]._set_flush();
	  mul_div._set_flush();
	  inst_Q.update_imm_tail(0); 
	  inst_Q.update_imm_head(0); 

	  inst_Q.invalidate_imm(True);
	  inst_Q.reset_entries_of_EROB();
	  frq.reset_entries_of_FRQ();

	  //Copy the values in rRAM to fRAM incase of flush
	  fRAM.update_whole_fRAM(rRAM.return_whole_rRAM);
	  inst_Q.reset_rob_head();
	  inst_Q.reset_rob_tail();
	  frq.reset_head();
	  frq.reset_tail();
	  rg_revert_map <= False;
	  
   endrule: rl_revert_map

	//Clear all the fifos in select and grant stage
	rule rl_revert_map_2(rg_revert_map);

	  select_grant.ff_alu_data_read_0_clear(True);
	  select_grant.ff_alu_data_read_1_clear(True);
	  select_grant.ff_ls_data_read_clear(True);
	  select_grant.ff_branch_data_read_clear(True);
	  select_grant.ff_alu_payload_0_clear(True);
	  select_grant.ff_alu_payload_1_clear(True);
	  select_grant.ff_ls_payload_clear(True);
	  select_grant.ff_branch_payload_clear(True);
	  select_grant.ff_mul_div_data_read_clear(True);
	  select_grant.ff_mul_div_payload_clear(True);

	endrule

   
//Send load buffer queue token and store buffer queue token to map stage.
   rule rl_update_from_ls_unit;
      
      wr_load_q_tail <= ls_unit.load_q_tail();
      wr_store_q_tail <= ls_unit.store_q_tail();
      
   endrule

////////////////////////////////////////////////////////////////////////////////
///VERIFICATION ENVIRONMENT 
////////////////////////////////////////////////////////////////////////////////
    rule rl_send_register_values;
	Vector#(`PRF_SIZE, Bit#(`REG_WIDTH)) prf_values;
	for(Integer i = 0; i < `PRF_SIZE;i=i+1)
	    prf_values[i] = ifc_regFile.read(fromInteger(i));
	commit._register_values(prf_values);
	Vector#(`REGFILE_SIZE, RAT_entry) rRAM_values;
	for(Integer i = 0; i < `REGFILE_SIZE; i=i+1)
	    rRAM_values[i] = rRAM.return_whole_rRAM[i];
	commit._rRAM_values(rRAM_values);
    endrule

//////////////////////////////////////////////////////////////////////
///VERIFICATION FRAMEWORK
//////////////////////////////////////////////////////////////////////



  // rule rl_display_iq_status;
  //    
  //    $fwrite(rg_iq_dump_file, "Time:%d\nHead = %d Tail = %d\n", 
  //  		  $time,inst_Q.send_entry_rob_tail, inst_Q.send_entry_rob_head);

  //    for(Integer i=0;i<`ENTRY_ROB_SIZE;i=i+1)
  //  	 begin
  //  		$fwrite(rg_iq_dump_file, "%d   %d   %d\n",i, 
  //  				inst_Q.rob_entries[i].valid, inst_Q.rob_entries[i].program_counter);
  //  	 end
  //    
  //    $fwrite(rg_iq_dump_file, "IMM_BUF:\n Head = %d Tail = %d\n", 
  //  		  inst_Q.send_imm_buf_head, inst_Q.send_imm_buf_tail);

  //    for(Integer i=0;i<`IMM_BUF_SIZE;i=i+1)
  //  	 $fwrite(rg_iq_dump_file, "%d %d\n", i, inst_Q.imm_entries[i].valid);
  //    
  //    $fwrite(rg_iq_dump_file, "\n");
  //    
  //    if(rg_revert_map)
  //  	 rg_squash_count <= rg_squash_count + 1;
  //    
  //    $fwrite(rg_iq_dump_file, "squash count %d", rg_squash_count);
  //    
  //    $fwrite(rg_iq_dump_file, "\n");
  //    
  //    
  // endrule   
  // 
  // 
  // 
  // rule rl_display_fram_contents;
  //        
  //        $fwrite(rg_fram_dump_file, "Time:%d\n", $time);
  //        
  //        for(Integer i=0;i<`REGFILE_SIZE;i=i+1)
  //      	 begin
  //      		$fwrite(rg_fram_dump_file, "%d %d     %d %d\n",i,fRAM.return_whole_fRAM[i], 
  //      				i, rRAM.return_whole_rRAM[i]);
  //      	 end
  //        $fwrite(rg_fram_dump_file, "\n");
  //        
  // endrule
  // 
  // 
  // rule rl_display_frq_contents;
  //        
  //        $fwrite(rg_frq_dump_file, "Time:%d\nHead = %d Tail = %d\n", 
  //      		  $time, frq.return_frq_head, frq.return_frq_tail);
  //        
  //        for(Integer i=0;i<`ENTRY_ROB_SIZE;i=i+1)
  //      	 begin
  //      		if(frq.return_whole_frq[i].valid)
  //      		   $fwrite(rg_frq_dump_file, "%d ", frq.return_whole_frq[i].free_reg);
  //      	 end
  //        $fwrite(rg_frq_dump_file, "\n");
  //        
  // endrule
  // 
  // 
  // rule rl_display_prf_contents;
  //        
  //        $fwrite(rg_prf_dump_file, "Time:%d\n", $time);
  //        
  //        for(Integer i=0;i<`PRF_SIZE;i=i+1)
  //      	 $fwrite(rg_prf_dump_file, "%d %d %d\n", i, 
  //      			 ifc_regFile.read(fromInteger(i)), inst_Q.send_prf_entries[i]);
  //        
  //        $fwrite(rg_prf_dump_file, "\n");
  //        
  // endrule


//////////////////////////////////////////////////////////////////////
///VERIFICATION FRAMEWORK
//////////////////////////////////////////////////////////////////////

// --------------------------------------------------------------------------------------------

	FIFO#(Bit #(`DCACHE_SIZE)) ff_icache_rd_req_addr <- mkSizedFIFO(2);
	FIFO#(Bit #(`DCACHE_SIZE)) ff_dcache_rd_req_addr <- mkSizedFIFO(4);
	Reg #(Bit #(`DCACHE_SIZE)) rg_dcache_wr_req_addr <- mkRegU;
	Reg #(Bit #(`DCACHE_SIZE)) rg_dcache_writeback_addr <- mkRegU;

	rule rl_request_to_memory_from_icache;
		let _req <- icache.request_to_memory;
		Req_Desc_CPU tlm_req = defaultValue;
		tlm_req.addr = _req.address;
		tlm_req.data = ?;
		tlm_req.command =  READ;
		tlm_req.burst_mode = WRAP;
		tlm_req.burst_length = 2;
		tlm_req.burst_size = 'b111;
		tlm_req.transaction_id = {1,_req.token};
		tlm_req.lock = True;
		tlm_req.prty = 0;
		Req_CPU req = tagged Descriptor tlm_req;

		ff_icache_rd_req_addr.enq(_req.address);
		$display("Block requested by icache to memory @ PC %h", _req.address);

		ff_request_to_memory_rd.enq(req);
	endrule

	Reg#(Bit#(64)) rg_temp_data_icache <- mkRegU; // The fourth will be appended to this data in the final cycle itself
	Reg#(Bit#(TLog#(`ICACHE_BLOCK_SIZE))) rg_last_index_icache <- mkReg(0);
	Vector#(3, Reg#(Bit#(64))) rg_temp_data_dcache <- replicateM(mkRegU); // The fourth will be appended to this data in the final cycle itself
	Reg#(Bit#(TLog#(`DCACHE_BLOCK_SIZE))) rg_last_index_dcache_rd <- mkReg(0);
	Reg#(Bit #(TMul #(8, TMul #(`Word_size,4)))) rg_writeback_data <- mkRegU;
	Reg#(Bit#(`DCACHE_BLOCK_SIZE)) rg_last_index_dcache_wr <- mkReg(0);


	(*conflict_free = "rl_response_from_memory_to_dcache_rd, rl_response_from_memory_to_icache"*)	
	(*conflict_free = "rl_response_from_memory_to_dcache_rd, rl_response_from_memory_to_dcache_wr"*)	
	(*conflict_free = "rl_response_from_memory_to_dcache_rd, rl_writeback_response_from_memory"*)	

	rule rl_response_from_memory_to_icache(ff_response_from_memory_rd.first.transaction_id[5]==1);
		let rsp = ff_response_from_memory_rd.first; 
		let offset_bits = valueOf(TAdd#(TLog#(`ICACHE_BLOCK_SIZE),TLog#(`Word_size)));
		let word_bits = valueOf(TLog#(`Word_size));
		Bit#(TLog#(`ICACHE_BLOCK_SIZE)) block_offset = ff_icache_rd_req_addr.first[offset_bits-1 :word_bits]
															+ rg_last_index_icache;
		From_Memory #(`ADDRESS_WIDTH, 8, 2, 16) rsp_from_memory = ?;
		rsp_from_memory.data_word = rsp.data;
		rsp_from_memory.bus_error = (rsp.status == SUCCESS) ? 0 : 1;
		rsp_from_memory.token = rsp.transaction_id[4:0];
		rsp_from_memory.address = {ff_icache_rd_req_addr.first[valueOf(`ADDRESS_WIDTH)-1:offset_bits],block_offset,3'b0};
		icache.response_from_memory(rsp_from_memory);
		if(rg_last_index_icache==fromInteger(valueOf(TSub#(`ICACHE_BLOCK_SIZE,1)))) begin
			$display("The instruction address from memory %h", rsp_from_memory.address);
			ff_icache_rd_req_addr.deq;
		end
		$display("The instruction fetched is %h", rsp.data);
		rg_last_index_icache <= rg_last_index_icache + 1;
		ff_response_from_memory_rd.deq;
	endrule

	rule rl_request_to_memory_from_dcache;
		let _req <- dcache.request_to_memory;
		Req_Desc_CPU tlm_req = defaultValue;
		tlm_req.addr = _req.address;
		tlm_req.command = (_req.ld_st == Load) ? READ : WRITE;
		tlm_req.burst_mode = WRAP;
		tlm_req.burst_length = (_req.ld_st == Load) ? 4 : 1;
		tlm_req.burst_size = 'b111;
		tlm_req.transaction_id = {0,tlm_req.transaction_id};
		tlm_req.lock = True;
		tlm_req.prty = 1;
		Req_CPU req = tagged Descriptor tlm_req;
		if (_req.ld_st == Load)	begin 
			ff_request_to_memory_rd.enq(req); 
			ff_dcache_rd_req_addr.enq(_req.address); 
		end
	endrule
	
	rule rl_response_from_memory_to_dcache_rd(ff_response_from_memory_rd.first.transaction_id[5]==0);
		let rsp = ff_response_from_memory_rd.first; 
		let offset_bits = valueOf(TAdd#(TLog#(`DCACHE_BLOCK_SIZE),TLog#(`Word_size)));
		let word_bits = valueOf(TLog#(`Word_size));
		Bit#(TLog#(`DCACHE_BLOCK_SIZE)) block_offset = ff_dcache_rd_req_addr.first[offset_bits-1 :word_bits]
															+ rg_last_index_dcache_rd;
		From_Memory_d #(`ADDRESS_WIDTH, 8, 4) rsp_from_memory = ?;
		rsp_from_memory.data_word = rsp.data;
		rsp_from_memory.bus_error = (rsp.status == SUCCESS) ? 0 : 1;
		rsp_from_memory.address = {ff_dcache_rd_req_addr.first[valueOf(`ADDRESS_WIDTH)-1:offset_bits],block_offset,3'b0};
		if(rg_last_index_dcache_rd==fromInteger(valueOf(TSub#(`DCACHE_BLOCK_SIZE,1)))) begin
			$display("The data address from memory %h", rsp_from_memory.address);
			ff_dcache_rd_req_addr.deq;
		end
		dcache.response_from_memory(rsp_from_memory);
		rg_last_index_dcache_rd <= rg_last_index_dcache_rd + 1;
		$display("The instruction data is %h", rsp.data);
		ff_response_from_memory_rd.deq;
		$display("Dequeued dcache response from main memory");
	endrule

	rule rl_response_from_memory_to_dcache_wr(!rg_revert_map);
		let rsp = ff_response_from_memory_wr.first; ff_response_from_memory_wr.deq;
		begin
			From_Memory_d #(`ADDRESS_WIDTH, 8, 4) rsp_from_memory = ?;
			rsp_from_memory.data_word = zeroExtend(pack(rsp.data));
			rsp_from_memory.bus_error = (rsp.status == SUCCESS) ? 0 : 1;
			rsp_from_memory.address = ff_dcache_rd_req_addr.first;
			ff_dcache_rd_req_addr.deq;
			dcache.response_from_memory(rsp_from_memory);
		end
	endrule


	rule rl_request_data_writeback_first (rg_last_index_dcache_wr == 0);
		let _req <- dcache.write_back_data;

		rg_writeback_data <= _req.data_line;

		Req_Desc_CPU tlm_req = defaultValue;
		tlm_req.addr = _req.address;
		tlm_req.data = _req.data_line[63:0];
		tlm_req.command = WRITE;
		tlm_req.burst_mode = INCR;
		tlm_req.burst_length = 4;
		tlm_req.burst_size = 'b111;
		tlm_req.lock = True;
		tlm_req.prty = 1;
		Req_CPU req = tagged Descriptor tlm_req;
		ff_request_to_memory_wr.enq(req);
		rg_dcache_writeback_addr <= _req.address;
		rg_last_index_dcache_wr <= rg_last_index_dcache_wr + 1;
	endrule

	rule rl_request_data_writeback_following (rg_last_index_dcache_wr > 0);
		Req_Data_CPU tlm_req = ?;
		case (rg_last_index_dcache_wr)
			1: tlm_req.data = rg_writeback_data[127:64];
			2: tlm_req.data = rg_writeback_data[191:128];
			3: tlm_req.data = rg_writeback_data[255:192];
		endcase
		Req_CPU req = tagged Data tlm_req;
		ff_request_to_memory_wr.enq(req);

		rg_last_index_dcache_wr <= rg_last_index_dcache_wr + 1;
	endrule

	rule rl_writeback_response_from_memory;
	let rsp = ff_response_from_memory_wr.first; ff_response_from_memory_wr.deq;
		begin
			From_Memory_d #(`ADDRESS_WIDTH, 8, 4) rsp_from_memory = ?;
			rsp_from_memory.data_word = 0;
			rsp_from_memory.bus_error = (rsp.status == SUCCESS) ? 0 : 1;
			rsp_from_memory.address = rg_dcache_writeback_addr;
			//dcache.response_from_memory(rsp_from_memory);
		end		
	endrule
			
	interface bus_rd_ifc =  toSendIFC(ff_request_to_memory_rd, ff_response_from_memory_rd);
	interface bus_wr_ifc =  toSendIFC(ff_request_to_memory_wr, ff_response_from_memory_wr);
// --------------------------------------------------------------------------------------------

	/*method Action response_from_memory_to_icache(From_Memory#(`ADDRESS_WIDTH,8,`BLOCK_SIZE,16) resp); // recieve response from the memory.
		icache.response_from_memory(resp);
	endmethod*/
	method flush_icache = rg_revert_map || decode.abandone_cache;
	method flush_dcache = rg_revert_map;
    //method request_to_memory_from_dcache = dcache.request_to_memory; // send request to memory
    //method Action response_from_memory_to_dcache(From_Memory_d#(`ADDRESS_WIDTH,8,4) resp); // recieve response from the memory.
	//	dcache.response_from_memory(resp);
	//endmethod
    //method write_back_data = dcache.write_back_data;

endmodule
endpackage
