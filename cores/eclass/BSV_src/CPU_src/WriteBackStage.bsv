/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Names : Abhinaya Agrawal
Email ID : agrawal.abhinaya@gmail.com
*/

// ================================================================
// Bluespec libraries

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import DefaultValue :: *;
import SpecialFIFOs :: *;
import ConfigReg	  :: *;

// ================================================================
// Project imports

import TLM2         :: *; // Using local copy
import Utils        :: *; // Utility functions and typeclasses to zip TLMRecvIFC and TLMSendIFC with FIFOs
import Req_Rsp      :: *; // Request and Response packet types
import ISA_Defs     :: *; // RISC-V Instruction specifications
import TypeDefs     :: *;
import DummyICache  :: *;
import CSRFile      :: *;
import ISA_Defs_PRV :: *;
import DummyDCache  :: *;
import DCache       :: *;

// Project inludes
`include "TLM.defines"   // Parameters for TLM packets
`include "RVC.defines"   // Local Definitions
`include "macro.defines" // System Configuration Parameters

// ================================================================
// Typedefs

`define DCACHE_ADDR 32
`define DCACHE_WAYS 4
`define DCACHE_BLOCK_SIZE 8
`define DCACHE_WORD_SIZE 4
`define DCACHE_SETS 64
// ===============================================================
// Modules

interface WriteBackIFC;
	method Action request (EXWB_Data req);
	method WBResponse response;
	interface TLMSendIFC #(Req_CPU, Rsp_CPU) bus_ifc;
	method Action reset (int verbosity);
endinterface

(*conflict_free = "rl_handle_writeback_requests, rl_data_memory_response, rl_handle_traps"*)
module mkWriteBack#(CSR_IFC csrfile)(WriteBackIFC);
	FIFOF #(Req_CPU) f_dmem_reqs <- mkBypassFIFOF;
	FIFOF #(Rsp_CPU) f_dmem_rsps <- mkBypassFIFOF;

	Wire #(EXWB_Data)   wr_request   <- mkWire;
	Wire #(WBResponse)  wr_response  <- mkWire;
	Wire #(TrapData)    wr_trap_data <- mkWire;

	Reg #(int) 			 rg_verbosity 	   <- mkConfigReg(0);
   Reg #(FetchState)  rg_dmem_state[2] <- mkCReg(2, REQ); // TODO: Clean this up

	Reg #(Addr) 									rg_burst_addr 	 <- mkConfigRegU;
	Reg #(TLMBurstSize#(`TLM_PRM_REQ_CPU)) rg_burst_size 	 <- mkConfigRegU;
	Reg #(TLMUInt#(`TLM_PRM_REQ_CPU)) 		rg_burst_length <- mkConfigRegU;

	Reg #(TLMUInt#(`TLM_PRM_REQ_CPU)) rg_burst_count <- mkConfigReg(0);
	Reg#(Bit#(TMul#(`DCACHE_BLOCK_SIZE, TMul#(8, `DCACHE_WORD_SIZE)))) rg_data_line <- mkConfigRegU;

	let this_pc = wr_request.pc;

	//Ifc_dcache dcache <- mkdcache;
	DCacheIFC dcache <- mkDummyDCache;

   function Addr increment_addr(TLMBurstMode mode, Addr addr, TLMBurstSize#(`TLM_PRM_REQ_CPU) size, TLMUInt#(`TLM_PRM_REQ_CPU) length);
      Addr new_addr = addr;
      if(length == 1 || mode == INCR) begin
         new_addr = addr + zeroExtend(size) + 1;
      end
      else if(mode == WRAP)
         if(length == 8) begin
            if(size == 0)
               new_addr[2:0] = addr[2:0]+1;
            else if(size == 1)
               new_addr[3:1] = addr[3:1]+1;
            else if(size == 3)
               new_addr[4:2] = addr[4:2]+1;
         end
         else if(length == 4)begin
            if(size == 0)
               new_addr[1:0] = addr[1:0] + 1;
            else if(size == 1)
               new_addr[2:1] = addr[2:1] + 1;
            else if(size == 3)
               new_addr[3:2] = addr[3:2] + 1;
         end
      return new_addr;
   endfunction

	function Action fn_handle_alu_ops(RequestWriteBack req);
	action
		wr_response <= WBResponse{rd: req.rd, data: tagged Valid req.data, next_pc: tagged Invalid, retire: True, breakpoint: False, halt: False};
   	if (rg_verbosity > 1)
		 $display($time, " CPU: WRITE-BACK: Instruction (PC: %h) performed an arithematic or logical operation. ARF Updated. REG: %d Value: %h", this_pc, req.rd, req.data);
	endaction
	endfunction

	function Action fn_handle_branch_ops(RequestJump req);
	action
		let data = tagged Invalid;
		let next_pc = tagged Invalid;
   	if (req.branchAddr matches tagged Valid .npc) begin
   	   next_pc = req.branchAddr;
   	   if (req.lr matches tagged Valid ._lr)
				data = tagged Valid _lr;
   	   if (rg_verbosity > 1) $display($time, " CPU: WRITE-BACK: Branch instruction (PC: %h) encountered. Taken. Updated PC: %h", this_pc, npc);
   	end
   	else if (rg_verbosity > 1) $display($time, " CPU: WRITE-BACK: Branch instruction (PC: %h) encountered. Not Taken", this_pc);
		wr_response <= WBResponse{rd: req.rd, data: data, next_pc: next_pc, retire: True, breakpoint: False, halt: False};
	endaction
	endfunction

	function Action fn_handle_memory_ops(ReqLS req);
	action
   	let mem_req = req.tlmreq;
		let mem_req_desc = mem_req.Descriptor;
		let cache_req = From_Cpu_D {
								address: mem_req_desc.addr,
								load_store: (mem_req_desc.command == READ) ? Load : Store,
								data: mem_req_desc.data,
								transfer_size: (mem_req_desc.burst_size == 3) ? 2 : mem_req_desc.burst_size
							 };
		dcache.request_from_cpu(cache_req);
   	rg_dmem_state[0] <= RESP;
   	if (rg_verbosity > 1) if (mem_req_desc.command == READ)
				$display($time, " CPU: WRITE-BACK: Instruction (PC: %h) requested a LOAD operation. Addr: %h", this_pc, mem_req_desc.addr);
   	if (rg_verbosity > 1) if (mem_req_desc.command == WRITE)
				$display($time, " CPU: WRITE-BACK: Instruction (PC: %h) requested a STORE operation. Addr: %h, data: %h", this_pc, mem_req_desc.addr, mem_req_desc.data);
	endaction
	endfunction

	function Action fn_handle_halt_ops(Bool req);
	action
		wr_response <= WBResponse{rd: 0, data: tagged Invalid, next_pc: tagged Invalid, retire: True, breakpoint: False, halt: True};
		if(rg_verbosity > 1) $display($time, " CPU: WRITE-BACK: [[HALTING]]. Reason: End of Instruction Stream. PC: %h", this_pc);
	endaction
	endfunction

	function Action fn_handle_system_ops(RequestSystem req);
	action
		let data = tagged Invalid;
		let next_pc = tagged Invalid;
		Bool retire = False;
      let csr_ret <- csrfile.try_system_instruction(req);
      if(csr_ret matches tagged TrapD .trap) wr_trap_data <= trap;
      else if(csr_ret matches tagged SysD .sys_data) begin
			retire = True;
         case (sys_data) matches
            tagged Value .csr:
               begin
						data = tagged Valid csr;
                  if (rg_verbosity > 1) $display($time, " CPU: WRITE-BACK: Instruction (PC: %h) performed a CSR operation. ARF Updated. REG: %d Value: %h", this_pc, req.rd, csr);
               end
            tagged Redirect .npc:
               begin
						next_pc = tagged Valid npc;
                  if (rg_verbosity > 1) $display($time, " CPU: WRITE-BACK: Instruction (PC: %h) performed a CSR (*RET) operation. Redirecting to PC: %h", this_pc, npc);
               end
         endcase
			wr_response <= WBResponse{rd: req.rd, data: data, next_pc: next_pc, retire: True, breakpoint: False, halt: False};
      end
	endaction
	endfunction

	rule rl_handle_writeback_requests(rg_dmem_state[0] == REQ);
		let data = wr_request.data;
		let pending_interrupt = csrfile.read_pending_interrupt;
		if(pending_interrupt matches tagged Valid .interrupt) begin
 		   wr_trap_data <= TrapData {bad_addr: tagged Invalid, trap: tagged Interrupt interrupt};
   	   if (rg_verbosity > 1) $display($time, " CPU: Interrupt pending. PC: %h, Cause: %h", this_pc, interrupt);
		end
		else if(data matches tagged TrapD .trap) wr_trap_data <= trap;
		else if(data matches tagged StageD .s_data) begin
			case (s_data) matches
				tagged WB .v: fn_handle_alu_ops(v);
				tagged LS .v: fn_handle_memory_ops(v);
				tagged JMP .v: fn_handle_branch_ops(v);
				tagged SYS .v: fn_handle_system_ops(v);
				tagged Halt .v: fn_handle_halt_ops(v);
				default: wr_trap_data <= TrapData{bad_addr: tagged Invalid, trap: tagged Exception MCAUSE_ILLEGAL_INSTRN};
			endcase
		end
	endrule

	rule rl_data_memory_response (wr_request.data matches tagged StageD .data &&& data matches tagged LS .req &&& rg_dmem_state[1] == RESP);
		let response <- dcache.response_to_cpu;
		if(response matches tagged Valid .rsp) begin
			let tlm_req = req.tlmreq;
			let tlm_req_desc = tlm_req.Descriptor;
			let data = tagged Invalid;

			//let rsp <- dcache.response_to_cpu;
      	rg_dmem_state[1] <= REQ;

      	// Check for Load/Store access faults
      	if (rsp.bus_error == 1)
      	case (tlm_req_desc.command)
      	   READ: wr_trap_data <= TrapData{bad_addr: tagged Valid rsp.address, trap: tagged Exception MCAUSE_LOAD_ACCESS_FAULT};
      	   WRITE: wr_trap_data <= TrapData{bad_addr: tagged Valid rsp.address, trap: tagged Exception MCAUSE_STORE_ACCESS_FAULT};
      	endcase
			// Check Load/Store misalignment
			else if(rsp.misaligned_error == 1)
      	case (tlm_req_desc.command)
      	   READ: wr_trap_data <= TrapData{bad_addr: tagged Valid rsp.address, trap: tagged Exception MCAUSE_LOAD_ADDR_MISALN};
      	   WRITE: wr_trap_data <= TrapData{bad_addr: tagged Valid rsp.address, trap: tagged Exception MCAUSE_STORE_ADDR_MISALN};
      	endcase
      	// Proceed if 'dmem' access was successful
      	else
			begin
      	   Data temp_data = rsp.data_word >> {tlm_req_desc.addr[1:0], 3'b0};
      	   Bit #(8)  data_8  = truncate(temp_data);
      	   Bit #(16) data_16 = truncate(temp_data);
      	   Bit #(24) data_24 = truncate(temp_data);
      	   Bit #(32) data_32 = truncate(temp_data);
      	   let rsize = tlm_req_desc.burst_size;
				let ext_type = (req.remark == UNSIGNED) ? zeroExtend : signExtend;
      	   case (rsize)
      	      0: temp_data = ext_type(data_8);
      	      1: temp_data = ext_type(data_16);
      	      2: temp_data = ext_type(data_24);
      	      3: temp_data = ext_type(data_32);
      	      default: temp_data = ext_type(rsp.data_word);
      	   endcase
      	   if (tlm_req_desc.command == READ) begin
					data = tagged Valid temp_data;
				end
      	   if (rg_verbosity > 1) begin
					if (tlm_req_desc.command == READ)
						$display($time, " CPU: WRITE-BACK: Instruction (PC: %h) performed a LOAD operation. Addr: %h, REG: %d, Value: %h", this_pc, rsp.address, req.rd, temp_data);
					if (tlm_req_desc.command == WRITE)
						$display($time, " CPU: WRITE-BACK: Instruction (PC: %h) performed a STORE operation. Addr: %h, Value: %h", this_pc, rsp.address, tlm_req_desc.data);
      	   end
				wr_response <= WBResponse{rd: req.rd, data: data, next_pc: tagged Invalid, retire: True, breakpoint: False, halt: False};
      	end
		end
   endrule
		
	rule rl_handle_traps;
		let next_pc = tagged Invalid;
		let breakpoint = False;
		`ifdef CPU_MODE_DEBUG
         if(wr_trap_data.trap matches tagged Exception .e &&& e == MCAUSE_BREAKPOINT)
				breakpoint = True;
		`endif
		if(breakpoint == False) begin
         let ret <- csrfile.try_pending_trap(wr_trap_data, this_pc);
			next_pc = ret;
		end
		wr_response <= WBResponse{rd: 0, data: tagged Invalid, next_pc: next_pc, retire: True, breakpoint: breakpoint, halt: False}; // TODO Retire should be true only for ecall, ebreak. Fix this
	endrule

	rule rl_handle_memory_request(rg_burst_count == 0);
      let cache_req <- dcache.request_to_memory;
      Req_Desc_CPU memReq = defaultValue;
      memReq.command    = (cache_req.ld_st == Load) ? READ : WRITE;
      memReq.addr       = cache_req.address;
      memReq.burst_mode = WRAP;
		memReq.data			= truncate(cache_req.data_line);
		// 'burst_size' field for 32-bit wide data channel is 2-bit long. '00' encodes a 'byte' transfer whereas '11' indicates 'word'
      // Therefore 'reqSz_bytes_i' function is required to convert the length of requested data to standard TLM format
      memReq.burst_size = (cache_req.transfer_size == 2) ? 3 : cache_req.transfer_size;
      memReq.burst_length = unpack(extend(cache_req.burst_length));
      memReq.transaction_id = 0; // Used to identify the source of request
      memReq.lock = False;
      memReq.prty = 0;
		rg_burst_addr <= memReq.addr;
		rg_burst_size <= memReq.burst_size;
		rg_burst_length <= memReq.burst_length;
		if(memReq.command == WRITE) begin 
			if(memReq.burst_length > 1) rg_burst_count <= 1;
			rg_data_line <= (cache_req.data_line >> valueof(XLEN));
		end
      Req_CPU req = tagged Descriptor memReq; // Only TLMRequest packet is sent over a connection. So RequestDescriptor or RequestData requests are converted to TLMRequest before being sent
		f_dmem_reqs.enq(req);
      if(rg_verbosity > 3) $display($time, " CPU: WRITE-BACK: Memory-I/O. Request. Type: ", fshow(memReq.command), " Addr: %h, Data: %h", memReq.addr, memReq.data);
	endrule

	rule rl_follow_up_store_request(rg_burst_count >= 1 && rg_burst_count < truncate(rg_burst_length));
		Req_data_CPU req = ?;
		req.data = truncate(rg_data_line);
		f_dmem_reqs.enq(tagged Data req);
		rg_data_line <= (rg_data_line >> valueof(XLEN));
		if(rg_burst_count == rg_burst_length - 1) rg_burst_count <= 0;
		else rg_burst_count <= rg_burst_count + 1;
	endrule

	rule rl_handle_memory_response;
		let rsp = f_dmem_rsps.first; f_dmem_rsps.deq;
		rg_burst_addr <= increment_addr(WRAP, rg_burst_addr, rg_burst_size, rg_burst_length);
      let last_response <- dcache.response_from_memory(From_Memory_D{data_line: rsp.data, bus_error: (rsp.status != SUCCESS) ? 1:0, misaligned_error: 0, address: rg_burst_addr});
      if(rg_verbosity > 3) $display($time, " CPU: WRITE_BACK: Memory-I/O. Response. Type: ", fshow(rsp.command), " Addr: %h, Data: %h, Size: %d, Length: %d", rg_burst_addr, rsp.data, rg_burst_size, rg_burst_length);
	endrule

   interface TLMSendIFC bus_ifc = toSendIFC(f_dmem_reqs, f_dmem_rsps);

	method Action request(EXWB_Data req);
		wr_request <= req;
	endmethod

	method WBResponse response;
		return wr_response;
	endmethod

	method Action reset(int verbosity);
		rg_verbosity <= verbosity;
		rg_burst_count <= 0;
		f_dmem_reqs.clear;
		f_dmem_rsps.clear;
	endmethod
endmodule
