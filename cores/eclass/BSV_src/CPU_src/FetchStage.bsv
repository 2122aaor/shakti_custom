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
import ConfigReg    :: *;

// ================================================================
// Project imports

import TLM2         :: *; // Using local copy
import Utils        :: *; // Utility functions and typeclasses to zip TLMRecvIFC and TLMSendIFC with FIFOs
import Req_Rsp      :: *; // Request and Response packet types
import ISA_Defs     :: *; // RISC-V Instruction specifications
import TypeDefs     :: *;
import DummyICache  :: *;
import ICache       :: *;

// Project inludes
`include "TLM.defines"   // Parameters for TLM packets
`include "RVC.defines"   // Local Definitions
`include "macro.defines" // System Configuration Parameters

// ================================================================
// Modules

interface FetchIFC;
   interface TLMSendIFC#(Req_CPU, Rsp_CPU) bus_ifc;
   method Action request;
   method ActionValue#(IFEX_Data) response;
   method Action redirect(Addr pc);
   method Action redirect_stage_2(Addr pc);
   method Action redirect_stage_3(Addr pc);
   method Addr last_fetched_pc; // For debug purposes
   method Action reset(Addr pc, int verbosity);
endinterface

module mkFetch(FetchIFC);

   Reg  #(Addr)         rg_fetch_pc          <- mkRegU; // PC of next instruction to be fetched. Latest instruction's PC is four less
   Reg  #(Maybe#(Addr)) rg_this_pc[2]        <- mkCReg(2, tagged Invalid); // PC of the instruction to be fetched in the current cycle. It's validity indicates a pending request.
   Reg  #(Maybe#(Addr)) rg_redirect_pc[2]    <- mkCReg(2, tagged Invalid);
   Wire #(Maybe#(Addr)) wr_redirect_2        <- mkDWire(tagged Invalid);
   Wire #(Maybe#(Addr)) wr_redirect_3        <- mkDWire(tagged Invalid);
   Reg  #(FetchState)   rg_imem_state[2]     <- mkCReg(2, REQ);
   Reg  #(int)          rg_verbosity         <- mkConfigReg(0);
   
   Reg #(Addr)                            rg_burst_addr   <- mkConfigRegU;
   Reg #(TLMBurstSize#(`TLM_PRM_REQ_CPU)) rg_burst_size   <- mkConfigRegU;
   Reg #(TLMUInt#(`TLM_PRM_REQ_CPU))      rg_burst_length <- mkConfigRegU;

   //Ifc_icache icache <- mkicache;
   ICacheIFC icache <- mkDummyICache;

   let isFlushed = (isValid(wr_redirect_2) || isValid(wr_redirect_3));

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

   rule rl_request_icache(rg_this_pc[1] matches tagged Valid .pc);
      rg_this_pc[1] <= tagged Invalid;
      let cacheReq = From_Cpu{address: pc};
      icache.request_from_cpu(cacheReq);
   endrule

   rule rl_handle_redirections (isFlushed);
      Maybe#(Addr) next_pc = tagged Invalid;
      if(wr_redirect_3 matches tagged Valid .redirect_pc) next_pc = tagged Valid redirect_pc;
      else if(wr_redirect_2 matches tagged Valid .redirect_pc) next_pc = tagged Valid redirect_pc;
      rg_redirect_pc[1] <= next_pc;
      rg_imem_state[1] <= REQ;
      rg_this_pc[1] <= tagged Invalid;
      icache.flush;
      if(rg_verbosity > 3) $display($time, " CPU: FETCH: Handling redirections");
   endrule

   interface TLMSendIFC bus_ifc;
      interface Get tx;
         method ActionValue#(Req_CPU) get;
            let cacheReq <- icache.request_to_memory;
            Req_Desc_CPU memReq = defaultValue;
            memReq.command    = READ;
            memReq.addr       = cacheReq.address;
            memReq.burst_mode = WRAP;
            // 'burst_size' field for 32-bit wide data channel is 2-bit long. '00' encodes a 'byte' transfer whereas '11' indicates 'word'
            // Therefore 'reqSz_bytes_i' function is required to convert the length of requested data to standard TLM format
            memReq.burst_size = reqSz_bytes_i (fromInteger(instlen));
            memReq.burst_length = unpack(extend(cacheReq.burst_length));
            memReq.transaction_id = 0; // Used to identify the source of request
            memReq.lock = False;
            memReq.prty = 0;
            rg_burst_addr <= memReq.addr;
            rg_burst_size <= memReq.burst_size;
            rg_burst_length <= memReq.burst_length;
            Req_CPU req = tagged Descriptor memReq; // Only TLMRequest packet is sent over a connection. So RequestDescriptor or RequestData requests are converted to TLMRequest before being sent
            if(rg_verbosity > 3) $display($time, " CPU: FETCH: Request to Memory. Addr: %h, burst_length: %d", memReq.addr, memReq.burst_length);
            return req;
         endmethod
      endinterface
      interface Put rx;
         method Action put(Rsp_CPU memRsp);
            rg_burst_addr <= increment_addr(WRAP, rg_burst_addr, rg_burst_size, rg_burst_length);
            let last_response <- icache.response_from_memory(From_Memory{data_line: memRsp.data, bus_error: (memRsp.status != SUCCESS) ? 1:0, address: rg_burst_addr});
            if(rg_verbosity > 3) $display($time, " CPU: FETCH: Response from Memory. Addr: %h, data: %h", rg_burst_addr, memRsp.data);
         endmethod
      endinterface
   endinterface

   method Action request if(rg_imem_state[1] == REQ && !isFlushed); // TODO: Clean this up
      Addr this_pc = rg_fetch_pc;
      if(rg_redirect_pc[0] matches tagged Valid .redirect_pc) begin
         this_pc = redirect_pc;
         rg_redirect_pc[0] <= tagged Invalid;
      end
      rg_this_pc[0] <= tagged Valid this_pc;
      rg_fetch_pc <= this_pc + 4;
      rg_imem_state[1] <= RESP;
      if (rg_verbosity > 1) $display ($time," CPU: FETCH: Transfer initiated for PC = %0h\n\n", this_pc);
   endmethod

   // Wait for instruction memory to respond. Setup Decode stage
   // TODO take of care of virtual address translation (base addition, bounds checking, etc.)
   //      Easy implementation: add a regiter name virtual_pc. Update this as virtual_pc <= rg_pc + base. Use this virtual_pc everywhere instead of rg_pc
   method ActionValue#(IFEX_Data) response if(rg_imem_state[0] == RESP && !isFlushed);
      let this_pc = rg_fetch_pc - 4;
      IFEX_Data retVal = ?;
      let rsp <- icache.response_to_cpu;
      
      // Check for instruction access fault
      if (rsp.misaligned_error == 1) begin
         retVal = IFEX_Data{
                     pc: this_pc,
                     data: tagged TrapD TrapData {bad_addr: tagged Valid this_pc, trap: tagged Exception MCAUSE_INSTRN_ADDR_MISALN}
                  };
         if(rg_verbosity > 1)
          $display($time, " CPU: FETCH: [[ERROR]]. Exception MCAUSE_INSTRN_ADDR_MISALN occurred for PC: %h", this_pc);
      end
      else if (rsp.bus_error == 1) begin
         retVal = IFEX_Data{
                     pc: this_pc,
                     data: tagged TrapD TrapData {bad_addr: tagged Valid this_pc, trap: tagged Exception MCAUSE_INSTRN_ACCESS_FAULT}
                  };
         if(rg_verbosity > 1)
          $display($time, " CPU: FETCH: [[ERROR]] Exception MCAUSE_INSTRN_ACCESS_FAULT occurred for PC: %h", this_pc);
      end
      else begin
         // Setup execute stage data
         retVal = IFEX_Data{
                     pc: this_pc,
                     data: tagged StageD ExecuteStageData {inst: truncate(rsp.data_word)}
                  };
         // Display debug message
         if(rg_verbosity > 1)
          $display($time, " CPU: FETCH: Response received from Instruction Memory for PC: %h. Recived Data: %h", this_pc, rsp.data_word);
      end
      rg_imem_state[0] <= REQ;
      return retVal;
   endmethod
   
   method Action redirect(Addr pc);
      rg_fetch_pc <= pc;
   endmethod

   method Action redirect_stage_2(Addr pc);
      wr_redirect_2 <= tagged Valid pc;
   endmethod

   method Action redirect_stage_3(Addr pc);
      wr_redirect_3 <= tagged Valid pc;
   endmethod

   method Addr last_fetched_pc;
      return rg_fetch_pc - 4;
   endmethod

   method Action reset(Addr pc, int verbosity);
      rg_imem_state[1] <= REQ;
      rg_fetch_pc <= pc;
      rg_this_pc[1] <= tagged Invalid;
      rg_verbosity <= verbosity;
      icache.flush;
   endmethod
endmodule
