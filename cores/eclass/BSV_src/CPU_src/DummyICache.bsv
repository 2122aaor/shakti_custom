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

// ================================================================
// Project imports

import TLM2         :: *; // Using local copy
import Utils        :: *; // Utility functions and typeclasses to zip TLMRecvIFC and TLMSendIFC with FIFOs
import Req_Rsp      :: *; // Request and Response packet types
import ISA_Defs     :: *; // RISC-V Instruction specifications
import TypeDefs     :: *;

// Project inludes
`include "TLM.defines"   // Parameters for TLM packets
`include "RVC.defines"   // Local Definitions
`include "macro.defines" // System Configuration Parameters

// ================================================================
// Typedefs

`define ICACHE_ADDR 32
`define ICACHE_WAYS 4
`define ICACHE_BLOCK_SIZE 8
`define ICACHE_WORD_SIZE 4
`define ICACHE_SETS 512

// ================================================================
// Modules

interface ICacheIFC;
   method Action request_from_cpu(From_Cpu#(`ICACHE_ADDR) req);
   method ActionValue#(To_Cpu#(`ICACHE_ADDR,`ICACHE_WORD_SIZE)) response_to_cpu;
   method ActionValue#(To_Memory#(`ICACHE_ADDR)) request_to_memory;
   method ActionValue#(Bool) response_from_memory(From_Memory#(`ICACHE_ADDR,`ICACHE_WORD_SIZE) resp);
   method Action cache_enable(Bool enable_);
   method Action flush();
endinterface

module mkDummyICache (ICacheIFC);
   // FIFOFs for requests to/responses from memory
   FIFOF #(To_Memory#(`ICACHE_ADDR)) f_imem_reqs <- mkPipelineFIFOF;  // Enqueued to make a request to instruction memory
   FIFOF #(To_Cpu#(`ICACHE_ADDR, `ICACHE_WORD_SIZE)) f_cpu_rsps <- mkPipelineFIFOF;

   Reg #(Bool) rg_response_valid[3] <- mkCReg(3, False);
   Reg #(FetchState) rg_imem_state[2] <- mkCReg(2, REQ);

   Wire #(Bool) wr_flush <- mkDWire(False);

   rule rl_discard_stale_responses (rg_response_valid[0] == False && rg_imem_state[0] == RESP);
      f_cpu_rsps.deq;
      rg_imem_state[0] <= REQ;
      //$display($time, " CPU: FETCH: DummyICache: Discarding memory response for PC: %h", f_cpu_rsps.first.address);
   endrule

   method Action request_from_cpu(From_Cpu#(`ICACHE_ADDR) req) if(rg_imem_state[1] == REQ);
      let this_pc = req.address;
      if (this_pc[1:0] != 0) // Check for instruction address misalignment
         f_cpu_rsps.enq(To_Cpu{address: this_pc, bus_error: 0, misaligned_error: 1, data_word: ?});
      else // Bypass request to memory
         f_imem_reqs.enq(To_Memory{address: this_pc, burst_length: 1, ld_st: Load});
      rg_response_valid[1] <= True;
      rg_imem_state[1] <= RESP;
   endmethod
   
   method ActionValue#(To_Cpu#(`ICACHE_ADDR,`ICACHE_WORD_SIZE)) response_to_cpu if(rg_response_valid[0] == True && rg_imem_state[0] == RESP);
      let rsp = f_cpu_rsps.first;
      f_cpu_rsps.deq;
      rg_imem_state[0] <= REQ;
      return rsp;
   endmethod

   method ActionValue#(To_Memory#(`ICACHE_ADDR)) request_to_memory;
      let req = f_imem_reqs.first; f_imem_reqs.deq;
      let mem_req = To_Memory{address: req.address, burst_length: req.burst_length, ld_st: req.ld_st};
      return mem_req;
   endmethod

   method ActionValue#(Bool) response_from_memory(From_Memory#(`ICACHE_ADDR,`ICACHE_WORD_SIZE) rsp);
      let cpu_rsp = To_Cpu{address: rsp.address, bus_error: rsp.bus_error, misaligned_error: 0, data_word: rsp.data_line};
      f_cpu_rsps.enq(cpu_rsp);
      return True;
   endmethod

   method Action cache_enable(Bool enable_);
      noAction;
   endmethod

   method Action flush;
      wr_flush <= True;
      rg_response_valid[2] <= False;
   endmethod
endmodule
