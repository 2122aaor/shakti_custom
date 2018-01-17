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

`define DCACHE_ADDR 32
`define DCACHE_WAYS 4
`define DCACHE_BLOCK_SIZE 8
`define DCACHE_WORD_SIZE 4
`define DCACHE_SETS 64

// ================================================================
// Modules

interface DCacheIFC;
  method Action request_from_cpu(From_Cpu_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE) req);
  method ActionValue#(Maybe#(To_Cpu_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE))) response_to_cpu;
  method ActionValue#(To_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)) request_to_memory;
  method ActionValue#(Bool) response_from_memory(From_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE) resp);
  method Action clear_all();
endinterface

module mkDummyDCache (DCacheIFC);
   // FIFOFs for requests to/responses from memory
   FIFOF #(To_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)) f_dmem_reqs <- mkPipelineFIFOF;  // Enqueued to make a request to instruction memory
   FIFOF #(To_Cpu_D#(`DCACHE_ADDR, `DCACHE_WORD_SIZE)) f_cpu_rsps <- mkPipelineFIFOF;

   method Action request_from_cpu(From_Cpu_D#(`DCACHE_ADDR, `DCACHE_WORD_SIZE) req);
      let address = req.address;
      let misaligned = False;
      // Check for address misalignment
      case(req.transfer_size)
         0: misaligned = False;
         1: misaligned = (address[0] == 1);
         2: misaligned = True;
         3: misaligned = (address[1:0] != 2'b0);
         default: misaligned = False;
      endcase
      /*if (misaligned)
         f_cpu_rsps.enq(To_Cpu_D{address: address, bus_error: 0, misaligned_error: 1, data_word: ?, load_store: req.load_store});
      else*/ // Bypass request to memory
         f_dmem_reqs.enq(To_Memory_D{address: address, burst_length: 1, ld_st: req.load_store, data_line: extend(req.data), transfer_size: req.transfer_size});
   endmethod
   
   method ActionValue#(Maybe#(To_Cpu_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE))) response_to_cpu;
      let rsp = f_cpu_rsps.first;
      f_cpu_rsps.deq;
      return tagged Valid rsp;
   endmethod

   method ActionValue#(To_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)) request_to_memory;
      let req = f_dmem_reqs.first; f_dmem_reqs.deq;
      return req;
   endmethod

   method ActionValue#(Bool) response_from_memory(From_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE) rsp);
      let cpu_rsp = To_Cpu_D{address: rsp.address, bus_error: rsp.bus_error, misaligned_error: 0, data_word: rsp.data_line, load_store: ?};
      f_cpu_rsps.enq(cpu_rsp);
      return True;
   endmethod

   method Action clear_all;
      noAction;
   endmethod
endmodule
