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

// This package models a CPU based on RISC-V ISA
// Supported RISC-V Extensions are Base Integer Instruction Set, 32-Bit, (RV32I) and Standard Extension for Multiplication and Division, 32-Bit, (RV32M)
// Basic configuration -
//    1. Data Width: 32-bit
//    2. Address Width: 32-bit
//    3. Number of General: Purpose Registers: 32
//    4. Width of each General: Purpose Register: 32-bit
//    5. Number of Pipeline stages: 3
// Pipeline Stages - Fetch:   Fetches next instruction. Update PC
//                   Execute: Decodes the instruction and performs ALU operations. Sets up Memory and WB operations
//                   WriteBack: Perform Data Memory operations
//                              Executes branch operations (branch offsets were calculated in the execute stage)
//                              Performs write-back into regfile at the end of instruction execution
// Always Not-taken static branch prediction is in place
// Privilege-Modes: U, M
// Specification: RISC-V User-Level ISA Specification 2.1, RISC-V Privileged ISA Specification 1.9

// ================================================================
// Bluespec libraries

import RegFile       :: *;
import FIFOF         :: *;
import GetPut        :: *;
import ClientServer  :: *;
import DefaultValue  :: *;
import SpecialFIFOs  :: *;

// ================================================================
// Project imports and includes

import TLM2             :: *; // Using local copy
import Utils            :: *; // Utility functions and typeclasses to zip TLMRecvIFC and TLMSendIFC with FIFOs
import Req_Rsp          :: *; // Request and Response packet types
import ISA_Defs         :: *; // RISC-V Instruction specifications
import TypeDefs         :: *;
import Interfaces       :: *;
import ISA_Defs_PRV     :: *; // RISC-V Privilege ISA specifications
import CPU              :: *;

`include "TLM.defines"        // Parameters for TLM packets
`include "RVC.defines"        // Local Definitions
`include "macro.defines"      // System Configuration Parameters

// ================================================================
// Modules

interface Proc_IFC;
   interface TLMSendIFC#(Req_CPU, Rsp_CPU) bus_ifc;
   interface CoreDebugIFC debug_ifc;
   interface InterruptIFC interrupt_ifc;
   interface UartIFC uart_ifc;
   method Action write_counter_time(CounterData data);
   method Bool halt;
endinterface

module mkProc(Proc_IFC);
   CPU_IFC core <- mkCPU_Model(0);
   
   FIFOF #(Req_CPU) f_bus_reqs <- mkBypassFIFOF;
   FIFOF #(Rsp_CPU) f_bus_rsps <- mkBypassFIFOF;

   Reg#(Bit#(2)) who <- mkReg(2);
   Reg#(UInt#(10)) burst_length <- mkReg(0);

   (*preempts = "rl_dmem_request, rl_imem_request"*)
   rule rl_imem_request(who == 2);
      let req <- core.imem_ifc.tx.get;
      f_bus_reqs.enq(req);
      if(req matches tagged Descriptor .r)
      burst_length <= (case (req) matches
         tagged Descriptor .r: r.burst_length;
      endcase);
      who <= 0;
   endrule

   rule rl_dmem_request(who == 2);
      let req <- core.dmem_ifc.tx.get;
      f_bus_reqs.enq(req);
      burst_length <= (case (req) matches
         tagged Descriptor .r: r.burst_length;
      endcase);
      who <= 1;
   endrule

   rule rl_imem_response(who == 0);
      let rsp = f_bus_rsps.first;
      f_bus_rsps.deq;
      core.imem_ifc.rx.put(rsp);
      if(burst_length == 1) who <= 2;
      burst_length <= burst_length - 1;
   endrule

   rule rl_dmem_response(who == 1);
      let rsp = f_bus_rsps.first;
      f_bus_rsps.deq;
      core.dmem_ifc.rx.put(rsp);
      if(burst_length == 1) who <= 2;
      burst_length <= burst_length - 1;
   endrule
   
   interface bus_ifc   = toSendIFC(f_bus_reqs, f_bus_rsps);
   interface debug_ifc = core.debug_ifc;
   interface uart_ifc  = core.uart_ifc;
   interface interrupt_ifc = core.interrupt_ifc;
   method Action write_counter_time(CounterData data) = core.write_counter_time(data);
   method Bool halt = core.halt;
endmodule
