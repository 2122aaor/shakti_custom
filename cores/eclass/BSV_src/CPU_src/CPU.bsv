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

package CPU;

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
import ConfigReg     :: *;

// ================================================================
// Project imports and includes

import TLM2             :: *; // Using local copy
import Utils            :: *; // Utility functions and typeclasses to zip TLMRecvIFC and TLMSendIFC with FIFOs
import Req_Rsp          :: *; // Request and Response packet types
import Sys_Configs      :: *; // Overall System Configuration file
import ISA_Defs         :: *; // RISC-V Instruction specifications
import TypeDefs         :: *;
import Interfaces       :: *;
import ISA_Defs_PRV     :: *; // RISC-V Privilege ISA specifications
import CSRFile          :: *;
import Resettable       :: *;
import FetchStage       :: *;
import ExecuteStage     :: *;
import WriteBackStage   :: *;

`include "TLM.defines"        // Parameters for TLM packets
`include "RVC.defines"        // Local Definitions
`include "macro.defines"      // System Configuration Parameters

// ================================================================
// Reasons why the CPU is currently stopped
// ----------------------------------------
// This error reporting is solely for debugging purposes and should be used only in debug mode
// These error messages are not a part of the RISC-V ISA specification
// RISC-V supported exceptions are implemented separately

function Action display_stop_reason (String pre, CPU_Stop_Reason reason, String post);
   action
      $write ($time, pre);
      case (reason)
    CPU_STOP_BREAK:           $write ("CPU_STOP_BREAK");       // A stop was explicitly requested (for ex. by testbench)
    CPU_STOP_EXIT:            $write ("CPU_STOP_EXIT");        // Exited as the end of instruction stream was reached
    CPU_STOP_INSTR_ERR:       $write ("CPU_STOP_INSTR_ERR");      // An error was encountered while executing an instruction (Maybe unrecognized instruction)
    CPU_STOP_INSTR_UNIMPLEM:  $write ("CPU_STOP_INSTR_UNIMPLEM"); // An instruction that is supposed to be supported but has not yet been implemented was encountered
    CPU_STOP_MEM_ERR:         $write ("CPU_STOP_MEM_ERR");        // Memory read/write resulted in an error
      endcase
      $write (post);
   endaction
endfunction

typedef enum {REQ, RESP} FetchState deriving(Bits, Eq);

// ================================================================
// CPU interface

// Interface layout for CPU
interface CPU_IFC;
   interface TLMSendIFC #(Req_CPU, Rsp_CPU)  imem_ifc;   // For Instruction Memory
   interface TLMSendIFC #(Req_CPU, Rsp_CPU)  dmem_ifc;   // For Data Memory
   interface InterruptIFC interrupt_ifc;
   interface CoreDebugIFC debug_ifc;
   interface UartIFC uart_ifc; // RS232 interface
   method Action write_counter_time(CounterData data);
   method Bool halt;
endinterface

// ================================================================
// CPU model

(* synthesize, conflict_free = "rl_update_execute, rl_read_execute" *)
module mkCPU_Model#(Data hartid)(CPU_IFC);

   // Architectural (General-Purpose) Register File
   RegFile #(RegName, Data) _regfile <- mkRegFileWCF(0, 31);
   ResetWrapper #(RegName, Data) regfile <- mkResetWrapper(_regfile, 0, 0, 31);

   // Counters
   Reg #(CounterData) counter_cycle   <- mkReg(0); // Number of clock cycles executed
   Reg #(CounterData) counter_instret <- mkReg(0); // Number of clock instructions retired

   // Control and Status register file
   CSR_IFC csrfile <- mkCSRFile(0, counter_cycle, counter_instret);

   // Misc. micro-architectural state
   Reg #(int)       rg_verbosity         <- mkReg(4); // Frequency of error messages to be printed 
   Reg #(CPU_State) rg_cpu_state         <- mkReg(STOPPED); // State of the CPU
   Reg #(CPU_Mode)  rg_cpu_mode          <- mkReg(NORMAL); // Mode of operation of CPU 
   Reg #(Addr)      rg_last_commit_pc    <- mkRegU;
   Reg #(Bool)      rg_stop_requested[2] <- mkCReg(2, False); // True if a stop was requested by the previous instruction
   Reg #(Bool)      rg_halt_requested    <- mkConfigReg(False);
   Reg #(Maybe #(CPU_Stop_Reason)) rg_stop_reason[2] <- mkCReg(2, tagged Invalid); // Reason why CPU is currently stopped. Used only in debug mode
   
   // Global Reset Signal
   // This reset is used only for software based debugger. A hard-reset on the other hand resets all nets to their default values
   PulseWire pwr_system_reset <- mkPulseWire; // Not used currently

   FIFOF #(Req_CPU) f_dmem_reqs <- mkBypassFIFOF;  // Enqueued to make a request to Data memory
   FIFOF #(Rsp_CPU) f_dmem_rsps <- mkPipelineFIFOF;  // Repsonse from Data memory arrives in this FIFO

   // Pipeline FIFOFs between stages
   FIFOF #(IFEX_Data)  f_if_ex  <- mkPipelineFIFOF; // FIFOF between IF and EX Stage.
   FIFOF #(EXWB_Data)  f_ex_wb  <- mkPipelineFIFOF; // FIFOF between EX and WB Stage.

   // ------------
   // Module instantiations
   FetchIFC fetch <- mkFetch; // Fetch stage
   Execute_IFC execute <- mkExecute; // Execute Stage
   WriteBackIFC writeback <- mkWriteBack(csrfile); // WriteBack Stage

   // ----------------
   // Misc Registers and Wires to pass data between rules
   PulseWire pwr_flush   <- mkPulseWire;
   PulseWire pwr_instret <- mkPulseWire;
   PulseWire pwr_stall_execute <- mkPulseWire;
   
   Wire#(Maybe#(Forward_Data)) wr_forwarded_data <- mkDWire(tagged Invalid);
   Reg #(FetchState) rg_dmem_state[2] <- mkCReg(2, REQ);

   // ---------
   // Helper functions

   // Helper function to evaluate explicit rule conditions
   function Bool fn_fetch_condition;
      Bool dbg = ((rg_cpu_mode == DEBUG)  && (rg_cpu_state == FETCH));
      Bool nrm = ((rg_cpu_mode == NORMAL) && (rg_cpu_state == FETCH));
      return (dbg || nrm);
   endfunction

   function Bool fn_decode_condition;
      Bool dbg = (rg_cpu_mode == DEBUG)  && (rg_cpu_state != STOPPED);
      Bool nrm = (rg_cpu_mode == NORMAL) && (rg_cpu_state != STOPPED);
      return (dbg || nrm);
   endfunction

   function Bool fn_execute_condition;
      Bool dbg = (rg_cpu_mode == DEBUG)  && (rg_cpu_state != STOPPED);
      Bool nrm = (rg_cpu_mode == NORMAL) && (rg_cpu_state != STOPPED);
      return (dbg || nrm);
   endfunction

   function Bool fn_debug_method_cond;
      Bool dbg = (rg_cpu_mode == DEBUG) && (rg_cpu_state == STOPPED);
      return dbg;
   endfunction
   
   // Function to read the Architectural (General-Purpose) Register File
   function Data fn_read_gpr (RegName r);
      return ((r == 0) ? 0 : regfile._read(r));
   endfunction

   // Function to read the Architectural (General-Purpose) Register File
   function Action fn_write_gpr (RegName r, Data d);
      action
         if (r != 0) regfile._write(r, d);
      endaction
   endfunction

   Bool        fetch_cond = fn_fetch_condition;
   Bool       decode_cond = fn_decode_condition;
   Bool      execute_cond = fn_execute_condition;
   Bool debug_method_cond = fn_debug_method_cond;

   // Update clock cycles
   rule rl_count_cycles (rg_cpu_state != STOPPED);
      counter_cycle <= counter_cycle + 1;
   endrule

   // Current state of the thread
   // TODO Add address translation and privilege checking using below variables
   let csrState = csrfile.read_csr_state;
   let vmInfo_I = csrfile.read_vmInfo_I;
   let vmInfo_D = csrfile.read_vmInfo_D;

   // ----------------
   // Misc. rules for debugger

   rule rl_stop_cpu (rg_stop_requested[1] == True && pwr_instret);
     // XXX Should I reset rg_stop_requested to False in this rule
     rg_cpu_state <= STOPPED;
     rg_stop_reason[0] <= tagged Valid CPU_STOP_BREAK;
   endrule

   // =================================================================================
   // STAGE-1: FETCH
   // --------------
   /* =================================================================================
   // Note on TLM Transactions
   // ------------------------
   // All exchanges external to CPU are made in the form of TLM transactions
   // Bluespec provides TLM library to standardize exchange of data between modules
   // This implementation uses TLM version 2
   // Two type of TLM transactions exist - 1. Request 2. Response
   // 1. Request: TLMRequest is a union that can take one of the following two forms
   // ----------- RequestDescriptor: For single transfers or first transfer of a burst
   //            Contains relevant fields like Addr, Data, Burst_length, transcation_id
   //            required to initiate a transaction
   //         RequestData: For follow-up transfers of a burst mode. Contains only data
   //         and transaction_id field as other information was set previously
   // 2. Response: TLMResponse is the response generated by the slave
   // ------------
   // Both of these packets are parameterised with the following parameters -
   // (id_size, addr_size, data_size, uint_size, cstm_type)
   // We have used TLM transactions to leverage standard libraries for components like
   // Buses provided by Bluespec
   // ===============================================================================*/
   
   rule rl_fetch_request(fetch_cond);
      fetch.request;
   endrule

   rule rl_fetch_response(fetch_cond);
      let retVal <- fetch.response;
      f_if_ex.enq(retVal);
   endrule
   // ===================== END: FETCH =============================================

   // ==============================================================================
   // Stage-2: INSTRUCTION EXECUTE
   // ---------------------------
   // Decode and execute instruction. Setup Write-back stage
   // ==============================================================================

   rule rl_update_execute (decode_cond && !pwr_stall_execute);
      let executeStageData = f_if_ex.first;
      let thisPC = executeStageData.pc;
      let thisData = executeStageData.data;
      let wbStageData = EXWB_Data{pc: thisPC, data: ?};
      /*Bypass if trapped*/
      if (thisData matches tagged TrapD .t) begin 
         wbStageData.data = tagged TrapD t;
         f_ex_wb.enq(wbStageData);
      end
      /*Setup execute unit otherwise*/
      else if (thisData matches tagged StageD .d) begin
         let instData = d.inst;
         // DECODE
         // -------------------------
         // Decode instruction fields
         InstFields iFields = toInstFields(instData);
         // Read operands from Architectural Register File
         Data v1 = fn_read_gpr(iFields.rs1), v2 = fn_read_gpr(iFields.rs2);
         if(wr_forwarded_data matches tagged Valid .fwd_data) begin
            if(fwd_data.rd == iFields.rs1) v1 = fwd_data.data;
            if(fwd_data.rd == iFields.rs2) v2 = fwd_data.data;
         end
         // Display debug messages
         if (rg_verbosity > 1)$display($time, " CPU: DECODE: Instruction decoded. PC: %h, Instruction: %h", thisPC, instData);
         if (rg_verbosity > 2)$display($time, " CPU: DECODE: Opcode: %b, rs1: %d, rs2: %d, rd: %d, f3: %b, f7: %b", iFields.opcode, iFields.rs1, iFields.rs2, iFields.rd, iFields.funct3, iFields.funct7);
         // Make data packet to be passed to the Execute stage
         let executeData = tagged ExecuteData{
            instFields:    iFields,
            v1:            v1,
            v2:            v2,
            instAddr:      thisPC,
            illegalInst:   instData == ('h0000006f) ? True:False // According to RISC-V ISA, '0000006F' is a self-loop instruction
            // For verification purposes, we append this instruction to the instruction stream to indicate its end
         };
         // EXECUTE
         // --------------------------
         // Update execute unit
         execute.request(executeData);
         execute.write_verbosity(rg_verbosity);
         if (rg_verbosity > 1) $display($time, " CPU: EXECUTE: Setting up EXECUTE unit. PC: %h", thisPC);
      end
   endrule
   
   // Receive response from ALU and setup Write-Back
   rule rl_read_execute (decode_cond);
      let executeOut <- execute.response;
      let wbStageData = EXWB_Data {pc: executeOut.pc, data: ?};
      // Next stage input
      if(executeOut.commitType matches tagged Valid .v) begin
         wbStageData.data = tagged StageD v;
         f_if_ex.deq;
      end
      else begin
         wbStageData.data = tagged TrapD TrapData{bad_addr: tagged Invalid, trap: tagged Exception MCAUSE_ILLEGAL_INSTRN};
         //f_if_ex.deq; // If an error was encounter, trap handler must execute and pipeline must be flushed
      end
      f_ex_wb.enq(wbStageData);
   endrule

   // ===================== END: INSTRUCTION EXECUTE ===============================
   
   // MEMORY-ACCESS and WRITE-BACK
   // ======================================
    
   rule rl_write_back_request (execute_cond);
      let req = f_ex_wb.first;
      writeback.request(req);
      rg_last_commit_pc <= req.pc;
   endrule

   rule rl_write_back_response (execute_cond);
      let rsp = writeback.response;
      let halt = False;
      if(rsp.halt == True) halt = True;
      else if(rsp.breakpoint == True) rg_stop_requested[0] <= True;
      else begin
         if(rsp.data matches tagged Valid .data) fn_write_gpr(rsp.rd, data);
         if(rsp.next_pc matches tagged Valid .next_pc) begin
            fetch.redirect_stage_3(next_pc);
            pwr_flush.send;
         end
      end
      if(rsp.retire == True) begin
         counter_instret <= counter_instret + 1;
         pwr_instret.send;
      end
      rg_halt_requested <= halt;
      f_ex_wb.deq;
   endrule

   // Rule to flush all stages
   rule rl_flush (pwr_flush);
      f_if_ex.clear;
      f_ex_wb.clear;
      if(rg_verbosity > 1) $display($time, " CPU: [FLUSH]\n");
   endrule  
   
   rule rl_handle_hazards(f_if_ex.first.data matches tagged StageD .exData &&& f_ex_wb.first.data matches tagged StageD .wbData);
      RegName dest = 0;
      let data = tagged Invalid;
      InstFields iFields = toInstFields(exData.inst);
      let rs1 = iFields.rs1, rs2 = iFields.rs2;
      case (wbData) matches
         tagged WB  .v: begin dest = v.rd; data = tagged Valid v.data; end
         tagged LS  .v: begin if(v.tlmreq.Descriptor.command == READ) dest = v.rd; end
         tagged JMP .v: begin dest = (isValid(v.lr) ? v.rd : 0); data = v.lr; end
         tagged SYS .v: begin dest = v.rd; end
      endcase
      if(dest != 0) begin
         if(dest == rs1 || dest == rs2) begin
            if(isValid(data)) wr_forwarded_data <= tagged Valid Forward_Data{rd: dest, data: fromMaybe(?, data)};
            else pwr_stall_execute.send();
         end
      end
   endrule

   // ===================== END: WRITE-BACK ========================================
  
   // INTERFACE
   // ----------------
   // Instruction and Data memories
   interface imem_ifc = fetch.bus_ifc;
   interface dmem_ifc = writeback.bus_ifc;//toSendIFC (f_dmem_reqs, f_dmem_rsps);

   // =====================================================================================================================
   // GDB control
   // ----------------

   interface CoreDebugIFC debug_ifc;
      method Action reset if (rg_cpu_state == STOPPED);
         // Reset architectural regfile
         regfile._reset;
         // Reset pipeline fifos
         f_if_ex.clear;
         f_ex_wb.clear;
         // Reset pipeline stages
         fetch.reset(`PC_INIT_VALUE, rg_verbosity);
         writeback.reset(rg_verbosity);
         // Reset counters
         counter_cycle <= 0;
         counter_instret <= 0;
         // Reset miscellanious state
         //rg_verbosity <= 0;
         rg_last_commit_pc <= `PC_INIT_VALUE;
        `ifdef CPU_MODE_NORMAL
          rg_cpu_mode <= NORMAL;
        `else
         rg_cpu_mode <= DEBUG;
        `endif
      endmethod

      method ActionValue#(Bool) reset_complete if(rg_cpu_state == STOPPED);
         let reset_done <- regfile._reset_complete;
         fn_write_gpr(5, 32'h80000000); // XXX: Done as a fix to AAPG
         return reset_done;
      endmethod

      method Action run_continue (Maybe #(Addr) mpc) if (debug_method_cond);
         //if (mpc matches tagged Valid .new_pc) rg_pc <= new_pc;
         rg_stop_requested[1] <= False;
         rg_cpu_state      <= FETCH;
      endmethod

      method Action run_step (Maybe #(Addr) mpc) if (debug_method_cond);
         //if (mpc matches tagged Valid .new_pc) rg_pc <= new_pc;
         rg_stop_requested[1] <= True;
         rg_cpu_state      <= FETCH;
      endmethod

      method Action stop;
         rg_stop_requested[1] <= True; // Requests stop before next instruction
      endmethod

      method CPU_Stop_Reason stop_reason () if (debug_method_cond);
         return fromMaybe(?, rg_stop_reason[0]);
      endmethod

      method Addr read_pc () if (debug_method_cond);
         return fetch.last_fetched_pc;
      endmethod

      method Addr read_exec_pc () if (debug_method_cond);
         return rg_last_commit_pc;
      endmethod

      method Action write_pc (Addr d) if(rg_cpu_state == STOPPED);
         fetch.redirect(d);
         if (rg_cpu_mode == NORMAL)
              rg_cpu_state <= FETCH;
      endmethod

      method Data read_gpr (RegName r) if (debug_method_cond);
        return fn_read_gpr (r);
      endmethod

      method Action write_gpr (RegName r, Data d) if (debug_method_cond);
         fn_write_gpr (r, d);
      endmethod

      method Action req_read_memW (Addr addr) if (debug_method_cond);
         Req_Desc_CPU req_desc;
         req_desc = defaultValue;
         req_desc.command    = READ;
         req_desc.data       = ?;
         req_desc.addr       = addr;
         req_desc.burst_size = reqSz_bytes_i (fromInteger(instlen)); // burst_size field for ASZ 32-bits is 2-bits long. b'01 -> 2 bytes of data
         req_desc.burst_mode = INCR;
         req_desc.transaction_id = 1;
         Req_CPU req = tagged Descriptor req_desc;

         f_dmem_reqs.enq(req);
      endmethod

      method ActionValue #(Data) rsp_read_memW () if (debug_method_cond);
         Data d = f_dmem_rsps.first.data; f_dmem_rsps.deq;
         rg_stop_requested[1] <= True;
         return d;
      endmethod

      method Action write_memW (Addr addr, Data d) if (debug_method_cond);
         Req_Desc_CPU req_desc;
         req_desc = defaultValue;
         req_desc.command    = WRITE;
         req_desc.data       = d;
         req_desc.addr       = addr;
         req_desc.burst_size = reqSz_bytes_i (fromInteger(instlen)); // burst_size field for ASZ 32-bits is 2-bits long. b'01 -> 2 bytes of data
         req_desc.burst_mode = INCR;
         req_desc.transaction_id = 1;
         Req_CPU req = tagged Descriptor req_desc;

         f_dmem_reqs.enq (req);
         rg_stop_requested[1] <= True;
      endmethod

      method CounterData read_instret if (debug_method_cond);
         return counter_instret;
      endmethod

      method CounterData read_cycle if (debug_method_cond);
         return counter_cycle;
      endmethod

      // ----------------
      // Misc.

      method Action set_verbosity (int verbosity);
         rg_verbosity <= verbosity;
      endmethod
   endinterface
   
   interface InterruptIFC interrupt_ifc = csrfile.interrupt_ifc;

   method Action write_counter_time(CounterData data) = csrfile.write_counter_time(data);

   // UART interface
   interface UartIFC uart_ifc = csrfile.uart_ifc;

   method Bool halt;
      return rg_halt_requested;
   endmethod
endmodule

// ================================================================

endpackage: CPU
