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

// This package models Execute stage of a RISC-V CPU
// Standard extension for Multiplication and Division (RV32M) is
// supported in addition to RISC-V Base Integer Instruction Set RV32I
// Architectural regfile is instantiated in this module. Therefore, operand fetch is done here as well
// Execute unit performs the following operation depending upon the type of instruction
// Integer Computational Instruction: Arithmetic and Logic operations on operands
// Data Transfer Instruction: Address calculation and operand manipulation
// Branch Instruction: Branch evaluation and target address calculation

// ================================================================
// Bluespec libraries

import FIFOF         :: *;
import DefaultValue  :: *;
import SpecialFIFOs  :: *;
import DefaultValue  :: *;
import DReg          :: *;
import ConfigReg     :: *;
import UniqueWrappers:: *;

// ================================================================
// Project imports

import TLM2          :: *; // Using local copy
import Req_Rsp       :: *;
import Sys_Configs   :: *;
import Vector        :: *;

import ISA_Defs      :: *;
import TypeDefs      :: *;

`include "TLM.defines"
`include "RVC.defines"

// ================================================================

interface Execute_IFC;
   method Action request (ExecuteData execData); // Input data. Data Forwarded from Decode Unit
   method ActionValue#(CommitData) response; // Returns data to be committed
   method Addr read_exec_pc; // Returns PC of instruction being executed
   method Action write_verbosity (int verbosity); // Update verbosity
endinterface

module mkExecute (Execute_IFC);
    
   // Utility wires and registers
   Wire #(int)         wr_verbosity <- mkBypassWire;
   Wire #(ExecuteData) wr_request   <- mkWire;
   Wire #(Maybe#(CommitType#(`TLM_PRM_REQ_CPU))) wr_response <- mkWire;
   Reg  #(Data) rg_last_exec_pc <- mkConfigRegU;   // PC of last instruction executed. Used for verification purposes only

   // ---------
   // Utility functions
   // Function to perform 32-bit multiplication
   function Data_2 fn_atomic_mul (Data op1, Data op2, Bool take_complement);
      Data_2 res = extend(op1) * extend(op2);
      if (take_complement == True) res = ~res+1;
      return res;
   endfunction

   function Data fn_atomic_div (Data op1, Data op2, Bool take_complement);
      Data res = (pack)(op1 / ((op2==0)?1:op2));
      if (take_complement == True) res = ~res+1;
      return res;
   endfunction

   function Data fn_atomic_mod (Data op1, Data op2, Bool sign);
      Data_S sop1 = unpack(op1);
      Data_S sop2 = unpack(op2);
      Data res = (sign == True) ? (pack)(sop1 % ((sop2==0)?1:sop2)) : (pack)(op1 % ((op2==0)?1:op2));
      return res;
   endfunction
   
   function Data fn_right_arith_shift (Data in, Bit #(5) amt);
      Bit #(64) _in;
      _in = (in[31] == 0) ? {zeroExtend(in)} : {signExtend(in)};
      return ((_in >> amt)[31:0]);
   endfunction

   function Data fn_less_than(Data op1, Data op2, Bool sign);
      Data_S sop1 = unpack(op1), sop2 = unpack(op2);
      Data_U uop1 = unpack(op1), uop2 = unpack(op2);
      Data res = ?;
      if(sign == True) res = (sop1 < sop2) ? 1:0;
      else res = (uop1 < uop2) ? 1:0;
      return res;
   endfunction

   Wrapper2#(Data, Data, Data)   wrapper_add_1 <- mkUniqueWrapper2( \+ );
   Wrapper2#(Data, Data, Data)   wrapper_sub_1 <- mkUniqueWrapper2( \- );
   Wrapper2#(Data, Data, Data)   wrapper_xor_1 <- mkUniqueWrapper2( \^ );
   Wrapper2#(Data, Data, Data)   wrapper_and_1 <- mkUniqueWrapper2( \& );
   Wrapper2#(Data, Data, Data)   wrapper_or_1  <- mkUniqueWrapper2( \| );
   Wrapper2#(Data, Bit#(5), Data)   wrapper_sll_1 <- mkUniqueWrapper2( \<< );
   Wrapper2#(Data, Bit#(5), Data)   wrapper_srl_1 <- mkUniqueWrapper2( \>> );
   Wrapper2#(Data, Bit#(5), Data)   wrapper_sra_1 <- mkUniqueWrapper2(fn_right_arith_shift);
   Wrapper3#(Data, Data, Bool, Data)   wrapper_lt_1  <- mkUniqueWrapper3(fn_less_than);
   Wrapper3#(Data, Data, Bool, Data)   wrapper_div_1 <- mkUniqueWrapper3(fn_atomic_div);
   Wrapper3#(Data, Data, Bool, Data)   wrapper_mod_1 <- mkUniqueWrapper3(fn_atomic_mod);
   Wrapper3#(Data, Data, Bool, Data_2) wrapper_mul_1 <- mkUniqueWrapper3(fn_atomic_mul);

   function Action fn_report_exec_instr   (Opcode opcode, Funct3 funct3, Funct7 funct7, Funct12 funct12,
                                  RegName rd, RegName rs1, RegName rs2,
                                  Data v1, Data v2, Imm_I imm_I, Imm_S imm_S, Imm_J imm_J, Imm_U imm_U, Imm_B imm_B);
      case (opcode)
         op_IMM    : $display ("OPCODE: %b FUNCT3: %h RD: %d RS1: %d V1: %h IMM: %h", opcode, funct3, rd, rs1, v1, imm_I);
         op_RR     : $display ("OPCODE: %b FUNCT7: %h FUNCT3: %h RD: %d RS1: %d RS2: %d", opcode, funct7, funct3, rd, rs1, rs2);
         op_LOAD   : $display ("OPCODE: %b FUNCT3: %h RD: %d RS1: %d IMM: %h", opcode, funct3, rd, rs1, imm_I);
         op_STORE  : $display ("OPCODE: %b FUNCT3: %h RS1: %d RS2: %d IMM: %h", opcode, funct3, rs1, rs2, imm_S);
         op_BRANCH : $display ("OPCODE: %b FUNCT3: %h RS1: %d IMM: %h", opcode, funct3, rs1, rs2, imm_B);
         op_JALR   : $display ("OPCODE: %b FUNCT3: %h RD: %d RS1: %d IMM: %h", opcode, funct3, rd, rs1, imm_I);
         op_JAL    : $display ("OPCODE: %b RD: %d IM: %h", opcode, rd, imm_J);
         op_LUI    : $display ("OPCODE: %b RD: %d IMM: %h", opcode, rd, imm_U);
         op_AUIPC  : $display ("OPCODE: %b RD: %d IMM: %h", opcode, rd, imm_U);
         op_SYSTEM : $display ("OPCODE: %b CSR Addr: %h FUNCT3: %h RD: %d RS1: %d V1: %d", opcode, funct12, funct3, rd, rs1, v1);
      endcase
   endfunction

   // Halt CPU if an illegal instruction was encountered. Used for Verification purposes
   function Action fn_exec_halt;
   action
      wr_response <= tagged Valid tagged Halt True;
   endaction
   endfunction
   
   // Execute instructions with immediate encoding
   function Action fn_exec_imm(Funct3 funct3, Funct7 funct7, RegName rd, Data v1, Data sx_imm_I);
   action
      let shift_amt = sx_imm_I[4:0];
      Data op1 = v1, op2 = sx_imm_I;
      let res = ?;
      let match1 = True, match2 = True;
      case (funct3)
         f3_ADDI  : res <- wrapper_add_1.func(op1, op2);
         f3_SLTI  : res <- wrapper_lt_1.func(op1, op2, True);
         f3_SLTIU : res <- wrapper_lt_1.func(op1, op2, False);
         f3_XORI  : res <- wrapper_xor_1.func(op1, op2);
         f3_ANDI  : res <- wrapper_and_1.func(op1, op2);
         f3_ORI   : res <- wrapper_or_1.func(op1, op2);
         default  : match1 = False;
      endcase
      case ({funct3, funct7})
         {f3_SLLI, f7_SLLI} : res <- wrapper_sll_1.func(op1, shift_amt);
         {f3_SRLI, f7_SRLI} : res <- wrapper_srl_1.func(op1, shift_amt);
         {f3_SRAI, f7_SRAI} : res <- wrapper_sra_1.func(op1, shift_amt);
         default: match2 = False;
      endcase
      if(match1 || match2) wr_response <= tagged Valid tagged WB RequestWriteBack{rd: rd, data: res};
      else wr_response <= tagged Invalid;
   endaction
   endfunction

   // Execute instructions performing LOAD
   function Action fn_exec_load(Funct3 funct3, RegName rd, Data v1, Data sx_imm_I);
   action
      Req_Desc_CPU tlm_req = defaultValue;
      tlm_req.addr = v1 + sx_imm_I;
      Maybe#(LS_Remark) remark = tagged Invalid;
      case (funct3)
         f3_LB   : begin tlm_req.burst_size = 0; remark = tagged Valid SIGNED;   end
         f3_LH   : begin tlm_req.burst_size = 1; remark = tagged Valid SIGNED;   end
         f3_LW   : begin tlm_req.burst_size = 3; remark = tagged Valid SIGNED;   end
         f3_LBU  : begin tlm_req.burst_size = 0; remark = tagged Valid UNSIGNED; end
         f3_LHU  : begin tlm_req.burst_size = 1; remark = tagged Valid UNSIGNED; end
         f3_LWU  : begin tlm_req.burst_size = 3; remark = tagged Valid UNSIGNED; end
      endcase
      if(remark matches tagged Valid .v) wr_response <= tagged Valid tagged LS ReqLS{tlmreq: tagged Descriptor tlm_req, rd: rd, remark: v};
      else wr_response <= tagged Invalid;
   endaction
   endfunction

   // Execute instructions performing LOAD
   function Action fn_exec_store(Funct3 funct3, Data v1, Data v2, Data sx_imm_S);
   action
      Req_Desc_CPU tlm_req = defaultValue;
      tlm_req.command = WRITE;
      tlm_req.addr = v1 + sx_imm_S;
      Bool no_match = False;
      case (funct3)
         f3_SB   : begin tlm_req.burst_size = 0; tlm_req.data = {24'b0, v2[7:0]};  end
         f3_SH   : begin tlm_req.burst_size = 1; tlm_req.data = {16'b0, v2[15:0]}; end
         f3_SW   : begin tlm_req.burst_size = 3; tlm_req.data = v2;                end
         default : no_match = True;
      endcase
      if(no_match == False) wr_response <= tagged Valid tagged LS ReqLS {tlmreq: tagged Descriptor tlm_req, rd: ?, remark: ?};
      else wr_response <= tagged Invalid;
   endaction
   endfunction

   // Execute control transfer instructions
   function Action fn_exec_branch(Funct3 funct3, Addr pc, Data v1, Data v2, Data sx_imm_B);
   action
      Data_S sign_v1 = unpack(v1);
      Data_S sign_v2 = unpack(v2);
      Data_U usign_v1 = unpack(v1);
      Data_U usign_v2 = unpack(v2);
      Addr branch_target = pc + sx_imm_B;
      Maybe #(Bool) res = tagged Invalid;
      case (funct3)
         f3_BEQ  : res = tagged Valid (v1 == v2); 
         f3_BNE  : res = tagged Valid (v1 != v2);
         f3_BLT  : res = tagged Valid (sign_v1 < sign_v2);
         f3_BGE  : res = tagged Valid (sign_v1 >= sign_v2);
         f3_BLTU : res = tagged Valid (usign_v1 < usign_v2);
         f3_BGEU : res = tagged Valid (usign_v1 >= usign_v2);
      endcase
      if(res matches tagged Valid .v) wr_response <= tagged Valid tagged JMP RequestJump{branchAddr: (v == True) ? tagged Valid branch_target : tagged Invalid, rd: ?, lr: tagged Invalid};
      else wr_response <= tagged Invalid;
   endaction
   endfunction
   
   // Execute JALR instruction
   function Action fn_exec_jalr(Funct3 funct3, Addr pc, RegName rd, Data v1, Data sx_imm_I);
   action
      Addr target_addr = v1 + sx_imm_I;
      target_addr[0] = 0;
      Addr lr = pc + zeroExtend(instlen_byte);
      if(funct3 == f3_JALR) wr_response <= tagged Valid tagged JMP RequestJump{branchAddr: tagged Valid target_addr, rd: rd, lr: tagged Valid lr};
      else wr_response <= tagged Invalid;
   endaction
   endfunction

   // Execute JAL instruction
   function Action fn_exec_jal(Addr pc, RegName rd, Data sx_imm_J);
   action
      Addr target_addr = pc + sx_imm_J;
      Addr lr = pc + zeroExtend(instlen_byte);
      wr_response <= tagged Valid tagged JMP RequestJump{branchAddr: tagged Valid target_addr, rd: rd, lr: tagged Valid lr};
   endaction
   endfunction

   // Execute LUI instruction
   function Action fn_exec_lui(RegName rd, Imm_U imm_U);
   action
      Data lui_imm = imm_U;
      wr_response <= tagged Valid tagged WB RequestWriteBack{rd: rd, data: lui_imm};
   endaction
   endfunction

   // Execute AUIPC instruction
   function Action fn_exec_auipc(Addr pc, RegName rd, Imm_U imm_U);
   action
      Addr lui_imm = imm_U;
      lui_imm = pc + lui_imm;
      wr_response <= tagged Valid tagged WB RequestWriteBack{rd: rd, data: lui_imm};
   endaction
   endfunction

   // Execute FENCE instruction
   function Action fn_exec_miscmem; // TODO Implement this properly. Implemented as nop for now
   action
      wr_response <= tagged Valid tagged WB RequestWriteBack{rd: 0, data: 0};
   endaction
   endfunction

   // =========== M Extension: MUL/DIV and Register-Register Instruction ============
   
   // =============== RV32M =========================================================
   // Execute M Extension instructions. Register-Register instruction are also defined here as they have the same opcode
   function Action fn_exec_mul_32(Funct3 funct3, Funct7 funct7, RegName rd, Data v1, Data v2);
   action
      let shift_amt = v2[4:0];
      Data op1 = v1, op2 = v2;
      Data_S sop1 = unpack(op1), sop2 = unpack(op2);
      Data_U uop1 = unpack(op1), uop2 = unpack(op2);
      Data res = ?;
      Data_2 res_2 = ?;
      Bit #(1) sn_op1 = op1[valueof(XLEN)-1], sn_op2 = op2[valueof(XLEN)-1];
      Bool take_complement = !(sn_op1 == sn_op2);
      Data mop1 = (sn_op1 == 1) ? (~op1+1) : op1;
      Data mop2 = (sn_op2 == 1) ? (~op2+1) : op2;
      Bool match1 = True;
      case ({funct3, funct7})
         // MUL/DIV Instructions
         {f3_MUL,    f7_MUL}     : begin res_2 <- wrapper_mul_1.func(op1, op2, False); res = truncate(res_2); end
         {f3_MULH,   f7_MULH}    : begin res_2 <- wrapper_mul_1.func(mop1, mop2, take_complement); res = truncateLSB(res_2); end
         {f3_MULHSU, f7_MULHSU}  : begin res_2 <- wrapper_mul_1.func(mop1, op2, unpack(sn_op1)); res = truncateLSB(res_2); end
         {f3_MULHU,  f7_MULHU}   : begin res_2 <- wrapper_mul_1.func(op1, op2, False); res = truncateLSB(res_2); end
         {f3_DIV,    f7_DIV}     : begin 
                                       if (sop2 == 0) res = -1;
                                       else if (sop1 == xlen_signed_low && sop2 == -1) res = pack(xlen_signed_low);
                                       else res <- wrapper_div_1.func(mop1, mop2, take_complement);
                                   end
         {f3_DIVU,   f7_DIVU}    : begin 
                                       if (uop2 == 0) res = pack(xlen_unsigned_high);
                                       else res <- wrapper_div_1.func(op1, op2, False);
                                   end
         {f3_REM,    f7_REM}     : begin 
                                       if (sop2 == 0) res = pack(sop1);
                                       else if (sop1 == xlen_signed_low && sop2 == -1) res = 0;
                                       else res <- wrapper_mod_1.func(op1, op2, True);
                                   end
         {f3_REMU,   f7_REMU}    : begin
                                       if (uop2 == 0) res = pack(uop1);
                                       else res <- wrapper_mod_1.func(op1, op2, False);
                                   end
         // Register-Register Instructions
         {f3_ADD, f7_ADD}        : res <- wrapper_add_1.func(op1, op2);
         {f3_SUB, f7_SUB}        : res <- wrapper_sub_1.func(op1, op2);
         {f3_SLL, f7_SLL}        : res <- wrapper_sll_1.func(op1, shift_amt);
         {f3_SRL, f7_SRL}        : res <- wrapper_srl_1.func(op1, shift_amt);
         {f3_SRA, f7_SRA}        : res <- wrapper_sra_1.func(op1, shift_amt);
         {f3_SLT, f7_SLT}        : res <- wrapper_lt_1.func(op1, op2, True);
         {f3_SLTU, f7_SLTU}      : res <- wrapper_lt_1.func(op1, op2, False);
         {f3_XOR, f7_XOR}        : res <- wrapper_xor_1.func(op1, op2);
         {f3_AND, f7_AND}        : res <- wrapper_and_1.func(op1, op2);
         {f3_OR,  f7_OR}         : res <- wrapper_or_1.func(op1, op2);
         default: match1 = False;
      endcase
      if(match1 == True) wr_response <= tagged Valid tagged WB RequestWriteBack{rd: rd, data: res};
      else wr_response <= tagged Invalid;
   endaction
   endfunction

   // =============== END: RV32M ====================================================

   // =============== END: M Extension: MUL/DIV =====================================

   // =============== Privileged Instructions =======================================
   function Action fn_exec_system(Opcode opcode, Funct3 funct3, Funct12 funct12, RegName rd, RegName rs1, Data v1);
   action
      let data = CSRData{
         opcode: opcode,
         funct3: funct3,
         data  : v1,
         zimm  : zeroExtend(rs1),
         rs1   : rs1,
         rd    : rd,
         csr_addr: funct12
      };
      wr_response <= tagged Valid tagged SYS data;
   endaction
   endfunction
   // =============== END: Privileged Instructions ==================================

   rule rl_handle_execute_requests;
      let executeData = wr_request;
      let iFields = executeData.instFields;
      let opcode  = iFields.opcode;
      let rs1     = iFields.rs1;
      let rs2     = iFields.rs2;
      let rd      = iFields.rd;
      let imm_I   = iFields.imm_I;
      let imm_S   = iFields.imm_S;
      let imm_B   = iFields.imm_B;
      let imm_U   = iFields.imm_U;
      let imm_J   = iFields.imm_J;
      let funct3  = iFields.funct3;
      let funct7  = iFields.funct7;
      let funct12 = iFields.funct12;
      let v1 = executeData.v1;
      let v2 = executeData.v2;
      let pc = executeData.instAddr;
      Data sx_imm_I = signExtend(imm_I);
      Data sx_imm_S = signExtend(imm_S);
      Data sx_imm_B = signExtend(imm_B);
      Data sx_imm_U = signExtend(imm_U);
      Data sx_imm_J = signExtend(imm_J);
      if(executeData.illegalInst == True) fn_exec_halt;
      else case(opcode)
         op_IMM:     fn_exec_imm    (funct3, funct7, rd, v1, sx_imm_I);
         op_LOAD:    fn_exec_load   (funct3, rd, v1, sx_imm_I);
         op_STORE:   fn_exec_store  (funct3, v1, v2, sx_imm_S);
         op_BRANCH:  fn_exec_branch (funct3, pc, v1, v2, sx_imm_B);
         op_JALR:    fn_exec_jalr   (funct3, pc, rd, v1, sx_imm_I);
         op_JAL:     fn_exec_jal    (pc, rd, sx_imm_J);
         op_LUI:     fn_exec_lui    (rd, imm_U);
         op_AUIPC:   fn_exec_auipc  (pc, rd, sx_imm_U);
         op_MUL_32:  fn_exec_mul_32 (funct3, funct7, rd, v1, v2);
         op_SYSTEM:  fn_exec_system (opcode, funct3, funct12, rd, rs1, v1);
         op_MISCMEM: fn_exec_miscmem;
         default: wr_response <= tagged Invalid;
      endcase
      if (wr_verbosity > 2) begin
         $write($time, " CPU: EXECUTE: ");
         //fn_report_exec_instr(Opcode opcode, Funct3 funct3, Funct7 funct7, Funct12 funct12,
      //                      RegName rd, RegName rs1, RegName rs2,
      //                      Data v1, Data v2, Imm_I imm_I, Imm_S imm_S, Imm_J imm_j, Imm_U imm_U, Imm_B imm_B);
      end
   endrule

   method Action request(ExecuteData executeData);
      wr_request <= executeData;
   endmethod

   method ActionValue#(CommitData) response;
      let ret = CommitData{pc: wr_request.instAddr, commitType: wr_response};
      rg_last_exec_pc <= wr_request.instAddr;
      return ret;
   endmethod

   method Addr read_exec_pc;
      return rg_last_exec_pc;
   endmethod

   method Action write_verbosity (int verbosity);
      wr_verbosity <= verbosity;
   endmethod

endmodule
