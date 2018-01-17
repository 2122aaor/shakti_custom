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

package ISA_Defs;

import TLM2 :: *;

`include "TLM.defines"
`include "RVC.defines"

// ================================================================
// RISC-V ISA defintions
// ================================================================

// Processor META Data
// ------

/*General Purpose Register Type*/
typedef 32 XLEN; // For RV32. Set to 64 for RV64
typedef TMul #(2, XLEN) XLEN_2;  // For multiplications etc.

/*Processor Version*/
typedef enum {RV32, RV64} RV_Version deriving (Eq, Bits);
RV_Version rv_version = ((valueOf (XLEN) == 32) ? RV32 : RV64);

/*Instruction Type*/
typedef  XLEN INSTLEN;
typedef  Bit #(INSTLEN)  Inst;
Integer  instlen = valueOf(INSTLEN);
Bit #(8) instlen_bit = fromInteger(instlen);
Bit #(5) instlen_byte = truncate(instlen_bit >> 3);

/*Data Type*/
typedef Bit #(XLEN) Data;     // Raw (unsigned) register data
typedef Int #(XLEN) Data_S;   // Signed register data
typedef UInt#(XLEN) Data_U;   // Unsigned register data

typedef Bit #(XLEN_2) Data_2;

/*Address Type*/
typedef Bit #(XLEN) Addr;

/*Counter Data Type*/
typedef 64 CounterSize;
typedef Bit #(CounterSize) CounterData;

// Utility definitions to indicate bounds on signed and unsigned integers
UInt #(XLEN) xlen_unsigned_low  = 0;
UInt #(XLEN) xlen_unsigned_high = 32'hffffffff;

Int #(XLEN) xlen_signed_low  = -2_147_483_648;
Int #(XLEN) xlen_signed_high = 2_147_483_647;

// ================================================================
// Instruction fields

typedef Bit #(7)  Opcode;
typedef Bit #(5)  RegName; // 32 registers, x0..x31
typedef Bit #(1)  AqFlag; // Aquire Flag
typedef Bit #(1)  RlFlag; // Release Flag
typedef Bit #(12) Imm_I;
typedef Bit #(12) Imm_S;
typedef Bit #(13) Imm_B;
typedef Bit #(32) Imm_U;
typedef Bit #(21) Imm_J;
typedef Bit #(3)  Funct3;
typedef Bit #(5)  Funct5;
typedef Bit #(7)  Funct7;
typedef Imm_I     Funct12;

typedef struct {
   Opcode   opcode; 
   RegName  rs1;
   RegName  rs2;
   RegName  rd;
   AqFlag   aq;  
   RlFlag   rl;  
   Imm_I    imm_I;
   Imm_S    imm_S;
   Imm_B    imm_B;
   Imm_U    imm_U;
   Imm_J    imm_J;
   Funct3   funct3;
   Funct5   funct5;
   Funct7   funct7;
   Funct12  funct12;
} InstFields deriving (Bits);

// Instruction decoding logic
function RegName instr_rs1    (Inst x) = x [19:15];
function RegName instr_rs2    (Inst x) = x [24:20];
function RegName instr_rd     (Inst x) = x [11:7];

function Imm_I   instr_imm_I  (Inst x) = { x[31], x[30:25], x[24:21], x[20] };
function Imm_S   instr_imm_S  (Inst x) = { x[31], x[30:25], x[11:8], x[7] };
function Imm_B   instr_imm_B  (Inst x) = { x[31], x[7], x[30:25], x[11:8], 'b0 };
function Imm_U   instr_imm_U  (Inst x) = { x[31], x[30:20], x[19:12], 12'b0 };
function Imm_J   instr_imm_J  (Inst x) = { x[31], x[19:12], x[20], x[30:25], x[24:21], 1'b0 };

function Opcode   instr_opcode  (Inst x) = x [6:0];
function Funct3   instr_funct3  (Inst x) = x [14:12];
function Funct7   instr_funct7  (Inst x) = x [31:25];
function Funct12  instr_funct12 (Inst x) = instr_imm_I(x);

// For Atomic Instructions
function Funct5    instr_funct5     (Inst x) = x [31:27];
function AqFlag    instr_aq         (Inst x) = x [26];
function RlFlag    instr_rl         (Inst x) = x [25];

function InstFields toInstFields(Inst inst);
   return InstFields {
      opcode:  instr_opcode(inst),
      rs1:     instr_rs1(inst),
      rs2:     instr_rs2(inst),
      rd:      instr_rd(inst),
      aq:      instr_aq(inst),
      rl:      instr_rl(inst),
      imm_I:   instr_imm_I(inst),
      imm_S:   instr_imm_S(inst),
      imm_B:   instr_imm_B(inst),
      imm_U:   instr_imm_U(inst),
      imm_J:   instr_imm_J(inst),
      funct3:  instr_funct3(inst),
      funct5:  instr_funct5(inst),
      funct7:  instr_funct7(inst),
      funct12: instr_funct12(inst)
   };
endfunction

// Register representation 
RegName x0  =  0;    RegName x1  =  1;    RegName x2  =  2;    RegName x3  =  3;
RegName x4  =  4;    RegName x5  =  5;    RegName x6  =  6;    RegName x7  =  7;
RegName x8  =  8;    RegName x9  =  9;    RegName x10 = 10;    RegName x11 = 11;
RegName x12 = 12;    RegName x13 = 13;    RegName x14 = 14;    RegName x15 = 15;
RegName x16 = 16;    RegName x17 = 17;    RegName x18 = 18;    RegName x19 = 19;
RegName x20 = 20;    RegName x21 = 21;    RegName x22 = 22;    RegName x23 = 23;
RegName x24 = 24;    RegName x25 = 25;    RegName x26 = 26;    RegName x27 = 27;
RegName x28 = 28;    RegName x29 = 29;    RegName x30 = 30;    RegName x31 = 31;

RegName lr = x1;  // link register
RegName sp = x2;  // stack pointer

// ================================================================
//  encoding space

// ==================== BASE INTEGER ISA ==========================
Bit #(2) tail = 'b11;
Opcode op_BRANCH = { 5'h18, tail };

Funct3 f3_BEQ   = 3'b000;     // 0
Funct3 f3_BNE   = 3'b001;     // 1
Funct3 f3_BLT   = 3'b100;     // 4
Funct3 f3_BGE   = 3'b101;     // 5
Funct3 f3_BLTU  = 3'b110;     // 6
Funct3 f3_BGEU  = 3'b111;     // 7

// JALR
Opcode op_JALR = { 5'h19, tail };
Funct3 f3_JALR  = 3'b000;     // 0

// JAL
Opcode op_JAL = { 7'b1101111 };

// LUI
Opcode op_LUI = { 5'h0D, tail };

// AUIPC
Opcode op_AUIPC = { 5'h05, tail };

// LOAD
Opcode op_LOAD = { 5'h00, tail };

Funct3 f3_LB  = 3'b000;    // 0 
Funct3 f3_LH  = 3'b001;    // 1
Funct3 f3_LW  = 3'b010;    // 2
Funct3 f3_LD  = 3'b011;    // 3  // RV64 only
Funct3 f3_LBU = 3'b100;    // 4
Funct3 f3_LHU = 3'b101;    // 5
Funct3 f3_LWU = 3'b110;    // 6  // RV64 only

// STORE
Opcode op_STORE = { 5'h08, tail };

Funct3 f3_SB = 3'b000;     // 0
Funct3 f3_SH = 3'b001;     // 1
Funct3 f3_SW = 3'b010;     // 2
Funct3 f3_SD = 3'b011;     // 3  // RV64 only


// MISC-MEM
Opcode op_MISCMEM = { 5'h03, tail };

Funct3 f3_FENCE   = 3'b000;   // 0
Funct3 f3_FENCE_I = 3'b001;   // 1


Opcode op_IMM = { 5'h04, tail };
Funct3 f3_ADDI   = 3'b000; // 0
Funct3 f3_SLTI   = 3'b010; // 2
Funct3 f3_SLTIU  = 3'b011; // 3
Funct3 f3_XORI   = 3'b100; // 4
Funct3 f3_ORI    = 3'b110; // 6
Funct3 f3_ANDI   = 3'b111; // 7
Funct3 f3_SLLI   = 3'b001; // 1
Funct7 f7_SLLI   = 7'b0000000;// 0
Funct3 f3_SRLI   = 3'b101; // 5
Funct7 f7_SRLI   = 7'b0000000;// 0
Funct3 f3_SRAI   = 3'b101; // 5
Funct7 f7_SRAI   = 7'b0100000;// 16

// NOP
Funct3 f3_NOP   = 3'b000;     // 0  // NOP = ALL ZEROS

Opcode op_RR = { 5'h0C, tail };  // Register-Register

Funct3 f3_ADD   = 3'b000;     // 0
Funct7 f7_ADD   = 7'b0000000; // 0
Funct3 f3_SUB   = 3'b000;     // 0
Funct7 f7_SUB   = 7'b0100000; // 16
Funct3 f3_SLL   = 3'b001;     // 1
Funct7 f7_SLL   = 7'b0000000; // 0
Funct3 f3_SLT   = 3'b010;     // 2
Funct7 f7_SLT   = 7'b0000000; // 0
Funct3 f3_SLTU  = 3'b011;     // 3
Funct7 f7_SLTU  = 7'b0000000; // 0
Funct3 f3_XOR   = 3'b100;     // 4
Funct7 f7_XOR   = 7'b0000000; // 0
Funct3 f3_SRL   = 3'b101;     // 5
Funct7 f7_SRL   = 7'b0000000; // 0
Funct3 f3_SRA   = 3'b101;     // 5
Funct7 f7_SRA   = 7'b0100000; // 16
Funct3 f3_OR    = 3'b110;     // 6
Funct7 f7_OR    = 7'b0000000; // 0
Funct3 f3_AND   = 3'b111;     // 7
Funct7 f7_AND   = 7'b0000000; // 0

// RV64 only
Opcode op_RR_32 = { 5'h0E, tail };  // Register-Register

Funct3 f3_ADDW = 3'b000;      // 0
Funct3 f3_SUBW = 3'b000;      // 0
Funct3 f3_SLLW = 3'b001;      // 1
Funct3 f3_SRLW = 3'b101;      // 5
Funct3 f3_SRAW = 3'b101;      // 5

// RV64 only
Opcode op_IMM_32 = { 5'h06, tail };

Funct3 f3_ADDIW = 3'b000;     // 0
Funct3 f3_SLLIW = 3'b001;     // 1
Funct3 f3_SRLIW = 3'b101;     // 5
Funct3 f3_SRAIW = 3'b101;     // 5

// SYSTEM INSTRUCTIONS - Very few are RV32I. Changes to be made
Opcode op_SYSTEM = { 5'h1C, tail };

Funct3  f3_ECALL     = 3'b000;   // 0
Funct12 f12_ECALL    = 12'b000000000000;  // 0
Funct3  f3_EBREAK    = 3'b000;   // 0
Funct12 f12_EBREAK   = 12'b000000000001;  // 1
Funct3  f3_URET      = 3'b000;   // 0
Funct12 f12_URET     = 12'b000000000010;
Funct3  f3_SRET      = 3'b000;   // 0
Funct12 f12_SRET     = 12'b000100000010;
Funct3  f3_HRET      = 3'b000;   // 0
Funct12 f12_HRET     = 12'b001000000010;
Funct3  f3_MRET      = 3'b000;   // 0
Funct12 f12_MRET     = 12'b001100000010;
Funct3  f3_WFI       = 3'b000;   // 0  // Not implemented currently
Funct12 f12_WFI      = 12'b000100000101;
Funct3  f3_SFENCE_VM = 3'b000;   // 0  // Not implemented currently
Funct3  f3_MRTH      = 3'b000;   // 0  // Not implemented currently
Funct3  f3_MRTS      = 3'b000;   // 0  // Not implemented currently
Funct3  f3_CSRRW     = 3'b001;   // 1
Funct3  f3_CSRRS     = 3'b010;   // 2
Funct3  f3_CSRRC     = 3'b011;   // 3
Funct3  f3_CSRRWI    = 3'b101;   // 5
Funct3  f3_CSRRSI    = 3'b110;   // 6
Funct3  f3_CSRRCI    = 3'b111;   // 7

// =============== END: BASE INTEGER ISA ==============================

// =============== M EXTENSION: MUL/DIV  ==============================

Opcode op_MUL_32 = {5'h0C, tail};

Funct3 f3_MUL     = 3'b000;     // 0
Funct7 f7_MUL     = 7'b0000001; // 1
Funct3 f3_MULH    = 3'b001;     // 1
Funct7 f7_MULH    = 7'b0000001; // 1
Funct3 f3_MULHSU  = 3'b010;     // 2
Funct7 f7_MULHSU  = 7'b0000001; // 1
Funct3 f3_MULHU   = 3'b011;     // 3
Funct7 f7_MULHU   = 7'b0000001; // 1
Funct3 f3_DIV     = 3'b100;     // 4
Funct7 f7_DIV     = 7'b0000001; // 1
Funct3 f3_DIVU    = 3'b101;     // 5
Funct7 f7_DIVU    = 7'b0000001; // 1
Funct3 f3_REM     = 3'b110;     // 6
Funct7 f7_REM     = 7'b0000001; // 1
Funct3 f3_REMU    = 3'b111;     // 7
Funct7 f7_REMU    = 7'b0000001; // 1

// =============== END: M EXTENSION: MUL/DIV ==========================

// =============== A EXTENSION: ATOMIC ================================
// @TODO Not implemented in DEX unit
Opcode op_ATOMIC_32 = {5'h0B, tail};

Funct5 f5_AMOADD  = {3'b0, 2'h0};   // 0, 0
Funct3 f3_AMOADD  = 3'h2;        // 2
Funct5 f5_AMOXOR  = {3'b1, 2'h0};   // 1, 0
Funct3 f3_AMOXOR  = 3'h2;        // 2
Funct5 f5_AMOOR   = {3'h2, 2'h0};   // 2, 0
Funct3 f3_AMOOR   = 3'h2;        // 2
Funct5 f5_AMOAND  = {3'h3, 2'h0};   // 3, 0
Funct3 f3_AMOAND  = 3'h2;        // 2
Funct5 f5_AMOMIN  = {3'h4, 2'h0};   // 4, 0
Funct3 f3_AMOMIN  = 3'h2;        // 2
Funct5 f5_AMOMAX  = {3'h5, 2'h0};   // 5, 0
Funct3 f3_AMOMAX  = 3'h2;        // 2
Funct5 f5_AMOMINU = {3'h6, 2'h0};   // 6, 0
Funct3 f3_AMOMINU = 3'h2;        // 2
Funct5 f5_AMOMAXU = {3'h7, 2'h0};   // 7, 0
Funct3 f3_AMOMAXU = 3'h2;        // 2
Funct5 f5_AMSWAP  = {3'h0, 2'h1};   // 0, 1
Funct3 f3_AMSWAP  = 3'h2;        // 2
Funct5 f5_AMO_LR  = {3'h0, 2'h2};   // 0, 2
Funct3 f3_AMO_LR  = 3'h2;        // 2
Funct5 f5_AMO_SC  = {3'h0, 2'h3};   // 0, 3
Funct3 f3_AMO_SC  = 3'h2;        // 2

// =============== END: A EXTENSION: ATOMIC ===========================

endpackage: ISA_Defs
