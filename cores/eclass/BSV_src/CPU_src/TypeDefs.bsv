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

import TLM2				:: *;
import ISA_Defs		:: *;
import ISA_Defs_PRV 	:: *;
import RegFile 		:: *;

`include "TLM.defines"
`include "RVC.defines"

// ================================================================
// TLM transaction utility types and functions

typedef enum {BYTES1, BYTES2, BYTES3, BYTES4} TLMBurstSize_Bytes deriving (Eq, Bits);

// Function to convert Data and Instruction lengths into bytes format as maintained by TLMRequest.burst_size
// TLMBurstSize #(`TLM_PRM) --> Bit #(TLog #(TDiv #(data_size), 8))
// For data_size = 32, TLMBurstSize --> Bit #(2)
//		       2'b00 => 1 byte burst
//		       2'b01 => 2 byte burst
//		       2'b10 => 3 byte burst
//		       2'b11 => 4 byte burst

function TLMBurstSize #(`TLM_PRM_REQ_CPU) reqSz_bytes_i (UInt#(8) sz);
   UInt #(8) bytes = sz>>3;
   TLMBurstSize_Bytes ret;
   case (bytes) matches
       1: ret = BYTES1;
       2: ret = BYTES2;
       3: ret = BYTES3;
	   4: ret = BYTES4;
	   default: ret = BYTES1; //@TODO
   endcase
   return pack(ret);
endfunction

// Function to convert request size forwarded by CPU into Memory acceptable format
// For CPU: 'b00 -> 1 byte; For Mem: 'b01 -> 1 byte;
// This is required as TLMBurstSize follows this format

function Bit #(8) reqSz_bytes_mem (TLMBurstSize #(`TLM_PRM_REQ_CPU) sz);
   Bit #(8) n_b = extend(sz);
   return n_b + 1;
endfunction

// ================================================================
/*Typedefs for implementing CSRs*/

// Exception Declaration
// The following exceptions are defined as per Machine cause register (mcause) values
// These exceptions are defined only when not caused by an interrupt
typedef enum {
				/*Instruction Address Misaligned*/ 	MCAUSE_INSTRN_ADDR_MISALN	= 0,
				/*Instruction Access Fault*/ 	     	MCAUSE_INSTRN_ACCESS_FAULT	= 1,
				/*Illegal Instruction*/ 		  		MCAUSE_ILLEGAL_INSTRN		= 2,
				/*Breakpoint*/ 					   	MCAUSE_BREAKPOINT				= 3,
				/*Load Address Misaligned*/ 	   	MCAUSE_LOAD_ADDR_MISALN		= 4,
				/*Load Access Fault*/ 			   	MCAUSE_LOAD_ACCESS_FAULT	= 5,
				/*Store/AMO Address Misaligned*/   	MCAUSE_STORE_ADDR_MISALN	= 6,
				/*Store/AMO Access Fault*/  	   	MCAUSE_STORE_ACCESS_FAULT	= 7,
				/*Environment Call From U-Mode*/   	MCAUSE_ENV_CALL_U				= 8,
				/*Environment Call From S-Mode*/   	MCAUSE_ENV_CALL_S				= 9,
				/*Environment Call From H-Mode*/   	MCAUSE_ENV_CALL_H				= 10,
				/*Environment Call From M-Mode*/   	MCAUSE_ENV_CALL_M				= 11,
				/*Illegal Exception*/					MCAUSE_ILLEGAL_EXCEPTION	= 15	// To get a 4-bit implementation
} MCause_Exception deriving (Bits, Eq);

// These exceptions are defined only when caused by an interrupt
// Software interrupt and Timer interrupt are the pre-defined exceptions caused due to interrupts
typedef enum {
				/*Software Interrupt*/	MCAUSE_U_SOFT_INT	 = 0,
				/*Software Interrupt*/	MCAUSE_S_SOFT_INT	 = 1,
				/*Software Interrupt*/	MCAUSE_H_SOFT_INT	 = 2,
				/*Software Interrupt*/	MCAUSE_M_SOFT_INT	 = 3,
				/*Timer Interrupt*/ 		MCAUSE_U_TIMER_INT = 4,
				/*Timer Interrupt*/ 		MCAUSE_S_TIMER_INT = 5,
				/*Timer Interrupt*/ 		MCAUSE_H_TIMER_INT = 6,
				/*Timer Interrupt*/ 		MCAUSE_M_TIMER_INT = 7,
				/*Host Interrupt*/		MCAUSE_U_EXT_INT   = 8,
				/*Host Interrupt*/		MCAUSE_S_EXT_INT   = 9,
				/*Host Interrupt*/		MCAUSE_H_EXT_INT   = 10,
				/*Host Interrupt*/		MCAUSE_M_EXT_INT   = 11,
				/*Illegal Interrupt*/	MCAUSE_ILLEGAL_INT = 15	// To get a 4-bit implementation
} MCause_Interrupt deriving (Bits, Eq);

typedef union tagged {
	MCause_Exception Exception;
	MCause_Interrupt Interrupt;
} MCause_Trap deriving (Bits, Eq);

// CSR Data Types
typedef union tagged {
	Data Value;
	Addr Redirect;
} SysInstResult deriving (Eq, Bits);

typedef struct{
	Opcode	opcode;
	Funct3	funct3;
	Data		data;
	Data		zimm;
	RegName  rs1;
	RegName	rd;
	CSR_Addr	csr_addr;
} CSRData deriving (Bits);

typedef union tagged {
	TrapData TrapD;
	SysInstResult SysD;
} CSRReturn deriving (Bits);

typedef struct {
    Bit #(2) prv;
	 //Bit #(3) frm;
    Bool f_enabled;
    Bool x_enabled;
} CSRState deriving (Bits, Eq);

typedef struct {
    Bit#(2) prv;
    //Asid    asid;
    Bit#(5) vm;
    Bool    mxr;
    Bool    pum;
    Addr    base;
    Addr    bound;
} VMInfo deriving (Bits, Eq, FShow);

// Virtual Memory Types
Bit#(5) vmMbare = 0;
Bit#(5) vmMbb   = 1;
Bit#(5) vmMbbid = 2;
Bit#(5) vmSv32  = 8;
Bit#(5) vmSv39  = 9;
Bit#(5) vmSv48  = 10;
Bit#(5) vmSv57  = 11;
Bit#(5) vmSv64  = 12;

/* CPU Misc. types */
// Reasons why the CPU is currently stopped
typedef enum {
	CPU_STOP_BREAK,
	CPU_STOP_EXIT,                // Currently done via a SYSCALL
	CPU_STOP_INSTR_ERR,           // Illegal opcodes
	CPU_STOP_INSTR_UNIMPLEM,      // Legal opcode, but not implemented
	CPU_STOP_MEM_ERR              // Mem access error
} CPU_Stop_Reason deriving (Eq, Bits);

// Current state of the processor. Fetches next instruction in Fetch mode. In Stopped mode, waits for a debug method to request further operation
typedef enum {FETCH, STOPPED} CPU_State deriving (Eq, Bits);

// Mode of operation of CPU. In DBG mode, the execution halts after every instruction (until run_continue is called by the debugger) and
// the control passes to the testbench which requests another operation through the debug methods
// In normal mode, all instructions until the end of instruction stream are executed. Thereafter, CPU waits for new instructions to arrive
typedef enum {NORMAL, DEBUG} CPU_Mode deriving (Eq, Bits);

typedef enum {REQ, RESP} FetchState deriving(Bits, Eq);

typedef enum {ALU, MEM, BRANCH, SYS, ILLEGAL} InstType deriving (Bits, Eq);

// Commit Data Types
/*Write-Back*/
typedef struct {
	Data data;
	RegName rd;
} RequestWriteBack deriving (Bits);

/*Load-Store*/
typedef enum {SIGNED, UNSIGNED} LS_Remark deriving (Bits, Eq);
typedef struct {
	TLMRequest #(`TLM_PRM) tlmreq;
	RegName rd;
	LS_Remark remark;
} RequestLoadStore #(`TLM_PRM_DCL) deriving (Bits);
typedef RequestLoadStore #(`TLM_PRM_REQ_CPU) ReqLS;

/*Branch*/
typedef struct {
	Maybe #(Data)	branchAddr;
	RegName 			rd;
	Maybe #(Data)	lr;
} RequestJump deriving (Bits);

typedef CSRData RequestSystem;

/*Commit Data Packet*/
typedef union tagged {
	RequestLoadStore #(`TLM_PRM) LS;
	RequestWriteBack WB;
	RequestJump JMP;
	RequestSystem SYS;
	Bool Halt; // Used only while functional verification
} CommitType #(`TLM_PRM_DCL) deriving (Bits);
typedef struct {
	Addr pc;
	Maybe #(CommitType #(`TLM_PRM_REQ_CPU)) commitType;
} CommitData deriving (Bits);

// Pipeline Data Types
// --------
/*Details about encountered trap*/
typedef struct{
	Maybe #(Addr) bad_addr;
	MCause_Trap trap;
} TrapData deriving (Bits);

/*Data passed between stages. Must have a PC field*/
typedef union tagged {
	a_t StageD;
	TrapData TrapD;
} StageData #(type a_t) deriving(Bits);
typedef struct{
	Addr pc;
	StageData #(a_t) data;
} PipeData #(type a_t) deriving (Bits);

/*Pipe between IF and EX stage*/
typedef struct{
	Data inst;
} ExecuteStageData deriving (Bits);

/*Pipe between EX and WB stage*/
typedef CommitType #(`TLM_PRM_REQ_CPU) WBStageData;

typedef struct{
	RegName rd;
	Maybe#(Data) data;
	Maybe#(Addr) next_pc;
	Bool retire;
	Bool breakpoint;
	Bool halt;
} WBResponse deriving (Bits);

typedef PipeData #(ExecuteStageData) IFEX_Data;
typedef PipeData #(WBStageData) EXWB_Data;

/*Input to Execute unit*/
typedef struct{
	InstFields	instFields;
	Data			v1;
	Data			v2;
	Addr			instAddr;
	Bool			illegalInst;	// indicates '0000006F' (self-loop) instruction. Useful to detect end of instruction stream while functional verification
} ExecuteData deriving(Bits);

typedef struct {
	RegName rd;
	Data data;
} Forward_Data deriving (Bits); // Used for operand forwarding

UInt #(8) sz_byte = 8, sz_hword = 16, sz_word = 32, sz_dword = 64, sz_qword = 128;

typedef enum {ADD, SLT, SLTU, XOR, OR, AND, SLL, SRL, SRA, SUB, MUL, MULSU, MULU, DIV, DIVU, REM, REMU} ALUFunct deriving (Bits, Eq);

typedef struct {
	ALUFunct aluFunct;
	Bool halfWord;
	Data op1;
	Data op2;
} ALUInfo deriving (Bits);

// ================================================================
// Instruction Cache primitives

typedef enum {Load, Store, Atomic} Access_type deriving (Bits, Eq, FShow);

typedef struct{
  Bit#(addr_width) address;
}From_Cpu#(numeric type addr_width) deriving(Bits,Eq);

typedef struct{
  Bit#(TMul#(word_size,8)) data_word;
  Bit#(1) bus_error;
  Bit#(1) misaligned_error;
  Bit#(addr_width) address;
}To_Cpu#(numeric type addr_width,numeric type word_size) deriving(Bits,Eq);

typedef struct{
  Bit#(addr_width) address;
  Bit#(5) burst_length; 
  Access_type ld_st;
}To_Memory#(numeric type addr_width) deriving(Bits,Eq);

typedef struct{
  Bit#(TMul#(8,word_size)) data_line;
  Bit#(1) bus_error;
  Bit#(addr_width) address;
}From_Memory#(numeric type addr_width,numeric type word_size) deriving(Bits,Eq);
	
// ================================================================
// Data Cache primitives

typedef struct{
  Bit#(addr_width) address;
  Access_type load_store;
  Bit#(TMul#(word_size,8)) data;
  Bit#(2) transfer_size; // Not currently used
}From_Cpu_D#(numeric type addr_width, numeric type word_size) deriving(Bits,Eq);

typedef struct{
  Bit#(TMul#(word_size,8)) data_word;
  Bit#(1) bus_error;
  Bit#(1) misaligned_error;
  Bit#(addr_width) address;
  Access_type load_store;
}To_Cpu_D#(numeric type addr_width,numeric type word_size) deriving(Bits,Eq);

typedef struct{
  Bit#(addr_width) address;
  Bit#(5) burst_length; 
  Bit#(TMul#(block_size,TMul#(8,word_size))) data_line;
  Access_type ld_st;
  Bit#(2) transfer_size; // 0 -8 bits, 1- 16 bits, 2 -32 bits;
} To_Memory_D#(numeric type addr_width, numeric type word_size, numeric type block_size) deriving(Bits,Eq);

typedef struct{
  Bit#(TMul#(8,word_size)) data_line;
  Bit#(1) bus_error;
  Bit#(1) misaligned_error;
  Bit#(addr_width) address;
}From_Memory_D#(numeric type addr_width,numeric type word_size, numeric type block_size) deriving(Bits,Eq);

