/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

Author Names : Rahul Bodduna, N Sireesh.
Email ID : rahul.bodduna@gmail.com
*/


package riscv_types;

`include "defined_parameters.bsv"
import Vector::*;
import DefaultValue:: *;


typedef enum {
   Taken, Not_taken
   } Actual_jump deriving (Eq,Bits,FShow);

// enum defining the prediction of the branch predictor for the current PC. 
typedef enum{
   Predict_taken,Predict_not_taken
   } Prediction deriving (Eq,Bits,FShow);

//typedef enum {
//   NOP,USER_INT,SPFPU,DPFPU,AMO,SIMD,SYSTEM
//   } Instruction_type deriving(Bits, Eq, FShow);

`ifdef CONFIG_RV64 typedef 64 XLEN;
`endif
`ifdef CONFIG_RV32
typedef 32 XLEN;
`endif

typedef XLEN DataSz;
typedef Bit#(DataSz) Data;

typedef enum {
   NOP, INT, FLOAT, SIMD
   } Regfile_type deriving(Bits, Eq, FShow);

typedef enum {
   ALU, MUL, DIV
   } ALU_type deriving(Bits, Eq, FShow);

typedef enum {
   ADD,SUB,SLL,SLT,SLTU,XOR, 		
   SRL,SRA,OR,AND,MUL,MULH,
   MULHSU,MULHU,DIV,DIVU,REM,REMU,LUI,AUIPC,
	 NOP
   } ALU_func deriving(Eq,Bits,FShow);

typedef struct {
	ALU_type		alu_type;
	ALU_func		alu_func;
	} Arithmetic_op deriving(Eq, Bits, FShow);
	
typedef enum { Div, Mul} MulDivOp deriving(Bits, Eq);

typedef enum {
   COND, UNCOND
   } Branch_type deriving(Bits,Eq, FShow);

typedef enum {
   BEQ, BNE, BLT, BGE, BLTU, BGEU, JAL, JALR
   } Branch_func deriving(Bits, Eq, FShow);

typedef struct {
	 Branch_type branch_type;
	 Branch_func branch_func;
	 } Branch_op deriving(Bits, Eq, FShow);

typedef enum {
  NOP, LD, STR
  } Mem_type deriving(Bits, Eq, FShow);

typedef Bit#(3) Mem_size;

typedef struct {
	Mem_type mem_type;
	Mem_size mem_size;
	} Mem_op deriving (Bits, Eq, FShow);	 

typedef enum {
    ECall,
    EBreak,
    URet,
    SRet,
    HRet,
    MRet,
    WFI,
    CSRRW,
    CSRRS,
    CSRRC,
    CSRR, // read-only CSR operation
    CSRW // write-only CSR operation
} SystemInst deriving (Bits, Eq, FShow);

typedef union tagged { 
	 Arithmetic_op			ALU;
	 Branch_op				Branch;
	 Mem_op					Memory;
	 SystemInst				System;
	 } Inst_op deriving(Bits, Eq, FShow);

function Bool isLD(Inst_op x);
	Bool y = False;
	if(x matches tagged Memory .memx)
		if(memx.mem_type == LD)
			y= True;
	return y;
endfunction

function Bool isSTR(Inst_op x);
	Bool y = False;
	if(x matches tagged Memory .memx)
		if(memx.mem_type == STR)
			y= True;
	return y;
endfunction

function Bool isArithmetic(Inst_op x);
	Bool y = False;
	if(x matches tagged ALU .arith)
			y= True;
	return y;
endfunction

function Bool isALU(Inst_op x);
	Bool y = False;
	if(x matches tagged ALU .arith &&& arith.alu_type==ALU)
			y= True;
	return y;
endfunction

function Bool isMemory(Inst_op x);
	Bool y = False;
	if(x matches tagged Memory .memx)
			y= True;
	return y;
endfunction

function Bool isBranch(Inst_op x);
	Bool y = False;
	if(x matches tagged Branch .brnch)
			y= True;
	return y;
endfunction

function Bool isCond(Inst_op x);
	Bool y = False;
	if(x matches tagged Branch .brnch)
		if(brnch.branch_type == COND)
			y= True;
	return y;
endfunction

function Bool isSystem(Inst_op x);
	Bool y = False;
	if(x matches tagged System .sys)
			y= True;
	return y;
endfunction

function Bool isNoTrap(TrapCause x);
	Bool y = False;
	if(x matches tagged No_trap)
			y= True;
	return y;
endfunction

function Bool isWireNoTrap(Wire#(TrapCause) x);
	Bool y = False;
	if(x matches tagged No_trap)
			y= True;
	return y;
endfunction

typedef enum {Load,Store} Access_type_d deriving(Bits,Eq,FShow);
/* JAL: effective PC calculated in Decode stage and stored
   in squash_pc field of IQ
   JALR: offset stored in the imm_buf
   Conditional branches: current_pc stored in imm_buf and 
   squash pc stored in squash_pc field of IQ */

typedef struct {
	Actual_jump actual_taken_or_not;
	Bool is_matching_prediction;
	Bit#(TLog#(`PRF_SIZE)) dest_addr;
	Bit#(`REG_WIDTH) effective_addr;
   } Branch_unit_output  deriving(Bits, Eq, FShow);


typedef struct {
  Bit#(`REG_WIDTH) pc; 
  Prediction predict_taken_or_not;
  } Bpu_packet deriving (Bits,Eq);

typedef enum {
    InstAddrMisaligned  = 4'd0,
    InstAccessFault     = 4'd1,
    IllegalInst         = 4'd2,
    Breakpoint          = 4'd3,
    LoadAddrMisaligned  = 4'd4,
    LoadAccessFault     = 4'd5,
    StoreAddrMisaligned = 4'd6,
    StoreAccessFault    = 4'd7,
    EnvCallU            = 4'd8,
    EnvCallS            = 4'd9,
    EnvCallH            = 4'd10,
    EnvCallM            = 4'd11,
    IllegalException    = 4'd15 // to get a 4-bit implementation
} ExceptionCause deriving (Bits, Eq, FShow);

typedef enum {
	USoftwareInterrupt  = 4'd0,
    SSoftwareInterrupt  = 4'd1,
    HSoftwareInterrupt  = 4'd2,
    MSoftwareInterrupt  = 4'd3,
    UTimerInterrupt     = 4'd4,
    STimerInterrupt     = 4'd5,
    HTimerInterrupt     = 4'd6,
    MTimerInterrupt     = 4'd7,
    UExternalInterrupt  = 4'd8,
    SExternalInterrupt  = 4'd9,
    HExternalInterrupt  = 4'd10,
    MExternalInterrupt  = 4'd11,
    IllegalInterrupt    = 4'd15 // to get 4-bit implementation
} InterruptCause deriving (Bits, Eq, FShow);

// Traps are either an exception or an interrupt
typedef union tagged {
	ExceptionCause Exception;
	InterruptCause Interrupt;
	void		   No_trap;
} TrapCause deriving (Bits, Eq, FShow);

function Data toCauseCSR(TrapCause x);
    case (x) matches
        tagged Exception .cause:
            return {0, pack(cause)};
        tagged Interrupt .cause:
            return {1'b1, 0, pack(cause)};
        default:
            return 0;
    endcase
endfunction

typedef Bit#(TLog#(`PRF_SIZE)) RAT_entry;

typedef struct {
   Bit#(TLog#(`PRF_SIZE)) free_reg;				//free register
   Bool valid;									//is the entry valid (to distinguish between frq empty and full)
} FRQ_entry deriving(Bits, Eq, FShow);

typedef struct {
   Bit#(TLog#(`ENTRY_ROB_SIZE)) free_reg_cp;
   Vector#(`ENTRY_ROB_SIZE, Bool) frq_mask;
   } FRQ_checkpoint deriving(Bits, Eq, FShow);

typedef struct {
   Bool valid;
   Bit#(`REG_WIDTH) imm;
   } Imm_buf_entry deriving(Bits, Eq, FShow);

instance DefaultValue#(Imm_buf_entry);
	defaultValue = Imm_buf_entry {
					 valid: False,
					 imm: 0
					 };
endinstance

typedef struct {
   Bit#(`REG_WIDTH) program_counter;
   Bit#(`INSTR_WIDTH) instruction;
   Prediction prediction;
   TrapCause exception;
   } Fetched_instruction deriving(Bits,Eq);

typedef struct {
   Bool valid;
   Fetched_instruction fetched_instruction;
   } Fetched_instruction_2 deriving(Bits,Eq, FShow);

typedef struct {
Fetched_instruction 	fi_1;
Fetched_instruction_2 	fi_2;
} F_to_D deriving(Bits, Eq);

typedef struct {
   Decoded_info_type instruction_decoded;       
   Prediction prediction;
   Bit#(`REG_WIDTH) program_counter;
   } Decoded_instruction deriving (Bits,Eq);


typedef struct {
   Vector#(`FETCH_WIDTH, Bool) valid;
   Vector#(`FETCH_WIDTH, Decoded_instruction) decode_packet;
   } Decode_packet deriving (Bits,Eq);

typedef struct {
   
   //Instruction_type								inst_type;

   Inst_op												inst_op;

   Bool														word_flag;
   
   Bool														imm_valid;    //if the instr has immediate operand

   Bit#(TLog#(`REGFILE_SIZE))			rs1;          //source operand 1
   
   Bool														rs1_valid;

   Bit#(TLog#(`REGFILE_SIZE))			rs2;          //source operand 2. If present, immediate operand
                                                //is stored here.
                                                //holds the register to be stored into memory
   Bool rs2_valid;

   Bit#(TLog#(`REGFILE_SIZE)) rd;               //destination register
   
   Bool rd_valid;
   
   Bit#(`REG_WIDTH) imm;                        //holds immediate value

   TrapCause exception;
   
   } Decoded_info_type deriving(Bits, Eq);

instance DefaultValue#(Decoded_info_type);

	defaultValue = Decoded_info_type {	
	  					word_flag	:		False,
						inst_op		:		tagged ALU Arithmetic_op{ alu_type : ALU, alu_func : NOP},
	  					imm_valid	:		False,
	  					rs1				:		0,
	  					rs1_valid	:		True,
	  					rs2				:		0,
	  					rs2_valid	:		True,
	  					rd				:		0,
	  					rd_valid	:		True,
	  					imm				:		0,
						exception	:		No_trap
	 					};
endinstance

/* Holds the Entry ROB information  */
typedef struct {
   Bool valid;
   
   //Instruction_type inst_type;
	 Inst_op	inst_op;
   
   //Mem_type mem_type;
   //
   //Mem_size mem_size;
   
   Bit#(TLog#(`MEMQ_SIZE)) mem_q_index;         //index of load or store queue
   
   //ALU_type alu_type;
   //
   //ALU_op alu_op;
   
   Bool word_flag;

   //Branch_type branch_type;
   //
   //Branch_op branch_op;

   //MACHINE_op csr_inst_type;
   
   Bool imm_valid;
   
   //Bool csr_valid;

   Bit#(TLog#(`PRF_SIZE)) op_1;                 //holds operand 1 in ALU instructions and
				                                //base register in the case of LS instructions
   
   Bit#(TLog#(`PRF_SIZE)) op_2;                 //holds immediate reg index in the case of ALU
				                                //and load instrs. Source reg in store instrs.

   Bit#(TLog#(`IMM_BUF_SIZE)) imm_index;
   
   Bit#(TLog#(`PRF_SIZE)) dest_op;              //immediate reg index in the case of store 
				                                //instructions is stored here
   Bit#(TLog#(`REGFILE_SIZE)) dest_arch;		//architectural dest. reg. Used during commit to update RRAM
   
   Bit#(`REG_WIDTH)  program_counter;			//To hold the program counter

   Prediction prediction;					    //Tells if the branch is taken or not

   } Entry_rob_type deriving(Bits,Eq);


instance DefaultValue#(Entry_rob_type);

	defaultValue = Entry_rob_type {
					  valid						: False,
					  inst_op 					: tagged ALU Arithmetic_op{ alu_type : ALU, alu_func : NOP},
					  word_flag                 : False,
					  imm_valid                 : False,
					  op_1						: 0,
					  op_2						: 0,
					  imm_index                 : 0,
					  dest_op					: 0,
					  dest_arch					: 0,
					  program_counter			: 0,
					  prediction				: Predict_not_taken
		 };
endinstance

/* Holds the inputs to be read from PRF */
typedef struct {
   ALU_func alu_op;
   Bool word_flag;
   ALU_type alu_type;
   Bool imm_valid;
   Bit#(TLog#(`PRF_SIZE)) op_1;
   Bit#(TLog#(`PRF_SIZE)) op_2;
   Bit#(TLog#(`IMM_BUF_SIZE)) imm_index;
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   Bit#(`REG_WIDTH) pc;
//   Bit#(TLog#(`TOTAL_THREADS)) thread_id;
   } ALU_data_read deriving(Bits,Eq, FShow);

typedef struct {                                    //TODO to be included in
   ALU_func alu_op;
   Bool word_flag;
   ALU_type alu_type;
   Bit#(TLog#(`PRF_SIZE)) op_1;
   Bit#(TLog#(`PRF_SIZE)) op_2;
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   } MUL_DIV_data_read deriving(Bits,Eq, FShow);


/* Holds the inputs to ALU */
typedef struct {
   ALU_func alu_op;
   Bool word_flag;
   ALU_type alu_type;
   Bit#(`REG_WIDTH) src_1;
   Bit#(`REG_WIDTH) src_2;
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   Bit#(`REG_WIDTH) pc;
   } ALU_payload_type deriving(Bits,Eq, FShow);

typedef struct {                                     //TODO to be included in 
   ALU_func alu_op;
   Bool word_flag;
   ALU_type alu_type;
   Bit#(`REG_WIDTH) src_1;
   Bit#(`REG_WIDTH) src_2;
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   } MUL_DIV_payload_type deriving(Bits,Eq, FShow);

/* Holds the inupts to be read from PRF */
typedef struct {
   Bit#(TLog#(`PRF_SIZE)) base;
   Bit#(TLog#(`IMM_BUF_SIZE)) offset;
   Bit#(TLog#(`PRF_SIZE)) op_2;          //Value to be stored
   Bit#(TLog#(`MEMQ_SIZE)) mem_q_index;
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   Mem_type mem_type;
   Mem_size mem_size;
   } LS_data_read deriving(Bits,Eq, FShow);
   
/* Holds the inputs to LS unit */
typedef struct {
   Bit#(`REG_WIDTH) base;
   Bit#(`REG_WIDTH) offset;
   Bit#(TLog#(`MEMQ_SIZE)) mem_q_index;
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   Maybe#(Bit#(`REG_WIDTH)) str_data;
   Mem_size mem_size;
   } LS_payload_type deriving(Bits,Eq, FShow);


typedef struct {
   Branch_func branch_op;
   Bit#(TLog#(`PRF_SIZE)) op_1;
   Bit#(TLog#(`PRF_SIZE)) op_2;
   Bit#(TLog#(`IMM_BUF_SIZE)) imm_index;
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   Bit#(`REG_WIDTH) program_counter;
   Prediction prediction;
   } Branch_data_read deriving(Bits,Eq, FShow);

typedef struct {
   Branch_func branch_op;
   Bit#(`REG_WIDTH) src_1;
   Bit#(`REG_WIDTH) src_2;
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   Bit#(`REG_WIDTH) imm;
   Bit#(`REG_WIDTH) program_counter;
   Prediction prediction;
   } Branch_payload_type deriving(Bits,Eq, FShow);

typedef struct {
   Bool imm_valid;
   Bit#(TLog#(`IMM_BUF_SIZE)) imm_index;   
   Bit#(TLog#(`PRF_SIZE)) op_1;
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   SystemInst			  system_inst;
   } Interrupt_data_read deriving(Bits, Eq, FShow);
   
typedef struct {
   Bit#(`REG_WIDTH) src_1;
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   Bit#(12) csr_address;
   SystemInst system_inst;
   } Interrupt_payload_type deriving(Bits, Eq, FShow); 
   
typedef union tagged {
   Interrupt_data_read System_inst;
   MUL_DIV_data_read   Mul_div_inst;
   } MUL_DIV_System_data_read deriving(Bits, Eq, FShow);

typedef union tagged {
   Interrupt_payload_type System_inst;
   MUL_DIV_payload_type   Mul_div_inst;
   } MUL_DIV_System_payload deriving(Bits, Eq, FShow);

typedef struct {
   Bit#(`ADDRESS_WIDTH) address;
   Mem_size str_size;
   Bit#(`REG_WIDTH) data;
   } Store_request deriving(Bits,Eq, FShow);

typedef struct {
   Bit#(`ADDRESS_WIDTH) address;
   Mem_size ld_size;
   Access_type_d ld_st;
   Bit#(`REG_WIDTH) data;
   Bit#(TLog#(`PRF_SIZE)) dest_reg;
   } Load_request deriving(Bits,Eq, FShow);

/* Holds the broadcast information */
typedef struct {
   Bool valid;
   Bit#(TLog#(`PRF_SIZE)) dest_tag;
   } Broadcast_type deriving(Bits,Eq, FShow);

typedef struct {
   Bool valid;
   Bit#(TLog#(`PRF_SIZE)) dest_tag;
   Bit#(`REG_WIDTH) result;
   } Load_Broadcast_type deriving(Bits,Eq, FShow);

typedef struct {
   Bit#(`REG_WIDTH) pc;
   Bit#(`REG_WIDTH) jump_pc;
   Actual_jump taken_or_not;
   } Training_packet deriving(Bits,Eq);

typedef struct {
   Actual_jump actual_taken_or_not; 
   Bit#(1) is_matching_prediction;
   Bit#(TLog#(`PRF_SIZE)) dest_addr;
   } Branch_op_packet deriving(Bits,Eq,FShow);


/* Structures of store and load queues */
typedef struct {
   Bool filled;									//tells if the entry is filled
   Bool valid;									//tells if the addr and data are valid
   Bit#(`ADDRESS_WIDTH) str_addr;
   Bit#(`REG_WIDTH) str_data;
   Mem_size str_size;
   } StoreQ_type deriving(Bits,Eq, FShow);

typedef struct {
   Bool filled;                                 //is the entry filled
   Bool valid;
   Vector#(`MEMQ_SIZE, Bool) store_mask;        //stores indices on which the load depends
   Bit#(`ADDRESS_WIDTH) ld_addr;
   Mem_size ld_size;
   } LoadQ_type deriving(Bits,Eq, FShow);

typedef struct {
   Vector#(`MEMQ_SIZE, Bool) store_mask;
   Bit#(`ADDRESS_WIDTH) eff_addr;
   Mem_size ld_size;
   Bit#(TLog#(`PRF_SIZE)) dest_reg;
   Bit#(TLog#(`MEMQ_SIZE)) load_q_index;
   } Load_FIFO deriving(Bits,Eq, FShow);

typedef struct {
	Bit#(`REG_WIDTH) reg_value;
	Bool 			  if_valid;
   } CSR_reg deriving(Bits,Eq, FShow);

typedef struct {
	Bool			if_load_q_full;
	Bool            if_store_q_full;
  } LS_status deriving(Bits, Eq);

typedef struct {
	Vector#(`PRF_SIZE, Prf_info)    prf_entries;
	Bit#(TLog#(`IMM_BUF_SIZE))  imm_buf_tail;
   } IQ_to_map deriving(Bits, Eq);	

typedef struct {
	FRQ_entry					entry_1;
	FRQ_entry					entry_2;
   } FRQ_to_map deriving(Bits, Eq);

typedef struct {                       
	Bool				_match;
	Bit#(`MAX_LATENCY)	_shift;	
	Bit#(`MAX_LATENCY)	_delay;	
   } Prf_info deriving(Bits, Eq);

typedef struct {	
	Bit#(TLog#(`PRF_SIZE)) prf_slot;
	Bit#(`MAX_LATENCY)     prf_delay;
   } Update_map_prf deriving(Bits, Eq);

typedef struct {
	Bit#(TLog#(`PRF_SIZE)) dest_tag;
	Bit#(`REG_WIDTH)	   _result;
   } Result_bypass_type deriving(Bits, Eq);

instance DefaultValue#(Prf_info);
	defaultValue = Prf_info {
					_match : False,
					_shift : 6'b111111,
					_delay : 6'b111111};
endinstance


instance DefaultValue#(FRQ_to_map);
	defaultValue = FRQ_to_map {
					 entry_1: FRQ_entry { free_reg : 'b0, valid : False},
					 entry_2: FRQ_entry { free_reg : 'b0, valid : False}
					 };
endinstance

/* Exception */
typedef union tagged { 	
   void No_exception;							// indicates that ther was no exception generated
   Bool Invalid;								// indicates that the operation is invalid
   Bool Divide_by_Zero;							// indicates that the division operation is a divide by zero.
   Bool Overflow;								// indicates an overflow
   Bool Underflow;								// indicates an underflow
   Bool Inexact;								// indicates that the produced result is inexact
   } Exception deriving(Bits, Eq, FShow);



Fmt nl = $format("\n");
instance FShow #(Decoded_info_type);
   function Fmt fshow (Decoded_info_type pckt);
		        return (fshow (" \n {")
						+ nl + $format("word_flag 	: ") + fshow (pckt.word_flag)	
                        + nl + $format("inst_op 	: ") + fshow (pckt.inst_op)		
                        + nl + $format("imm_valid 	: ") + fshow (pckt.imm_valid)	
                        + nl + $format("rs1 		: ") + $format ("%d",pckt.rs1)			
                        + nl + $format("rs1_valid 	: ") + fshow (pckt.rs1_valid)	
                        + nl + $format("rs2 		: ") + $format ("%d",pckt.rs2)	
                        + nl + $format("rs2_valid 	: ") + fshow (pckt.rs2_valid)	
                        + nl + $format("rd 			: ") + $format ("%d",pckt.rd)			
                        + nl + $format("rd_valid 	: ") + fshow (pckt.rd_valid)	
                        + nl + $format("imm 		: %h",pckt.imm)
				        + nl + $format("exception 	: ") + fshow (pckt.exception)	
									+
						fshow ("}"));
	endfunction
endinstance

instance FShow #(Decoded_instruction);
   function Fmt fshow (Decoded_instruction dinstr);
		        return (fshow ("\nDecoded_instruction{")
					+ nl + fshow(dinstr.instruction_decoded)
					+ nl + $format("prediction 	: ") + fshow(dinstr.prediction)
					+ nl + $format("pc 			: ") + fshow(dinstr.program_counter)
					+ fshow("}"));
	endfunction
endinstance

instance FShow #(Decode_packet);
   function Fmt fshow (Decode_packet dpckt);
		        return (fshow ("\nDecoded_packet {")
					+ nl + fshow(dpckt.valid)
					+ nl + fshow(dpckt.decode_packet)
					+ fshow("}"));
	endfunction
endinstance

instance FShow #(Entry_rob_type);
   function Fmt fshow (Entry_rob_type entry);
		        return (fshow ("\n ROB{")
						+ nl + $format("valid 			: ") + fshow (entry.valid)	
                        + nl + $format("inst_op 		: ") + fshow (entry.inst_op)		
						+ nl + $format("word_flag 		: ") + fshow (entry.word_flag)	
                        + nl + $format("imm_valid 		: ") + fshow (entry.imm_valid)	
                        + nl + $format("op_1 			: ") + fshow (entry.op_1)			
                        + nl + $format("op_2 			: ") + fshow (entry.op_2)	
                        + nl + $format("imm_index 		: ") + fshow (entry.imm_index)			
                        + nl + $format("dest_op 		: ") + fshow (entry.dest_op)			
                        + nl + $format("dest_arch 		: ") + fshow (entry.dest_arch)			
				        + nl + $format("program_counter : %h", entry.program_counter)
				        + nl + $format("prediction 		: ") + fshow (entry.prediction)	
									+
						fshow ("}\n"));
	endfunction
endinstance

instance FShow #(Fetched_instruction);
   function Fmt fshow (Fetched_instruction instr);
				return (fshow("\nFetched_instruction {")
				+ nl + $format("PC 			: ") + fshow(instr.program_counter)
				+ nl + $format("instruction : ") + fshow(instr.instruction)    
				+ nl + $format("prediction 	: ") + fshow(instr.prediction)	  
				+ nl + $format("exception 	: ") + fshow(instr.exception) 
					  		+
				fshow("}\n"));
	endfunction
endinstance

instance FShow #(Training_packet);
   function Fmt fshow (Training_packet pckt);
				return (fshow("\n Training packet {")
				+ nl + $format("PC 			: ") + fshow(pckt.pc)
				+ nl + $format("Target PC   : ") + fshow(pckt.jump_pc)    
				+ nl + $format("prediction 	: ") + fshow(pckt.taken_or_not)	  
					  		+
				fshow("}\n"));
	endfunction
endinstance

endpackage 
