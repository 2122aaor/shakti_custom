package riscv_types;

`include "defined_parameters.bsv"
//import ls_types::*;
import Vector::*;
import DefaultValue:: *;


typedef enum {
   Taken, Not_taken
   } Actual_jump deriving (Eq,Bits,FShow);

// enum defining the prediction of the branch predictor for the current PC. 
typedef enum{
   Predict_taken,Predict_not_taken
   } Prediction deriving (Eq,Bits,FShow);

typedef enum {
   NOP,USER_INT,SPFPU,DPFPU,AMO,SIMD,SUPERVISOR
   } Instruction_type deriving(Bits, Eq, FShow);

typedef enum {
   NOP, INT, FLOAT, SIMD
   } Regfile_type deriving(Bits, Eq, FShow);

typedef enum {
   NOP, ALU, MUL, DIV
   } ALU_type deriving(Bits, Eq, FShow);

typedef enum {
   NOP, COND, UNCOND
   } Branch_type deriving(Bits,Eq, FShow);

typedef enum {Load,Store} Access_type_d deriving(Bits,Eq,FShow);
/* JAL: effective PC calculated in Decode stage and stored
   in squash_pc field of IQ
   JALR: offset stored in the imm_buf
   Conditional branches: current_pc stored in imm_buf and 
   squash pc stored in squash_pc field of IQ */

typedef enum {
   BEQ, BNE, BLT, BGE, BLTU, BGEU, JAL, JALR
   } Branch_op deriving(Bits, Eq, FShow);

typedef enum {
   NOP,LUI,AUIPC
   } Operation deriving(Eq,Bits,FShow);


typedef enum {
   ADD,SUB,SLL,SLT,SLTU,XOR, 		
   SRL,SRA,OR,AND,MUL,MULH,
   MULHSU,MULHU,DIV,DIVU,LUI,AUIPC		
   } ALU_op deriving(Eq,Bits,FShow);

typedef enum {
	NOP, CSRRW, CSRRS, CSRRC, 
	CSRRWI, CSRRSI, CSRRCI
   } MACHINE_op deriving(Eq,Bits,FShow);

typedef struct {
	Actual_jump actual_taken_or_not;
	Bool is_matching_prediction;
	Bit#(TLog#(`PRF_SIZE)) dest_addr;
	Bit#(`REG_WIDTH) effective_addr;
//	Bit#(TLog#(`TOTAL_THREADS)) thread_id;
   } Branch_unit_output  deriving(Bits, Eq, FShow);


typedef struct {
   Bit#(`REG_WIDTH) pc; 
   Prediction predict_taken_or_not;
   } Bpu_packet deriving (Bits,Eq);

typedef enum {
   NOP,LD,STR
   } Mem_type deriving(Bits, Eq, FShow);

typedef enum {
   NOP,JAL,JALR,BEQ,BNE,BLT,BGE,BLTU,BGEU
   } Jump_type deriving(Bits, Eq, FShow);

//typedef enum {
//   NOP, B, H, W, BU, HU, WU, D
//   } Mem_size deriving(Bits, Eq, FShow);

typedef Bit#(3) Mem_size;

typedef enum {
	Instruction_misaligned, 
	Instruction_access_fault,
	Illegal_instruction,
	Breakpoint,
	Load_address_misaligned,
	Load_access_fault,
	Store_address_misaligned,
	Store_access_fault,
	Environment_call_from_M_mode,
	No_exception
	} Exception_type deriving(Bits, Eq, FShow);

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
//   Bit#(TLog#(`TOTAL_THREADS)) thread_id;
   Bit#(`REG_WIDTH) program_counter;
   Bit#(`INSTR_WIDTH) instruction;
   Prediction prediction;
   Exception_type exception;
   } Fetched_instruction deriving(Bits,Eq, FShow);

typedef struct {
   Bool valid;
   Fetched_instruction fetched_instruction;
   } Fetched_instruction_2 deriving(Bits,Eq, FShow);

typedef struct {
Fetched_instruction 	fi_1;
Fetched_instruction_2 	fi_2;
} F_to_D deriving(Bits, Eq);

typedef struct {
//   Bit#(TLog#(`TOTAL_THREADS)) thread_id;
   Decoded_info_type instruction_decoded;       
   Prediction prediction;
   Bit#(`REG_WIDTH) program_counter;
   } Decoded_instruction deriving (Bits,Eq, FShow);


typedef struct {
   Vector#(`FETCH_WIDTH, Bool) valid;
   Vector#(`FETCH_WIDTH, Decoded_instruction) decode_packet;
   } Decode_packet deriving (Bits,Eq, FShow);

typedef struct {
   
//   Regfile_type regfile_type;                   //tells which register file to use (INT, FP..)
   Instruction_type inst_type;

   Mem_type mem_type;                           //if it is a load or store

   Mem_size mem_size;                           //size of mem. operation
   
   ALU_type alu_type;                               //If it a single cycle instr or MUL or DIV
   
   ALU_op alu_op;
   
   Bool word_flag;
   
   Branch_type branch_type;
   
   Branch_op branch_op;

   MACHINE_op csr_inst_type;
      
   Bool imm_valid;                                 //if the instr has immediate operand

   Bool csr_addr_valid;

   Bit#(TLog#(`REGFILE_SIZE)) rs1;              //source operand 1
   
   Bool rs1_valid;

   Bit#(TLog#(`REGFILE_SIZE)) rs2;              //source operand 2. If present, immediate operand
                                                //is stored here.
                                                //holds the register to be stored into memory
   
   Bool rs2_valid;

   Bit#(TLog#(`REGFILE_SIZE)) rd;               //destination register
   
   Bool rd_valid;
   
   Bit#(`REG_WIDTH) imm;                        //holds immediate value

   Exception_type exception;
   
   } Decoded_info_type deriving(Bits, Eq, FShow);

instance DefaultValue#(Decoded_info_type);

	defaultValue = Decoded_info_type {
						inst_type:NOP,
	  					mem_type:NOP,
	  					mem_size:3'b00,
	  					alu_type:NOP,
	  					alu_op : ADD,
	  					word_flag: False,
	  					branch_type: NOP,
	  					branch_op: BEQ,
	  					csr_inst_type: NOP,
	  					imm_valid: False,
	  					csr_addr_valid : False,
	  					rs1:0,
	  					rs1_valid: True,
	  					rs2:0,
	  					rs2_valid: True,
	  					rd:0,
	  					rd_valid: True,
	  					imm:0,
						exception: No_exception
	 					};
endinstance

/* Holds the Entry ROB information  */
typedef struct {
   Bool valid;
   
//   Bit#(TLog#(`TOTAL_THREADS)) thread_id;
   
//   Regfile_type regfile_type;
   Instruction_type inst_type;
   
   Mem_type mem_type;
   
   Mem_size mem_size;
   
   Bit#(TLog#(`MEMQ_SIZE)) mem_q_index;         //index of load or store queue
   
   ALU_type alu_type;
   
   ALU_op alu_op;
   
   Bool word_flag;

   Branch_type branch_type;
   
   Branch_op branch_op;

   MACHINE_op csr_inst_type;
   
   Bool imm_valid;
   
   Bool csr_valid;

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

   } Entry_rob_type deriving(Bits,Eq, FShow);


instance DefaultValue#(Entry_rob_type);

	defaultValue = Entry_rob_type {
					  valid						: False,
					  inst_type 				: NOP,
					  mem_type                  : NOP,
					  mem_size                  : 3'b000,
					  mem_q_index               : 0,
					  alu_type                  : NOP,
					  alu_op                    : ADD,
					  word_flag                 : False,
					  branch_type               : NOP,
					  branch_op                 : BEQ,
					  csr_inst_type			    : NOP,
					  imm_valid                 : False,
					  csr_valid				    : False,
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
   ALU_op alu_op;
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


/* Holds the inputs to ALU */
typedef struct {
   ALU_op alu_op;
   Bool word_flag;
   ALU_type alu_type;
   Bit#(`REG_WIDTH) src_1;
   Bit#(`REG_WIDTH) src_2;
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   Bit#(`REG_WIDTH) pc;
//   Bit#(TLog#(`TOTAL_THREADS)) thread_id;
   } ALU_payload_type deriving(Bits,Eq, FShow);

/* Holds the inupts to be read from PRF */
typedef struct {
   Bit#(TLog#(`PRF_SIZE)) base;
   Bit#(TLog#(`IMM_BUF_SIZE)) offset;
   Bit#(TLog#(`PRF_SIZE)) op_2;          //Value to be stored
   Bit#(TLog#(`MEMQ_SIZE)) mem_q_index;
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   Mem_type mem_type;
   Mem_size mem_size;
//   Bit#(TLog#(`TOTAL_THREADS)) thread_id;
   } LS_data_read deriving(Bits,Eq, FShow);
   
/* Holds the inputs to LS unit */
typedef struct {
   Bit#(`REG_WIDTH) base;
   Bit#(`REG_WIDTH) offset;
   Bit#(TLog#(`MEMQ_SIZE)) mem_q_index;
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   Maybe#(Bit#(`REG_WIDTH)) str_data;
   Mem_size mem_size;
//   Bit#(TLog#(`TOTAL_THREADS)) thread_id;
   } LS_payload_type deriving(Bits,Eq, FShow);


typedef struct {
   Branch_op branch_op;
   Bit#(TLog#(`PRF_SIZE)) op_1;
   Bit#(TLog#(`PRF_SIZE)) op_2;
   Bit#(TLog#(`IMM_BUF_SIZE)) imm_index;
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   Bit#(`REG_WIDTH) program_counter;
   Prediction prediction;
//   Bit#(TLog#(`TOTAL_THREADS)) thread_id;
   } Branch_data_read deriving(Bits,Eq, FShow);

typedef struct {
   Branch_op branch_op;
   Bit#(`REG_WIDTH) src_1;
   Bit#(`REG_WIDTH) src_2;
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   Bit#(`REG_WIDTH) imm;
   Bit#(`REG_WIDTH) program_counter;
   Prediction prediction;
//   Bit#(TLog#(`TOTAL_THREADS)) thread_id;
   } Branch_payload_type deriving(Bits,Eq, FShow);

typedef struct {
   Bool imm_valid;
   Bool csr_valid;
   Bit#(TLog#(`IMM_BUF_SIZE)) imm_index;   
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   } Intrpt_data_read deriving(Bits, Eq, FShow);
   
typedef struct {
   Bit#(`REG_WIDTH) src_1;
   Bool csr_valid;
   Bit#(TLog#(`PRF_SIZE)) dest_op;
   } Interrupt_payload_type deriving(Bits, Eq, FShow); 

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
//   Regfile_type result_type;
   Bit#(TLog#(`PRF_SIZE)) dest_tag;
 //  Bit#(TLog#(`TOTAL_THREADS)) thread_id;
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
   } Training_packet deriving(Bits,Eq,FShow);

typedef struct {
   Actual_jump actual_taken_or_not; 
   Bit#(1) is_matching_prediction;
   Bit#(TLog#(`PRF_SIZE)) dest_addr;
//   Bit#(TLog#(`TOTAL_THREADS)) thread_id;
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
	Vector#(`PRF_SIZE, Bool)    prf_entries;
	Bit#(TLog#(`IMM_BUF_SIZE))  imm_buf_tail;
   } IQ_to_map deriving(Bits, Eq);	

typedef struct {
	//Bool 						frq_empty;
	FRQ_entry					entry_1;
	FRQ_entry					entry_2;
   } FRQ_to_map deriving(Bits, Eq);

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

endpackage 
