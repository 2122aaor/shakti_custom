/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Names : Neel Gala, Arjun Menon
Email ID : neelgala@gmail.com

Description : 

This files contains all the types and structures that are used in any of the modules.
*/
package defined_types;

`include "defined_parameters.bsv"

  typedef enum {Load, Store, Atomic} Access_type deriving (Bits,Eq,FShow);
  typedef enum {User=2'd0,Machine=2'd3} Privilege_mode deriving (Bits,Eq,FShow);
	typedef enum {Idle,Stall,Handling_Request,Handling_Memory} Cache_State deriving (Bits,Eq);
  
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

  typedef struct{
    Bit#(addr_width) address;
    Access_type load_store;
    Bit#(TMul#(word_size,8)) data;
    Bit#(2) transfer_size; // 0 -8 bits, 1- 16 bits, 2 -32 bits;
		`ifdef atomic Bit#(5) atomic_op;`endif
  }From_Cpu_D#(numeric type addr_width, numeric type word_size) deriving(Bits,Eq);


  typedef struct{
    Bit#(TMul#(8,TMul#(word_size,block_size))) line;
    Bit#(addr_width) address;
    Bit#(TLog#(ways)) replace_block;
  } Current_Store#(numeric type ways, numeric type addr_width, numeric type block_size, numeric type word_size) deriving(Bits,Eq);
  
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

// TODO remove this struct
//typedef enum {
//    None,Instruction_misaligned, Load_misaligned, Store_misaligned, Instruction_buserr, Load_buserr, Store_buserr, Illegal_instruction, System_class, Breakpoint,
//		Invalid, Overflow, Underflow, Divide_by_Zero,Inexact
//} Exception deriving (Eq, Bits, FShow);

typedef enum{
	Taken, Notaken
}Actual_jump deriving (Eq,Bits,FShow); // actual branch condition used in the branch execution unit.

// enum defining the prediction of the branch predictor for the current PC.
typedef enum{
	Predicted_taken,Predicted_notaken
}Prediction_type deriving (Eq,Bits,FShow); // output from the branch prediction unit.

// A typedef defining , if the prediction by the branch predictor was correct or wrong. 
typedef union tagged{
	Bit#(`Addr_width) Mispredicted;
	Bit#(`Addr_width) Correct_prediction;
}Prediction_result deriving (Eq,Bits); // result of prediuction from the branch execution unit.

typedef struct{
	Bit#(addr_width) prog_counter_;
  Prediction_type prediction_;
  Bool jump;
} Predictor_output#(numeric type addr_width) deriving(Bits, Eq);    // the program counter from the branch prediction unit.

typedef struct{
	Bit#(`Reg_width) data_forward;
	Bit#(5) rd_forward;
	Bool valid;
	Register_type rd_type;
//  Bit#(2); // 0-INTEGER 1-FLOATING 2-SYSTEM_INSTRD
}Operand_forwading_type deriving (Bits,Eq);	// the data structure for operand forwarding from any stage

typedef struct{
	Bit#(`Reg_width) rs1;
	Bit#(`Reg_width) rs2;
  `ifdef spfpu Bit#(`Reg_width) rs3;`endif
} Output_for_operand_fetch deriving (Bits,Eq); // output from the register file to the decode stage

typedef enum {
	ALU,ILLEGAL,SYSTEM_INSTR,NOP
}Instruction_type deriving(Bits, Eq,FShow); // the type of the decoded instruction.

// to distuingish between integer and floating point RF
typedef enum {IntegerRF, FloatingRF} Register_type deriving(Bits,Eq,FShow);

// the output data structure of the decoder. 
typedef struct {
	Instruction_type inst_type;  //to hold the istruction type
	Bit#(5) rs1;    //address of 1st source operand
	Bit#(5) rs2;    //address of 2nd source operand
  `ifdef spfpu	Bit#(5) rs3;   `endif  //address of 3rd source operand
	Bit#(5) rd;     //address of destination register
	Register_type rd_type;
	Register_type rs1_type;
	Register_type rs2_type;
  Bit#(5) opcode;
  Bit#(3) funct3;
  Bit#(7) funct7;
  Bit#(20) immediate_value;
  Bool is_imm;
  Prediction_type pred_type;
} Decoded_info_type deriving(Bits, Eq);


// the data stucture for the pipeline FIFO between fetch and decode.
typedef struct{
	Bit#(`Addr_width) program_counter;
	Bit#(32) instruction;
	Prediction_type prediction;
  Maybe#(Exception_cause) exception;
}IF_ID_type deriving (Bits,Eq);

typedef struct{
	Bit#(`Reg_width) rs1;
	Bit#(`Reg_width) rs2;
	`ifdef spfpu Bit#(`Reg_width) rs3; `endif
	Bit#(5) opcode;
	Bit#(3) funct3;
	Bit#(7) funct7;
	Bit#(20) immediate_value;
	Bool is_imm;
	}Alu_inputs deriving (Eq,Bits);

typedef union tagged{
	Arithout RESULT;
	Arithout CSR;
	Alu_inputs ALU_INPUTS;
	}Op_fetch_result deriving(Bits,Eq);

typedef struct{
	Op_fetch_result op_fetch_data;
	Bit#(`Addr_width) program_counter;
  Maybe#(Exception_cause) exception;
	Bit#(5) destination;
	Register_type rd_type;
}ID_IE_type deriving (Bits,Eq);

typedef struct{
	Execution_output execresult;
	Bool system_instruction;
	Bit#(`Addr_width) program_counter;
  Maybe#(Exception_cause) exception;
	Bit#(5) destination;
	Register_type rd_type;
}IE_IMEM_type deriving (Bits,Eq);

typedef struct{
	Bit#(`Addr_width) branchresult;
	Training_data training_data;
	Prediction_result pred_result;
	Maybe#(Exception_cause) exception;
} Branchout deriving(Bits,Eq); // output from the branch execution unit.

typedef struct{
	Bit#(`Reg_width) aluresult;
  Bit#(5) fflags;
} Arithout deriving(Bits,Eq); // output struct from the alu.
	
typedef struct{
	Bit#(`Reg_width) address;
	Bit#(`Reg_width) memory_data; // data to be written in the memory
	Bit#(2) word_size; // size of the data transfer
	Bit#(1) signextend; // whether the loaded value has to be signextended
	Access_type mem_type; // STORE or AMO or LOAD
	`ifdef atomic Bit#(5) atomic_op;`endif
}Memout deriving(Bits,Eq);

typedef union tagged{
	Arithout RESULT;
	Memout MEMORY;
	void Busy;
} Execution_output deriving(Bits,Eq);

typedef struct{
  Bit#(`Addr_width) badaddr;
	Arithout commit_data;
	Bool system_instruction;
	Bit#(`Addr_width) program_counter;
  Maybe#(Exception_cause) exception;
	Bit#(5) destination;
	Register_type rd_type;
}IMEM_IWB_type deriving(Bits,Eq);

typedef struct{
	Bit#(`Reg_width) destination_value;
  Maybe#(Exception_cause) exception;
  Bit#(`Addr_width) badaddr;
}MemoryUnitResponse deriving(Eq,Bits);


typedef struct {
	Bit#(`Addr_width) pc;
	Bit#(`Addr_width) branch_address;
	Actual_jump actual;} Training_data deriving (Bits, Eq);

typedef enum {SWAP,ADD,XOR,AND,OR,MINU,MAXU,MIN,MAX} Atomic_funct deriving(Bits,Eq,FShow);

typedef struct{
		Bit#(width) final_result;					// the final result for the operation
		Bit#(5) fflags; 					// indicates if any exception is generated.
	}Floating_output#(numeric type width) deriving(Bits,Eq);				// data structure of the output FIFO.

typedef enum {
  Inst_addr_misaligned=0,
  Inst_access_fault=1,
  Illegal_inst=2,
  Breakpoint=3,
  Load_addr_misaligned=4,
  Load_access_fault=5,
  Store_addr_misaligned=6,
  Store_access_fault=7,
  Ecall_from_user=8,
  Ecall_from_machine=11
} Exception_cause deriving (Bits,Eq,FShow);

typedef enum{
  User_soft_int=0,
  Machine_soft_int=3,
  User_timer_int=4,
  Machine_timer_int=7,
  User_external_int=8,
  Machine_external_int=11
} Interrupt_cause deriving (Bits,Eq,FShow);

typedef union tagged{
  Exception_cause Exception;
  Interrupt_cause Interrupt;
  void None;
} Trap_type deriving(Bits,Eq,FShow);

typedef struct{
  Bit#(`Addr_width) address;
  Bool redirect;
  Bit#(`Reg_width) destination_value;
} CSRResult deriving(Bits,Eq,FShow);

typedef enum {Normal,Privilege} WriteBackInstr deriving(Eq,Bits,FShow);

typedef struct{
	Bit#(`Reg_width)  rs1_data;
  Bool is_privilege;
  Maybe#(Exception_cause) exception;
  Bit#(`Addr_width) pc;
  Bit#(`Addr_width) badaddr;
  Bit#(`Reg_width) rd_data;
  Bit#(5) fflags;
} CSRInsn deriving (Bits);

typedef struct{
  Trap_type trap;
  Bit#(`Addr_width) badaddr;
}Trap_info deriving(Bits,Eq);
endpackage
