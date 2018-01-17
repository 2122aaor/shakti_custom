/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module name: Riscv_arithmetic_unit.
author name: Rishi Naidu, Neel Gala
Email id:    rishinaidu.rn@gmail.com, neelgala@gmail.com
last update done: 11th February 2014 

This module is the arithmetic execution unit for the RISCV ISA. It is a 64 bit implementation which is named as RV64.
The instruction with a "W" are RV64 instructions which ignore the upper 32 bits and operate on the lower 32 bits.
The arithmetic unit is implemented as a single case statement where the instruction bits define the various operations to be executed.

This module contains single cycle MUL/DIV instruction execution.To use the single cycle MUL/DIV operation *****uncomment line 113:318 and 369 ******

Separate module (not in this file) is made for the execution of MUL and DIV instructions, in which the instruction execution takes multiple cycles.

*/

package riscv_arithmetic_unit;


`include "defined_parameters.bsv"
import SpecialFIFOs::*;
import FIFO::*;
import FIFOF::*;
import DReg::*;
import arithmetic_functions::*;
import riscv_types::*;

interface Ifc_riscv_arithmetic_unit;
        	        	
	// Input Methods

	method Action _inputs( ALU_func _instruction, Bit#(`REG_WIDTH) _operand1, Bit#(`REG_WIDTH) _operand2, Bool word_flag, Bit#(TLog#(`PRF_SIZE)) _dest, Bit#(`REG_WIDTH) _pc, Bit#(TLog#(`TOTAL_THREADS)) thread_id);//receives input from the TB
	method Action _set_flush(Bool _flush);	// initiates the flushing of the unit.
	
	// Output Methods

	method ActionValue#(Bit#(`REG_WIDTH)) arith_out_();	// the final result method
	method ActionValue#(Bit#(TLog#(`PRF_SIZE))) dest_out_();	// the final result method
	method Bit#(TLog#(`TOTAL_THREADS)) thread_id_();

endinterface: Ifc_riscv_arithmetic_unit

(*synthesize*)
module mkriscv_arithmetic_unit(Ifc_riscv_arithmetic_unit);
	

	Wire#(Bit#(`REG_WIDTH)) wr_result<-mkWire();	//FIFO to hold the final result
	Wire#(Bit#(TLog#(`PRF_SIZE))) wr_dest<-mkWire();	
	Reg#(Bit#(TLog#(`TOTAL_THREADS))) rg_thread_id <-mkReg(0);
	
	Wire#(Bool) wr_flush <-mkDWire(False);				// wire for to indicate that wr_flush has occured.

	/*
	This methods execute takes in the input which is the instruction and the operands. The method is a huge case statement where the instructions are executed based on the opcode of the 32 bit instruction input. 
	Currently the inputs are provided by the test bench module.
	*/
	method Action _inputs( ALU_func _instruction, Bit#(`REG_WIDTH) _operand1, Bit#(`REG_WIDTH) _operand2, Bool word_flag, Bit#(TLog#(`PRF_SIZE)) _dest, Bit#(`REG_WIDTH) _pc, Bit#(TLog#(`TOTAL_THREADS)) thread_id)if(!wr_flush);//receives input from the TB
            $display("ALU has got input");
			rg_thread_id<=thread_id;
			wr_dest <= _dest;
			case (_instruction) matches

			LUI:	begin
					wr_result <= _operand2;
				end		

			AUIPC: 	begin
					wr_result <= fn_addsub(_pc,_operand2, 0 , 0);
	
				end

			ADD: 	begin //ADD/ADDI/SUB 
			 		wr_result <= fn_addsub(_operand1, _operand2, pack(word_flag) , 0);
									 
			      	end

			SUB: 	begin //ADD/ADDI/SUB 
			 		wr_result <= fn_addsub(_operand1, _operand2, pack(word_flag) , 1);
									 
			     	end

			SLL: 	begin //SLL/SLLI
					 wr_result <= fn_sll(_operand1, _operand2, pack(word_flag));
			     	end
						
			SLT: 	begin //SLT/SLTI
			       		 wr_result <= fn_slt(_operand1,_operand2);
			      	end

			SLTU: 	begin //SLTU/SLTIU
		  			 wr_result <= fn_sltu(_operand1,_operand2);
		   	      	end

			XOR: 	begin  
					  wr_result <= _operand1 ^ _operand2;
				end
					
			SRL: 	begin //SRL/SRLI/SRAI/SRA/
					 wr_result <= fn_sra_srl(_operand1,_operand2, pack(word_flag),0);
			      	end
			SRA: 	begin //SRL/SRLI/SRAI/SRA/
					 wr_result <= fn_sra_srl(_operand1,_operand2, pack(word_flag),1);
                               	end
			      
			OR: 	begin //OR/ORI
					wr_result <= _operand1 | _operand2;
				end

			AND: 	begin //AND/ANDI
					wr_result <= _operand1 & _operand2;
				end
						
			endcase
	endmethod

	// provides the final result.
	method ActionValue#(Bit#(`REG_WIDTH)) arith_out_();
	   return wr_result;
	endmethod
	
	method ActionValue#(Bit#(TLog#(`PRF_SIZE))) dest_out_();
	   return wr_dest;
	endmethod

	// method when invoked initiate the wr_flush routine.
	method Action _set_flush(Bool _flush);
		wr_flush<=_flush;
	endmethod

	method Bit#(TLog#(`TOTAL_THREADS)) thread_id_();
		return rg_thread_id;
	endmethod

endmodule : mkriscv_arithmetic_unit

endpackage : riscv_arithmetic_unit
