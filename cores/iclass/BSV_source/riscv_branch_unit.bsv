/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer 
   in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
 BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module name: Riscv_branch_unit.
author name: Neel Gala
Email id:    neelgala@gmail.com
last update done: 15th October 2013 

This module is the branch execution unit for the RISCV ISA. It is a 64 bit implementation which is named as RV64.
The following instructions are executed by this unit : 
	J, JAL, JALR, BEQ, BNE, BLT, BLTU, BGE, BGEU
The branch unit is implemented as a single case statement where the instruction bits define the various operations to be executed.


The unit when synthesized in 65nm technology gives the followins stats : 

Cell count : Sequential - 137, Combinational - 2093
Max operating Frequency : 2.41GHz

*/

package riscv_branch_unit;


`include "defined_parameters.bsv"
`include "defined_parameters_rs.bsv"
import FIFO::*;
import defined_types::*;
import DReg::*;
import riscv_types::*;
import ls_types::*;   // For memory instructions

// enum defining the whether the conditoin was actially taken or not. 
interface Ifc_riscv_branch_unit;
        	        	
	// Input Methods

	method Action _inputs(Bit#(TLog#(`Thread_numbers)) _thread_id,Instruction_type _inst_type,Bit#(`Addr_width) _current_pc, Bit#(32) _instruction, Bit#(`Reg_width) _operand1, Bit#(`Reg_width) _operand2, Prediction_type _prediction, Bit#(TLog#(`Rob_size)) _token, SSN  _store_number_count, Bit#(2) _flip_rs1_rd_registerfile, Bit#(1) _tag_rd);//receives input from the TB
	method Action _set_flush(Bit#(TLog#(`Thread_numbers)) _thread_id);	// initiates the flushing of the unit.
	method Action _release_branch();			//	deq the FIFO
	
	// Output Methods

	method Bit#(5) destination_address_();	// provides the register file destination address of Rd.
	method Bit#(`Addr_width) destination_value_();	// the value to be written into the desitnation in case of JAL/JALR instructions.
	method Bit#(TLog#(`Rob_size)) token_();
	method Maybe#(Prediction_result) jump_address_(); // effective address to jump to.
	method Training_data training_data_();
        method Bit#(32) program_counter_();
        method Exception exception_(); 
        method Bit#(TLog#(`Thread_numbers)) thread_id_(); 
        method Instruction_type inst_type_(); 
        method Bit#(32) instruction_();  
        method SSN  store_number_count_();
        method Bit#(2) flip_rs1_rd_registerfile_();
        method Bit#(1) tag_rd_();  

        method Bool check_branch_ready();
        //method Action _set_branch_ready(Bool set_ready);

				
endinterface: Ifc_riscv_branch_unit

typedef struct {
	Bit#(32) pc;
	Bit#(32) branch_address;
	Actual_jump actual;} Training_data deriving (Bits, Eq);

(*synthesize*)
module mkriscv_branch_unit(Ifc_riscv_branch_unit);
	

	FIFO#(Bit#(`Addr_width)) ff_result<-mkFIFO1();	//FIFO to hold the final result
	Reg#(Bit#(5)) rg_destination_address <-mkReg(0);		// the destination address in the register file for the current instruction
	Reg#(Bit#(TLog#(`Rob_size))) rg_token <-mkReg(0);
	Wire#(bit) wr_flush <-mkDWire(0);				// wire for to indicate that wr_flush has occured.
	Reg#(Maybe#(Prediction_result)) rg_prediction_result <-mkReg(tagged Invalid);		//TRUE when instruction executed

	Reg#(Actual_jump) rg_actual <-mkReg(Notaken);
	Reg#(Bit#(32)) rg_program_counter <-mkReg(0);
	Reg#(Bit#(32)) rg_branch_address <-mkReg(0);
        Reg#(Exception) rg_exception <- mkReg(tagged No_exception);
        Reg#(Bit#(TLog#(`Thread_numbers))) rg_thread_id <- mkReg(0);
        Reg#(Instruction_type)  rg_inst_type <- mkReg(NOP);
        Reg#(SSN) rg_store_number_count <- mkReg(0);
        Reg#(Bit#(2)) rg_flip_rs1_rd_registerfile <- mkReg(0);
        Reg#(Bit#(1)) rg_tag_rd <- mkReg(0);   
        Reg#(Bit#(32)) rg_instruction <- mkReg(0);
       
        Reg#(Bool) rg_ready <-mkReg(False);
	/*
	This rule is called when ever the execution unit needs to be flushed. This rule will only fire when the 
	wr_flush wire is made true. In this rule all the registers are set to their default values. 
	*/
	rule rl_flush_data(wr_flush ==1 );
		rg_destination_address<=0;
		ff_result.clear();
	endrule: rl_flush_data

        rule rl_start(isValid(rg_prediction_result));

               $display("BRANCH0 started corresponding to thread_id %d, ROB_number %d",rg_thread_id,rg_token);                     

        endrule

	/*
	This methods execute takes in the input which is the instruction and the operands. The method is a huge case statement where the instructions are executed based on the opcode of the 32 bit instruction input. 
	Currently the inputs are provided by the test bench module.
	*/
	method Action _inputs(Bit#(TLog#(`Thread_numbers)) _thread_id,Instruction_type _inst_type,Bit#(`Addr_width) _current_pc, Bit#(32) _instruction, Bit#(`Reg_width) _operand1, Bit#(`Reg_width) _operand2, Prediction_type _prediction, Bit#(TLog#(`Rob_size)) _token, SSN  _store_number_count, Bit#(2) _flip_rs1_rd_registerfile, Bit#(1) _tag_rd)if(wr_flush==0);
		rg_ready <= True;
		rg_token<=_token;
		Bit#(`Addr_width) lv_target_offset=0; // variable captures the target offset for various instructions
		Bit#(`Addr_width) lv_effective=0;     // captures the final effective that the pc needs to jump to using the target offset.
		Actual_jump lv_actual = Notaken; // captures if the conditional branch is to actually taken or not.
		case(_instruction[6:2]) matches
		
			'b11010: begin// J	
				lv_target_offset=signExtend(_instruction[31:7]);
				lv_effective=(lv_target_offset<<1) + _current_pc;
				rg_destination_address<=0;
				ff_result.enq(0);
				lv_actual=Taken;
			end
			'b11011: begin// JAL
				lv_target_offset=signExtend(_instruction[31:7]);
				lv_effective=(lv_target_offset<<1) + _current_pc;
				rg_destination_address<='d1;
				ff_result.enq(_current_pc+'d4);
				lv_actual=Taken;
			end
			'b11001: begin// JALR
				lv_target_offset=signExtend(_instruction[21:10]);
				lv_effective = {(lv_target_offset + _operand1[`Addr_width-1:0])[`Addr_width-1:1],1'b0};
				rg_destination_address<=_instruction[31:27];
				ff_result.enq(_current_pc+'d4);
				lv_actual=Taken;
			end
			'b11000: begin // conditional jumps
				lv_target_offset=signExtend({_instruction[31:27],_instruction[16:10]});
				lv_effective=(lv_target_offset<<1) + _current_pc;
				rg_destination_address<=0;
				ff_result.enq(0);
				case(_instruction[9:7]) matches
					'b000: begin// BEQ
						if(_operand1==_operand2)
							lv_actual=Taken;
					end
					'b001: begin// BNE
						if(_operand1!=_operand2)
							lv_actual=Taken;
					end
					'b100: begin// BLT
						if(_operand1[`Reg_width-1]==_operand2[`Reg_width-1] && _operand1<_operand2 && _operand1[1]==0)
							lv_actual=Taken;
						else if (_operand1[`Reg_width-1]==_operand2[`Reg_width-1] && _operand2<_operand1 && _operand1[1]==1) // TODO verify with arjun.
							lv_actual=Taken;
						else if (_operand1[`Reg_width-1]!=_operand2[`Reg_width-1] && _operand1[1]==1)
							lv_actual=Taken;
							
					end
					'b101: begin// BGE
						if(_operand1[`Reg_width-1]==_operand2[`Reg_width-1] && _operand1>=_operand2 && _operand1[1]==0)
							lv_actual=Taken;
						else if (_operand1[`Reg_width-1]==_operand2[`Reg_width-1] && _operand2>=_operand1 && _operand1[1]==1) // TODO verify with arjun.
							lv_actual=Taken;
						else if (_operand1[`Reg_width-1]!=_operand2[`Reg_width-1] && _operand1[1]==0)
							lv_actual=Taken;
					end
					'b110: begin// BLTU
						if(_operand1<_operand2)
							lv_actual=Taken;
					end
					'b111: begin// BGEU
						if(_operand1>=_operand2)
							lv_actual=Taken;
					end
				endcase
			end
		endcase

		// the following if-else structure find out if the branch predictor was a correct or wron. If wrong then the new PC value is given to jump to.
		if(_prediction==Predicted_taken && lv_actual==Taken) 
			rg_prediction_result <= tagged Valid Correct_prediction (lv_effective);
		else if (_prediction==Predicted_notaken && lv_actual==Taken)
			rg_prediction_result <= tagged Valid Mispredicted (lv_effective);
		else if (_prediction==Predicted_taken && lv_actual==Notaken)
			rg_prediction_result <= tagged Valid Mispredicted (_current_pc+'d4);
		else if (_prediction==Predicted_notaken && lv_actual==Notaken)
			rg_prediction_result <= tagged Valid Correct_prediction (lv_effective);

		rg_program_counter <= _current_pc;
		rg_actual<=lv_actual;
		rg_branch_address<=lv_effective;
                rg_thread_id <= _thread_id; 
                rg_inst_type <= _inst_type; 
                rg_instruction <= _instruction;
                rg_store_number_count <= _store_number_count;
                rg_flip_rs1_rd_registerfile <= _flip_rs1_rd_registerfile;
                rg_tag_rd <= _tag_rd;
                 

	endmethod

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        //Method to selectively flush only the matching thread ids..

        method Action _set_flush(Bit#(TLog#(`Thread_numbers)) _thread_id);

        Bit#(TLog#(`Thread_numbers)) thread_id = _thread_id;

          if(rg_thread_id == thread_id )   
              	wr_flush <= 1;
         
        endmethod:_set_flush
        ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	method Action _release_branch();			//	deq the FIFO
		ff_result.deq();
	endmethod
	// Output Methods

	method Bit#(5) destination_address_();	// provides the register file destination address of Rd.
		return rg_destination_address;
	endmethod
	method Bit#(`Addr_width) destination_value_();	// the value to be written into the desitnation in case of JAL/JALR instructions.
		return ff_result.first();
	endmethod
	method Maybe#(Prediction_result) jump_address_(); // effective address to jump to.
		return rg_prediction_result;
	endmethod

	method Bit#(TLog#(`Rob_size)) token_();
		return rg_token;
	endmethod

	method Training_data training_data_();
		return Training_data {pc : rg_program_counter,
				      	actual : rg_actual,
					branch_address : rg_branch_address};
	endmethod

        method Bit#(32) program_counter_();
           return rg_program_counter;
        endmethod 
 
        method Exception exception_(); 
           return rg_exception;
        endmethod

        method Bit#(TLog#(`Thread_numbers)) thread_id_();
           return rg_thread_id;
        endmethod     
  
        method Instruction_type inst_type_();       
           return rg_inst_type;
        endmethod

        method Bit#(32) instruction_();  
           return rg_instruction;
        endmethod
   
        method SSN  store_number_count_();
           return rg_store_number_count;
        endmethod
 
        method Bit#(2) flip_rs1_rd_registerfile_();
            return  rg_flip_rs1_rd_registerfile;
        endmethod
   
        method Bit#(1) tag_rd_(); 
            return rg_tag_rd;
        endmethod
   
        method Bool check_branch_ready();
        return rg_ready;
        endmethod

       // method Action _set_branch_ready(Bool set_ready);
       // wr_ready <= set_ready;
       // endmethod


endmodule
endpackage
