
/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module name: ALU.
author name: Neel Gala, Rishi
Email id:    neelgala@gmail.com
last update done: 11th February 2013 

This module is the top level ALU which combines the multiplier, divider and the arithmetic unit. The divider output is given priority over the multiplier which is in turn given priority over the arithmetic unit. This priority is set using the preempts rule attribute. This makes coding easy.
*/

package mul_div;

import riscv_types::*;
//import integer_divider_riscv::*;
import divider::*;
import integer_multiplier_riscv::*;
import ConfigReg::*;
`include "defined_parameters.bsv"

interface Ifc_mul_div;
   method Action mul_inputs(ALU_func instruction, Bool word_flag, Bit#(`REG_WIDTH) _operand1, Bit#(`REG_WIDTH) _operand2, Bit#(TLog#(`PRF_SIZE)) _destination);//receives input from the TB
   method Action div_inputs(ALU_func instruction, Bool word_flag, Bit#(`REG_WIDTH) _operand1, Bit#(`REG_WIDTH) _operand2, Bit#(TLog#(`PRF_SIZE)) _destination);//receives input from the TB
   method Action _set_flush();	// initiates the flushing of the unit.
   
   // Output Methods

   method Bool inputs_taken_();																											// Output method to indicate whether the input has been taken
   method MulDivOp result_from;
   method Result_bypass_type get_broadcast_packet();

endinterface

(*preempts= "rl_get_output_from_divider, rl_get_output_from_multiplier" *)
(*synthesize*)

module mkmul_div(Ifc_mul_div);
   
   Wire#(Bool) wr_flush <-mkDWire(False);																								// wire to indicate that a flush has occured in the processor and all buffer need to be cleared.
   Wire#(Result_bypass_type) wr_broadcast <- mkWire();
   Wire#(MulDivOp) wr_result_from <-mkWire();
   Wire#(Bool) wr_inputs_taken <-mkDWire(False);														

     
   //Create a register to hold destination address
   Reg#(Bit#(TLog#(`PRF_SIZE))) rg_dest_address <- mkConfigReg(0);

   //Ifc_integer_divider_riscv divider <-mkinteger_divider_riscv();
   Ifc_divider	divider <- mkdivider();
   Ifc_integer_multiplier_riscv multiplier <-mkinteger_multiplier_riscv();

   rule rl_get_output_from_divider(!wr_flush);																							// This rule reads the output from the divider when ready
	  Bit#(`REG_WIDTH) lv_result <- divider.result_;
	  $display("Got output from Divider \n Result \t ab : %h, Register : %h", lv_result, divider.destination_address_);
	  wr_broadcast <= Result_bypass_type {
		 dest_tag: divider.destination_address_(),
		 _result : lv_result
		 };
	  wr_result_from <= Div;
	  
	  //release divider after broadcast
	  //divider._release();
   endrule   
   
   rule rl_get_output_from_multiplier(!wr_flush); // this rule get the output from the multiplier. Prioritized after the divider.
	  Bit#(`REG_WIDTH) lv_result = multiplier.result_;
	  $display("Got output from Multiplier \n Result \tab : %h, Register : %h", lv_result, multiplier.destination_address_);
	  wr_broadcast<= Result_bypass_type {
					 dest_tag: multiplier.destination_address_(),
		 			 _result : lv_result
					 };
	  
	  wr_result_from <= Mul;
	  //release mult after broadcast
	  multiplier._release();
   endrule
   

   method Action mul_inputs(ALU_func _instruction, Bool word_flag, Bit#(`REG_WIDTH) _operand1, Bit#(`REG_WIDTH) _operand2, Bit#(TLog#(`PRF_SIZE)) _destination);//receives input from the TB
  		 multiplier._start(_operand1,_operand2,_instruction,pack(word_flag),0,_destination);
   endmethod

   method Action div_inputs(ALU_func _instruction, Bool word_flag, Bit#(`REG_WIDTH) _operand1, Bit#(`REG_WIDTH) _operand2, Bit#(TLog#(`PRF_SIZE)) _destination);//receives input from the TB
		 divider._start(_operand1,_operand2,_instruction,pack(word_flag),_destination);
   endmethod

   method MulDivOp result_from;
		return wr_result_from;
   endmethod

   method Action _set_flush();	// initiates the flushing of the unit.
	  wr_flush <= True;
	  divider._set_flush;
	  multiplier._set_flush(True);
   endmethod
   
   
   // Output Methods

   method Result_bypass_type get_broadcast_packet();
	  return wr_broadcast;
   endmethod
   
   method Bool inputs_taken_();
	  return wr_inputs_taken;
   endmethod
   
endmodule

endpackage
