
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

package alu;

import riscv_types::*;
import riscv_arithmetic_unit::*;
import ConfigReg::*;
`include "defined_parameters.bsv"


interface Ifc_alu;
   method Action _inputs( ALU_type _i_type, ALU_func _instruction, Bool word_flag, Bit#(`REG_WIDTH) _operand1, Bit#(`REG_WIDTH) _operand2, Bit#(TLog#(`PRF_SIZE)) _destination, Bit#(`REG_WIDTH) _pc);//receives input from the TB
   method Action _set_flush();	// initiates the flushing of the unit.
   
   // Output Methods

   method Result_bypass_type get_broadcast_packet();

endinterface

(*synthesize*)
module mkalu(Ifc_alu);
   
   Wire#(Bool) wr_flush <-mkDWire(False);																								// wire to indicate that a flush has occured in the processor and all buffer need to be cleared.
   Wire#(Bit#(`REG_WIDTH)) wr_result <-mkWire();
   Wire#(Result_bypass_type) wr_broadcast <- mkWire();
   Wire#(Bool) wr_inputs_taken <-mkDWire(False);														

     
   //Create a register to hold destination address
   Reg#(Bit#(TLog#(`PRF_SIZE))) rg_dest_address <- mkConfigReg(0);

   Ifc_riscv_arithmetic_unit arith <-mkriscv_arithmetic_unit();

      
   rule rl_get_output_from_arithmetic(!wr_flush);
	  $display("Got output from Arithmetic");
	  let lv_result <- arith.arith_out_;
	  let lv_dest <- arith.dest_out_();
	  wr_result <= lv_result;
	  wr_broadcast<= Result_bypass_type {
					 dest_tag: lv_dest,
					 _result : lv_result
					 };
	  
   endrule
   
   method Action _inputs(ALU_type _i_type, ALU_func _instruction, Bool word_flag, Bit#(`REG_WIDTH) _operand1, Bit#(`REG_WIDTH) _operand2, Bit#(TLog#(`PRF_SIZE)) _destination, Bit#(`REG_WIDTH) _pc);//receives input from the TB
	
		 arith._inputs(_instruction, _operand1, _operand2, word_flag, _destination, _pc, 0);
		
   endmethod

   method Action _set_flush();	// initiates the flushing of the unit.
	  wr_flush <= True;
	  arith._set_flush(True);
   endmethod
   
   
   // Output Methods

   method Result_bypass_type get_broadcast_packet();
	  return wr_broadcast;
   endmethod
   
endmodule

endpackage
