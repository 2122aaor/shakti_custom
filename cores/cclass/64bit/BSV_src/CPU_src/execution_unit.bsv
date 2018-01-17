/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author : Neel Gala
Email : neelgala@gmail.com
*/
package execution_unit;
  import alu::*;
  import branch::*;
  `include "defined_parameters.bsv"
  import defined_types::*;
  import muldiv::*;

  interface Ifc_execution_unit;
    method ActionValue#(Maybe#(Execution_output)) inputs (Bit#(5) _opcode, Bit#(3) _funct3 , Bit#(7) _funct7, Bit#(`Reg_width) _operand1, Bit#(`Reg_width) _operand2, Bit#(20) _immediate_value,Bit#(`Addr_width) program_counter, Bit#(5) dest_addr, Prediction_type pred_type, Bool is_imm);
  endinterface

  (*synthesize*)
  module mkexecution_unit(Ifc_execution_unit);
    Ifc_muldiv muldiv <-mkmuldiv();
    Reg#(Bool) rg_state <-mkReg(False);

    method ActionValue#(Maybe#(Execution_output)) inputs (Bit#(5) _opcode, Bit#(3) _funct3 , Bit#(7) _funct7, Bit#(`Reg_width) _operand1, Bit#(`Reg_width) _operand2, Bit#(20) _immediate_value,Bit#(`Addr_width) program_counter, Bit#(5) dest_addr, Prediction_type pred_type, Bool is_imm);
      if((_opcode=='b01100 ||_opcode=='b01110) && _funct7==1)begin // Mul and Div operations.
	let x<-muldiv._start(_operand1,_operand2,_funct3,_opcode[1]);
	if(x matches tagged Valid .res)begin
	  $display($time,"Output from DIV/MUL : %h",res);
	  rg_state<=False;
	  return tagged Valid (Execution_output{aluresult:res,
			      bypass:True,
			      memory_data:0,
			      word_size:0,
			      signextend:0,
			      mem_type:LOAD,
			      destination:dest_addr,
			      pred_result:tagged Correct_prediction 0,
			      training_data:?});
	end
	else 
	  return tagged Invalid;
      end
      else begin// Arithmetic Operations
        Execution_output out;
        if(_opcode[4:2]!='b110) begin // Arithmetic and Load store related operations
          $display($time,"\t Sending inputs to Arithmetic unit");
          let y=_operand2;
          if(is_imm)
            y=signExtend(_immediate_value);
          let x = fn_arithmetic(_opcode,_funct3,_funct7,_operand1,y,_immediate_value,program_counter);
          out=Execution_output{aluresult:x.aluresult,
                              bypass:x.bypass,
                              memory_data:_operand2,
                              word_size:x.word_size,
                              signextend:x.signextend,
                              mem_type:x.mem_type,
                              destination:dest_addr,
                              training_data:?,
                              pred_result: tagged Correct_prediction 0};
        end
        else begin// conditional branch operations + JAL + JALR
          $display($time,"\t Sending inputs to branch unit");
          let x = fn_branch(dest_addr,_opcode,_funct3,program_counter,_immediate_value,_operand1,_operand2,pred_type);
          out=Execution_output{aluresult:x.branchresult,
                              bypass:True,
                              memory_data:0,
                              word_size:0,
                              signextend:0,
                              mem_type:STORE,
                              destination:x.destination,
                              pred_result:x.pred_result,
                              training_data:x.training_data};
        end
        return tagged Valid out;
      end
    endmethod

  endmodule
endpackage
