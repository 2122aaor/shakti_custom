/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module name: Riscv_branch_unit.
author name: Neel Gala
Email id:    neelgala@gmail.com
last update done: 11th February 2014

This module is the branch execution unit for the RISCV ISA. It is a 64 bit implementation which is named as RV64.
The following instructions are executed by this unit : 
    J, JAL, JALR, BEQ, BNE, BLT, BLTU, BGE, BGEU
The branch unit is implemented as a single case statement where the instruction bits define the various operations to be executed.


*/
package branch;

import defined_types::*;
`include "defined_parameters.bsv"

	(*noinline*)
	function Branch_output fn_branch(Bit#(5) _dest_addr, Bit#(5) _opcode, Bit#(3) _funct3, Bit#(`Addr_width) _current_pc, Bit#(20) _immediate_value, Bit#(`Reg_width) _operand1, Bit#(`Reg_width) _operand2, Prediction_type _prediction);
		Bit#(`Addr_width) lv_target_offset=0; // variable captures the target offset for various instructions
		Bit#(`Addr_width) lv_effective=0;     // captures the final effective that the pc needs to jump to using the target offset.
		Bit#(`Addr_width) lv_destination_value=0;
    Bit#(5) destination_address=0;
		Actual_jump lv_actual = Notaken; // captures if the conditional branch is to actually taken or not.
		case(_opcode) matches
			'b11011: begin// JAL
				lv_target_offset=signExtend(_immediate_value);
				lv_effective=(lv_target_offset<<1) + _current_pc;
				lv_destination_value =(_current_pc+'d4);
				lv_actual=Taken;
        destination_address=_dest_addr;
			end
			'b11001: begin// JALR
				lv_target_offset=signExtend(_immediate_value);
				lv_effective = {(lv_target_offset+_operand1)[`Addr_width-1:1],1'b0};
				lv_destination_value =(_current_pc+'d4);
				lv_actual=Taken;
        destination_address=_dest_addr;
			end
			'b11000: begin // conditional jumps
				lv_target_offset=signExtend(_immediate_value);
				lv_effective=(lv_target_offset<<1) + _current_pc;
				case(_funct3)
					'b000: begin// BEQ
						if(_operand1==_operand2)
							lv_actual=Taken;
					end
					'b001: begin// BNE
						if(_operand1!=_operand2)
							lv_actual=Taken;
					end
					'b100: begin// BLT
						if(_operand1[`Reg_width-1]==_operand2[`Reg_width-1] && _operand1<_operand2)// && _operand1[31]==0)
							lv_actual=Taken;
						else if (_operand1[`Reg_width-1]!=_operand2[`Reg_width-1] && _operand1[`Reg_width-1]==1)
							lv_actual=Taken;
					end
					'b101: begin// BGE
						if(_operand1[`Reg_width-1]==_operand2[`Reg_width-1] && _operand1>=_operand2 )//&& _operand1[31]==0)
							lv_actual=Taken;
						else if (_operand1[`Reg_width-1]!=_operand2[`Reg_width-1] && _operand1[`Reg_width-1]==0)
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
		Prediction_result prediction_result=tagged Mispredicted (_current_pc+'d4);
		// the following if-else structure find out if the branch predictor was a correct or wron. If wrong then the new PC value is given to jump to.
    if(_opcode!='b11001)begin // If not JALR
      if(_prediction==Predicted_taken && lv_actual==Taken) 
        prediction_result=tagged Correct_prediction (lv_effective);
      else if (_prediction==Predicted_notaken && lv_actual==Taken)
        prediction_result= tagged Mispredicted (lv_effective);
      else if (_prediction==Predicted_taken && lv_actual==Notaken)
        prediction_result= tagged Mispredicted (_current_pc+'d4);
      else if (_prediction==Predicted_notaken && lv_actual==Notaken)
        prediction_result= tagged Correct_prediction (lv_effective);
    end
    else
        prediction_result= tagged Mispredicted (lv_effective); // JALR
		Training_data data = Training_data{pc:_current_pc,
											actual:lv_actual,
											branch_address:lv_effective};

		return (Branch_output{branchresult:lv_destination_value,
              destination:destination_address,
							training_data:data,
							pred_result:prediction_result});

		
	endfunction

endpackage
