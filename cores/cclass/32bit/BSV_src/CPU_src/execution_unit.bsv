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
//  import branch::*;
  `include "defined_parameters.bsv"
  import defined_types::*;
	`ifdef muldiv
    import muldiv::*;
	`endif
  `ifdef spfpu
    import fpu::*;
  `endif

  interface Ifc_execution_unit;
    method ActionValue#(Tuple2#(Execution_output,Maybe#(Exception_cause))) inputs (Bit#(5) _opcode, Bit#(3) _funct3 , Bit#(7) _funct7, Bit#(`Reg_width) _operand1, Bit#(`Reg_width) _operand2, `ifdef spfpu Bit#(`Reg_width) _operand3,`endif Bit#(20) _immediate_value,Bit#(`Addr_width) program_counter, Bool is_imm);
  endinterface

  (*synthesize*)
  module mkexecution_unit(Ifc_execution_unit);
		`ifdef muldiv		
	     Ifc_muldiv muldiv <-mkmuldiv();
		`endif
    `ifdef spfpu
      Ifc_fpu fpu <- mkfpu();
    `endif

    method ActionValue#(Tuple2#(Execution_output,Maybe#(Exception_cause))) inputs (Bit#(5) _opcode, Bit#(3) _funct3 , Bit#(7) _funct7, Bit#(`Reg_width) _operand1, Bit#(`Reg_width) _operand2, `ifdef spfpu Bit#(`Reg_width) _operand3,`endif Bit#(20) _immediate_value,Bit#(`Addr_width) program_counter, Bool is_imm);
			Execution_output result_to_be_sent=tagged Busy;
			Maybe#(Exception_cause) exception=tagged Invalid;
      `ifdef spfpu 
      if(_opcode==`SPFLOAT_op || _opcode==`FNMSUB_op || _opcode==`FNMADD_op || _opcode == `FMADD_op || _opcode==`FMSUB_op)begin // SPFPU
        $display($time,"\tEXECUTION UNIT: Giving inputs to FPU ");
        let x<-fpu._start(_operand1,_operand2,_operand3,_opcode,_funct7,_funct3,_immediate_value[1:0],0);
        let y<-fpu.exception;
				exception=y;
        if(x matches tagged Valid .res)
          result_to_be_sent= tagged RESULT (Arithout{aluresult:res.final_result,
                                         fflags:res.fflags});
      end
      `endif
      `ifdef muldiv
      	if(_opcode==`MULDIV_op && _funct7==1 && _funct3[2]==1)begin // Div operations.
        	$display($time,"\tEXECUTION UNIT: Giving inputs to MULDIV ");
          let x<-muldiv._start(_operand1,_operand2,_funct3,_opcode[1]);
          if(x matches tagged Valid .res)
            result_to_be_sent= tagged RESULT(Arithout{aluresult:res,
                                          fflags:0});
        end
      `endif
       if(!(_opcode==`MULDIV_op && _funct7==1 && _funct3[2]==1) && (_opcode==`IMM_ARITH_op || _opcode==`ARITH_op || _opcode==`FLOAD_op || _opcode==`LOAD_op || _opcode==`FSTORE_op 
			 				|| _opcode==`STORE_op || _opcode==`ATOMIC_op || _opcode==`AUIPC_op || _opcode==`LUI_op)) begin // Arithmetic and Load store related operations
         $display($time,"\t Sending inputs to Arithmetic unit Op1: %h Op2: %h",_operand1,_operand2);
         result_to_be_sent = fn_arithmetic(_opcode,_funct3,_funct7,_operand1,_operand2,_immediate_value,program_counter,is_imm);
       end
			 return tuple2(result_to_be_sent,exception);
    endmethod

  endmodule
endpackage
