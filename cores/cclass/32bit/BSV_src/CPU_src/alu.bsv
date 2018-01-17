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

This module is the arithmetic execution unit for the RISCV ISA. It is a 64 bit implementation which is named as RV64.
The instruction with a "W" are RV64 instructions which ignore the upper 32 bits and operate on the lower 32 bits.
The arithmetic unit is implemented as a single case statement where the instruction bits define the various operations to be executed.

This module contains single cycle MUL instruction execution.

*/

package alu;

import arithmetic_functions::*;
import defined_types::*;
`include "defined_parameters.bsv"
	function Execution_output fn_arithmetic(Bit#(5) _opcode, Bit#(3) _funct3 , Bit#(7) _funct7, Bit#(`Reg_width) _operand1, Bit#(`Reg_width) _operand2, Bit#(20) _immediate_value,Bit#(`Reg_width) program_counter, Bool is_imm);
			Bit#(`Reg_width) alu_result=0;
			Bit#(`Reg_width) zero=0;
      Bool lv_bypass=True;
      Bit#(1) add_sub = 0;
      if(_opcode==`ARITH_op && _funct3==`ADD_SUB_f3 && _funct7[5]==1) // subtraction
        add_sub=1;
     
			Bit#(`Reg_width) op2=_operand2;
		  
			Access_type mem_type=Load;
      if(_opcode==`STORE_op || _opcode==`FSTORE_op)begin
        mem_type=Store;
      end
			`ifdef atomic
				if(_opcode==`ATOMIC_op)
					mem_type=Atomic;
			`endif

      if(is_imm)begin
        _operand2=signExtend(_immediate_value);
      end

      if(_opcode==`LUI_op)
            alu_result =(_operand2<<12);   //LUI
			if (_opcode==`AUIPC_op)begin
						_operand1=_operand2<<12;
						_operand2=program_counter;
						add_sub=0;
			end
      if(_opcode==`AUIPC_op || _opcode==`LOAD_op || _opcode==`FLOAD_op || _opcode==`STORE_op || _opcode==`FSTORE_op || (_opcode==`IMM_ARITH_op && _funct3==`ADD_SUB_f3) ||  (_opcode==`ARITH_op && _funct3==`ADD_SUB_f3)) begin
         let x=(fn_addsub(_operand1,_operand2,add_sub));
          alu_result=x;
      end
			`ifdef muldiv
			if(_opcode==`MULDIV_op && _funct7[0]==1)
					alu_result=multiplication(_operand1,_operand2,_funct3);
			`endif
			if(_opcode==`ATOMIC_op && (_funct7[3:2]=='b11 || _funct7[3:2]=='b10))
				alu_result=_operand1;
			if ((_opcode==`ARITH_op && _funct7[0]==0) || _opcode==`IMM_ARITH_op)begin 
				if(_funct3==	`SLT_SLTI_f3 || _funct3==`SLTU_SLTIU_f3)
          alu_result =(fn_slt(_operand1,_operand2,~_opcode[3], _funct3[0]));
	      if(_funct3==`XOR_XORI_f3) //XOR/XORI
            alu_result =(_operand1 ^ _operand2);
  	    if(_funct3==`SR_SRI_f3 || _funct3==`SLL_SLLI_f3)
          alu_result =(fn_sra_srl(_operand1,_operand2,_funct7[5], ~_opcode[3],~_funct3[2]));
      	if(_funct3==`OR_ORI_f3) //OR/ORI
            alu_result =(_operand1 | _operand2);
      	if(_funct3==`AND_ANDI_f3) //AND/ANDI
            alu_result =(_operand1 & _operand2);
			end

      if(_opcode==`LOAD_op || _opcode==`FLOAD_op || _opcode==`FSTORE_op || _opcode==`STORE_op || _opcode==`ATOMIC_op) // Load or Store
				return tagged MEMORY (Memout{
					address:alu_result,
					memory_data:op2,
					word_size:_funct3[1:0],
					signextend:_funct3[2],
					mem_type:mem_type
					`ifdef atomic ,atomic_op:_funct7[6:2] `endif	});
			else
				return tagged RESULT (Arithout{
					aluresult:alu_result,
          fflags:0});
	endfunction
	
endpackage
