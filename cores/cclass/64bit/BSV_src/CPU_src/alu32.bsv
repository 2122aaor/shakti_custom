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
	(*noinline*)
	function Alu_output fn_arithmetic(Bit#(5) _opcode, Bit#(3) _funct3 , Bit#(7) _funct7, Bit#(`Reg_width) _operand1, Bit#(`Reg_width) _operand2, Bit#(20) _immediate_value, Bit#(1) add_sub);
			Bit#(`Reg_width) alu_result=0;
			Bit#(`Reg_width) zero=0;
      if(_opcode[0]== 1'b1) begin
        if (_opcode[3]==1)  // LUI
            alu_result =(signExtend(_immediate_value)<<12);   //LUI
        else
            alu_result =((signExtend(_immediate_value)<<12)+_operand1);//AUIPC // here operand1 will contain the PC. 
      end
      else begin
        if(_opcode=='b00000 || _opcode='b01000 || (_opcode=='b00100 && _funct3='b000) ||  (_opcode=='b01100 && _funct3='b000)) // LOAD or STORE or ADDI or ADD/SUB
           alu_result=signExtend(fn_addsub(_operand1,_operand2,_immediate_value[11:0],add_sub,~(_opcode[3]&_opcode[2])));
        else if(_funct3=='b001) // SLL/SLLI
            alu_result =(fn_sll(_operand1, _operand2, _immediate_value[11:0], ~_opcode[3]));
                // (_operand1, _operand2 , immediate, Immediate flag)
        else if (_funct3==	3'b010) //SLT/SLTI
            alu_result =(fn_slt(_operand1,_operand2,_immediate_value[11:0],~_opcode[3]));
                // (_operand1, _operand2 , immediate,  Immediate flag)
        else if (_funct3==3'b011) //SLTU/SLTIU
            alu_result =(fn_sltu(_operand1,_operand2,_immediate_value[11:0],~_opcode[3]));
                // (_operand1, _operand2 , immediate,  Immediate flag	)
        else if(_funct3==3'b100) //XOR/XORI
            if (_opcode[3]==1) 
              alu_result =(_operand1 ^ _operand2);
            else 
              alu_result =(_operand1 ^ signExtend(_immediate_value[11:0]));
        else if(_funct3==3'b101) //SRL/SRLI/SRAI/SRA/
            // Shift_rigfht_logical/arithmetic flag ==1 for arithmetic shift and 0 for logical shift
            alu_result =(fn_sra_srl(_operand1,_operand2,_immediate_value[11:0],_funct7[5], ~_opcode[3]));
        else if(_funct3==3b110) //OR/ORI
            if (_opcode[3]==1) 
              alu_result =(_operand1 | _operand2);
            else 
              alu_result =(_operand1 | signExtend(_immediate_value[11:0]));
        else if (_funct3==3'b111) //AND/ANDI
            if (_opcode[3]==1) 
              alu_result =(_operand1 & _operand2);
            else 
              alu_result =(_operand1 & signExtend(_immediate_value[11:0]));
      end
	return Alu_output{aluresult:alu_result};
	endfunction
	
endpackage
