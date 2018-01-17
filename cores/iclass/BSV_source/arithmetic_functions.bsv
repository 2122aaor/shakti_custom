/* Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


Module name: Arithmetic Functions.
author name: Rishi Naidu
Email id:    rishinaidu.rn@gmail.com
last update done: 26th September 2013 

Contains different arithmetic functions as per the instructions. (*noinline) is included before every function so that it creates different module at compilation

_word_flag =1 for word instructions in which the operation is carried out only on lower 32 bits
_imm_flag =1 for immediate instructions
_sub_flag =1 for subtract instructions
_rl_ra_flag=1 for shift right arithmetic and =0 for shift right logical
_immediate is the immediate value from instruction
*/



package  arithmetic_functions;

`include "defined_parameters.bsv"

//************** ADD/SUB INSTRUCTIONS **************************//
(*noinline*)
function  Bit#(`REG_WIDTH) fn_addsub( Bit#(`REG_WIDTH) _in1 , Bit#(`REG_WIDTH) _in2 , Bit#(1) _word_flag,  Bit#(1) _sub_flag );
	
	Bit#(32) op1_w=_in1[31:0];
	Bit#(32) op2_w=0;

	Bit#(64) op1=_in1;
	Bit#(64) op2=0;
	if (_word_flag==1) begin //ADDW ADDIW SUBW	(Word Instructions)
		if (_sub_flag==1) op2_w = ~_in2[31:0] +1;
		else op2_w = _in2[31:0]; 

		return ( signExtend(op1_w + op2_w));
	end
	else begin	//ADD ADDI SUB
		if (_sub_flag==1) op2 = ~_in2 + 1;
		else op2  = _in2;
	
		return (op1 + op2);
	end

endfunction

//************** SET LESS THAN SIGNED AND UNSIGNED **************************//

//Function create a module for comparator which is then used multiple times
//Reduces hardware
(*noinline*)
function Bool fn_comparator( Bit#(`REG_WIDTH) _op1, Bit#(`REG_WIDTH) _op2);
	return (_op1 <_op2);
endfunction

//SIGNED INSTRUCTIONS
(*noinline*)
function Bit#(`REG_WIDTH) fn_slt(Bit#(`REG_WIDTH)_in1, Bit#(`REG_WIDTH) _in2);
	//SLT,SLTI 
	Bit#(`REG_WIDTH) rs1 = _in1;
	Bit#(`REG_WIDTH) rs2 = _in2;

	if (rs1[63]==1 && rs2[63]==0) return 1;
	else if ((~(rs1[63] ^ rs2[63])==1) && fn_comparator(rs1,rs2)) return 1;
	else return 0;

endfunction

//UNSIGNED INSTRUCTIONS
(*noinline*)
function Bit#(`REG_WIDTH) fn_sltu(Bit#(`REG_WIDTH)_in1, Bit#(`REG_WIDTH) _in2);
	//SLTU,SLTIU
	Bit#(`REG_WIDTH) rs1 = _in1;
	Bit#(`REG_WIDTH) rs2 = _in2;

	if (fn_comparator(rs1,rs2)) return 1;
	else return 0;
endfunction

//************************* SHIFT LEFT LOGICAL**************************//

//Function create a module for shift left which is then used multiple times
//Reduces hardware
(*noinline*)
function Bit#(128) fn_shiftleft (Bit#(128) _input, Bit#(6) _shiftamt);
	return _input << _shiftamt;
endfunction

(*noinline*)
function Bit#(`REG_WIDTH) fn_sll( Bit#(`REG_WIDTH) _in1 , Bit#(`REG_WIDTH) _in2 , Bit#(1) _word_flag);
	Bit#(6) shift_amt=0;
	Bit#(5) shift_amt_word=0;

	if (_word_flag==1) begin //SLLW/SLLIW (Word Instructions)
		shift_amt_word= _in2[4:0];
		return signExtend(fn_shiftleft({_in1,64'b0},{0,shift_amt_word})[95:64]);
			end
	else begin//SLL/SLLI
		shift_amt = _in2[5:0];
		return (fn_shiftleft ({_in1,64'b0},shift_amt) [127:64]);
	end

endfunction

//*****************SHIFT RIGHT LOGICAL/ARITHMETIC INSTRUCITONS ******************//


//Function create a module for shift right which is then used multiple times
//Reduces hardware
(*noinline*)
function Bit#(128) fn_shiftright (Bit#(128) _input , Bit#(6) _shiftamt);
	return _input >> _shiftamt;
endfunction

(*noinline*)//SRL/SRLI/SRLW/SRLIW/SRAI/SRA/SRAIW/SRAW
function Bit#(`REG_WIDTH) fn_sra_srl(Bit#(`REG_WIDTH) _in1,Bit#(`REG_WIDTH) _in2, Bit#(1) _word_flag, Bit#(1) rl_ra_flag);
	Bit#(6) shift_amt=0;
	Bit#(5) shift_amt_word=0;

	`ifdef RV64
	if (_word_flag==1) begin //SRLW/SRLIW/SRAW/SRAIW (Word Instructions)
		shift_amt_word= _in2[4:0];
		
		if (_in1[31] ==0 || rl_ra_flag==0) return signExtend(fn_shiftright({96'b0,_in1[31:0]},{0,shift_amt_word})[31:0]);
		else return signExtend(fn_shiftright({96'hFFFFFFFFFFFFFFFFFFFFFFFF,_in1[31:0]},{0,shift_amt_word})[31:0]);
 
	end
	else begin//SRL/SRLI/SRA/SRAI
	
		shift_amt = _in2[5:0];
	
		if (_in1[63] ==0 || rl_ra_flag==0) return (fn_shiftright({64'b0,_in1},shift_amt)[63:0]);
		else return fn_shiftright({64'hFFFFFFFFFFFFFFFF,_in1},shift_amt)[63:0];	
	end
	`endif

endfunction




endpackage 

