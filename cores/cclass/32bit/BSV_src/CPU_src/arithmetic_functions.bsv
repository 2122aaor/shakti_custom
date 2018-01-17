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


package arithmetic_functions;
`include "defined_parameters.bsv"

//************** ADD/SUB INSTRUCTIONS **************************//
function  Bit#(`Reg_width) fn_addsub (Bit#(`Reg_width) _in1 , Bit#(`Reg_width) _in2 , Bit#(1) _sub_flag);
	Bit#(`Reg_width) op1=_in1;
	Bit#(`Reg_width) op2=_in2;
	
   		//ADD ADDI SUB
	if (_sub_flag==1) 
    op2 = ~_in2 + 1;

	return (op1 + op2);

endfunction

function Bit#(`Reg_width) multiplication (Bit#(`Reg_width) in1,Bit#(`Reg_width) in2,Bit#(3) funct3);
	Bit#(TMul#(2,`Reg_width)) op1= ((funct3[0]^funct3[1])==1 && in1[`Reg_width-1]==1)?zeroExtend((~in1)+1):zeroExtend(in1);
	Bit#(TMul#(2,`Reg_width)) op2= (funct3[1:0]==1 && in2[`Reg_width-1]==1)?zeroExtend((~in2)+1):zeroExtend(in2);
	Bit#(1) lv_take_complement = 0;
	if(funct3[1:0]==1)
		lv_take_complement=((in1[`Reg_width-1]^in2[`Reg_width-1])==1)?1:0;
	else if(funct3[1:0]==2)
		lv_take_complement=in1[`Reg_width-1];
	
	let product=op1*op2;
	product=(lv_take_complement==1)?(~product+1): product;
	if(funct3==0)
			return product[`Reg_width-1:0];
	else
			return product[2*`Reg_width-1:`Reg_width];
endfunction

//************** SET LESS THAN SIGNED AND UNSIGNED **************************//

//Function create a module for comparator which is then used multiple times
//Reduces hardware
function Bool fn_comparator( Bit#(`Reg_width) _op1, Bit#(`Reg_width) _op2);
	return (_op1 <_op2);
endfunction

//SIGNED INSTRUCTIONS
function Bit#(`Reg_width) fn_slt(Bit#(`Reg_width)_in1, Bit#(`Reg_width) _in2 , Bit#(1) _imm_flag, Bit#(1) unsign);
	//SLT,SLTI 
	Bit#(`Reg_width) rs1 = _in1;
	Bit#(`Reg_width) rs2 = _in2;
  let x= fn_comparator(rs1,rs2);

	if (unsign==0 && rs1[`Reg_width-1]==1 && rs2[`Reg_width-1]==0) // signed an rs1 is negative
    return 1;
	else if ((~(rs1[`Reg_width-1] ^ rs2[`Reg_width-1])==1 || unsign==1) && fn_comparator(rs1,rs2)) // unsigned or (rs1 and rs2 are same signs)
    return 1;
	else 
    return 0;
endfunction


//************************* SHIFT LEFT LOGICAL**************************//

//Function create a module for shift left which is then used multiple times
//Reduces hardware

function Bit#(`Reg_width) fn_shiftleft (Bit#(`Reg_width) _input, Bit#(6) _shiftamt);
	return _input << _shiftamt;
endfunction

//*****************SHIFT RIGHT LOGICAL/ARITHMETIC INSTRUCITONS ******************//


//Function create a module for shift right which is then used multiple times
//Reduces hardware
function Int#(32) fn_shiftright (Int#(33) _input , Bit#(5) _shiftamt);
	return truncate(_input >> _shiftamt);
endfunction

//SRL/SRLI/SRAI/SRA
function Bit#(`Reg_width) fn_sra_srl(Bit#(`Reg_width) _in1,Bit#(`Reg_width) _in2, Bit#(1) rl_ra_flag, Bit#(1) _imm_flag, Bit#(1) shift_left);
	Bit#(5) shift_amt=0;

  shift_amt=_in2[4:0];
  
  Bit#(32) temp=0;
  if(shift_left==1)
    _in1=reverseBits(_in1);
  temp= pack(fn_shiftright(unpack({_in1[31]&rl_ra_flag,_in1}),shift_amt));
  if(shift_left==1)
    temp=reverseBits(temp);
  
    return temp;
endfunction

endpackage

