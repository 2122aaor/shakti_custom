/*

Copyright (c) 2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name     : 64 bit ALU
Author Name     : Neel Gala, Sumanth Sridhar, Vinod.G
e-mail Id       : neelgala@gmail.com, sumanthsridhar.009@gmail.com, g.vinod1993@gmail.com
Last updated on : 5th November 2016
*/

package ALU64;

//************** ADD/SUB INSTRUCTIONS **************************//
(*noinline*)
function  Bit#(64) fn_addsub (Bit#(64) _in1 , Bit#(64) _in2 , Bit#(32) _immediate , Bit#(1) _imm_flag, Bit#(1) _sub_flag , Bit#(1) _w_flag);

	Bit#(64) op1= (_w_flag== 1'b1)? signExtend(_in1[31:0]) : _in1;
	Bit#(64) op2=0;
	Bit#(64) in2= (_w_flag== 1'b1)? signExtend(_in2[31:0]) : _in2;
	
   		//ADD ADDI SUB
	if (_imm_flag==1'b1) op2 = signExtend(_immediate);
	else if (_sub_flag==1) op2 = ~in2 + 1;
	else op2  = in2;

	Bit#(64) out = (op1+op2) ;

	if (_w_flag==1'b1)
		return signExtend(out[31:0]);
	else
		return out;

endfunction

function  Bit#(64) fn_addsub_inline (Bit#(64) _in1 , Bit#(64) _in2 , Bit#(32) _immediate , Bit#(1) _imm_flag, Bit#(1) _sub_flag , Bit#(1) _w_flag);

	Bit#(64) op1= (_w_flag== 1'b1)? signExtend(_in1[31:0]) : _in1;
	Bit#(64) op2=0;
	Bit#(64) in2= (_w_flag== 1'b1)? signExtend(_in2[31:0]) : _in2;
	
   		//ADD ADDI SUB
	if (_imm_flag==1'b1) op2 = signExtend(_immediate);
	else if (_sub_flag==1) op2 = ~in2 + 1;
	else op2  = in2;

	Bit#(64) out = (op1+op2) ;

	if (_w_flag==1'b1)
		return signExtend(out[31:0]);
	else
		return out;

endfunction

//************** SET LESS THAN SIGNED AND UNSIGNED **************************//

//Function create a module for comparator which is then used multiple times
//Reduces hardware
(*noinline*)
function Bool fn_comparator( Bit#(64) _op1, Bit#(64) _op2);
	return (_op1 <_op2);
endfunction

//SIGNED INSTRUCTIONS
(*noinline*)
function Bit#(64) fn_slt(Bit#(64)_in1, Bit#(64) _in2, Bit#(32)_immediate , Bit#(1) _imm_flag);
	//SLT,SLTI 
	Bit#(64) rs1 = _in1;
	Bit#(64) rs2 = (_imm_flag==1'b1) ? signExtend(_immediate) : _in2;

	if (rs1[63]==1'b1 && rs2[63]==1'b0) return 1;
	else if ( ((rs1[63] ^ rs2[63])==1'b0) && fn_comparator(rs1,rs2)) return 1;
	else return 0;
endfunction

function Bit#(64) fn_slt_inline(Bit#(64)_in1, Bit#(64) _in2, Bit#(32)_immediate , Bit#(1) _imm_flag);
	//SLT,SLTI 
	Bit#(64) rs1 = _in1;
	Bit#(64) rs2 = (_imm_flag==1'b1) ? signExtend(_immediate) : _in2;

	if (rs1[63]==1'b1 && rs2[63]==1'b0) return 1;
	else if ( ((rs1[63] ^ rs2[63])==1'b0) && fn_comparator(rs1,rs2)) return 1;
	else return 0;
endfunction

//UNSIGNED INSTRUCTIONS
(*noinline*)
function Bit#(64) fn_sltu(Bit#(64)_in1, Bit#(64) _in2, Bit#(32)_immediate , Bit#(1) _imm_flag);
	//SLTU,SLTIU
	Bit#(64) rs1 = _in1;
	Bit#(64) rs2 = (_imm_flag==1'b1) ? signExtend(_immediate) : _in2;

	if (fn_comparator(rs1,rs2)) return 1;
	else return 0;
endfunction

function Bit#(64) fn_sltu_inline(Bit#(64)_in1, Bit#(64) _in2, Bit#(32)_immediate , Bit#(1) _imm_flag);
	//SLTU,SLTIU
	Bit#(64) rs1 = _in1;
	Bit#(64) rs2 = (_imm_flag==1'b1) ? signExtend(_immediate) : _in2;

	if (fn_comparator(rs1,rs2)) return 1;
	else return 0;
endfunction

//************************* SHIFT LEFT LOGICAL**************************//

//Function create a module for shift left which is then used multiple times
//Reduces hardware
(*noinline*)
function Bit#(64) fn_shiftleft (Bit#(64) _input, Bit#(6) _shiftamt, Bit#(1) _w_flag);
	
	Bit#(64) out	= (_input << _shiftamt);

	if (_w_flag==1'b1)
		return signExtend(out[31:0]);
	else
		return out;
endfunction

// For both sll and sllw
(*noinline*)
function Bit#(64) fn_sll( Bit#(64) _in1 , Bit#(64) _in2 , Bit#(6) _immediate , Bit#(1) _imm_flag, Bit#(1) _w_flag);

	Bit#(64) in1 = (_w_flag==1'b1)? signExtend(_in1[31:0]) : _in1;
	Bit#(6) shift_amt=0;
	
	if (_imm_flag==1'b1) shift_amt= _immediate;
	else shift_amt = _in2[5:0];
	return (fn_shiftleft (in1,shift_amt, _w_flag));
		
endfunction

function Bit#(64) fn_sll_inline( Bit#(64) _in1 , Bit#(64) _in2 , Bit#(6) _immediate , Bit#(1) _imm_flag, Bit#(1) _w_flag);

	Bit#(64) in1 = (_w_flag==1'b1)? signExtend(_in1[31:0]) : _in1;
	Bit#(6) shift_amt=0;
	
	if (_imm_flag==1'b1) shift_amt= _immediate;
	else shift_amt = _in2[5:0];
	return (fn_shiftleft (in1,shift_amt, _w_flag));
		
endfunction

//*****************SHIFT RIGHT LOGICAL/ARITHMETIC INSTRUCITONS ******************//


//Function create a module for shift right which is then used multiple times
//Reduces hardware
(*noinline*)
function Bit#(64) fn_shiftright (Bit#(128) _input , Bit#(6) _shiftamt, Bit#(1) _w_flag);
	Bit#(64) out = (_input >> _shiftamt)[63:0];

	if (_w_flag==1'b1)
		return signExtend(out[31:0]);
	else
		return out;
endfunction

(*noinline*)//SRL/SRLI/SRLW/SRLIW/SRAI/SRA/SRAIW/SRAW
function Bit#(64) fn_sra_srl(Bit#(64) _in1,Bit#(64) _in2,Bit#(6) _immediate, Bit#(1) _imm_flag, Bit#(1) _sra_flag, Bit#(1) _w_flag);
	Bit#(6) shift_amt=0;

	Bit#(64) in1 = (_w_flag==1'b1)? signExtend(_in1[31:0]) : _in1;
	
	if (_imm_flag==1'b1) shift_amt= _immediate;
	else shift_amt= _in2[5:0];

	if (in1[63] ==0 || _sra_flag==0) return fn_shiftright({'0,in1},shift_amt,_w_flag);
	else return fn_shiftright({'1,in1},shift_amt,_w_flag);
 
endfunction

function Bit#(64) fn_sra_srl_inline(Bit#(64) _in1,Bit#(64) _in2,Bit#(6) _immediate, Bit#(1) _imm_flag, Bit#(1) _sra_flag, Bit#(1) _w_flag);
	Bit#(6) shift_amt=0;

	Bit#(64) in1 = (_w_flag==1'b1)? signExtend(_in1[31:0]) : _in1;
	
	if (_imm_flag==1'b1) shift_amt= _immediate;
	else shift_amt= _in2[5:0];

	if (in1[63] ==0 || _sra_flag==0) return fn_shiftright({'0,in1},shift_amt,_w_flag);
	else return fn_shiftright({'1,in1},shift_amt,_w_flag);
 
endfunction



//**************************************** ALU Module ****************************************//

interface ALU64Ifc;
	method Bit#(64) doALUOp (Bool isVectorCompare, Bit#(3) funct3, 
		Bit#(64) _in1, Bit#(64) _in2, 
		Bit#(1) _w_flag, Bit#(1) _alt_flag);
endinterface: ALU64Ifc

(* synthesize *)
module mkLaneALU (ALU64Ifc);

	method Bit#(64) doALUOp (Bool isVectorCompare, Bit#(3) funct3, Bit#(64) _in1, Bit#(64) _in2, Bit#(1) _w_flag, Bit#(1) _alt_flag);
		if (isVectorCompare) begin
			case (funct3)
				'd0	: return ((_in1==_in2)? 'b1 : 'b0);				// vcmpeq
				'd4	: return fn_slt_inline (_in1, _in2, '0, '0);	// vcmplt
				'd6	: return fn_sltu_inline (_in1, _in2, '0, '0);	// vcmpltu
			endcase
		end else begin
			case (funct3)
				'd0	:	begin
							return fn_addsub_inline (_in1, _in2, '0, '0, _alt_flag, _w_flag);
						end
				'd1	:	begin
							return fn_sll_inline (_in1, _in2, '0, '0, _w_flag);
						end
				'd2	:	begin
							return fn_slt_inline (_in1, _in2, '0, '0);
						end
				'd3	:	begin
							return fn_sltu_inline (_in1, _in2, '0, '0);
						end
				'd4	:	begin
							return (_in1 ^ _in2);
						end
				'd5	:	begin
							return fn_sra_srl_inline (_in1, _in2, '0, '0, _alt_flag, _w_flag);
						end
				'd6	:	begin
							return (_in1 | _in2);
						end
				'd7	:	begin
							return (_in1 & _in2);
						end
			endcase
		end

	endmethod: doALUOp

endmodule: mkLaneALU

endpackage
