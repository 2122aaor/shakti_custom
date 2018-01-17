/*

Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name 	: Single and Double Precision Floating Point Classify Module  
Author Name 	: Vinod.G, Renuka Venkat
e-mail Id	: g.vinod1993@gmail.com, renukacv21.95@gmail.com
Last updated on : 19th January 2016

This module examines the floating point input and outputs the class of floating point number. The corresponding bit of the output register is set according to the class of instructions given by the table below,
                           bit                  Class
                            0                   Negative Infinity
                            1                   Negative Normal Number 
                            2                   Negative Subnormal Number
                            3                   Negative Zero
                            4                   Positive Zero
                            5                   Positive Subnormal Number
                            6                   Positive Normal Number
                            7                   Positive Infinity
                            8                   Signaling NaN
                            9                   Quiet NaN

The module examines and finds the class as follows 
Sign	Exponent (e)	Fraction (f)	Value
0	00⋯00	          00⋯00	        +0

0	00⋯00	          00⋯01        Positive Denormalized Real
                          ⋮                   0.f × 2(−b+1)
                          11⋯11	

0       00⋯01             XX⋯XX	       Positive Normalized Real
           ⋮                                   1.f × 2(e−b)                         
        11⋯10	            

0	11⋯11	          00⋯00	        +∞

0	11⋯11	          00⋯01        SNaN
                          ⋮
                          01⋯11	

0	11⋯11	          1X⋯XX	       QNaN


1	00⋯00	          00⋯00	       −0

1	00⋯00	          00⋯01        Negative Denormalized Real
                          ⋮                   −0.f × 2(−b+1)
                          11⋯11	

1	00⋯01             XX⋯XX	       Negative Normalized Real
        ⋮                                      −1.f × 2(e−b)
        11⋯10	

1	11⋯11	          00⋯00	       −∞

1	11⋯11	          00⋯01        SNaN
                          ⋮
                          01⋯11	

1	11⋯11	          1X⋯XX	       QNaN



*/
package fpu_fclass;

import FIFO::*;
import riscv_types::*;
import SpecialFIFOs::*;
/*typedef struct{
	      Bit#(5) destination;				      // holds the destination address of the operation in the registerfile
	      Bit#(32) fsr;				       	      // the file status register containing all the flags and control bits
	      Bit#(n) final_result;				      // the final result for the operation
//	      Exception exception; 				      // indicates if any exception is generated.
	      }Output_type#(numeric type n) deriving(Bits,Eq);		      // data structure of the output FIFO.
*/								      //TODO Custom ROB Entry
interface Ifc_fpu_fclass#(numeric type fpinp, numeric type fpman, numeric type fpexp);
	method Action _start(Bit#(fpinp) operand1);
        method Action _deque_buffer_();
        method Action _set_flush(Bool _flush);
        method Output_type result_(); 
        method Bool ready_();
endinterface

module mkfpu_fclass(Ifc_fpu_fclass#(fpinp,fpman,fpexp))
	provisos (
		 Add#(TAdd#(fpexp,fpman),1,fpinp),	                //Defining fpinp to be fpexp + fpman + 1
                 Add#(fpexp,2,fpexp2)
		 );

	let fPINP  = valueOf(fpinp);
        let fPMAN  = valueOf(fpman);
        let fPEXP  = valueOf(fpexp);
        FIFO#(Output_type) ff_final_out <- mkPipelineFIFO();

	Reg#(Bool) rg_ready_signal <- mkReg(False);                     // Indicating output value in output_FIFO
        Wire#(Bool) wr_flush <- mkDWire(False);
       

        rule rl_flush(wr_flush);
		ff_final_out.clear();
		rg_ready_signal<= False;
	endrule

	method Action _start(Bit#(fpinp) operand1);
	
	Bit#(10) result_fclass = 'd0;
	Bit#(fpman) mantissa;
        Bit#(fpexp) exponent;
	bit sign1;
	
	//getting the exponent and mantissa of each operand.....
		exponent = operand1[fPINP-2:fPMAN];
                sign1 = operand1[fPINP-1];
                mantissa = operand1[fPMAN-1:0];
	
	bit exp_is_ones = &(exponent);//=>1,&(11111111) ;=>0,&(10101010)
	bit man_is_ones = &(mantissa);
	bit exp_not_zero = |(exponent);//=>1,&(11010101) ;=>0,|(00000000)
	bit man_not_zero = |(mantissa);	
	
	if(sign1 == 1 && exp_is_ones == 1 && man_not_zero == 0)  //negtive infinity
	begin	
	result_fclass[0] = 'b1;	
//	$display("Negative Infinity");
	end

	
	else if (operand1[fPMAN-1]==0 && operand1[0]==1) //Signaling NaN	
	begin	
	result_fclass[8] = 'b1; 
//	$display("Signaling NaN");
	end

	else if(sign1==0 && exp_is_ones == 1 && man_not_zero == 0) //positive infinity
	begin
	result_fclass[7] = 'b1;	
//	$display("Positive Infinity");
	end

	else if (exp_is_ones == 1 && operand1[fPMAN-1]==1) //quite NaN	
	begin
	result_fclass[9] = 'b1; 
//	$display("Quite Nan");
	end

	else if(sign1 == 1 && exp_not_zero == 1)  //negative normal
	begin	
	result_fclass[1] = 'b1;
//	$display("Negative Normal");
	end

	else if(sign1==0 && exp_not_zero == 1)  //positive normal
	begin
	result_fclass[6] = 'b1;
//	$display("Positive Normal");
	end

	else if(sign1 == 1 &&  exp_not_zero == 0 && man_not_zero == 1) //negative subnormal
	begin
	result_fclass[2] = 'b1;
//	$display("Negative Subnormal");
	end

	else if( sign1==0 && exp_not_zero == 0 && man_not_zero == 1) //positive subnormal	
	begin
	result_fclass[5] = 'b1;
//	$display("Positive Subnormal");
	end

	else if(sign1 == 1 && exp_is_ones == 0 && man_is_ones == 0)  //-0
	begin
	result_fclass[3] = 'b1;
//	$display("Negative Zero");
	end

	else if(sign1==0 && exp_not_zero == 0 && man_not_zero == 0) // +0
	begin	
	result_fclass[4] = 'b1;
//	$display("Positive Zero");
	end
		
	ff_final_out.enq(Output_type { final_result : zeroExtend(result_fclass) });
	endmethod

	method Output_type result_(); 
	     return ff_final_out.first();
	endmethod

	method Action _set_flush(Bool _flush);
		wr_flush<= True;
	endmethod

method Action _deque_buffer_();	
	        ff_final_out.deq();
		endmethod


endmodule

(*synthesize*)
	module mkTb_fpu_fclass();
	Ifc_fpu_fclass#(32,23,8) inst_fpu_fclass <- mkfpu_fclass();
	Reg#(Bit#(32)) rg_clock <- mkReg(0);
//Reg#(Bit#(32)) rg_operand1<-mkReg('hff800000); //negative infinity set 0
//Reg#(Bit#(32)) rg_operand1<-mkReg('h7f800000); //positive infinity set 7
//Reg#(Bit#(32)) rg_operand1<-mkReg('h807f0fff); //negative subnormal set 2
//Reg#(Bit#(32)) rg_operand1<-mkReg('h006abf8c); //positive subnormal set 5
//Reg#(Bit#(32)) rg_operand1<-mkReg('h80000000); //negative zero set 3
//Reg#(Bit#(32)) rg_operand1<-mkReg('h00000000); //positive zero set 4
//Reg#(Bit#(32)) rg_operand1<-mkReg('hff80f000); //negative normal set 1
Reg#(Bit#(32)) rg_operand1<-mkReg('h7f8ff000); //positive normal set 6
//Reg#(Bit#(32)) rg_operand1<-mkReg('h7fcf87f5); //quite Nan set 9
//Reg#(Bit#(32)) rg_operand1<-mkReg('h7fbfffff); //Signaling Nan set 8
	rule get_input(rg_clock == 0);
	inst_fpu_fclass._start(rg_operand1);
	rg_clock <= rg_clock + 1;
	endrule

	rule get_output;
	let lv_result = inst_fpu_fclass.result_();  
	$display("Result is: %h",lv_result.final_result);
	$finish(0);
	endrule



endmodule		 

     

endpackage

