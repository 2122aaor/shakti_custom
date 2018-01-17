/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name   	: FPU Compare and Min/Max execution unit
Author's Name 	: Arjun C. Menon, Vinod.G, Aditya Govardhan
e-mail id		: c.arjunmenon@gmail.com, g.vinod1993@gmail.com, dtgovardhan@gmail.com
Last updated on : 6th June, 2016

	This unit Executes the FPU Compare and Min/Max instructions namely, FMIN.S, FMAX.S, FEQ.S, FLT.S, FLE.S, FMIN.D, FMAX.D, FEQ.D, FLT.D, FLE.D
The unit computes the result and Enqueues it in a FIFO in one clock cycle.
*/


package fpu_compare_min_max;
import defined_types::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;

interface Ifc_fpu_compare_min_max#(numeric type fpinp, numeric type fpman, numeric type fpexp);
	method Action _start(Bit#(fpinp) operand1, Bit#(fpinp) operand2, Bit#(3) which_cmp_instr, bit cmp_or_min_max, Bit#(32) fsr);
	method Floating_output result_();
	method Action _deque_buffer();
endinterface

//(*noinline*)
function Bit#(2) fn_comparator(bit sign1, Bit#(fpexp) exponent1, Bit#(fpman1) mantissa1, bit sign2, Bit#(fpexp) exponent2, Bit#(fpman1) mantissa2);

	Bit#(2) magnitude;					//01 means inp2's magnitude is greater than inp1's magnitude
										//10 means inp1's magnitude is greater than inp2's magnitude
										//11 means inp2's magnitude is equal to inp1's magnitude
	if(exponent1<exponent2)
		magnitude= 2'b01;
	else if(exponent1==exponent2)
	begin
		if(mantissa1<mantissa2)
			magnitude = 2'b01;
		else if(mantissa1==mantissa2)
			magnitude = 2'b11;
		else magnitude = 2'b10;
	end
	else
		magnitude = 2'b10;

	if(sign1==0) begin
		if(sign2==1)
			return 2'b10;
		else 
			return magnitude;
	end
	else begin
		if(sign2==1)
			return {magnitude[0],magnitude[1]};
		else
			return 2'b01;
	end
endfunction

module mkfpu_compare_min_max(Ifc_fpu_compare_min_max#(fpinp, fpman, fpexp))
	provisos(
		    Add#(TAdd#(fpexp,fpman),1,fpinp),
			Add#(fpman,1,fpman1),
			//per request of bsc
			Add#(a__,fpinp,64)
		    );
	FIFO#(Floating_output) ff_result <- mkPipelineFIFO(); //FIFO Which stores the result. FIFO of depth 1.

	method Action _start(Bit#(fpinp) operand1, Bit#(fpinp) operand2, Bit#(3) which_cmp_instr,bit cmp_or_min_max, Bit#(32) fsr);
		let fPMAN = valueOf(fpman);
		let fPEXP = valueOf(fpexp);
		let fPINP = valueOf(fpinp);

        Exception lv_exception = None;

		Bit#(fpinp) lv_result = 0;
	    bit lv_invalid = 0;
		bit lv_zero = 0;
	    Bit#(fpman1) mantissa1, mantissa2;
		Bit#(fpexp) exponent1, exponent2;
	    bit sign1, sign2;

        sign1 = operand1[fPINP-1];
		exponent1 = operand1[fPINP-2:fPMAN];
    	bit exp1_is_ones = &(exponent1);
	 	bit exp1_is_not_zeros= |(exponent1);

		if(exp1_is_not_zeros==0)
			mantissa1 = {1'b0,operand1[fPMAN-1:0]};
		else
			mantissa1 = {1'b0,operand1[fPMAN-1:0]};

		sign2 = operand2[fPINP-1];
		exponent2 = operand2[fPINP-2:fPMAN];
		bit exp2_is_not_zeros= |(exponent2);
        bit exp2_is_ones = &(exponent2);

		if(exp2_is_not_zeros==0)
			mantissa2 = {1'b0,operand2[fPMAN-1:0]};
		else
			mantissa2 = {1'b0, operand2[fPMAN-1:0]};

	 	bit man1_is_ones = &(mantissa1);
	 	bit man2_is_ones = &(mantissa2);
        bit man1_is_not_zeros = |(mantissa1);
		bit man2_is_not_zeros = |(mantissa2);

		if(which_cmp_instr != 3'b010) begin  //Extra Condition because FEQ.S and FEQ.D compares quiet NaN's whereas all the other comparison is considered invalid according to spec.
			if((exp1_is_ones==1 && man1_is_not_zeros==1) || (exp2_is_ones == 1 && man2_is_not_zeros == 1))
				lv_invalid = 1;
		end
		else if((mantissa1[fPMAN-1] & mantissa2[fPMAN-1]) != 1)
			  lv_invalid = 1;

		else if((exp1_is_not_zeros==0 && man1_is_not_zeros==0) && (exp1_is_not_zeros==0 && man1_is_not_zeros==0))
			lv_zero = 1;

		Bit#(2) lv_compare_result = fn_comparator(sign1,exponent1,mantissa1,sign2,exponent2,mantissa2);

 		if(lv_invalid == 1) begin
			lv_result = 'd0;
			lv_exception = Invalid;
		end
        else begin
			if(cmp_or_min_max=='b0) begin						//checking if compare instruction
				if(which_cmp_instr==3'b010) begin				//FEQ.D, FEQ.S
					//$display("FEQ");
					if(lv_compare_result==2'b11 || lv_zero==1)	//checking if op1=op2
						lv_result[0]=1;					//writing result
				end
				else if(which_cmp_instr==3'b001) begin				//FLT.D, FLT.S
					//$display("FLT");
					if(lv_compare_result==2'b01 && lv_zero==0)	//checking if op1<op2
												//Also, if op1 is -0 and op2 is +0, lv_result[0] should be zero. lv_compare_is_zero takes care of that
						lv_result[0]=1;					//writing result
				end
				else if(which_cmp_instr==3'b000) begin				//FLE.D, FLE.S
					//$display("FLE");
					if(lv_compare_result[0]==1'b1 || lv_zero==1)	//checking if op1<=op2; since less than is 01 and equal to is 11, enough to check LSB
												//Also, if op1 is +0 and op2 is -0, lv_result[0] should be zero. lv_compare_is_zero takes care of that
						lv_result[0]=1;					//writing result
				end
			end
	
			else begin
				if((which_cmp_instr[0]==0 && lv_compare_result==2'b01) || (which_cmp_instr[0]==1 && lv_compare_result==2'b10))	//FMIN.D, FMIN.S, FMAX.D, FMAX.S
					lv_result= operand1;
				else
					lv_result= operand2;
			end
		end
	
			ff_result.enq( Floating_output{
						     fsr			: fsr,
						     final_result	: zeroExtend(lv_result),
						     exception	  	: lv_exception
					 	  });
	  endmethod
 	//Output method which returns the result of the operation
	method Floating_output result_();
		return ff_result.first();
	endmethod

	// this method is called once the data from the output_ FIFO has been read in the top module
	method Action _deque_buffer();
		ff_result.deq();
	endmethod

  endmodule

 //  module mkTb_fpu_compare_min_max(Empty);
 //  Reg#(Bit#(32)) rg_operand1 <- mkReg(32'hb3240000);
 //  Reg#(Bit#(32)) rg_operand2 <- mkReg(32'hac47ce40);
 //  Reg#(Bit#(32)) rg_clock <- mkReg(0);
 //  Ifc_fpu_compare_min_max#(32,23,8) inst <- mkfpu_compare_min_max();
  
 //  rule rg_clock_cnt;
	//   rg_clock <= rg_clock + 1;
 //  endrule

 //  rule rl_start_1(rg_clock=='d0);
	//   $display("Giving Inputs to the compare module");
	//   inst._start(rg_operand1, rg_operand2, 3'b000, 0,0,0,0,0);
 //  endrule

 //  rule rl_display_result;
	//   let abc = inst.result_();
	//   inst._deque_buffer();
	//   $display("Final Result = %h", abc.final_result);
 //  endrule

 //  rule end_clock(rg_clock == 'd10);
	//   $finish(0);
 //  endrule
 // endmodule 
endpackage
