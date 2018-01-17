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
Author's Name 	: Arjun C. Menon
e-mail id	: c.arjunmenon@gmail.com
Last updated on : 23rd October 2013

	This unit Executes the FPU Compare and Min/Max instructions namely, FMIN.S, FMAX.S, FEQ.S, FLT.S, FLE.S, FMIN.D, FMAX.D, FEQ.D, FLT.D, FLE.D
The unit computes the result and Enqueues it in a FIFO in one clock cycle.

*********Performance****************:
Using UMCIP 65nm library in SYNOPSYS
Critical Path Length     :    0.44 ns
Max. Operating Frequency :    2.27 GHz
Combinational Cell Count :    1125
Sequential Cell Count    :    142

The critical path is _start/operand1 ff_result.
*/

package fpu_compare_min_max;
import riscv_types::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;

interface Ifc_fpu_compare_min_max ;
	method Action _start(Bit#(64) operand1, Bit#(64) operand2, bit is_dp, Bit#(3) which_cmp_instr , bit cmp_or_min_max, Bit#(5) destination,  Bit#(32) fsr, Bit#(4) rob_number, Bit#(32) pc);	
	method Output_type result_();					// output method
	method Action _deque_buffer();					// input method to deque output buffer
	method Action _set_flush(Bool f);				// input method to initiate the flush routine
endinterface

(*noinline*)
function Bit#(2) fn_fp_comparitor(Bit#(64) fp1, Bit#(64) fp2, bit is_dp);
	Bit#(53) mantissa1;
	Bit#(11) exponent1;
	bit	 sign1;
	Bit#(53) mantissa2;
	Bit#(11) exponent2;
	bit	 sign2;

	if(is_dp==0) begin					//checking if the input is Single Precision(SP) Floating Point (FP) number	
		exponent1= {3'd0,fp1[30:23]};			//appending 3 zeros so as to make the 8-bit exponent 11-bits
		sign1= fp1[31];
		exponent2= {3'd0,fp2[30:23]};
		sign2= fp2[31];
	end
	else begin						//the input is Double Precision(DP) FP number
		exponent1= fp1[62:52];
		sign1= fp1[63];
		exponent2= fp2[62:52];
		sign2= fp2[63];
	end

	if(exponent1==0) begin					//if input 1 is denormal
		if(is_dp==0)					//if input is SPFP
			mantissa1={30'd0,fp1[22:0]};		//implicit bit is 0
		else						//the input is DPFP
			mantissa1={1'd0,fp1[51:0]};		//implicit bit is 1
	end
	else begin						//if input 1 is normal
		if(is_dp==0)					//if input is SPFP
			mantissa1={29'd0,1'b1,fp1[22:0]};	//implicit bit is 1
		else						//the input is DPFP
			mantissa1={1'd1,fp1[51:0]};		//implicit bit is 1
	end

	if(exponent2==0) begin					//if input 2 is denormal
		if(is_dp==0)					//if input is SPFP
			mantissa2={30'd0,fp2[22:0]};		//implicit bit is 0
		else						//the input is DPFP
			mantissa2={1'd0,fp2[51:0]};		//implicit bit is 0
	end
	else begin						//if input 2 is normal
		if(is_dp==0)					//if input is SPFP
			mantissa2={29'd0,1'b1,fp2[22:0]};	//implicit bit is 1
		else						//the input is DPFP
			mantissa2={1'd1,fp2[51:0]};		//implicit bit is 1
	end

	Bit#(2) magnitude;					//01 means inp2's magnitude is greater than inp1's magnitude
								//10 means inp1's magnitude is greater than inp2's magnitude
								//11 means inp2's magnitude is equal to inp1's magnitude
	if(exponent1<exponent2)
		magnitude= 2'b01;
	else if(exponent1== exponent2)
	begin
		if(mantissa1<mantissa2)
			magnitude= 2'b01;
		else if(mantissa1==mantissa2)
			magnitude= 2'b11;
		else magnitude= 2'b10;
	end
	else
		magnitude= 2'b10;

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

(*synthesize*)
module mkfpu_compare_min_max(Ifc_fpu_compare_min_max);
	
	FIFO#(Output_type) ff_result<- mkPipelineFIFO();// FIFO which stores the result.
							// ff_result is a FIFO of depth=1; it can be enq and dequed at the same time when data is present in it.

	Wire#(Bool) wr_flush <-mkDWire(False); 		// wire to indicate that a flush has occured in the processor and all buffer need to be cleared.

	rule rl_flush_output_fifo(wr_flush);
		ff_result.clear();
	endrule
  
	method Action _start(Bit#(64) operand1, Bit#(64) operand2, bit is_dp, Bit#(3) which_cmp_instr , bit cmp_or_min_max, Bit#(5) destination,  Bit#(32) fsr, Bit#(4) rob_number, Bit#(32) pc);

		Exception lv_exception= tagged No_exception;
		Bit#(64) lv_result= 0;
		bit lv_compare_is_invalid= 0;
		bit lv_compare_is_zero= 0;
		Bit#(2) lv_compare_result= fn_fp_comparitor(operand1, operand2, is_dp);
	
		if(is_dp==0) begin			//checking if the input is single precision
			if((operand1[30:23]=='hff && operand1[22:0]!=0) || (operand2[30:23]=='hff && operand2[22:0]!=0))
				lv_compare_is_invalid= 1;
			if(operand1[30:0]==0 && operand2[30:0]==0)
				lv_compare_is_zero= 1;
			end

		else begin
			if((operand1[62:52]=='hff && operand1[51:0]!=0) || (operand2[62:52]=='hff && operand2[51:0]!=0))
				lv_compare_is_invalid=1;
			if(operand1[62:0]==0 && operand2[62:0]==0)
				lv_compare_is_zero= 1;
		end


		if(lv_compare_is_invalid==1)begin
				lv_result='d0;							//TODO what value needs to be assigned when the operation is invalid
				lv_exception= tagged Inexact True;
			end
		else begin
			if(cmp_or_min_max=='b1) begin						//checking if compare instruction
				if(which_cmp_instr==3'b010) begin				//FEQ.D, FEQ.S
					//$display("FEQ");
					if(lv_compare_result==2'b11 || lv_compare_is_zero==1)	//checking if op1=op2
						lv_result[0]=1;					//writing result
				end
				else if(which_cmp_instr==3'b001) begin				//FLT.D, FLT.S
					//$display("FLT");
					if(lv_compare_result==2'b01 && lv_compare_is_zero==0)	//checking if op1<op2
												//Also, if op1 is -0 and op2 is +0, lv_result[0] should be zero. lv_compare_is_zero takes care of that
						lv_result[0]=1;					//writing result
				end
				else if(which_cmp_instr==3'b000) begin				//FLE.D, FLE.S
					//$display("FLE");
					if(lv_compare_result[0]==1'b1 || lv_compare_is_zero==1)	//checking if op1<=op2; since less than is 01 and equal to is 11, enough to check LSB
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
	
			ff_result.enq( Output_type{
						     destination	: destination,
						     fsr		: fsr,
						     final_result	: lv_result,
						     exception	  	: lv_exception 
					 	  });
	  endmethod

	//Output method which returns the result of the operation
	method Output_type result_();
		return ff_result.first();
	endmethod


	// when a flush is initiated in the processor this method will also be called.
	// it sets the flsuh wire to True
	method Action _set_flush(Bool f);
		wr_flush<=f;
	endmethod

	// this method is called once the data from the output_ FIFO has been read in the top module
	method Action _deque_buffer();
		ff_result.deq();
	endmethod

  
endmodule
endpackage
