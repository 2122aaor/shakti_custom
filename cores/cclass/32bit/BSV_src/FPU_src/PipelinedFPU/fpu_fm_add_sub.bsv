/*
Copyright (c) 2013-2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name   	: Pipelined Integer to Floating Point Conversion unit
Author's Name 	: Aditya Govardhan, Vinod.G
e-mail id	: dtgovardhan@gmail.com, g.vinod1993@gmail.com
Last updated on : 6th January 2016

*/

package fpu_fm_add_sub;

import functions::*; //Contains function for getting the number of leading zeros and trailing zeros in mantissa
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*; 
import integermultiplier_for_fma::*;
import defined_types::*;
import RegFile::*;

interface Ifc_fpu_fm_add_sub;

	//Input Methods
	method Action _start(Bit#(32) operand1, Bit#(32) operand2, Bit#(32) _operand3, Bit#(32) fsr, Bit#(3) rounding_mode, bit operation, bit _negate);  
	//Output Methods
	method Action deque_buffer();
	method Floating_output get_result();
	
endinterface

typedef struct{
	Bit#(24) mantissa1; 	// mantissa of operand1
	Bit#(24) mantissa2; 	// mantissa of operand2
    Bit#(32) _operand3;
	bit sign; 		// sign bit of the result
	Bit#(10) lv_summed_exponent;// exponent of the resultant
	Bit#(1) infinity;	// indicating that the ff_output is infinity.
	Bit#(1) invalid;	// indicating that the ff_output is NaN.
	Bit#(1) zero;		// indicating that the ff_output is zero.
	Bit#(32) fsr;		// the floating-point status register containing all the flags and control bits for dynamic rounding.
    Bit#(3) rounding_mode;  // static rounding mode encoded in the instruction
    bit _operation;
    bit _negate;
	}Input_data_type deriving (Bits,Eq);

typedef struct{
	Bit#(32) fsr;		// the floating-point status register containing all the flags and control bits for dynamic rounding.
    Bit#(3) rounding_mode;
    bit _negate;
    bit sign2;
    bit sign3;
    Bit#(49) mantissa2;
    Bit#(49) mantissa3;
    Bit#(10) exponent2;
    Bit#(10) exponent3;
    bit overflow;
    bit operation;
    bit result_is_invalid;
    Bit#(2) result_is_inf;
    Bit#(2) result_is_zero;
    }Stage3_data_type deriving (Bits,Eq);

typedef struct{
	Bit#(32) fsr;
	Bit#(3) rounding_mode;
	bit _negate;
	bit resultant_sign;
	Bit#(10) resultant_exponent;
	Bit#(49) resultant_mantissa;
	bit overflow;
	bit result_is_invalid;
	Bit#(2) result_is_inf;
	Bit#(2) result_is_zero;
	Bit#(2) add_sub_is_zero;
	}Stage4_data_type deriving (Bits,Eq);

module mkfpu_fm_add_sub(Ifc_fpu_fm_add_sub);
	FIFOF#(Floating_output) ff_final_out<-mkFIFOF(); 	// ff_output FIFO which holds the final result. This is of type FIFOF.
	FIFO#(Stage3_data_type) ff_stage3 <-mkPipelineFIFO();	// intermediate FIFO
	FIFO#(Stage4_data_type) ff_stage4 <-mkPipelineFIFO();
	FIFO#(Input_data_type) ff_input_register<-mkPipelineFIFO();	// input FIFO

	Ifc_integer_multiplier_for_fma integer_multiplier<-mkinteger_multiplier_for_fma; //Instance of the 4-Cylce Integer Multiplier.

	rule rl_stage1_after_input_stage;
		integer_multiplier._start({8'b0,ff_input_register.first().mantissa1}, {8'b0,ff_input_register.first().mantissa2},ff_input_register.first()._operand3, ff_input_register.first().fsr, ff_input_register.first().rounding_mode, ff_input_register.first().sign, ff_input_register.first().lv_summed_exponent, ff_input_register.first().invalid,ff_input_register.first().infinity,ff_input_register.first().zero,ff_input_register.first()._operation,ff_input_register.first()._negate);
		ff_input_register.deq();	
	endrule:rl_stage1_after_input_stage

	rule rl_stage_3_after_integer_multiplier_stage;
	Bit#(32) lv_fsr				= integer_multiplier.result_().fsr;                   
		Bit#(3) lv_rounding_mode	= integer_multiplier.result_().rounding_mode;
		Bit#(32) lv_operand3		= integer_multiplier.result_()._operand3;
		Bit#(10) lv_summed_exponent = integer_multiplier.result_().summed_exponent();
		bit lv_sign					= integer_multiplier.result_().sign;
		bit operation				= integer_multiplier.result_().operation;
		bit lv_negate 				= integer_multiplier.result_()._negate;
		Bit#(1) lv_infinity 		= integer_multiplier.result_().infinity;
		Bit#(1) lv_invalid 			= integer_multiplier.result_().invalid;
		Bit#(1) lv_zero 			= integer_multiplier.result_().zero;
		Bit#(48) mult_mantissa		= integer_multiplier.result_().final_result[47:0];

	 	integer_multiplier._deque();

	 	// //$display("lv_summed_exponent %b mult_mantissa = %b out of integer multiplier", lv_summed_exponent, mult_mantissa);

	 	//Remmoving the leading zeros if any and shifting the mantissa
	 	Bit#(6) leading_mantissa_zeros = pack(countZerosMSB(mult_mantissa));
		mult_mantissa = mult_mantissa << leading_mantissa_zeros;

		lv_summed_exponent = lv_summed_exponent - {4'b0000, leading_mantissa_zeros} + 1; // considered only for normal numbers

		// //$display("lv_summed_exponent %b mult_mantissa = %b after shifting mantissa", lv_summed_exponent, mult_mantissa);

		bit lv_underflow = 0;
		bit lv_overflow = 0;

		if(lv_summed_exponent[9] == 1) begin
			lv_underflow = 1;
			lv_summed_exponent = 'd0;
			mult_mantissa = 'd0; //If underflow, number would be rounded to zero
		end
		else if(lv_summed_exponent[7:0] == 'd-1 || lv_summed_exponent[8] == 1) begin
			lv_overflow = 1;
			// mult_mantissa = 0;
			// lv_summed_exponent = {2'b00, 'd-1};
		end
		// //$display("lv_summed_exponent %b mult_mantissa = %b after overflow underflow adjustment", lv_summed_exponent, mult_mantissa);

		Bit#(1) sign2 = lv_sign;
		Bit#(10) exponent2 = lv_summed_exponent;
		Bit#(49) mantissa2 = {0,mult_mantissa}; // To have one MSB bit 0 for the carry out

		Bit#(1) sign3 = lv_operand3[31];
		Bit#(10) exponent3 = {2'b0, lv_operand3[30:23]};
		Bit#(49) mantissa3 = 0;

		Bit#(23) check_mantissa3 = lv_operand3[22:0];
		bit lv_op3_is_invalid = 0;
		bit lv_op3_is_inf = 0;
		bit lv_op3_is_zero = 0;

		if(exponent3 == 10'b0011111111) begin
			if(check_mantissa3 == 0) lv_op3_is_inf = 1;
			else if(check_mantissa3 != 0) lv_op3_is_invalid = 1;
		end
		else if(exponent3 == 10'b0000000000) begin
			if(check_mantissa3 == 0) lv_op3_is_zero = 1;
			//else if(check_mantissa3 != 0) mantissa3 = {2'b0, lv_operand3[22:0], 39'b0}; //denormal case
		end
		else mantissa3 = {2'b01, lv_operand3[22:0], 24'b0};

		// //$display("sign2 %b exponent2 %b mantissa2 %b sign3 %b exponent3 %b mantissa3 %b", sign2, exponent2, mantissa2, sign3, exponent3, mantissa3);

		bit result_is_inf = 0;
		bit result_is_invalid = 0;
		bit result_is_zero = 0;
		bit result_sign = 0;

    	bit lv_round_down = 0;
    	if(lv_rounding_mode == 'b010) lv_round_down = 1;

    	//Result is invalid cases
		if(lv_op3_is_invalid == 1 || lv_invalid == 1)
			result_is_invalid = 1;

		//Result is zero cases
		else if(lv_op3_is_zero == 1 && (lv_zero == 1 || lv_underflow == 1)) begin
		// //$display("both zero");
			if(lv_round_down==1 &&(sign2|(operation^sign3))==1) begin                        //Using Karnaugh maps, determining if its -0 or +0
				result_is_zero= 1;
				result_sign = 1;                                                                //Minus zero
			end	 
	  		else if (lv_round_down==0 && (sign2&(operation^sign3))==1) begin                 //Using Karnaugh maps, determining if its -0 or +0
				result_is_zero = 1;	
				result_sign = 1;			                                 //Minus zero
			end
			else begin
				result_is_zero = 1;
				result_sign = 0;				       	                         //Plus zero
			end
		end

		//Result is infinity cases
		else if(lv_infinity == 1 && lv_op3_is_inf == 1) begin
			result_is_inf = ~(sign2 ^ (operation ^ sign3));
			result_is_invalid = ~ result_is_inf;
			result_sign = sign2;
		end
		else if(lv_infinity == 1 || lv_op3_is_inf == 1) begin
			result_is_inf = 1;
			result_sign = ((lv_infinity & ~lv_op3_is_inf) & sign2) | ((~lv_infinity & lv_op3_is_inf) & (operation ^ sign3));
		end

		ff_stage3.enq(Stage3_data_type{ 	
											fsr: lv_fsr,
											rounding_mode: lv_rounding_mode,
											_negate: lv_negate,
											sign2: sign2,
    										sign3: sign3,
    										mantissa2: mantissa2,
    										mantissa3: mantissa3,
    										exponent2: exponent2, 
    										exponent3: exponent3,
    										operation: operation,
    										overflow: lv_overflow,
    										result_is_invalid: result_is_invalid,
    										result_is_inf: {result_sign,result_is_inf},
    										result_is_zero: {result_sign,result_is_zero}});

	endrule:rl_stage_3_after_integer_multiplier_stage

	rule rl_stage_4 ;

		Bit#(32) lv_fsr				= ff_stage3.first().fsr;		
	    Bit#(3) lv_rounding_mode	= ff_stage3.first().rounding_mode;
	    bit lv_negate 				= ff_stage3.first()._negate;
	    bit lv_sign2				= ff_stage3.first().sign2;
	    bit lv_sign3				= ff_stage3.first().sign3;
	    Bit#(49) lv_mantissa2		= ff_stage3.first().mantissa2;
	    Bit#(49) lv_mantissa3		= ff_stage3.first().mantissa3;
	    Bit#(10) lv_exponent2		= ff_stage3.first().exponent2;
	    Bit#(10) lv_exponent3		= ff_stage3.first().exponent3;
	    bit lv_overflow				= ff_stage3.first().overflow;
	    bit lv_operation			= ff_stage3.first().operation;
	    bit lv_result_is_invalid	= ff_stage3.first().result_is_invalid;
	    Bit#(2) lv_result_is_inf	= ff_stage3.first().result_is_inf;
	    Bit#(2) lv_result_is_zero	= ff_stage3.first().result_is_zero;

	    ff_stage3.deq();

	    Bit#(10) exponent_difference = 0;
	    bit op2_gt_op3 = 0;
	    Bit#(10) resultant_exponent = 0;
	    
	    if(lv_exponent2 > lv_exponent3) begin
	    	exponent_difference = lv_exponent2 - lv_exponent3;
	    	op2_gt_op3 = 1;
	    	resultant_exponent = lv_exponent2;
	    end
	    else begin
	    	exponent_difference = lv_exponent3 - lv_exponent2;
	    	op2_gt_op3 = 0;
	    	resultant_exponent = lv_exponent3;
	    end

	    // //$display("op2_gt_op3 = %b", op2_gt_op3);

	    if(op2_gt_op3 == 1) begin
	    	lv_mantissa3 = lv_mantissa3 >> exponent_difference;
	    	lv_exponent3 = lv_exponent3 + exponent_difference;
	    end
	    else begin
	    	lv_mantissa2 = lv_mantissa2 >> exponent_difference;
	    	lv_exponent2 = lv_exponent2 + exponent_difference;
	    end

	    bit man2_gt_man3 = 0;

	    if(lv_mantissa2 > lv_mantissa3) man2_gt_man3 = 1;
	    else 							man2_gt_man3 = 0;

	    // //$display("man2_gt_man3 = %b lv_mantissa2 = %b lv_mantissa3 = %b", man2_gt_man3, lv_mantissa2, lv_mantissa3);

	    Bit#(49) resultant_mantissa = 0;
	    bit resultant_sign = (man2_gt_man3 & lv_sign2) | (~man2_gt_man3 & (lv_operation ^ lv_sign3)); 	// Using Karnaugh maps
	    bit actual_operation = lv_sign2 ^ (lv_operation ^ lv_sign3); 									// 0 for addition 1 for subtraction

		if(actual_operation == 0) 	resultant_mantissa = lv_mantissa2 + lv_mantissa3;
		else if(man2_gt_man3 == 1) 	resultant_mantissa = lv_mantissa2 - lv_mantissa3;
		else						resultant_mantissa = lv_mantissa3 - lv_mantissa2;

		//Case when Mantissa2 = Mantissa3 and hence the result is zero
		Bit#(2) add_sub_is_zero = 0;

		if(resultant_mantissa == 0) begin
			if(lv_rounding_mode == 3'b010) 	add_sub_is_zero = 2'b11;
			else							add_sub_is_zero = 2'b01;			// checks the resultant mantissa for zero
		end

		Bit#(6) resultant_mantissa_leading_zeros = pack(countZerosMSB(resultant_mantissa)); // used when subtraction leads to loss of the implicit bit
		if(resultant_mantissa[48] == 1) begin
			resultant_exponent = resultant_exponent + 1;
			resultant_mantissa = resultant_mantissa >> 1;
		end
		else if(resultant_mantissa[47] != 1) begin
			resultant_mantissa = resultant_mantissa << (resultant_mantissa_leading_zeros - 1);
			resultant_exponent = resultant_exponent - ({4'b0000, resultant_mantissa_leading_zeros} - 1);
		end

		// //$display("resultant_sign = %b resultant_exponent = %b resultant_mantissa = %b", resultant_sign, resultant_exponent, resultant_mantissa);

		//A special case when exponent is 00000001 and mantissa is 0.1xxxxx...
		//Now due to above logic mantissa is shifted left and the exponent reduces to 00000000, making the mantissa normal
		//with implicit bit 1 but exponent = 0, thus the following shift is done to make the number denormal
		if(resultant_exponent == 0) begin				
			resultant_exponent = resultant_exponent + 1;
			resultant_mantissa = resultant_mantissa >> 1;
			// //$display("resultant_sign = %b resultant_exponent = %b resultant_mantissa = %b", resultant_sign, resultant_exponent, resultant_mantissa);
		end



		ff_stage4.enq(Stage4_data_type{	fsr:				lv_fsr,
										rounding_mode:		lv_rounding_mode,
										_negate:			lv_negate,
										resultant_sign:		resultant_sign,
										resultant_exponent:	resultant_exponent,
										resultant_mantissa:	resultant_mantissa,
										overflow:			lv_overflow,
										result_is_invalid:	lv_result_is_invalid,
										result_is_inf:		lv_result_is_inf,
										result_is_zero:		lv_result_is_zero,
										add_sub_is_zero:	add_sub_is_zero});

	endrule:rl_stage_4

	rule rl_stage_5_final_stage;

		Bit#(32) lv_fsr					= ff_stage4.first().fsr;		
	    Bit#(3) lv_rounding_mode		= ff_stage4.first().rounding_mode;
		bit lv_negate 					= ff_stage4.first()._negate;
	    bit lv_resultant_sign			= ff_stage4.first().resultant_sign;
	    Bit#(10) lv_resultant_exponent	= ff_stage4.first().resultant_exponent;
	    Bit#(49) lv_resultant_mantissa	= ff_stage4.first().resultant_mantissa;
	    bit lv_overflow_2				= ff_stage4.first().overflow;
	    bit lv_result_is_invalid		= ff_stage4.first().result_is_invalid;
	    Bit#(2) lv_result_is_inf		= ff_stage4.first().result_is_inf;
	    Bit#(2) lv_result_is_zero		= ff_stage4.first().result_is_zero;
	    Bit#(2) lv_add_sub_is_zero		= ff_stage4.first().add_sub_is_zero;

	    ff_stage4.deq();

    	Bit#(25) lv_rounded_mantissa = lv_resultant_mantissa[48:24];
	    bit lv_guard = lv_resultant_mantissa[23];
    	bit lv_round = lv_resultant_mantissa[22];
    	bit lv_sticky = |(lv_resultant_mantissa[21:0]);
	    bit lv_round_up = 0;

    	bit lv_inexact = lv_guard | lv_round | lv_sticky;

	    if(lv_rounding_mode == 'b000) 		lv_round_up = lv_guard & (lv_resultant_mantissa[24] | lv_round | lv_sticky);
	    else if(lv_rounding_mode == 'b100) 	lv_round_up = lv_guard & (lv_round | lv_sticky | ~lv_resultant_sign);
	    else if(lv_rounding_mode == 'b010) 	lv_round_up = lv_inexact & (lv_resultant_sign);
	    else if(lv_rounding_mode == 'b011) 	lv_round_up = lv_inexact & (~lv_resultant_sign);

	    else if(lv_rounding_mode == 'b111) begin
			if(lv_fsr[7:5] == 'b000)		lv_round_up = lv_guard & (lv_resultant_mantissa[24] | lv_round | lv_sticky);                    	
			else if(lv_fsr[7:5] == 'b100)	lv_round_up = lv_guard & (lv_round | lv_sticky | ~lv_resultant_sign);                     	
			else if(lv_fsr[7:5] == 'b010)	lv_round_up = lv_inexact & (lv_resultant_sign);                                          	
			else if(lv_fsr[7:5] == 'b011)	lv_round_up = lv_inexact & (~lv_resultant_sign); 
		end

		// //$display("lv_rounded_mantissa %b before roundup", lv_rounded_mantissa);

	    if(lv_round_up == 1) lv_rounded_mantissa = lv_rounded_mantissa + 1;

	    // //$display("lv_rounded_mantissa %b after roundup", lv_rounded_mantissa);

	    if(lv_rounded_mantissa[24] == 1) begin
	    	lv_resultant_exponent = lv_resultant_exponent + 1;
	    	lv_rounded_mantissa = lv_rounded_mantissa >> 1;
	    end

	    bit lv_underflow = 0;
	    bit lv_overflow = 0;
	    Exception e = None;

	    if(lv_resultant_exponent[9] == 1 || (lv_resultant_exponent[7:0] == 0 && lv_resultant_exponent[8] != 1)) begin
	    	lv_underflow = 1;
	    	e = Underflow;
	    	lv_resultant_exponent = 'b0;
	    	lv_rounded_mantissa = 'b0;
	    end
	    else if(lv_resultant_exponent[8] == 1 || lv_resultant_exponent[7:0] == 8'b11111111) begin
	    	lv_overflow = 1;
	    	e = Overflow;
	    	//lv_resultant_exponent = {2'b00, 'd-1};
	    	//lv_resultant_mantissa = 'b0;
	    end

	    Bit#(32) lv_final_output = 0;

	    if(lv_overflow == 1 || lv_overflow_2 == 1) begin
	    	e = Overflow;
		    if(lv_rounding_mode == 'b001) 									lv_final_output={lv_resultant_sign,'h7f7fffff}; //??
			else if(lv_rounding_mode == 'b010 && lv_resultant_sign == 0)	lv_final_output={lv_resultant_sign,'h7f7fffff}; //??
			else if(lv_rounding_mode == 'b011 && lv_resultant_sign == 1)	lv_final_output={lv_resultant_sign,'h7f7fffff}; //??
			else begin															
				lv_final_output={lv_resultant_sign,8'd-1,23'd0};
			end
        end
	    else if(lv_result_is_zero[0] == 1) begin
		    lv_final_output = {lv_result_is_zero[1], 8'b0, 23'b0};
		end
	    else if(lv_add_sub_is_zero[0] == 1) lv_final_output = {lv_add_sub_is_zero[1], 8'b0, 23'b0};
	    else if(lv_result_is_inf[0] == 1) begin
	    	lv_final_output = {lv_result_is_inf[1], 8'b11111111, 23'b0};
	    	e = Overflow;
	    end
	    else if(lv_result_is_invalid == 1) begin
	    	lv_final_output = {1'b0, 8'b11111111, 'd-1};
	    	e = Invalid;
	    end
	    // else if(lv_result_is_zero == 1) lv_final_output = {lv_resultant_sign, 8'b0, 23'b0};
        else begin
	    	if(lv_negate == 0)	lv_final_output = {lv_resultant_sign, lv_resultant_exponent[7:0], lv_rounded_mantissa[22:0]};
	    	else				lv_final_output = {~lv_resultant_sign, lv_resultant_exponent[7:0], lv_rounded_mantissa[22:0]};
	    end


	    // //$display("resultant_sign = %b resultant_exponent = %b resultant_mantissa = %b", lv_final_output[31], lv_final_output[30:23], lv_final_output[22:0]);

	    ff_final_out.enq(Floating_output{ 	fsr:			lv_fsr,
	    									final_result:	{32'b0, lv_final_output},
	    									exception:		e});

	endrule

	method Action _start(Bit#(32) _operand1,Bit#(32) _operand2, Bit#(32) _operand3, Bit#(32) _fsr, Bit#(3) rounding_mode, bit operation, bit _negate);
		
		Bit#(1) lv_exp1_is_all_ones= _operand1[30:23]=='b11111111 ? 1:0;			//1 if all the bits of exponent are set; used to check if op1 is infinity or NaN
		Bit#(1) lv_exp2_is_all_ones= _operand2[30:23]=='b11111111 ? 1:0;			//1 if all the bits of exponent are set; used to check if op2 is infinity or NaN
		Bit#(1) lv_exp1_is_zero= _operand1[30:23]==0? 1:0;				//1 if exponent of operand1 is 0
		Bit#(1) lv_exp2_is_zero= _operand2[30:23]==0? 1:0;				//1 if exponent of operand2 is 0
		Bit#(1) lv_man1_is_zero= _operand1[22:0]== 0 ? 1:0;				//1 if mantissa of op1 is 0
		Bit#(1) lv_man2_is_zero= _operand2[22:0]== 0 ? 1:0;				//1 if mantissa of op2 is 0
		Bit#(1) lv_op1_is_zero= lv_man1_is_zero & lv_exp1_is_zero;		//1 when operand1=0
		Bit#(1) lv_op2_is_zero= lv_man2_is_zero & lv_exp2_is_zero;
		
		Bit#(1) lv_inf=0;
		Bit#(1) lv_inv=0;
		Bit#(1) lv_zero=0;
			
		if((lv_exp1_is_all_ones == 1 && lv_man1_is_zero == 0) || (lv_exp2_is_all_ones == 1 && lv_man2_is_zero == 0))		// either of the operands are NaN
			lv_inv=1;
		else if((lv_exp1_is_all_ones == 1 && lv_man1_is_zero == 1) || (lv_exp2_is_all_ones == 1 && lv_man2_is_zero == 1))	// checks if op1 or op2 are infinite
		begin				
			if(lv_op2_is_zero == 1 || lv_op1_is_zero == 1)														// if either op1 or op2 are zero, then 0*infinity results in NaN
				lv_inv=1;
			else 																						//if both are infinite, result is infinite
				lv_inf=1;
		end
		else if(lv_op1_is_zero == 1 || lv_op2_is_zero == 1)
			lv_zero=1;
		
		Bit#(1) lv_sign= _operand1[31]^_operand2[31];
		Bit#(10) lv_exponent= {2'b0,_operand1[30:23]}+{2'b0,_operand2[30:23]} - 10'b0001111111;

		if(lv_exp1_is_zero == 0 && lv_exp2_is_zero == 0)												//checking if both op1 and op2 are NOT denormal
			ff_input_register.enq(Input_data_type{	lv_summed_exponent:	lv_exponent,
													sign:				lv_sign,
													mantissa1:			{1'b1,_operand1[22:0]},
													mantissa2:			{1'b1,_operand2[22:0]},
							                        _operand3:			_operand3,
													fsr:				_fsr,
							                        rounding_mode : 	rounding_mode,
													zero:				lv_zero,
													infinity:			lv_inf,
													invalid:			lv_inv,
													_operation : 		operation,
							                        _negate : 			_negate});

		else if(lv_exp1_is_zero == 1 && lv_exp2_is_zero == 0)											//checking if op1 is denormal
			ff_input_register.enq(Input_data_type{	lv_summed_exponent: 	lv_exponent,
													sign:				lv_sign,
													mantissa1:			{1'b0,_operand1[22:0]},
													mantissa2:			{1'b1,_operand2[22:0]},
							                        _operand3:			_operand3,
													fsr:				_fsr,
							                        rounding_mode :		rounding_mode,
													zero:				lv_zero,
													infinity:			lv_inf,
													invalid:			lv_inv,
													_operation : 		operation,
							                        _negate : 			_negate});

		else if(lv_exp1_is_zero == 0 && lv_exp2_is_zero == 1)											//checking if op2 is denormal
			ff_input_register.enq(Input_data_type{	lv_summed_exponent:	lv_exponent,
													sign:				lv_sign,
													mantissa1:			{1'b1,_operand1[22:0]},
													mantissa2:			{1'b0,_operand2[22:0]},
							                        _operand3:			_operand3,
													fsr:				_fsr,
							                        rounding_mode :		rounding_mode,
													zero:				lv_zero,
													infinity:			lv_inf,
													invalid:			lv_inv,
													_operation :		operation,
							                        _negate : 			_negate});

		else 																					//if both operand are denormal
			ff_input_register.enq(Input_data_type{	lv_summed_exponent: 	lv_exponent,
													sign:				lv_sign,
													mantissa1:			{1'b0,_operand1[22:0]},
													mantissa2:			{1'b0,_operand2[22:0]},
							                        _operand3:			_operand3,
													fsr:				_fsr,
							                        rounding_mode :		rounding_mode,
													zero:				lv_zero,
													infinity:			lv_inf,
													invalid:			lv_inv,
													_operation :		operation,
							                        _negate :			_negate });
	endmethod


	method Action deque_buffer();
		ff_final_out.deq();
	endmethod
	
	method Floating_output get_result();
		return ff_final_out.first();
	endmethod

endmodule


// module mkTb_fpu_fm_add_sub(Empty);

// 	Ifc_fpu_fm_add_sub uut <- mkfpu_fm_add_sub();

//     Reg#(Bit#(32)) rg_clock <-mkReg(0);
//     Reg#(Bit#(32)) operand1 <- mkReg(32'h023e1971);
//     Reg#(Bit#(32)) operand2 <- mkReg(32'h3cbd8a27);
//     Reg#(Bit#(32)) operand3 <- mkReg(32'h00000000);

//     rule rl_count_clock ;
//       	rg_clock<=rg_clock+1;
//       	if(rg_clock=='d20) $finish(0);
//     endrule

//     rule rl_input1(rg_clock==1);
//         // //$display("giving inputs at %0d", rg_clock);
//         uut._start(operand1, operand2, operand3, 0, 0, 0, 3'b011, 0, 0, 0);

//     endrule

//     rule rl_finish;
//         // //$display("Output = %h at %0d",uut.get_result().final_result[31:0], rg_clock);
//         uut.deque_buffer();
//     endrule

// endmodule

// module mkTb_fpu_fm_add_sub_2 (Empty);
	
// 	RegFile #(Bit #(16), Bit #(100))  input_data <- mkRegFileFullLoad("regfile_operands1.hex");
// 	Reg #(Bit #(16)) index <- mkReg(0);

// 	// Reg #(Bit #(10)) srno <- mkReg(1);
 
// 	Ifc_fpu_fm_add_sub multiplier <- mkfpu_fm_add_sub();
// 	Reg #(Bit #(32)) state_clock <- mkReg(1);

// 	Reg#(int) cnt <- mkReg(0);                  //File Variable
// 	let fh <- mkReg(InvalidFile) ;				//File handler		

// 	//rule for file creation
// 	rule open (cnt == 0 ) ;
// 		File tb_mul_output <- $fopen("tb_madd_output.hex", "w+"); 
// 		fh <= tb_mul_output;
// 		cnt <= 1 ;
// 	endrule

// 	rule state_clock_count;
// 		//$display("------------------ Inside the rule state_clock_count at %0d ----------------------", $time);
// 		state_clock <= state_clock + 1;
// 	endrule

// 	rule take_input_in (state_clock <= 18262);
// 		//$display("The input1 %h and input2 %h with rounding %b", input_data.sub(index)[67:36], input_data.sub(index)[35:4], input_data.sub(index)[2:0],$time);
// 		multiplier._start(input_data.sub(index)[99:68],input_data.sub(index)[67:36],input_data.sub(index)[35:4],0,0,0,input_data.sub(index)[2:0],0,0,0);
// 		index <= index + 1;
	
// 	endrule

// 	rule display_output;

// 		let abc = multiplier.get_result();
// 		multiplier.deque_buffer();
// 		// //$display("The ouptput is available at %0d", $time);
// 		// //$display("%h",abc.final_result[31:0]);
// 		$fwrite(fh, "%h\n", abc.final_result[31:0]);
// 		//srno <= srno + 1;
// 		//$display("Final result= %h, fsr: %h", abc.final_result, abc.fsr);

// 	endrule

// 	rule end_testing (state_clock == /*18274*/ 43);

// 		//$display("Inside the rule end_testing at %0d", $time);
// 		$finish(0);

// 	endrule : end_testing

// endmodule

endpackage
