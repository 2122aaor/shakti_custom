/*

Copyright (c) 2013-2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name 	: Floating Point single precision/double precision floating number to word/long converter 
Author Name 	: Aditya Govardhan, Vinod.G
e-mail Id	: dtgovardhan@gmail.com, g.vinod1993@gmail.com
Last updated on : 11th June 2016

Instructions supported: FCVT.W.S, FCVT.L.S, FCVT.WU.S, FCVT.LU.S, FCVT.W.D, FCVT.L.D, FCVT.WU.D, FCVT.LU.D

*/

//Note:
//	Exception flags yet to be set.

package fpu_fp_to_int;

import defined_types::*;

interface Ifc_fpu_fp_to_int#(numeric type fpinp, numeric type fpman, numeric type fpexp, numeric type fpbias);

	method Action _start(Bit#(fpinp) input_sp, bit convert_unsigned, bit convert_long, Bit#(32) fsr, Bit#(3) rounding_mode);
	method Floating_output#(fpinp) get_result(); 

endinterface

module mkfpu_fp_to_int(Ifc_fpu_fp_to_int#(fpinp, fpman, fpexp, fpbias))
	provisos(	Add#(TAdd#(fpman,fpexp),1,fpinp)
//				Add#(a__, TAdd#(fpman, 1), 64)		//BSV compiler proviso
				);

	Reg#(Floating_output#(fpinp)) wr_final_out <- mkWire();

	let fpINP = valueOf(fpinp);
	let fpMAN = valueOf(fpman);
	let fpEXP = valueOf(fpexp);
	let fpBIAS = valueOf(fpbias);

	method Action _start(Bit#(fpinp) input_sp, bit convert_unsigned, bit convert_long, Bit#(32) fsr, Bit#(3) rounding_mode);

		//Exception and other flags
		bit lv_overflow = 0;
		bit lv_invalid = 0;
		bit lv_zero = 0;
		Exception lv_exception = None;

		Bit#(fpman) lv_mantissa = input_sp[fpMAN-1:0];
		Bit#(fpexp) lv_exponent = input_sp[fpINP-2:fpMAN];
		bit lv_sign = input_sp[fpINP-1];

		bit lv_exponent_is_zero = ~ (|(lv_exponent));
		bit lv_exponent_is_all_one = &(lv_exponent);
		bit lv_mantissa_is_zero = ~ (|lv_mantissa);

		if(lv_exponent_is_zero == 1) begin
			if(lv_mantissa_is_zero == 1) 		lv_zero = 1;		// number is zero
		end
		else if(lv_exponent_is_all_one == 1) begin
			lv_invalid = 1;		// number is NaN or infinity
			lv_exception = Invalid;
		end
		else lv_mantissa = input_sp[fpMAN-1:0];						// number is normal

		$display("input is %b", input_sp);
		$display("sign = %b exponent = %b mantissa = %b zero_flag = %b invalid_flag = %b", lv_sign, lv_exponent, lv_mantissa, lv_zero, lv_invalid);

		Bit#(fpexp) lv_original_exponent = lv_exponent - fromInteger(fpBIAS);		// removing the bias
		$display("original_exponent = %b", lv_original_exponent);


		Bit#(fpinp) final_result = 1;								// The implicit bit is already stored in the final_result

		//mantissa is shifted left and MSB bits go into final result LSB according to the exponent
		Bit#(TAdd#(fpman, fpinp)) x = {final_result, lv_mantissa};

		if(convert_long == 0) begin												//32 bit output is required
			if(lv_original_exponent[fpEXP-1] == 1) begin
				final_result = 0;
			end
			else if(convert_unsigned == 1 && lv_original_exponent < 'd32) begin	//if output is unsigned then max 31 left shifts possible
				$display("unsigned");
				x = x << lv_original_exponent;
				final_result = x[fpMAN+fpINP-1:fpMAN];
				lv_mantissa = x[fpMAN-1:0];
			end
			else if(convert_unsigned == 0 && lv_original_exponent < 'd31) begin	//if output is signed the max 30 left shifts possible
				$display("signed");
				x = x << lv_original_exponent;
				final_result = x[fpMAN+fpINP-1:fpMAN];
				lv_mantissa = x[fpMAN-1:0];
			end
			else begin
				$display("overflow");
				lv_overflow = 1;
			end
		end
		else if(convert_long == 1) begin										//64 bit output is required
			if(lv_original_exponent[fpEXP-1] == 1) begin
				final_result = 0;
			end
			else if(convert_unsigned == 1 && lv_original_exponent < 'd64) begin	//if output is unsigned then max 63 left shifts possible
				$display("unsigned");
				x = x << lv_original_exponent;
				final_result = x[fpMAN+fpINP-1:fpMAN];
				lv_mantissa = x[fpMAN-1:0];
			end
			else if(convert_unsigned == 0 && lv_original_exponent < 'd63) begin	//if output is signed the max 62 left shifts possible
				$display("signed");
				x = x << lv_original_exponent;
				final_result = x[fpMAN+fpINP-1:fpMAN];
				lv_mantissa = x[fpMAN-1:0];
			end
			else begin
				$display("overflow");
				lv_overflow = 1;
			end
		end

		$display("final_result = %b", final_result);
		$display("mantissa = %b", lv_mantissa);

		/***********************************************Rounding stage*************************************************/


		bit lv_guard = lv_mantissa[fpMAN-1];	//MSB of the already shifted mantissa is guard bit
    	bit lv_round = lv_mantissa[fpMAN-2];	//next bit is round bit
    	bit lv_sticky = |(lv_mantissa<<2);		//remaining bits determine the sticky bit
	    bit lv_round_up = 0;

    	bit lv_inexact = lv_guard | lv_round | lv_sticky;

	    if(rounding_mode == 'b000) 		lv_round_up = lv_guard & (final_result[0] | lv_round | lv_sticky);	//Round to nearest ties to even
	    else if(rounding_mode == 'b100) lv_round_up = lv_guard & (lv_round | lv_sticky | ~lv_sign);			//Round to nearest ties to max magnitude
	    else if(rounding_mode == 'b010) lv_round_up = lv_inexact & (lv_sign);								//Round down to -infinity
	    else if(rounding_mode == 'b011) lv_round_up = lv_inexact & (~lv_sign);								//Round up to +infinity

	    else if(rounding_mode == 'b111) begin																//Dynamic rounding
			if(fsr[7:5] == 'b000)		lv_round_up = lv_guard & (final_result[0] | lv_round | lv_sticky);	//Round to nearest ties to even
			else if(fsr[7:5] == 'b100)	lv_round_up = lv_guard & (lv_round | lv_sticky | ~lv_sign);			//Round to nearest ties to max magnitude                    	
			else if(fsr[7:5] == 'b010)	lv_round_up = lv_inexact & (lv_sign);								//Round down to -infinity                                         	
			else if(fsr[7:5] == 'b011)	lv_round_up = lv_inexact & (~lv_sign);								//Round up to +infinity
		end

		if(lv_round_up == 1) final_result = final_result + 1;

		$display("round_up = %b", lv_round_up);

		if(convert_unsigned == 0 && lv_sign == 1)		//Negating the output if floating point number is negative and converted to signed word/long
			final_result = ~final_result + 1;
		else if(convert_unsigned == 1 && lv_sign == 1)	//TODO What happens when negative floating point is converted to unsigned int, right now rounded to zero
			final_result = 0;
		else if(lv_zero == 1)
			final_result = 0;


		wr_final_out<= Floating_output{
										fsr: fsr,
										final_result: final_result,
										exception: lv_exception};

	endmethod

	method Floating_output#(fpinp) get_result();
		return wr_final_out;
	endmethod

endmodule

// module mkTb_fpu_fp_to_int(Empty);

// 	// Ifc_fpu_fp_to_int#(64, 52, 11, 1023) converter <- mkfpu_fp_to_int();
// 	Ifc_fpu_fp_to_int#(32, 23, 8, 127) converter <- mkfpu_fp_to_int();
// 	Reg#(Bit#(32)) state_clock <- mkReg(0);

// 	// Reg#(Bit#(64)) input_float <- mkReg('h4059400000000000);
// 	Reg#(Bit#(32)) input_float <- mkReg('h33D6BF95);

// 	rule state_clock_count;
// 		state_clock <= state_clock + 1;
// 		if(state_clock == 'd5) $finish;
// 	endrule

// 	rule give_input(state_clock == 'd1);
// 		$display("input %h given at %0d", input_float, state_clock);
// 		converter._start(input_float, 0, 0, 0, 3'b000);
// 	endrule

// 	rule take_output;
// 		Int#(64) result = unpack(converter.get_result().final_result); //Why?
// 		$display("output %b at %0d", converter.get_result().final_result, state_clock);
// 		$display("output %0d at %0d", result, state_clock);
// 		converter.deque_buffer();
// 	endrule

// endmodule

endpackage
