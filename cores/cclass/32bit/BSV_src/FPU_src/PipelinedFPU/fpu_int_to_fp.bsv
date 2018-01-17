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
Author's Name 	: Arjun C. Menon, G.Vinod, Aditya Govardhan
e-mail id	: c.arjunmenon@gmail.com, g.vinod1993@gmail.com, dtgovardhan@gmail.com
Last updated on : 6th January 2016

This unit converts a 32-bit or 64-bit, signed or unsigned integer into single or double precision floating point number. The unit is pipelined and takes 1 clock to produce the result.
Risc-V instruction set architecture has been used.
instruction[13] indicates whether the integer is 32-bit or 64-bit. =1 implies that input is 32-bit integer; =0 implies that input is 64-bit integer
instruction[12] indicates whether the integer is signed or unsigned. =1 implies that input is unsigned; =0 implies that input is signed
instruction[7] indicates whether the integer should be converted to single or double precision floating point representation. 
		=1 implies that input is to  be converted to double precision; =0 implies that input is to  be converted to single precision

The Algorithm is as follows:
1. First the input is checked if it is 32-bit or 64-bit integer and sign extended.
1. The exponent is formed based on whether the result is single or double precision.
   Also, the magnitude of the integer is obtained.
2. Then the leading zeros in the integer are calculated and all the data is enqued in a FIFO.
3. In the next stage,the integer is left shifted by the amount of leading zeros.
   The exponent is also reduced by the same amount.
3. The new mantissa is formed by taking the first 24bits, in which the MSB bit is the implicit bit. The implicit
   bit (i.e. 1) is not taken; instead the mantissa is pre-fixed with a 0 so as to detect mantissa overflow while rounding.
4. Rounding is done based on the rest of the bits, and the mantissa is checked for overflow.
   If there is an overflow, exponent is incremented by 1.
5. Finally, the new fsr and the final result are computed and enqued in the output FIFO.

The following sites have been used to verify the obtained results:
1. http://www.h-schmidt.net/FloatConverter/
2. http://www.binaryconvert.com/convert_float.html
3. http://www.binaryhexconverter.com/hex-to-decimal-converter


*********Performance****************:
Using UMCIP 65nm library in SYNOPSYS
Critical Path Length     :    0.556 ns
Max. Operating Frequency :    1.8 GHz
Combinational Cell Count :    3067
Sequential Cell Count    :    295
Critical path is ff_input_stage to final_out.
*/

package fpu_int_to_fp;
import lead_zero_detect64::*;
import FIFO::*;
import SpecialFIFOs::*;
import defined_types::*;

interface Ifc_fpu_int_to_fp;
	method Action _start(Bit#(64) inp_int, Bit#(32) instruction, Bit#(32) fsr, Bit#(3) rounding_mode); // inputs method
	method Floating_output result_();	// output method
	method Action _deque_buffer();			// action method to deque output buffer
endinterface

typedef struct{
	Bit#(32) fsr;					// the file status register containing all the flags and control bits.
	Bit#(3) rounding_mode;
	bit to_sp_or_dp;				// tells whether to convert to single or double precision FP representation
	bit sign;					// the sign of the result
	bit zero;					// indicates whether the result is zero
	Bit#(6) lzd;					// number of leading zeros in the integer
	Bit#(64) integ_num;				// the integer number from which mantissa is being calculated
	Bit#(11) exponent;				// generated exponent
	}Input_stage_type deriving(Bits, Eq);

(*synthesize*)
module mkfpu_int_to_fp(Ifc_fpu_int_to_fp);

	FIFO#(Floating_output) ff_final_out <-mkPipelineFIFO();	// output FIFO
	FIFO#(Input_stage_type) ff_input_stage <-mkPipelineFIFO();	// intermediate FIFO

rule rl_last_rule;
	
	Bit#(52) lv_mantissa;
	Bit#(64) lv_integ_num= ff_input_stage.first().integ_num;
	Bit#(11) lv_exponent= ff_input_stage.first().exponent;
	let rounding_mode = ff_input_stage.first().rounding_mode;
	bit lv_sign= ff_input_stage.first().sign;
	Bit#(6) lv_lzd= ff_input_stage.first().lzd;
	lv_integ_num= (lv_integ_num << lv_lzd);				//The normalised mantissa being calculated
	lv_exponent = lv_exponent-({5'b0,lv_lzd});			//Exponent decremented by the amount of left shifts done on the mantissa
	//Mantissa is the 24 most significant bits of the shifted integer number, lv_integ_num[31] being the implicit bit which is 1

	bit lv_guard;
	bit lv_round;
	//sticky bit for conversion to double precision floating point representation
	bit lv_sticky= |(lv_integ_num[8:0]);

	if(ff_input_stage.first().to_sp_or_dp==0)begin			//checking if the integer number needs to be converted to single precision floating point representation
		lv_mantissa= {29'b0,lv_integ_num[62:40]};		//taking the 23 MSB of the mantissa without the MSB(i.e. 64th bit) which is the implicit bit
		lv_guard= lv_integ_num[39];
		lv_round = lv_integ_num[38];
		//sticky bit for single precision floating point representation is 1 if either of bits 37 down to 0 are set. lv_sticky is 'or' of bits 8 down to 0
		lv_sticky= lv_sticky | (|(lv_integ_num[37:9]));
	end
	else begin
		lv_mantissa= lv_integ_num[62:11];			//taking the 52 MSB of the mantissa without the MSB(i.e. 64th bit) which is the implicit bit
		lv_guard= lv_integ_num[10];
		lv_round= lv_integ_num[9];
	end

	bit lv_roundup=0;						//indicates whether the mantissa should be incremented by 1 or not

	//rounding is done
	if(rounding_mode == 'b000) 
		lv_roundup = lv_guard & (lv_mantissa[0] | lv_round | lv_sticky);
	else if (rounding_mode == 'b100)
		lv_roundup = lv_guard & (lv_round | lv_sticky | ~lv_sign);
	else if (rounding_mode == 'b011)
		lv_roundup = (lv_guard | lv_round | lv_sticky) & (~lv_sign);
	else if (rounding_mode == 'b010)
		lv_roundup = (lv_guard | lv_round | lv_sticky) & (lv_sign);
	else if(rounding_mode == 'b111) begin
		if(ff_input_stage.first().fsr[7:5] == 'b000)			// round to nearest, ties to even
			 lv_roundup = lv_guard & (lv_mantissa[0] | lv_round | lv_sticky);
		else if(ff_input_stage.first().fsr[7:5] == 'b100)		// round to nearest, ties to max magnitude
			lv_roundup = lv_guard & (lv_round | lv_sticky | ~lv_sign);
		else if(ff_input_stage.first().fsr[7:5] == 'b011 )		// round up
			 lv_roundup = (lv_guard | lv_round | lv_sticky) & (~lv_sign);
		else if(ff_input_stage.first().fsr[7:5] == 'b010)		// round down		
			 lv_roundup = (lv_guard | lv_round | lv_sticky) & (lv_sign);
	 end
	// else if the rounding mode is round_to_zero, roundup should be zero. Since the default value is zero, we needn't have an else statement for that.

	Bit#(53)lv_rounded_mantissa;
	if( lv_roundup==1)						//mantissa needs to be incremented by 1
	begin
		if(ff_input_stage.first().to_sp_or_dp==0)		//checking if the integer number needs to be converted to single precision floating point representation
			lv_rounded_mantissa = {({1'b0,lv_mantissa[22:0]} + 24'd1), 29'd0};	
		else	
			lv_rounded_mantissa = {1'b0,lv_mantissa}+1;	//mantissa is incremented by 1

		if(lv_rounded_mantissa[52]==1)				//checking if there is a mantissa overflow after rounding
			lv_exponent = lv_exponent + 1;			//if there is mantissa overflow exponent is incremented by 1
	end
	else begin
		if(ff_input_stage.first().to_sp_or_dp==0)
			lv_rounded_mantissa = {1'b0,lv_mantissa[22:0], 29'd0};
		else	lv_rounded_mantissa = {1'b0,lv_mantissa};
	end

	Exception lv_exception = None;			//by default no exceptions are generated
	bit lv_inexact= lv_guard | lv_round | lv_sticky;		//used to set the inexact flag in the fsr
	
	if(lv_inexact==1)						// check for inexact exception
		lv_exception= Inexact;

	Bit#(32) lv_fsr ={ff_input_stage.first().fsr[31:9],ff_input_stage.first().zero,ff_input_stage.first().fsr[7:1],lv_inexact};	// form the new fsr
	Bit#(64) lv_output_result;
	
	if(ff_input_stage.first().to_sp_or_dp==0)			//checking if the integer number needs to be converted to single precision floating point representation
		lv_output_result= {'d0,lv_sign,lv_exponent[7:0], lv_rounded_mantissa[51:29]};
	else	
		lv_output_result= {lv_sign,lv_exponent, lv_rounded_mantissa[51:0]};

	ff_input_stage.deq();
	ff_final_out.enq( Floating_output{
					fsr		  : lv_fsr,
					final_result	  : lv_output_result,
					exception	  : lv_exception });
		
endrule

method Action _start(Bit#(64) inp_int, Bit#(32) instruction, Bit#(32) fsr, Bit#(3) rounding_mode);
	Bit#(64) lv_integ_num= inp_int;					//temporary variable to calculate the mantissa
	Bit#(11) lv_exponent;						//temporary variable to calculate the exponent
	bit lv_zero=0;							//=1 implies that the input is zero
	bit lv_sign= inp_int[63];			                //=1 implies that the input is negative
	
	$display("start %h",inp_int);
	if(instruction[21]==1) begin					//checking if the input integer is 32-bit
		if(instruction[20]==0)					//checking if the input integer is signed
			lv_integ_num= signExtend(inp_int[31:0]);
		else	lv_integ_num= {32'd0, inp_int[31:0]};		//since input is unsigned, appending 0s at the MSB
	end

	if(lv_integ_num[63:0] == 64'd0) begin				//This is All Zero Condition	
		lv_integ_num= 0;
		lv_exponent= 0;						// form the exponent
		lv_zero= 1;
	end
	else begin
		if(instruction[25]==0)					//checking if the integer number needs to be converted to single precision floating point representation
			lv_exponent= 11'd127+ 11'd63;			//forming the exponent of the single precision result
		else	
			lv_exponent= 11'd1023+ 11'd63;			//forming the exponent of the double precision result

		if(lv_sign == 1'b1)					// when the input is negative
		begin
			lv_integ_num= ~lv_integ_num+1;			// take 2's complemt of the input
		end
	end
	Bit#(6) lv_lzd= fn_lead_zeros64(lv_integ_num[63:0])[5:0];	//invoking the function which calculates number of leading zeros

	ff_input_stage.enq(Input_stage_type{
					     fsr		: fsr,
						 rounding_mode : rounding_mode,
					     to_sp_or_dp	: instruction[7],
					     sign		: lv_sign,
					     zero	  	: lv_zero,
					     lzd		: lv_lzd,
					     integ_num	  	: lv_integ_num,
					     exponent	  	: lv_exponent
					   });	
endmethod


method Floating_output result_();
	return ff_final_out.first();
endmethod


// This method is called once the data from the output_ FIFO has been
// read in the top module. This method will hence empty the output_ FIFO 
method Action _deque_buffer();
	ff_final_out.deq();
endmethod

endmodule


// //(*synthesize*)
// module mkTb_fpu_int_to_fp(Empty);

// Reg#(Bit#(64)) input_value1<-mkReg(64'd4294967040);		//Result= 0x41efffffe0000000	
// Reg#(Bit#(64)) input_value2<-mkReg(64'd-1);			//Result= 0x43f0000000000000
// Reg#(Bit#(64)) input_value3<-mkReg(64'd32);			//Result= 0x4040000000000000
// Reg#(Bit#(64)) input_value4<-mkReg(64'hff87230900000002);	//Result= 0x4000000000000000

// /*
// Reg#(Bit#(64)) input_value1<-mkReg(64'd18446744073709549568);	//Result= 0x43efffffffffffff
// Reg#(Bit#(64)) input_value2<-mkReg(64'h0000000000000001);	//Result= 0x3ff0000000000000
// Reg#(Bit#(64)) input_value3<-mkReg(64'hffffffff80000000);	//Result= 0x43effffffff00000	
// Reg#(Bit#(64)) input_value4<-mkReg(64'hffffffff80000001);	//Result= 0x43effffffff00000	
// */
// Reg#(Bit#(32)) clk<-mkReg(0);
// Ifc_fpu_int_to_fp converter <- mkfpu_int_to_fp();

// rule rl_clock_;
// 	clk <= clk+1;
// 	$display("%d",clk);
// endrule

// //in all the following test cases input is considered unsigned and the conversion is to double precision floating point.

// rule rl_input_1(clk == 'd2);
// 	converter._start(input_value1,32'h00001080,2,32'h00300060,0,0,0);
// endrule
// rule rl_input_2(clk == 'd3);
// 	converter._start(input_value2,32'h00001080,0,0,0,0,0);
// endrule
// rule rl_input_3(clk == 'd4);
// 	converter._start(input_value3,32'h00001080,0,0,0,0,0);
// endrule
// rule rl_input_4(clk == 'd5);
// 	converter._start(input_value4,32'h00003080,0,0,0,0,0);	//instruction[13]=1 i.e. input is 32-bit
// endrule

// rule rl_end_(clk == 'd10);
// 	$finish(0);
// endrule

// rule rl_output_;
// 		let abc = converter.result_();
// 		converter._deque_buffer();
		
// 		//$display("Sign=%b Exponent=%b Mantissa=%b",abc.final_result[31],abc.final_result[30:23],abc.final_result[22:0]);
// 	$display("result= %h",abc.final_result);
// 		// if(abc.exception matches tagged No_exception.*)
// 		// 	$display("NO EXCEPTION");
// 		// else if(abc.exception matches tagged Invalid .*)
// 		// 	$display("INVALID EXCEPTION");
// 		// else if(abc.exception matches tagged Inexact.*)
// 		// 	$display("INEXACT EXCEPTION");
// 		// else if(abc.exception matches tagged Overflow.*)
// 		// 	$display("OVERFLOW EXCEPTION");
			
// 		$display("fsr= %h",abc.fsr);
// endrule 
		
// endmodule

endpackage

