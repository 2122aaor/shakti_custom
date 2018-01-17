/*

Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name 	: SP to DP FP, and vice versa conversion unit
Author Name 	: C. Arjun Menon
e-mail Id	: c.arjunmenon@gmail.com
Last updated on : 26th October 2013

	This unit performs the conversion from a single precision floating point number to a double precision floating point number and vice-versa. In case of SP to DP conversion, there won't be any overflow, underflow or inexact conditions as all single precision numbers can be represented accurately in a double precision format. When converting from DPFP to SPFP there might be an overflow or underflow, and the result may be inexact. Also, the resulting SPFP number might be denormal.

SP to DP conversion algorithm:
	The input is checked if it is not a number, infinity, zero, denormal or normal. If it is infinity, NaN or zero, the result is assigned the corresponding value directly else the exponent is first calculated by subtracting the bias value of SPFP number(i.e. 127) and adding the bias of DPFP number(i.e. 1023). If the input is normal, the 23MSBs of the mantissa will be same as the input's mantissa(which is 23 bits excluding the implicit bit).Otherwise, if the input is denormal, The leading number of zeros are calculated and the mantissa is left shifted by the corrsponding amount. Also the new exponent is formed by subtracting the number of leading zeros and subtracting 1. 1 needs to be subtracted since the count of leading zeros includes the implicit 0 bit of the mantissa.

DP to SP conversion algorithm:
	The input is checked if it is not a number, infinity, zero, denormal or normal. If it is infinity, NaN or zero, the result is assigned the corresponding value directly else the exponent is checked so as to detect overflow, underflow or if the resultant SPFP number will be denormal. If there is an overflow, the result is infinity and if there is an underflow, the result might be denormal or zero (or may be even normal after rounding) . If neither of these cases, then the result will be normal. If the number is denormal, the mantissa is shifted accordingly. If the number is normal, the exponent is formed by subtracting the bias value of DPFP number(i.e. 1023) and adding the bias of SPFP number(i.e. 127). The mantissa of the normal SPFP number is formed by taking the 23MSBs of the DPFP number's mantissa. Rest of the bits are used for rounding. If either of bits from 0 to 28 are set, the result is inexact. Based on the rounding mode( which is given in the fsr) the mantissa might get incremented by 1. After this, the exponent is again checked for overflow condition. Finally the result is formed by concatenating the sign bit, the calculated exponent and the rounded mantissa. Also the new fsr is formed, exception, if any are indicated and all these are enqued in the output FIFO.

The following sites have been used to verify the obtained results:
1. http://www.h-schmidt.net/FloatConverter/
2. http://www.binaryconvert.com/convert_float.html
3. http://www.binaryhexconverter.com/hex-to-decimal-converter


*********Performance****************:
Using UMCIP 65nm library in SYNOPSYS
Critical Path Length     :    0.445ns
Max. Operating Frequency :    2.25GHz
Combinational Cell Count :    1650
Sequential Cell Count    :    142

*/

package fpu_convert_sp_dp;
import riscv_types::*;
import FIFO::*;
import SpecialFIFOs::*;
import lead_zero_detect32::*;						//Function to calculate the leading number of zeros in a 32-bit number

interface Ifc_fpu_convert_sp_dp;
	method Action _start(Bit#(64) input_fp, bit is_dp, Bit#(5) destination,  Bit#(32) fsr, Bit#(4) rob_number, Bit#(32) pc); // input method to start the floating point operation
	method Output_type result_();					// Output method
	method Action _deque_buffer();					// input method to deque output buffer
	method Action _set_flush(Bool f);				// Input method to initiate the flush routine
endinterface

(*preempts= "rl_flush_output_fifo,_start"*)
(*synthesize*)
module mkfpu_convert_sp_dp(Ifc_fpu_convert_sp_dp);			

FIFO#(Output_type) ff_final_out <-mkPipelineFIFO();			// output FIFO

Wire#(Bool) wr_flush<- mkDWire(False);					//wire that indicates when to flush the output FIFO in case of a processor pipeline flush by firing the rule rl_flush

rule rl_flush_output_fifo(wr_flush);					//rule that clears the contents of the FIFO in case of a flush
	ff_final_out.clear();
endrule

method Action _start(Bit#(64) input_fp, bit is_dp, Bit#(5) destination,  Bit#(32) fsr, Bit#(4) rob_number, Bit#(32) pc);	

	bit lv_is_nan=0;						//=1 => result is NaN
	bit lv_is_zero=0;						//=1 => result is zero
	bit lv_is_infinity=0;						//=1 => result is plus or minus infinity
	bit lv_is_denormal=0;						//=1 => SPFP result is denormal; since in SP to DP conversion the result is always normal
	bit lv_is_normal=0;						//=1 => result is normal
	bit lv_sign;							//=1 => represents the sign of the result
	bit lv_sticky= 0;						//=1 => SPFP result is inexact
	bit lv_overflow= 0;						//=1 => The SPFP result overflows
	bit lv_underflow= 0;						//=1 => The SPFP result underflows
	
	Bit#(11) lv_exponent= 0;					//variable used to calculate the new exponent; in case of DP to SP FP conversion, the lower 8 bits can be taken.
	Bit#(53) lv_mantissa= 0;					//variable used to calculate the new mantissa; in case of DP to SP FP conversion, the upper 23 bits can be taken.
	Exception lv_exception = tagged No_exception;			//variable that tells us which exception is generated
	Bit#(64) lv_result;						//variable that holds the final result; in case of DP to SP FP conversion, the lower 32 bits can be taken
	Bit#(32) lv_fsr= fsr;						//variable that holds the new value of fsr
	
	bit lv_is_sp_mantissa_not_zero=|(input_fp[22:0]);		//=1 if mantissa!=0
	Bit#(5) lv_lzd= 0;
	bit lv_is_dp_mantissa_not_zero= (|(input_fp[51:23]))|lv_is_sp_mantissa_not_zero;
	if(is_dp==0) begin
		lv_sign= input_fp[31];
		if(input_fp[30:23]==1) begin				//if all bits of exp are 1
			if(lv_is_sp_mantissa_not_zero==0)		//if mantissa==0
				lv_is_infinity= 1;
			else						//if mantissa!=0
				lv_is_nan= 1;
		end
		else if(input_fp[30:23]==0) begin			//if exponent=0
			if(lv_is_sp_mantissa_not_zero==0)		//if mantissa=0
				lv_is_zero= 1;
			else						//if mantissa!=0
				lv_is_denormal=1;
		end
		else lv_is_normal=1;					//the result is normal

		lv_lzd= fn_lead_zeros32({~lv_is_denormal, input_fp[22:0],'d0})[4:0];  	//invoking the function which calculates the leading number of zeros
		if(lv_is_nan==1) begin					//if the result is NaN
			lv_sign= 0;
			lv_exponent= 'd-1;				//All bits of exponent are set
			lv_mantissa= 'h18000000000000;			//mantissa!=0
		end
		else if(lv_is_zero==1) begin				//if result is zero
			lv_exponent= 'd0;
			lv_mantissa= 'd0;
		end
		else if(lv_is_infinity==1) begin			//if result is infinity
			lv_exponent= 'd-1;				//All bits of exponent are set
			lv_mantissa= 'd0;				//mantissa=0
		end
		else if(lv_is_denormal==1 || lv_is_normal==1) begin	//if the result is denormal or normal
			lv_exponent= {3'd0, input_fp[30:23]} + 11'd896;	//896 = -127+1023
			lv_mantissa= {1'b0, input_fp[22:0], 29'd0};
		end
		
		if(lv_is_denormal==1) begin				//if the input is denormal, the new mantissa and the new exponent are calculated
			lv_mantissa= lv_mantissa<<lv_lzd;		//mantissa being shifted by amount of leading zeros
			lv_exponent= lv_exponent - {6'd0,lv_lzd} + 'd1;	//the previous calculated exponent being decremented by  less then
		end
		lv_result= {lv_sign,lv_exponent, lv_mantissa[51:0]};
	end
	else begin
		lv_sign= input_fp[63];
		bit lv_v_sticky=|(input_fp[26:0]);
			
		if(input_fp[62:52]==1) begin				//if all bits of exp are 1
			if(lv_is_dp_mantissa_not_zero==1)		//if mantissa!=0
				lv_is_nan= 1;
			else
				lv_is_infinity= 1;
		end
		else if(input_fp[62:52]==0 && lv_is_dp_mantissa_not_zero==0) //if exponent=0 and mantissa=0
			lv_is_zero= 1;
		else 
			lv_is_normal=1;
		
		if(lv_is_nan==1) begin					//if the input is NaN
			lv_sign= 0;
			lv_exponent= 'd-1;				//All bits of exponent are 1
			lv_mantissa= 'h18000000000000;
		end
		else if(lv_is_zero==1) begin				//if the input is zero
			lv_exponent= 'd0;				
			lv_mantissa= 'd0;
		end
		else if(lv_is_infinity==1) begin			//if the input is infinity
			lv_exponent= 'd-1;
			lv_mantissa= 'd0;
		end

		else if(lv_is_normal==1) begin				//if input is a valid number
			if(input_fp[62:52]>'d1150) begin		// If the DPFP num is beyond single precision range i.e. above 1023+127=1150	
				lv_overflow= 1;				//there is an overflow
				lv_exponent= 'd-1;			//the result is plus or minus infinity
				lv_mantissa= 0;
			end
			else if(input_fp[62:52]<'d874) begin		// If the DPFP num is below single precision range i.e. below 1023-127-22=874
				lv_underflow= 1;			//there is an underflow
				lv_exponent= 0;				//result is 0
				lv_mantissa= 0;
			end
			else if(input_fp[62:52]<='d896) begin		//if the resulting single precision no. is denormal i.e. its exponent is equal to or above 874(which is implicit since
									//we've an 'else if' condition) and is equal to or below 1023-127=896
				lv_is_denormal= 1;			//the resultant SPFP number might be denormal(exception being the case where all bits of mantissa, and there is a rounding
									// which occurs in which the mantissa gets incremented by 1)
				lv_exponent= 0;

				let lv_mantissa_shift_temp= 11'd896-input_fp[62:52];		//calulating the amount of shifts to be done on the mantissa in case if the result is denormal
				lv_mantissa={1'b0,(input_fp[51:0]>>lv_mantissa_shift_temp)};	//mantissa being shifted by the calculated amount
				lv_sticky= (lv_v_sticky | input_fp[27]);			//since the 23 bits of mantissa also includes the implicit bit of the DPFP input (i.e. 1), one more
												// bit from the end gets discarded and therefor that is also checked if it is 1 so as to know if 
												//we've lost precision
			end
			else begin					//the number is normal
				lv_exponent= input_fp[62:52]-11'd896;	//The new exponent is calculated. new_exponent= old_exponent -1023(bias of DPFP) +127(bias of SPFP)= old_exponent-896
				lv_mantissa= {1'b1, input_fp[51:0]};	//The implicit 1 is added
				lv_sticky= lv_v_sticky;			//Since the number is normal, the sticky bit is same as what we have already calculated
			end
		end


		bit lv_guard= lv_mantissa[28];				//guard bit
		bit lv_round= lv_mantissa[27];				//round bit

		bit lv_roundup= 0;					//=1 => the mantissa needs to be incremented by 1
		bit lv_inexact= 0;					//=1 => the result is inexact

		if((lv_guard | lv_round | lv_sticky)==1)		//If either of guard, round or sticky bits are set, the result is inexact
			lv_inexact= 1;

		if(lv_fsr[7:5] == 'b000)				// round to nearest, ties to even
			lv_roundup = lv_guard & (lv_mantissa[29] | lv_round | lv_sticky);
		else if(lv_fsr[7:5] == 'b100)				// round to nearest, ties to max magnitude
			lv_roundup = lv_guard & (lv_round | lv_sticky | ~lv_sign);
		else if(lv_fsr[7:5] == 'b011 )				// round up
			 lv_roundup = lv_inexact & (~lv_sign);
		else if(lv_fsr[7:5] == 'b010)				// round down		
			 lv_roundup = lv_inexact & (lv_sign);
		// else if the rounding mode is round_to_zero, roundup should be zero. Since the default value is zero, we needn't have an else if statement for that.

		if(lv_roundup==1 && lv_is_normal==1) begin						//checking if mantissa needs to be incremented
			lv_mantissa[24:0]= {1'b0,lv_mantissa[52:29]}+1;					//a zero is appended at the MSB so as to check if there is any mantissa overflow after rounding
			if(lv_mantissa[24]==1 || (lv_mantissa[23]==1 && lv_is_denormal==1)) begin	//checking if there is mantissa overflow i.e. the MSB is checked if it is set in case 
													//of a normal number
													//also if the result was denormal, since the implicit bit i.e.mantissa[23] itself was zero.
													//therefore if there was an overflow, it can be detected by checking the 23rd bit
				lv_exponent= lv_exponent + 1;						
				lv_is_denormal= 0;							//in case the result was denormal, and there is a mantissa overflow, the result is no 
													//longer denormal
			end
		end
		else											//the mantissa needn't be incremented
			lv_mantissa[24:0]= {1'b0,lv_mantissa[52:29]};

			

		if( lv_exponent[7:0]==1 && lv_is_normal==1)begin					//checking for exponent overflow i.e. checking if after rounding all the bits of the exponent
													//are set so as to ensure that if the input was already infinity, in which case all bits of 
													//the exponent will be set, but is not an overflow condition, we have one more condition that
													//the orginal input should be normal
			lv_overflow=1;
			lv_result= {32'd0,lv_sign,8'hff,23'd0};						//final result is plus or minus infinity
		end
		else
			lv_result={32'd0,lv_sign,lv_exponent[7:0],lv_mantissa[22:0]};			//final result is computed by concatenating the sign, exponent and mantissa

		lv_underflow= lv_underflow | lv_is_denormal;						//Since there is an underflow condition even if the result is denormal

		//Exceptions are generated, if any
		if(lv_overflow==1)
			lv_exception= tagged Overflow True;
		else if(lv_underflow==1)
			lv_exception= tagged Underflow True;
		else if(lv_inexact==1)
			lv_exception= tagged Inexact True;
		
		 lv_fsr= {fsr[31:10],1'b0,lv_is_zero,fsr[7:5],2'b0,lv_overflow,lv_underflow,lv_inexact}; //new fsr is calculated
	end

	ff_final_out.enq( Output_type{  destination	  : destination,				//The result being enqueued in the output FIFO
					fsr		  : lv_fsr,
				
			
					final_result	  : lv_result,
					exception	  : lv_exception });
endmethod

//Output method which returns the result
method Output_type result_();
	return ff_final_out.first();
endmethod


// This method is called once the data from the output_ FIFO has been
// read in the top module. This method will hence empty the output_ FIFO 
method Action _deque_buffer();
	ff_final_out.deq();
endmethod

// when a flush is initiated in the processor this method will also be called.
// it sets the flsuh wire to True
method Action _set_flush(Bool f);
	wr_flush<=f;
endmethod
endmodule


//Test bench
module mkTb(Empty);
Reg#(Bit#(64)) input_fp<-mkReg(64'hC7EFFFFFE0000000);	// operand 1

Reg#(Bit#(32)) clk<-mkReg(0);				// Clock
Ifc_fpu_convert_sp_dp inst <- mkfpu_convert_sp_dp();	// An instance of the floating point converter


//rule that increments clock and keeps track of it
rule rl_clock_;
	clk <= clk+1;
	$display("%d",clk);
	
endrule

//rule which specifies when to stop simulation
rule rl_end_(clk == 'd10);
	$finish(0);
endrule

//The following rules calls the action method '_start' and gives inputs.
rule rl_input_0(clk == 'd0);
	input_fp<= 64'hC7EFFFFFE8000000;
	inst._start(input_fp, 1'b1, 5'd3, 32'h00300040, 4'd7, 'd1);
endrule

rule rl_input_1(clk == 'd1);
	input_fp<= 64'h47EFFFFFE0000000;
	inst._start(input_fp, 1'b1, 5'd3, 32'h00300040, 4'd7, 'd1);
endrule

rule rl_input_2(clk == 'd2);
	input_fp<= 64'h47EFFFFFE0000001;
	inst._start(input_fp, 1'b1, 5'd3, 32'h00300060, 4'd7, 'd1);
endrule

rule rl_input_3(clk == 'd3);
	input_fp<= 64'h47EFFFFFF0000000;
	inst._start(input_fp, 1'b1, 5'd3, 32'h00300060, 4'd7, 'd1);
endrule

rule rl_input_4(clk == 'd4);
	input_fp<= 64'h47EFFFFFF0000000;
	inst._start(input_fp, 1'b1, 5'd3, 32'h00300000, 4'd7, 'd1);
endrule

rule rl_input_5(clk == 'd5);
	inst._start(input_fp, 1'b1, 5'd3, 32'h00300080, 4'd7, 'd1);
endrule


//rule to get output
rule rl_get_output_;
	
	inst._deque_buffer();						//dequeing the output FIFO so that the next instruction can write result onto the FIFO
	let abc= inst.result_;	
	$display("result= %h",abc.final_result);

	$display("fsr= %h",abc.fsr);

	//inst._set_flush(True);
	if(abc.exception matches tagged No_exception.*)
		$display("NO EXCEPTION");
	else if(abc.exception matches tagged Invalid .*)
		$display("INVALID EXCEPTION");
	else if(abc.exception matches tagged Inexact.*)
		$display("INEXACT EXCEPTION");
	else if(abc.exception matches tagged Underflow.*)
		$display("UNDERFLOW EXCEPTION");
	else if(abc.exception matches tagged Overflow.*)
		$display("OVERFLOW EXCEPTION");
			
endrule 
endmodule
endpackage
