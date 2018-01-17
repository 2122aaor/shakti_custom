/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name   	: Pipelined Single precision Floating point to integer unit
Author's Name 	: Rishi Naidu
e-mail id	: rishinaidu.rn@gmail.com
Last updated on : 24th December 2013

This module will include following instructions:
FCVT.W.S
FCVT.WU.S

It converts the single precision floating point to 32 bit word unsigned or signed integer based on the instructions used.
The only exceptions currently raised are INVALID and INEXACT exception.

INVALID is raised = when the converted integer is outside the range of converted destination format an invalid flag is raised
INEXACT is raised = when the value converted is representable in destination format, but is not the exact value then the inexact flag is raised

Default values of flags like OVERFLOW, UNDERFLOW is 0 and remains unchanged

A test bench is also created, which tests the module for corner cases to maximum extent.
The test bench considers the following value for different rounding modes and for signed and unsigned integer conversion.
	//0,-0.235,-0.5,-0.6
	//0,0.235,0.5,0.6
	//1,1.3,1.5,1.7
	//-10,-10.3,-10.5,-10.7

convert_unsigned bit = _instruction[12];
rounding_mode = fsr[7:5]
*/


package fpu_fp_to_int;
import riscv_types::*;
import RegFile::*;
import FIFO::*;
import SpecialFIFOs::*;
/*
typedef union tagged{ 	
		void No_exception;					// indicates that there was no exception generated
		Bool Invalid;						// indicates that the operation is invalid
		Bool Divide_by_Zero;				// indicates that the division operation is a divide by zero.
		Bool Overflow;						// indicates an overflow
		Bool Underflow;						// indicates an underflow
		Bool Inexact;						// indicates that the produced result is inexact
		}Exception deriving(Bits, Eq);
	
typedef struct{
		Bit#(32) fsr;						// the file status register containing all the flags and control bits.
		Bit#(32) final_result;				// the final result for the operation
		Exception exception; 				// indicates if any exception is generated.
		}Output_type deriving(Bits,Eq);		// data structure of the output FIFO.
*/
//*********************Interface***********************************************//
interface Ifc_fpu_fp_to_int;

	method Action _start(Bit#(32) input_sp,bit convert_unsigned,Bit#(32) fsr);
	method Action deque_buffer();
	method Output_type get_result();

endinterface

//***************************Main Module**************************************//
(*synthesize*)
module mkfpu_fp_to_int(Ifc_fpu_fp_to_int);

	FIFO#(Output_type) ff_final_out <-mkPipelineFIFO();	

	method Action _start(Bit#(32) input_sp,bit convert_unsigned,Bit#(32) fsr);
		//Declaring some exception flags
		bit lv_underflow=0;                     
		bit lv_overflow=0;
		bit lv_zero=0;
		bit lv_invalid=0;
		bit _decimal_less_than_one=0;

		Bit#(3) rounding_mode=fsr[7:5];
		bit lv_sign = input_sp [31];
		Bit#(24) mantissa = {0,input_sp[22:0]}; 
		Bit#(8) exponent = input_sp[30:23];
		
			
		if(input_sp[30:0]==0)  //WHen the input is zero 
			lv_zero=1; 		   //Setting zero flag

		//Calculating real exponent
		Bit#(8) real_exponent = exponent - 'd127; //Calculating real exponent
		
		
		if (exponent <127 && exponent!=0) begin //It represents all numbers between 0 and 1
			_decimal_less_than_one=1;
		end
		//*****************Setting invalid bit*****************//
		//Setting invalid when the converted integer cannot be represented in destination format			
		if (real_exponent >31 && _decimal_less_than_one==0 && lv_zero==0)
				lv_invalid=1;

		//************Setting implicit bit**************//
		if (exponent!=0)
			mantissa[23]=1;

		Bit#(56) extended_mantissa= zeroExtend(mantissa);
	 	Bit#(8) compl_real_exponent = ~real_exponent +1;
		bit sticky_temp=0;
		
		//*************Shifting mantissa based on the exponent***************//
		if(_decimal_less_than_one==0) 
			extended_mantissa = extended_mantissa <<real_exponent;
		else  begin//This takes care 
			if (compl_real_exponent>23) begin
				sticky_temp=1;
			end
			extended_mantissa = extended_mantissa >> compl_real_exponent;
		end
		//***********Setting guard,round,sticky bits*****************************//
		bit lv_guard = extended_mantissa[22];
		bit lv_round = extended_mantissa[21];
		bit lv_sticky = |(extended_mantissa[20:0])|sticky_temp;
		bit lv_inexact = lv_guard|lv_round|lv_sticky;
		bit lv_roundup = 0;
		
		//****************rounding is done************************//
		if(rounding_mode== 'b000)			// round to nearest, ties to even
			 lv_roundup = lv_guard & (extended_mantissa[23] | lv_round | lv_sticky);
		else if(rounding_mode == 'b100)		// round to nearest, ties to max magnitude
			lv_roundup = lv_guard & (lv_round | lv_sticky | ~lv_sign);
		else if(rounding_mode == 'b011 )		// round up
			 lv_roundup = (lv_guard | lv_round | lv_sticky) & (~lv_sign);
		else if(rounding_mode == 'b010)		// round down		
			 lv_roundup = (lv_guard | lv_round | lv_sticky) & (lv_sign);
		// else if the rounding mode is round_to_zero, roundup should be zero. Since the default value is zero, we needn't have an else statement for that.

		Bit#(33) final_output= extended_mantissa[55:23]; //Determining the output after shifting mantissa

		if (lv_roundup==1)
			final_output = final_output +1; 

		if (lv_zero==1|| (lv_sign==1 && convert_unsigned==1)) //Conditions when the final output shoudl be zero
			final_output =0;	
		if (final_output[32]==1)  //When the output overflows invalid bit it set
			lv_invalid =1;
		if(lv_sign==1 && convert_unsigned==0) //Signed output and input is negative then we take 2's complement of the output
			final_output = ~final_output +1;
	
		//*****************Setting FSR register***************************//
		Bit#(32) lv_fsr= 0;
		lv_fsr = {fsr[31:10],1'b0,lv_zero,rounding_mode,2'b0,lv_overflow,lv_underflow,lv_inexact}; 
		//***************Setting exception flags********************//
		Exception lv_exception = tagged No_exception;  
		if(lv_underflow==1)	
			lv_exception=tagged Underflow True;
		else if(lv_invalid==1)
			lv_exception= tagged Invalid True;
		else if(lv_overflow==1)
			lv_exception= tagged Overflow True;
		else if(lv_inexact==1)
			lv_exception= tagged Inexact True;
		//***************Final Enquing  of output, exception and new fsr value****************************//	
		ff_final_out.enq(Output_type{fsr :lv_fsr,
					     final_result : zeroExtend(final_output[31:0]),
					     exception : lv_exception});
	endmethod

	method Action deque_buffer();
		ff_final_out.deq();
	endmethod
	
	method Output_type get_result();
		return ff_final_out.first();
	endmethod	
endmodule
//************************Test bench*********************************************//
(*synthesize*)
module mkTb_fpu_fp_to_int(Empty);
	Reg#(Bit#(32)) rg_clock <-mkReg(0);     //Clock registercalvin harris i need your love
	Reg#(Bit#(32)) count_line <-mkReg(0); // counter to keep track of the line number being read (also the index of the register in RegFile)
	
	RegFile#(Bit#(32),Bit#(72)) input_text_file <- mkRegFileLoad("Tb_fsp_to_int.txt",0,127); // Initializing the RegFile register with the contents of testcases
	Reg#(Bit#(72)) rg_input <- mkReg(0);						//Hold the 72 input from test file which are in format {32 bit input ,4 bits rounding_mode,4 unsigned/signed,32 bit output}                 
	Reg#(Bit#(32)) rg_verify_output <- mkReg(0);				//To store the 32 bit verify output from text file in when state=0
	Reg#(bit) rg_state <- mkReg(0);								//A state counter state 0 => take inputs --- state 1=> get outputs and compare
	Ifc_fpu_fp_to_int fp_to_int_convert <- mkfpu_fp_to_int;		//Declaring main module interface
	
	//***********Clock incrementing rule*****************//
	rule rl_clock;
		
		rg_clock<=rg_clock+1;
		$display("CLOCK=%d",rg_clock);

		if(rg_clock=='d270)
		$finish(0);
	endrule
	
	//***************Start rule : Executed once to set give initial value to rg_input******************//
	rule give_input(rg_clock==1);
		rg_input <=input_text_file.sub(count_line);	
		count_line <=count_line +1;
	endrule
	//***************Rule to give inputs and store the verify output in register******************//
	rule give_input1(rg_state==0 && rg_clock>1);
		rg_input<=input_text_file.sub(count_line);
		rg_verify_output <= rg_input[31:0];
		fp_to_int_convert._start(rg_input[71:40],rg_input[32],{24'd0,rg_input[38:36],5'd0});		
		$display("Input");
		$display("Input=%h, rounding_mode=%d , unsigned=%d", rg_input[71:40],rg_input[38:36],rg_input[32]);
		rg_state <=1;	
	endrule
	
	//***************Rule to get outputs and compare if it with test bench output****************//
	rule get_output(rg_state==1);
		let lv_output = fp_to_int_convert.get_result();
		count_line <= count_line +1;
		rg_state <=0;
		$display("Output= %h, Verify_output=%h Equality=%d" , lv_output.final_result,rg_verify_output,(lv_output.final_result==zeroExtend(rg_verify_output)));
		if(lv_output.exception matches tagged No_exception.*)
			$display("NO EXCEPTION");
		else if(lv_output.exception matches tagged Invalid .*)
			$display("INVALID EXCEPTION");
		else if(lv_output.exception matches tagged Inexact.*)
			$display("INEXACT EXCEPTION");
		else if(lv_output.exception matches tagged Overflow.*)
			$display("OVERFLOW EXCEPTION");
				
		fp_to_int_convert.deque_buffer();
	endrule
endmodule

endpackage


