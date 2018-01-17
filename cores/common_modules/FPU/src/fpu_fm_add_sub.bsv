/*

Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name 	: Floating Point fused multiply add-sub single precision 
Author Name 	: Rishi Naidu
e-mail Id	: rishinaidu.rn@gmail.com
Last updated on : 20th December 2013

This is a single precision fused multiply adder module. It will execute 4 instructions of fused type which are., FMADD, FMSUB, FNMSUB,FNMADD. 
Currently the module is not pipelined as it is being implemented to check the functionality of module. After the functionality is thoroughly tested the module will be pipelined to attain the desired speed.

Fused multiply is executed normally, by first multiplying the first two operands and then adding or subtracting it with the third operand. 
The output after multiplication of two 24 bit mantissa (Including hidden or implicit bit) is the 48 bit value. Out of this 48 bits we consider the upper 26 bits and then set the ground bit, round bit and sticky bit (total 29 bits).
 We extend this 29 bits to 58 bits with zeros, so that after shifting we can get the sticky bit value. This helps to avoid using a trailing zeros function, which increases the delay.

The operand 3 is also extended with zeros to make it 58 bits, so that finally it can be added or subtracted with the Multiply mantissa output.
The addition/sub of the mantissa obtained after multiplication and mantissa of operand3, if done in similar manner in which floating point single precision addition and subtraction is executed.

*/

package fpu_fm_add_sub;
import functions::*; //Contains function for getting the number of leading zeros and trailing zeros in mantissa
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*; 
import integermultiplier_for_spfpu::*;
import riscv_types::*;
/*
typedef union tagged{ 	
		void No_exception;					// indicates that there was no exception generated
		Bool Invalid;						// indicates that the operation is invalid
		Bool Divide_by_Zero;					// indicates that the division operation is a divide by zero.
		Bool Overflow;						// indicates an overflow
		Bool Underflow;						// indicates an underflow
		Bool Inexact;						// indicates that the produced result is inexact
		}Exception deriving(Bits, Eq);
*/
typedef struct{
		
		Bit#(9) mul_exp_out;					//Exp_out after multiplication
		Bit#(24) mantissa3;				        //Mantissa of operand3
		Bit#(9) exponent3;                                      //Exponent of operand3
		bit sign3;					        //Sign bit of operand3
		bit is_invalid;						//Invalid bit 
		bit is_overflow;					//Overflow bit
		Bit#(2) is_zero;					//01=> plus zero 11=>Minus zero
		Bit#(2) is_infinity;					//01=>plus infinity 11=>Minus infinity
		bit _operation;					        //0 => Addition 1=>Subtraction
		bit round_down;						//When rounding mode is round_down
		Bit#(32) fsr_val;					//FSR value from fsr input
		bit sign_mul_out;					//Sign of result after multiplication of operand1 and operand2
		bit output_negate;
		} Stage_buffer deriving(Bits, Eq);
typedef struct {
                Bit#(58) mantissa_add;
                Bit#(32) fsr;
                bit sign_bit_output;
                bit output_negate;
                Bit#(9) final_exp_out;
                bit is_invalid;
                Bit#(2) is_infinity;
                Bit#(2) final_zero;
                bit final_overflow;} Stage3_data deriving(Bits, Eq);
typedef struct {
                Bit#(58) mantissa_add;
                Bit#(58) mul_mantissa_out;
                Bit#(58) addend;
                bit sign_mul_out;
                bit operation;
                Bit#(32) fsr;
                bit sign3;
                Bit#(9) final_exp_out;
                bit output_negate;
                bit is_invalid;
                Bit#(2) is_infinity;
                Bit#(2) final_zero;
                bit final_overflow;
} Stage2_data deriving(Bits,Eq);
/*	
typedef struct{
		Bit#(32) fsr;						// the file status register containing all the flags and control bits.
		Bit#(32) final_result;					// the final result for the operation
		Exception exception; 					// indicates if any exception is generated.
		}Output_type deriving(Bits,Eq);				// data structure of the output FIFO.
*/

interface Ifc_fpu_fm_add_sub;
	//Input Methods
	method Action _start(Bit#(32) operand1, Bit#(32) operand2, Bit#(32) operand3, bit operation, Bit#(32) fsr, bit _negate);  
	//Output Methods
	method Action deque_buffer();
	method Output_type get_result();
	
endinterface

(*synthesize*)
module mkfpu_fm_add_sub(Ifc_fpu_fm_add_sub);
	FIFOF#(Output_type) ff_final_out <-mkFIFOF();				       // output FIFO	
	FIFO#(Stage_buffer) ff_stage1 <- mkPipelineFIFO();				       //Inter stage FIFO	
	FIFO#(Stage2_data) ff_stage2 <- mkPipelineFIFO();				       //Inter stage FIFO	
	FIFO#(Stage3_data) ff_stage3 <- mkPipelineFIFO();				       //Inter stage FIFO	
	Ifc_integer_multiplier_for_spfmul integer_multiplier<-mkinteger_multiplier_for_spfmul; //Instantiation of the 7-Cylce Integer Multiplier.
	Wire#(Bool) wr_flush <-mkDWire(False);						 
	//Rule to flush data	
	rule rl_flush_data(wr_flush);
		ff_stage1.clear();
		ff_final_out.clear();
	endrule:rl_flush_data

	//Rule to get integer multiplier output and perform further operations

	rule stage2(!wr_flush);
		let lv_data = ff_stage1.first();						//Getting stage1 data
		ff_stage1.deq;
		Bit#(9) final_exp_out;								//For final_output exponent
		Bit#(9) exp_diff;								
		Bit#(58) mantissa_add;								//To hold the final mantissa addition output
		bit lv_final_overflow = lv_data.is_overflow;					//Final overflow bit
		Bit#(2) lv_final_zero = lv_data.is_zero; 

		Bit#(58) mul_mantissa_out = {1'b0,integer_multiplier.result_().final_result[47:21],30'b0}; //Extended multiplcation output	 

		if (integer_multiplier.result_().final_result[20:0]!=0) begin
			mul_mantissa_out[29] = 1; //Setting the sticky bit
		end
		
		Bit#(58) addend = {2'b0,lv_data.mantissa3,32'b0}; //Extended mantissa of operand3
		
		//Case when the output of the multiplier and operand3 is equal and we perform subtraction, then zero flag is raised
		if (({lv_data.sign3,lv_data.exponent3,addend}=={lv_data.sign_mul_out,lv_data.mul_exp_out,mul_mantissa_out}) && lv_data._operation==1) begin
			if (lv_data.round_down==1)                
				lv_final_zero = 2'b11;      //Minus zero
			else 
				lv_final_zero = 2'b01;     //Plus zero
		end 
		
		//******************************Calculating final_exponent and the shifting mantissa based on exponent difference*************************//	
		if (lv_data.mul_exp_out > lv_data.exponent3) begin          //When Mul_exp_out > exponent3
			final_exp_out = lv_data.mul_exp_out;                //Final exp is max exponent
			exp_diff = lv_data.mul_exp_out - lv_data.exponent3; //Finding exponent difference

			addend = addend>>exp_diff;  			    //Right shifting mantissa of operand3
			
			
			
		end
		else begin
			final_exp_out = lv_data.exponent3;		  
			exp_diff = lv_data.exponent3 - lv_data.mul_exp_out;
			mul_mantissa_out = mul_mantissa_out >>exp_diff;

		end
		
		//*******************Adding/Subtracting mantissa based on the sign bit************************//	
		bit lv_add_sub = ((lv_data.sign_mul_out^lv_data.sign3) & (~lv_data._operation)) | (~(lv_data.sign_mul_out^lv_data.sign3) & (lv_data._operation)); //0=>Mantissa addition
																				  //1=>Mantissa subtraction
		
		if (lv_add_sub==0)								
			mantissa_add = mul_mantissa_out + addend;                      //Mantissa addition
		else 	
			mantissa_add = mul_mantissa_out - addend;		       //Mantissa subtraction
        
        ff_stage2.enq(Stage2_data{mantissa_add :mantissa_add,
                               mul_mantissa_out : mul_mantissa_out,
                               addend : addend,
                               sign_mul_out : lv_data.sign_mul_out,
                               operation : lv_data._operation,
                               fsr : lv_data.fsr_val,
                               sign3 : lv_data.sign3,
                               final_exp_out:final_exp_out,
                               is_invalid: lv_data.is_invalid,
                               is_infinity:lv_data.is_infinity,
                               final_zero : lv_final_zero,
                               final_overflow : lv_final_overflow,
                               output_negate : lv_data.output_negate});
                           
    endrule

	rule stage3(!wr_flush);	
        let lv_data_stage2 = ff_stage2.first();
	ff_stage2.deq;
        let mantissa_add = lv_data_stage2.mantissa_add;
        let sign_mul_out = lv_data_stage2.sign_mul_out;
        let addend = lv_data_stage2.addend;
        let mul_mantissa_out = lv_data_stage2.mul_mantissa_out;
        let _operation = lv_data_stage2.operation;
        let sign3 = lv_data_stage2.sign3;
        let final_exp_out = lv_data_stage2.final_exp_out;
        bit lv_final_overflow = lv_data_stage2.final_overflow;

		//**********************************Determining the sign bit of the output**********************
		bit lv_sign_bit_output=0;
		if ( ((mul_mantissa_out<addend) && (sign_mul_out==0) && ((_operation ^ sign3)==1)) || ((sign_mul_out==1) &&((mul_mantissa_out>addend) || ((_operation ^ sign3)==1))) )	//Condition determined using k maps
			lv_sign_bit_output = 1;      
		else 
			lv_sign_bit_output =0;	      
		//*************************Changing exponent based on carry out and left zeros********************//
		
		Bit#(9) lv_leading_zeros = 0;
		if (mantissa_add[57]==1) begin         //When the MSB is one
			mantissa_add = mantissa_add>>2;//Mantissa_add shifted right by 2
			final_exp_out = final_exp_out +2;//Exponent incremented by 2
		end
		else if(mantissa_add[56]==1) begin  //When the second most signficant bit is one
			mantissa_add = mantissa_add >>1; //Mantissa_add shifted right by 1
			final_exp_out = final_exp_out +1;//Exponent incremented by 1
		end
		else begin
			lv_leading_zeros = fn_count_leadingzeros(mantissa_add[55:0]); //Counting the number of leading zeros
			mantissa_add = mantissa_add <<lv_leading_zeros;   //Left shift  mantissa_add by leading zeros
			final_exp_out = final_exp_out - lv_leading_zeros; //Decrement exponent by leading zeros
		end
		if (final_exp_out[8]==1)
			lv_final_overflow = 1;        //When exponent overflows
        
        ff_stage3.enq(Stage3_data{mantissa_add : mantissa_add,
                                  fsr : lv_data_stage2.fsr,
                                  sign_bit_output : lv_sign_bit_output,
                                  output_negate : lv_data_stage2.output_negate,
                                  final_exp_out : final_exp_out,
                                  is_invalid: lv_data_stage2.is_invalid,
                                  is_infinity:lv_data_stage2.is_infinity,
                                  final_zero : lv_data_stage2.final_zero,
                                  final_overflow : lv_final_overflow});

    endrule

    rule stage4(!wr_flush);
        let lv_data_stage3 = ff_stage3.first();
	ff_stage3.deq;
        let fsr = lv_data_stage3.fsr;
        let lv_sign_bit_output = lv_data_stage3.sign_bit_output;
        let mantissa_add = lv_data_stage3.mantissa_add;
        let final_exp_out = lv_data_stage3.final_exp_out;
        let lv_final_zero = lv_data_stage3.final_zero;
        let lv_final_overflow = lv_data_stage3.final_overflow;
		//***************Carry out the rounding operations*************************//
		bit lv_guard = mantissa_add[31];    //Setting guard bit
		bit lv_round = mantissa_add[30];    //Setting round bit
		bit lv_sticky=0;
		bit lv_roundup=0;
		if (mantissa_add[29:0] !=0)
			lv_sticky = 1;          //Setting sticky bit
		bit lv_inexact = lv_guard | lv_round | lv_sticky; //Setting inexact bit
         
        Bit#(3) rounding_mode = fsr [7:5];
		if(rounding_mode == 'b000)		                        // round to nearest, ties to even
			lv_roundup = lv_guard & (mantissa_add[32] | lv_round | lv_sticky);
			//lv_roundup = lv_guard & (lv_round | lv_sticky | ~lv_sign_bit_output);                     	
		else if(rounding_mode == 'b100)					// round to nearest, ties to max magnitude	
			lv_roundup = lv_guard & (lv_round | lv_sticky | ~lv_sign_bit_output);                     	
		else if(rounding_mode == 'b011)				// round down                               
			lv_roundup = lv_inexact & (lv_sign_bit_output);                                          	
		else if(rounding_mode == 'b010)				        // round up		                  	
			lv_roundup = lv_inexact & (~lv_sign_bit_output);  
		
		Bit#(26) final_mantissa=mantissa_add[57:32];  //Setting final_mantissa
		if (lv_roundup==1) begin                  
			final_mantissa = final_mantissa + 1;  //Mantissa is added
		end
		if (final_mantissa[24]==1) begin	     //When there is carry in mantissa_out
			final_mantissa = final_mantissa >>1; //Shift mantissa right
			final_exp_out = final_exp_out+1;     //Increment exponent by 1
		end
		//**************Negate output sign bit based on the negate input***************//	
		if (lv_data_stage3.output_negate==1) begin
			lv_sign_bit_output = ~lv_sign_bit_output;
		end	
		/****************The final output is determined based on exceptions flags***************************/
		bit lv_final_inexact = 0;                    
		bit lv_underflow=0;
		Bit#(32) lv_final_output=0;   //Variable to set the final output

		if(lv_data_stage3.is_invalid==1) begin //When invalid flag is raised
			lv_final_output = 32'b0_11111111_1000000000000000000000; // In case of invalid the default result is quiet NaN
		end
		else if(lv_data_stage3.is_infinity!=0) begin  //When infinity flag is raised
			lv_final_inexact = 1;		//Inexact is also set 1 when its infinity
			if(lv_data_stage3.is_infinity==2'b01) begin //Plus infinity
				lv_final_output=32'b0_11111111_0000000000000000000000;
			end
			else if (lv_data_stage3.is_infinity==2'b11) begin //Minus infinity
				lv_final_output=32'b1_11111111_0000000000000000000000;	
			end 
		end
		else if(final_exp_out[7:0]==8'b11111111) begin //When overflow condition
			lv_final_overflow = 1;	      
			lv_final_output= {lv_sign_bit_output,final_exp_out[7:0],final_mantissa[22:0]};
		end
		
		else if(final_exp_out[7:0]==8'b00000000 && final_mantissa !=0) begin//When underflow condition
			lv_underflow=1;
			lv_final_output= {lv_sign_bit_output,final_exp_out[7:0],final_mantissa[22:0]};	
		end
		else if (lv_inexact==1) begin     //When inexact is set
			lv_final_inexact = 1;
			lv_final_output= {lv_sign_bit_output,final_exp_out[7:0],final_mantissa[22:0]};	
					
		end
		else if(lv_final_zero!=0) begin  //Zero
			if(lv_final_zero == 2'b11) begin  //minus zero
				lv_final_output = 32'b1_00000000_000_0000_0000_0000_0000_0000;
			end
			else if(lv_final_zero == 2'b10) begin //Plus zero
				lv_final_output = 32'b0_00000000_00000000000000000000000;
			end
		end
		else begin  //Else the final output
			lv_final_output= {lv_sign_bit_output,final_exp_out[7:0],final_mantissa[22:0]};	
		end
		
		//***************Determining the floating point status register value*******************//
		//Details for FSR are in the riscv manual
		Bit#(32) lv_fsr= 0;
		lv_fsr = {fsr[31:10],1'b0,lv_final_zero[1],rounding_mode,2'b0,lv_final_overflow,lv_underflow,lv_final_inexact}; 

		//**********************************Setting exception flags***************************************//
		Exception lv_exception = tagged No_exception;
		if(lv_underflow==1)	
			lv_exception=tagged Underflow True;
		else if(lv_data_stage3.is_invalid==1)
			lv_exception= tagged Invalid True;
		else if(lv_final_overflow==1)
			lv_exception= tagged Overflow True;
		else if(lv_final_inexact==1)
			lv_exception= tagged Inexact True;

		ff_final_out.enq(Output_type{fsr :lv_fsr,
					     final_result : zeroExtend(lv_final_output),
					     exception : lv_exception});

		
				
	endrule	
	
	//Start method	
	method Action _start(Bit#(32) operand1, Bit#(32) operand2, Bit#(32) operand3,bit operation, Bit#(32) fsr,bit _negate); //operation=0 for add & 1 for subraction
		Bit#(2) lv_is_zero=0;						//=01 => result is plus zero & =11=> minus zero
		Bit#(2) lv_is_infinity=0;					//=01 => result is plus infinity and 11=> minus infinity
		bit lv_is_invalid=0;						//=1 => Invalid result
		bit lv_sticky= 0;						//=1 => SPFP result is inexact
		bit lv_overflow= 0;						//=1 => The SPFP result overflows
		//bit lv_underflow= 0;						//=1 => The SPFP result underflows
		bit lv_round_down =0;						//=1 => when rounddown rounding mode
		
		Bit#(24) mantissa1;						//Mantissa for operand1
		Bit#(8) exponent1;						//Exponenet for operand1
		bit sign1;							//Sign bit of operand1
		Bit#(24) mantissa2;						//Mantissa for operand2
		Bit#(8) exponent2;                                             	//Exponenet for operand2
		bit sign2;                                                     	//Sign bit of operand2
		Bit#(24) lv_mantissa3;						//Mantissa for operand2
		Bit#(9) lv_exponent3;                                             	//Exponenet for operand2
		bit lv_sign3;                                                     	//Sign bit of operand2
		Bit#(9) lv_mul_exp_out;

		Bit#(3) lv_rounding_mode = fsr[7:5];  				//Assigning the rounding mode from FSR input 
		if (lv_rounding_mode==3'b011) 					//WHen rounding mode==3 its round down
			lv_round_down=1;

		//*********Assinging values to exponent,mantissa and sign bit**************//
		exponent1=operand1[30:23];
		sign1=operand1[31];
		mantissa1={1'd0,operand1[22:0]}; 
 
		exponent2=operand2[30:23];
		sign2=operand2[31];
		mantissa2={1'd0,operand2[22:0]}; 
		
		lv_exponent3={1'b0,operand3[30:23]};
		lv_sign3=operand3[31];
		lv_mantissa3={1'd0,operand3[22:0]}; 
		
 		bit lv_sign_mul_out = sign1 ^sign2;
		
		//************************Handle the exceptions***********************************//
		if ((exponent1==8'b11111111 && mantissa1!=0) || (exponent2==8'b11111111 && mantissa2!=0) ||(lv_exponent3[7:0]==8'b11111111 && lv_mantissa3!=0)) begin //When any of three inputs is NaN
			lv_is_invalid = 1;
		end
		else if((exponent1==8'b11111111 && mantissa1==0) && (operand2[30:0]==0)) begin //Operand1 = inf, Operand2 =0
			lv_is_invalid=1;
		end
		else if((exponent2==8'b11111111 && mantissa2==0) && (operand1[30:0]==0)) begin //Operand1 = 0, Operand2 = inf
			lv_is_invalid=1;
		end
		else if ((exponent1==8'b11111111 && mantissa1==0) || (exponent2==8'b11111111 && mantissa2==0) && (lv_exponent3[7:0]==8'b11111111 && lv_mantissa3==0)) begin //When any of operand1 or operand2 is inf and operand 3 is also inf
			lv_is_invalid = 1;
		end
		else if ((exponent1==8'b11111111 && mantissa1==0) || (exponent2==8'b11111111 && mantissa2==0)) begin
			lv_is_infinity = {(sign1^sign2),1}; //11=> minus infinity 01=>Plus infinity
		end
		else if (lv_exponent3[7:0]==8'b11111111 && lv_mantissa3==0) begin
			lv_is_infinity= {(lv_sign3 ^ operation),1};
		end
		else if (operand1[30:0]==0 || operand2[30:0]==0 || operand3[30:0]==0) begin	
				
			if(lv_round_down==1 &&(lv_sign_mul_out|(operation^sign2))==1) begin  //Using Karnaugh maps, determining if its -0 or +0
				lv_is_zero= 2'b11;                              //Minus zero
			end
			else if (lv_round_down==0 && (lv_sign_mul_out & (operation^sign2))==1) begin//Using Karnaugh maps, determining if its -0 or +0
				lv_is_zero = 2'b11;		              //Minus zero 
			end
			else begin
				lv_is_zero = 2'b01;				//Plus zero
			end
	
		
		end
		
			
		
		//Operation
		lv_mul_exp_out = {0,exponent1} +{0,exponent2}-9'b001111111 ;

		if(lv_mul_exp_out[8]==1) 
			lv_overflow=1;

		//Setting the implicit bit
		if (exponent1!=0)  //if its normal number
			mantissa1[23]=1;
		if (exponent2!=0) //if its normal number
			mantissa2[23]=1;
		if (lv_exponent3!=0) //if its normal number
			lv_mantissa3[23]=1;
			
		integer_multiplier._start({8'b0,mantissa1},{8'b0,mantissa2},5'b0,32'b0,32'b0,4'b0,0,10'b0,0,1'b0,1'b0);
		
		ff_stage1.enq(Stage_buffer{mul_exp_out:lv_mul_exp_out,
					   exponent3:lv_exponent3,
					   mantissa3:lv_mantissa3,
					   sign3:lv_sign3,
					   is_invalid : lv_is_invalid,
					   is_zero : lv_is_zero,
					   is_infinity:lv_is_infinity,
					   is_overflow:lv_overflow,
					   _operation:operation,
					   sign_mul_out:lv_sign_mul_out,
					   round_down : lv_round_down,
					   fsr_val: fsr,
					   output_negate:_negate
					   });
	endmethod
		  	


	method Action deque_buffer();
		ff_final_out.deq();
	endmethod
	
	method Output_type get_result();
		return ff_final_out.first();
	endmethod

		

endmodule
//**********************Test bench*************************//
(*synthesize*)
module mkTb_fpu_fm_add_sub(Empty);
	Reg#(Bit#(32)) rg_clock <-mkReg(0);     //Clock register
	Reg#(Bit#(32)) rg_input1 <- mkReg(32'h4181170A);//16.13625 //2.5 in decimal
	Reg#(Bit#(32)) rg_input2 <- mkReg(32'h411A0000);//9.625//1.25 in decimal
	Reg#(Bit#(32)) rg_input3 <- mkReg(32'h41240000);//10.25 //4.5 in decimal
	
	Ifc_fpu_fm_add_sub fused_add_sub <- mkfpu_fm_add_sub;

	rule rl_clock;
		
		rg_clock<=rg_clock+1;
		//$display("CLOCK=%d",rg_clock);

		if(rg_clock=='d50)
		$finish(0);
	endrule
	
	rule give_input(rg_clock==1);
		fused_add_sub._start(rg_input1, rg_input2, rg_input3,0,32'b0,0); //FMA fused multiply add=((a*b)+c) FMSUB fused multiply sub f(a,b,c) = ((a*b)-c)			
	endrule
		
	rule get_output(rg_clock >1);
		let lv_output = fused_add_sub.get_result();
		//$display("Sign bit= %b ", lv_output.final_result[63]);
		$display("Output= %h" , lv_output.final_result);
		//$display("Exponent= %b ", lv_output.final_result[62:52]);
		//$display("Mantissa=%b " , lv_output.final_result[51:0]);
		fused_add_sub.deque_buffer();
	endrule
endmodule
endpackage
