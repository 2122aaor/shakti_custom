/*

Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name 	: Floating Point addition_subtraction Single and Double precision 
Author Name 	: Vinod.G
e-mail Id	: g.vinod1993@gmail.com
Last updated on : 5th January 2016


*/

package fpu_add_sub;
 //import functions::*;
   import riscv_types::*;                                           //Contains function for getting the number of leading zeros and trailing zeros in mantissa
   import FIFOF::*;
   import FIFO::*;
   import SpecialFIFOs::*;

/*   typedef struct{
//		  Bit#(5) destination;				      // holds the destination address of the operation in the registerfile
		  Bit#(32) fsr;				       	      // the file status register containing all the flags and control bits
		  Bit#(n) final_result;				      // the final result for the operation
//		  Exception exception; 				      // indicates if any exception is generated.
		} Output_type#(numeric type n) deriving(Bits,Eq);      // data structure of the output FIFO.
*/								      //TODO Custom ROB Entry
  typedef struct{ bit sign1;
                  bit sign2;
                  bit operation;
                  Bit#(TAdd#(man,5)) mantissa1;
                  Bit#(TAdd#(man,5)) mantissa2;
                  Bit#(expp) exponent_out; 
                  bit lv_is_invalid;
                  Bit#(2) lv_is_infinity;
                  Bit#(2) lv_is_zero;
		  Bit#(3) _rounding_mode;
		  bit lv_NaN;
		  bit lv_zero;
                  bit lv_infinity;
                  bit lv_negative;
                  bit lv_normal;
                } Stage1_data#(numeric type man, numeric type expp) deriving(Bits,Eq);

 typedef struct { Bit#(TAdd#(man,5)) new_mantissa;
                  bit sign_bit_output;
		  bit is_invalid; 
		  Bit#(2) is_infinity;
		  Bit#(2) is_zero;
		  bit _overflow;
		  bit _underflow;
		  Bit#(expp) exponent_out;
		  Bit#(3) _rounding_mode;
		  bit carry_out;
                  bit lv_NaN;
                  bit lv_zero;
		  bit lv_infinity;
                  bit lv_negative;
                  bit lv_normal;
		} Stage2_data#(numeric type man, numeric type expp) deriving(Bits,Eq);
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
  interface Ifc_fpu_add_sub#(numeric type fpinp, numeric type fpman, numeric type fpexp);
        method Action _start(Bit#(fpinp) operand1, Bit#(fpinp) operand2, bit operation, Bit#(3) rounding_mode);
        method Action _deque_buffer_();
        method Action _set_flush(Bool _flush);
        method Output_type _result(); 
    //    method Bool ready_();
  endinterface 	

 
  //(*synthesize*)
  module mkfpu_add_sub(Ifc_fpu_add_sub#(fpinp,fpman,fpexp))
        provisos( 
                Add#(TAdd#(fpexp,fpman),1,fpinp),	                //Defining fpinp to be fpexp + fpman + 1
                Add#(fpexp,2,fpexp2),
                Add#(fpman,5,fpman5),
                Add#(fpman,4,fpman4),
                Log#(fpman,fplog),
                Add#(fpinp1,1,fpinp),
 
		//Provisos required by the BSV Compiler
		Add#(a__, TLog#(TAdd#(1, fpman5)), fpexp),
                Add#(b__, 1, fpman5),
                Add#(2, c__, fpman5),
                Add#(d__, 3, c__),
                Add#(e__, 1, b__),
                Add#(f__, 1, fpexp2),
                Add#(g__, fplog, fpexp2),
                Log#(TAdd#(1, fpman4), fplog),
                Add#(1, i__, fpman),
                Add#(h__,fpinp,64)                
                );
        let fPINP  = valueOf(fpinp);
	let fPMAN  = valueOf(fpman);
	let fPEXP  = valueOf(fpexp);

        FIFO#(Output_type) ff_final_out <- mkPipelineFIFO();
        FIFO#(Stage1_data#(fpman,fpexp)) ff_stage1 <- mkPipelineFIFO();
	FIFO#(Stage2_data#(fpman,fpexp)) ff_stage2 <- mkPipelineFIFO(); 
        Reg#(Bool) rg_ready_signal <- mkReg(False);                     // Indicating output value in output_FIFO
        Wire#(Bool) wr_flush <- mkDWire(False);
       

        rule rl_flush(wr_flush);
		ff_final_out.clear();
		ff_stage1.clear();
		ff_stage2.clear();	
		rg_ready_signal<= False;
	endrule

        rule rl_stage2(!wr_flush);
        	let lv_data_stage1 = ff_stage1.first();
		ff_stage1.deq;
                let mantissa1 = lv_data_stage1.mantissa1;
                let mantissa2 = lv_data_stage1.mantissa2;
                let sign1 = lv_data_stage1.sign1;
                let sign2 = lv_data_stage1.sign2;
                let exponent_out  = lv_data_stage1.exponent_out;
                let lv_is_infinity = lv_data_stage1.lv_is_infinity;
                let lv_is_invalid = lv_data_stage1.lv_is_invalid;
                let lv_is_zero= lv_data_stage1.lv_is_zero;
                let operation = lv_data_stage1.operation; 
		let _rounding_mode = lv_data_stage1._rounding_mode;
		let lv_NaN = lv_data_stage1.lv_NaN;
		let lv_zero = lv_data_stage1.lv_zero;
		let lv_infinity = lv_data_stage1.lv_infinity;	
 		let lv_negative = lv_data_stage1.lv_negative;
		let lv_normal = lv_data_stage1.lv_normal;
		bit lv_overflow= 0;						//=1 => The FP result overflows
		bit lv_underflow = 0;

        /************************************************Add/Sub Mantissa based on sign bit**********************************************/
              
                Bit#(fpman5) lv_sum_mantissa;
                bit lv_sign_bit_output;
		bit lv_add_sub= ((sign1^sign2) & (~operation)) | (~(sign1^sign2) & (operation)); //0 for add and 1 for sub,determined using k Maps
                bit man2_gt_man1=0;
		bit man2_eq_man1=0;
		$display("man1: %h mantissa2: %h",mantissa1, mantissa2);
		if(mantissa2 > mantissa1)
			man2_gt_man1=1;
		else if(mantissa2==mantissa1)
			man2_eq_man1=1;
	
		if (lv_add_sub==0)                                             
			lv_sum_mantissa = mantissa1 + mantissa2;
		else if(man2_gt_man1==1) 
			lv_sum_mantissa = mantissa2- mantissa1;
		else 
			lv_sum_mantissa =  mantissa1- mantissa2;
	
		$display("lv_sum_mantissa= %h",lv_sum_mantissa);

		if(((mantissa1<mantissa2) && (sign1==0) && ((operation ^ sign2)==1)) || ((sign1==1) &&((mantissa1>mantissa2) || ((operation ^ sign2)==1))))			//Condition determined using k maps
			lv_sign_bit_output = 1;
		else 
			lv_sign_bit_output =0;
		$display("lv_sign_bit_output: %b",lv_sign_bit_output);
                Bit#(fplog) lv_zeros_on_left;
		bit lv_carry_out = lv_sum_mantissa[fPMAN+4];
		Bit#(fpman4) lv_v_sum_mantissa= lv_sum_mantissa[fPMAN+3:0];
		if (lv_carry_out==0 && lv_v_sum_mantissa!='d0)			  	  //Only when no carry out and mantissa !=0 	
			lv_zeros_on_left = pack(countZerosLSB(lv_v_sum_mantissa));       //Here the guard, round and sticky bit are also considered
		else 
			lv_zeros_on_left=0;  						  //No point in shifting if the whole mantissa is 0 or if the lv_carry_out=1
			Bit#(fpexp2) lv_exp_extend = {2'b0,exponent_out} + {'d0,lv_carry_out}-{'d0,lv_zeros_on_left}; 

		//*****************Determining the final mantissa and exponent output************************************
			Bit#(fpexp) lv_exp_out;                                        			  //To store the final output exponent
			Bit#(fpman5) lv_new_mantissa;							  //Store the new mantissa
			//**if the exp_extend is all 0, then we take care of it in exceptions
			if(lv_exp_extend[fPEXP+1]==1'b1) begin
				lv_underflow=1;
				lv_new_mantissa=0;
			end
			else if(lv_exp_extend[fPEXP]==1'b1) begin //When Exponent overflow we keep exp value to max exponent and mantissa same. We take care of the exceptions later
				lv_overflow=1;	      //Raising overflow flag
				lv_new_mantissa= 0;
			end						
			else if (lv_carry_out==1) begin //When there is carry out we shift mantissa by 1 rest exponent is taken care of earlier
				lv_new_mantissa = {1'b0, lv_sum_mantissa[fPMAN+4:2],lv_sum_mantissa[0]|lv_sum_mantissa[1]};
			end
			else begin
				lv_new_mantissa =  lv_sum_mantissa[fPMAN+4:0] << lv_zeros_on_left;  //Shift mantissa by number of left zeros
			end
	
			if( (lv_exp_extend[fPEXP+1] | lv_exp_extend[fPEXP]) == 1)
				lv_exp_out=0;
			else
				lv_exp_out = lv_exp_extend[fPEXP-1:0];  //Keep the exponent same

				ff_stage2.enq (Stage2_data{ new_mantissa    : lv_new_mantissa,   
                                            sign_bit_output : lv_sign_bit_output,
                                            is_invalid      : lv_is_invalid,       
                                            is_infinity     : lv_is_infinity,      
                                            is_zero         : lv_is_zero,
					    _underflow      : lv_underflow,        
                                            _overflow       : lv_overflow,         
                                            exponent_out    : lv_exp_out,
					    _rounding_mode  : _rounding_mode,
					    carry_out       : lv_carry_out,
                                            lv_NaN            : lv_NaN,
					     lv_zero          : lv_zero,
                                            lv_infinity       : lv_infinity,
					     lv_negative      : lv_negative,
					     lv_normal        : lv_normal
                                               });
                
 
	    endrule

	    rule rl_stage3(!wr_flush);
	            let lv_data_stage2 = ff_stage2.first();
		    ff_stage2.deq;
	            let lv_new_mantissa = lv_data_stage2.new_mantissa;
	            let lv_sign_bit_output = lv_data_stage2.sign_bit_output;
	            let lv_is_invalid = lv_data_stage2.is_invalid;
	            let lv_is_infinity= lv_data_stage2.is_infinity;
	            let lv_is_zero = lv_data_stage2.is_zero;
	            let lv_overflow = lv_data_stage2._overflow;
	            let lv_exp_out = lv_data_stage2.exponent_out;
	            let lv_underflow = lv_data_stage2._underflow;
	            let rounding_mode = lv_data_stage2._rounding_mode;
		    let lv_carry_out = lv_data_stage2.carry_out;
                    let lv_NaN = lv_data_stage2.lv_NaN;
		    let lv_zero = lv_data_stage2.lv_zero;
 	   	    let lv_infinity = lv_data_stage2.lv_infinity;
		    let lv_negative = lv_data_stage2.lv_negative; 
		    let lv_normal = lv_data_stage2.lv_normal;     	
	    
		    bit lv_guard = lv_new_mantissa[2];				//guard bit
    		    bit lv_round = lv_new_mantissa[1];				//round bit
		    bit lv_roundup = 0;					//=1 => the mantissa needs to be incremented by 1
                    bit lv_sticky = lv_new_mantissa[0];
		    bit lv_inexact = lv_guard|lv_round|lv_new_mantissa[0];	//=1 => the result is inexact	
		    bit lv_final_inexact=0;

		if(rounding_mode == 'b000)				// round to nearest, ties to even
			lv_roundup = lv_guard & (lv_new_mantissa[3] | lv_round | lv_sticky);
			//lv_roundup = lv_guard & (lv_round | lv_sticky | ~lv_sign_bit_output);                     	
		else if(rounding_mode == 'b100)			// round to nearest, ties to max magnitude	
			lv_roundup = lv_guard & (lv_round | lv_sticky | ~lv_sign_bit_output);                     	
		else if(rounding_mode == 'b011 )			// round down                               	
			 lv_roundup = lv_inexact & (lv_sign_bit_output);                                          	
		else if(rounding_mode == 'b010)				// round up		                  	
			 lv_roundup = lv_inexact & (~lv_sign_bit_output);    
		//rounding to zero will be b'001                                     	
		// else if the rounding mode is round_to_zero, roundup should be zero. Since the default value is zero, we needn't have an else if statement for that.		
		//After determing the roundup
		if(lv_roundup==1) begin	
			lv_new_mantissa = lv_new_mantissa + 'd8;	
		end
		if(lv_new_mantissa[fPMAN+4]==1) begin
			lv_new_mantissa= lv_new_mantissa >> 1;
			lv_exp_out= lv_exp_out +1;
		end
               /*********************************************Final Output Based on Exception Flags*****************************************************/
		Bit#(fpinp) lv_final_output=0;
			Bit#(fpexp) all_ones= 'd-1;
			if(lv_is_invalid==1) begin //When invalid flag is raised
				lv_final_output = {1'b0,all_ones,1'b1,'d0};
	            		lv_NaN = 1;
				lv_inexact= 0;
				lv_overflow = 0;
				lv_zero= 0;
	            		lv_carry_out=0;				
			end
                       	else if(lv_is_infinity!=0) begin  //When infinity flag is raised
				//lv_final_inexact = 1;		//Inexact is also set 1 when its infinity
	            	  	lv_infinity = 1;
	            	  	lv_carry_out=0;   
			  	lv_overflow = 0;           
			  	//lv_is_invalid= lv_inf_inv;
				lv_final_output={lv_is_infinity[1],all_ones,'d0};
				lv_negative= lv_is_infinity[1];
			end
			else if(lv_overflow==1 || lv_exp_out=='d-1) begin //When overflow condition
				$display("dpfloat_add_sub: Overflow has occured");
				lv_overflow=1;
				lv_final_inexact = 1;
				lv_normal=0;
				Bit#(fpinp1) lv_intermediate_result;
				if((rounding_mode == 'b011 && lv_sign_bit_output==0) || (rounding_mode == 'b010 && lv_sign_bit_output==1) || (rounding_mode=='b000))		// (round up and +ve result) or (round down and -ve result) or (round to even)
				begin
					lv_intermediate_result= {all_ones,'d0};
					lv_infinity= 1;
				end
				else begin
					Bit#(fpexp) max_finite_exp= 'd-2;
					lv_intermediate_result= {max_finite_exp,'d-1};
					lv_normal= 1;
				end
				lv_final_output= {lv_sign_bit_output, lv_intermediate_result};
				lv_negative= lv_sign_bit_output;
			end
			else if( ( |(lv_exp_out)==0 && lv_new_mantissa!=0 ) || lv_underflow==1) begin	//Checking underflow condition
				$display("underflow");	
				lv_underflow=1;
				lv_final_inexact = 1;
				lv_normal=0;
				lv_final_output= {lv_sign_bit_output,'d0};	
			end
			else if(lv_is_zero!=0) begin  //Zero
				lv_normal=0;
				if(lv_is_zero == 2'b11) begin  //minus zero
					lv_final_output = {1'b1,'d0};
				end
				else if(lv_is_zero == 2'b10) begin //Plus zero
					lv_final_output = 'd0;
				end
			end
			else begin
				lv_final_output= {lv_sign_bit_output,lv_exp_out,lv_new_mantissa[fPMAN+2:3]};	
				if (lv_inexact==1)
					lv_final_inexact = 1;
			end




	      /****************************************************************************************************************************************/
	
		//***************Determining the floating point status register value*******************//
		//Details for FSR are in the riscv manual
		Bit#(32) lv_fsr;
		lv_fsr = {24'b0,rounding_mode,2'b0,lv_overflow,lv_underflow,lv_final_inexact}; 
                ff_final_out.enq(Output_type{fsr : lv_fsr,
					     final_result : zeroExtend(lv_final_output)
					     //exception : lv_exception
                                             });	
		
 
	    endrule


        method Action _start (Bit#(fpinp) operand1, Bit#(fpinp) operand2, bit operation, Bit#(3) rounding_mode) if(!wr_flush);
        	bit lv_normal = 0;
	        bit lv_negative = 0;
	        bit lv_NaN = 0;
	        bit lv_zero = 0;
	        bit lv_infinity = 0;
                Bit#(2) lv_is_zero=0;						//=01 => result is plus zero & =11=> minus zero
		Bit#(2) lv_is_infinity=0;					//=01 => result is plus infinity and 11=> minus infinity
		bit lv_is_invalid=0;						//=1 => Invalid result
		bit lv_sticky= 0;						//=1 => FP result is inexact
		bit lv_underflow= 0;//psw[10];					//=1 => The FP result underflows
		bit round_down =0;						//=1 => when rounddown rounding mode
                Bit#(fpman5) mantissa1;
                Bit#(fpexp) exponent1;
                bit sign1;
                Bit#(fpman5) mantissa2;
                Bit#(fpexp) exponent2;
                bit sign2;

                if(rounding_mode == 'b010)
                    round_down = 1;
                /**********************************Getting The Exponent and Mantissa for Each Operand**********************************/

                exponent1 = operand1[fPINP-2:fPMAN];
                sign1 = operand1[fPINP-1];
                mantissa1 = {2'b00,operand1[fPMAN-1:0],3'b000};               // 3'b000 - Guard, Round and Sticky bit ; 2'b00 - Carry and Hidden

                exponent2 = operand2[fPINP-2:fPMAN];
                sign2 = operand2[fPINP-1];
                mantissa2 = {2'b00, operand2[fPMAN-1:0],3'b000};

                /**********************************************************************************************************************/    
                                 
                bit exp1_is_ones = &(exponent1);
		bit exp2_is_ones = &(exponent2);
	 	bit exp1_is_not_zeros= |(exponent1);
		bit exp2_is_not_zeros= |(exponent2);
	 	bit man1_is_ones = &(mantissa1);
	 	bit man2_is_ones = &(mantissa2);
	 	Bit#(fpman) lv_man11=mantissa1[fPMAN+2:3];
	 	bit man1_is_not_zeros= |(lv_man11);
	 	Bit#(fpman)lv_man22=mantissa2[fPMAN+2:3];
	 	bit man2_is_not_zeros= |(lv_man22);
	 	Bit#(fpinp1) is_zero= 'd0;
                              
               /********************************************Exceptions & Flags*********************************************************/
                 Bit#(fpinp1) lv_mod_op1_value= operand1[fPINP-2:0];
		 Bit#(fpinp1) lv_mod_op2_value= operand2[fPINP-2:0];

                 if ((exp1_is_ones == 1 && man1_is_not_zeros == 0) && (exp2_is_ones == 1 && man2_is_not_zeros == 0))       // Both are Infinity -> Invalid
                         lv_is_invalid = 1;
                
		 else if ((exp1_is_ones==1 && man1_is_not_zeros==1) || (exp2_is_ones==1 && man2_is_not_zeros==1))  // Any is NaN ->Invalid
			 lv_is_invalid = 1;
                
		 else if (exp1_is_ones==1 && man1_is_not_zeros==0)                                                //operand1 is infinity and mantissa1!=0
			 lv_is_infinity={sign1,1};
		
		 else if (exp2_is_ones==1 && man2_is_not_zeros==0)                                                //Operand2 is infinity and mantissa2!=0
			 lv_is_infinity={(sign2^operation),1};  

                 else if (operand1[fPINP-2:0]==is_zero && operand2[fPINP-2:0]==is_zero) begin                     //Both are zeros -> Zero

			 if(round_down==1 &&(sign1|(operation^sign2))==1) begin                        //Using Karnaugh maps, determining if its -0 or +0
			 	lv_is_zero= 2'b11;                                                                //Minus zero
			 end
			 
  			 else if (round_down==0 && (sign1&(operation^sign2))==1) begin                 //Using Karnaugh maps, determining if its -0 or +0
			 	lv_is_zero = 2'b11;				                                 //Minus zero
			 end

			 else begin
				lv_is_zero = 2'b01;				       	                         //Plus zero
			 end
		  end

		  else if(lv_mod_op1_value == lv_mod_op2_value && (operation^sign2) != sign1) begin //For Sub instruction, when operand1==operand2

			 if (round_down==1)  begin              
				lv_is_zero = 2'b11;  
				//$display("op1=op2 and round down. Hence result= -0.0");
			 end
			 else 
				lv_is_zero = 2'b01;
			 
                  end

                /******************************************Implicit Bit based on Normal/Denormal Numbers***************************************/	
 				
                  if (exp1_is_not_zeros==1)  //if its normal number
			 mantissa1[fPMAN+3]=1'b1;
		  else
		         mantissa1 = 'd0;
	
		  if (exp2_is_not_zeros==1) 						 //if its normal number
			 mantissa2[fPMAN+3]=1'b1;
		  else
			 mantissa2 = 'd0;

                  Bit#(fpexp) exp_diff;                           			 //Difference between two exponents
		  Bit#(fpexp) exponent_out=0;			    			 //Exponent of the output
		  Bit#(fpexp) lv_zeros_on_right;	            			 //Store the zeros on the right of the last 1 from MSB
		  Bit#(fpexp) lv_minuend, lv_subtrahend;
		  Bit#(fpman5) mantissa_to_shift;
		  bit op1_gt_op2;
		  if(exponent1>exponent2) begin		    				 //When exp1>exp2
		  	lv_minuend = exponent1;
			lv_subtrahend = exponent2;   				         //Calculating exp diff
			mantissa_to_shift = mantissa2;
			op1_gt_op2 = 1;
		  end
		  else begin								//When exp2>exp1                                          
			lv_minuend = exponent2;
			lv_subtrahend = exponent1;
			mantissa_to_shift = mantissa1;
			op1_gt_op2 = 0;
		  end
	
			exponent_out= lv_minuend;                                       //Ouput exponent ==Largest exponent
			exp_diff = lv_minuend - lv_subtrahend;			        //Calculating exp diff
		

                  lv_zeros_on_right = zeroExtend(pack(countZerosMSB(mantissa_to_shift)));
		  $display("zeros on right: %d exp_diff: %d",lv_zeros_on_right, exp_diff);
		  mantissa_to_shift = (mantissa_to_shift >>exp_diff);                   //Right shifting mantissa by exp_diff and setting the sticky bit
                  if ( (lv_zeros_on_right<exp_diff) || mantissa_to_shift[0]==1 )    				 
				lv_sticky =1;
			mantissa_to_shift = {mantissa_to_shift[fPMAN+4:1],lv_sticky};
	
			if(op1_gt_op2==1)
				mantissa2= mantissa_to_shift;
			else
				mantissa1= mantissa_to_shift;
      
                        ff_stage1.enq(Stage1_data {sign1: sign1,
                                      sign2: sign2,
                                     operation: operation,
                                      mantissa1: mantissa1,
                                      mantissa2: mantissa2,
                                      exponent_out: exponent_out,
                                      lv_is_invalid:lv_is_invalid,
                                      lv_is_infinity:lv_is_infinity,
                                      lv_is_zero : lv_is_zero,
				      _rounding_mode : rounding_mode,
				      lv_NaN : lv_NaN,
				      lv_zero : lv_zero,
                                      lv_infinity: lv_infinity,
                                      lv_negative : lv_negative,
                                      lv_normal : lv_normal
                                      });                                                    //Remainder, use fsr if needed!
       endmethod 
 

method Action _deque_buffer_();	
	        ff_final_out.deq();
		endmethod

method Action _set_flush(Bool _flush);
			wr_flush<= True;
	    endmethod
/*
method Bool ready_();
	        return rg_ready_signal;
	    endmethod
*/
method Output_type _result();
          return ff_final_out.first();
endmethod

endmodule

/*
(*synthesize*)
module mkTb_fpu_add_sub();


//32 bit inputs
Ifc_fpu_add_sub#(32,23,8) instance_fpu_add_sub <- mkfpu_add_sub();
Reg#(Bit#(32)) rg_clock <- mkReg(0);
Reg#(Bit#(32)) rg_operand1<-mkReg('hff800000); 
Reg#(Bit#(32)) rg_operand2<-mkReg('hff800000); 

/* 
//64 bit inputs
Ifc_fpu_add_sub#(64,52,11) instance_fpu_add_sub <- mkfpu_add_sub();

Reg#(Bit#(32)) rg_clock <- mkReg(0);
Reg#(Bit#(64)) rg_operand1<-mkReg('hfff00000_00000000); 
Reg#(Bit#(64)) rg_operand2<-mkReg('hfff00000_00000000); 
*/
/*
rule get_input(rg_clock == 0);
instance_fpu_add_sub._start(rg_operand1,rg_operand2,1,000);
rg_clock <= rg_clock + 1;
endrule


rule get_output;
let lv_result = instance_fpu_add_sub._result();  
instance_fpu_add_sub._deque_buffer_();
$display("Result is: %h",lv_result.final_result);
$display("Sign=%b Exponent=%b Mantissa=%b",lv_result.final_result[63],lv_result.final_result[62:52],lv_result.final_result[51:0]);
$finish(0);
endrule
endmodule
*/

endpackage
