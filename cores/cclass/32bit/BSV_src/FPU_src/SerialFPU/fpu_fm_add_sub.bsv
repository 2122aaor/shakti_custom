/*
Copyright (c) 2013-2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name   	: Single and Double Precision Fused Multiply-Add Unit
Author's Name 	: Aditya Govardhan, Vinod.G
e-mail id   	: dtgovardhan@gmail.com, g.vinod1993@gmail.com
Last updated on : 17th August 2016

Paper Reference: Floating Point Fused Multiply-Add Architectures (http://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=4487224)

*/

package fpu_fm_add_sub;

import integermultiplier::*;
import defined_types::*;
import RegFile::*;

interface Ifc_fpu_fm_add_sub#(numeric type fpinp, numeric type fpman, numeric type fpexp, numeric type loop);
	method Action _start(Bit#(fpinp) operand1, Bit#(fpinp) operand2, Bit#(fpinp) _operand3, Bit#(3) rounding_mode, bit operation, bit _negate,bit mul);  
	method Floating_output#(fpinp) get_result();	
endinterface

typedef struct{
	Bit#(TAdd#(fpman,1)) mantissa1; 							// mantissa of operand1
	Bit#(TAdd#(fpman,1)) mantissa2; 							// mantissa of operand2
	Bit#(TAdd#(fpexp,2)) lv_summed_exponent;				// exponent of the resultant
	bit sign; 														// sign bit of the result
   Bit#(fpinp) _operand3;
	Bit#(1) invalid;												// indicating that the ff_output is NaN.
	Bit#(1) infinity;												// indicating that the ff_output is infinity.
	Bit#(1) zero;													// indicating that the ff_output is zero.
   Bit#(3) rounding_mode;  									// static rounding mode encoded in the instruction
   bit _operation;                                    // bit denoting the operation to be performed 0 - Add, 1 - Sub
   bit _negate;                                       // bit denoting whether the operands should be negated or not
	bit mul;                                           // bit denoting whether the operation is mul or not
	}Input_data_type#(numeric type fpinp, numeric type fpman, numeric type fpexp) deriving (Bits,Eq);

typedef struct{
   Bit#(TMul#(fpinp,4)) final_result;                 //The result of the integer multiplier stage
   Bit#(1) zero;
   Bit#(1) invalid;
   Bit#(1) infinity;
   bit _negate;
   bit operation;
   bit sign;
   Bit#(TAdd#(fpexp,2)) summed_exponent;
   Bit#(fpinp) _operand3;
   Bit#(3) rounding_mode;
	bit mul;
   }Stage2_data_type#(numeric type fpinp, numeric type fpexp) deriving (Bits,Eq);

typedef struct{
   bit sign2;
   bit sign3;
   Bit#(TAdd#(fpexp,2)) exponent2;
   Bit#(TAdd#(fpexp,2)) exponent3;
   Bit#(TAdd#(TMul#(fpman,3),4)) mantissa2;  
   Bit#(TAdd#(TMul#(fpman,3),4)) mantissa3;
   Bit#(3) rounding_mode;
   bit operation;
   bit _negate;
   bit result_is_invalid;
   Bit#(2) result_is_infinity;
   Bit#(2) result_is_zero;
   bit product_overflow;
   bit product_underflow;
	bit mul;
   }Stage3_data_type#(numeric type fpman, numeric type fpexp) deriving (Bits,Eq);

typedef struct{
	bit resultant_sign;
	Bit#(TAdd#(fpexp,2)) resultant_exponent;
	Bit#(TAdd#(TMul#(fpman,3),4)) resultant_mantissa;
	Bit#(3) rounding_mode;
	bit _negate;
	bit result_is_invalid;
	Bit#(2) result_is_infinity;
	Bit#(2) result_is_zero;
	Bit#(2) add_sub_is_zero;
	bit product_overflow;
	bit product_underflow;
	bit mul;
	}Stage4_data_type#(numeric type fpman, numeric type fpexp) deriving (Bits,Eq);

module mkfpu_fm_add_sub(Ifc_fpu_fm_add_sub#(fpinp,fpman,fpexp,loop))
     provisos(
              Add#(TAdd#(fpexp,fpman),1,fpinp),
              Add#(fpexp,2,fpexp2),
              Add#(TMul#(fpman,3),4,fmaman),
              Add#(fpman,1,fpman1),
              Add#(fpexp,1,fpexp1),
              Add#(a__,loop,128),
              Add#(b__,fpinp,64),
              Mul#(TAdd#(fpman,1),2,impfpman2),
              Add#(fpinp,fpinp,fpinp2),
              //per request of bsc
              Add#(c__, TSub#(fpexp, 1), fpexp1),
              Add#(d__,1,fpexp2),          
              Add#(e__, fpexp2, fpman),
              Add#(f__, TSub#(fpexp, 1), fpman),
              Add#(g__, TAdd#(fpman, 1), fpinp),
              Add#(h__, TSub#(fpexp, 1), fpexp2),
              Add#(i__, TLog#(TAdd#(1, TAdd#(impfpman2, 1))), fpexp2),
              Add#(j__, TLog#(TAdd#(1, fmaman)), fpexp2),
              Add#(k__, TAdd#(impfpman2, 1), fmaman),
              Add#(l__, 1, fmaman),
              Add#(m__, TAdd#(2, fpman), fmaman),
              Add#(n__,TAdd#(fpman,1),fpinp2)
             );

	Wire#(Floating_output#(fpinp)) ff_final_out										<-		mkWire(); 					//Register holding the final output of the operation
   Reg#(Maybe#(Stage2_data_type#(fpinp,fpexp))) ff_stage2 						<-		mkReg(tagged Invalid);
	Reg#(Maybe#(Stage3_data_type#(fpman,fpexp))) ff_stage3 						<-		mkReg(tagged Invalid);	// intermediate FIFO
	Reg#(Maybe#(Stage4_data_type#(fpman,fpexp))) ff_stage4 						<-		mkReg(tagged Invalid);
	Reg#(Maybe#(Input_data_type#(fpinp,fpman,fpexp))) ff_input_register		<-		mkReg(tagged Invalid);	// input FIFO

    function zeroExtendLSB(inp_man) = unpack(reverseBits(extend(reverseBits(pack(inp_man)))));           //Function performing zeroExtend on LSB bits
  
    let fPMAN 		= 	valueOf(fpman);
    let fPINP 		= 	valueOf(fpinp);
    let fPEXP 		= 	valueOf(fpexp);
    let iMPFPMAN2 = 	valueOf(impfpman2); 
    let fMAMAN 	= 	valueOf(fmaman);
    Ifc_integermultiplier#(fpinp2,loop) mult 										<- 	mkintegermultiplier();  //Instantiation of Cycle parameterized simple shift add integer multiplier


    rule rl_stage1_after_input_stage(ff_input_register matches tagged Valid .lv_input_stage);

        let x <- mult._start(zeroExtend(lv_input_stage.mantissa1),zeroExtend(lv_input_stage.mantissa2));

		  if(x matches tagged Valid .res) begin
        ff_stage2<= tagged Valid Stage2_data_type{ final_result      			  : 			res,
                                           		   zero              			  : 			lv_input_stage.zero,
                                        			   invalid           			  : 			lv_input_stage.invalid,
                                        				infinity          			  : 			lv_input_stage.infinity,
                                        				_negate           			  : 			lv_input_stage._negate,
                                        				operation         			  : 			lv_input_stage._operation,
                                        				sign              			  : 			lv_input_stage.sign,
                                        				summed_exponent   			  : 			lv_input_stage.lv_summed_exponent,
                                        				_operand3         			  : 			lv_input_stage._operand3,
                                        				rounding_mode     			  : 			lv_input_stage.rounding_mode,
										          				mul               			  : 			lv_input_stage.mul 
										                   };
        ff_input_register <= tagged Invalid;
        end

    endrule


	rule rl_stage_3_after_integer_multiplier_stage(ff_stage2 matches tagged Valid .integer_multiplier);

	   Bit#(TAdd#(impfpman2,1)) lv_product_mantissa			= 		{integer_multiplier.final_result[iMPFPMAN2-1:0],1'b0}; // extra zero for 10.xxxx case
		Bit#(fpexp2) lv_product_exponent 						= 		integer_multiplier.summed_exponent();
		Bit#(fpinp) lv_operand3										= 		integer_multiplier._operand3;
		let lv_product_is_invalid 									= 		integer_multiplier.invalid;
		let lv_product_is_infinity 								= 		integer_multiplier.infinity;
		let lv_product_is_zero 										= 		integer_multiplier.zero;	
		let lv_rounding_mode											= 		integer_multiplier.rounding_mode;
		let operation													= 		integer_multiplier.operation;
		let lv_negate 													= 		integer_multiplier._negate;
		let lv_product_sign											= 		integer_multiplier.sign;
		let mul 															= 		integer_multiplier.mul;
		bit lv_product_underflow 									= 		0;
		bit lv_product_overflow 									= 		0;

		ff_stage2 <= tagged Invalid;


      $display("lv_complete_mantissa = %h", integer_multiplier.final_result);
	 	$display("lv_product_mantissa = %b", lv_product_mantissa);

	 	Bit#(TSub#(fpexp,1)) bias 					 		= 	'1;
	 	Int#(fpexp2) lv_actual_product_exponent 		= 	unpack(lv_product_exponent - {3'b0,bias});
	 	let msb_zeros 									 		= 	pack(countZerosMSB(lv_product_mantissa));
	 	let lsb_zeros 											= 	0;
      Bit#(fpman) bias_temp 								= 	zeroExtend(bias);
      Int#(fpman) lv_actual_product_exponent_temp 	=  zeroExtend(lv_actual_product_exponent);


	 	// lv_product_is_subnormal construct is like a flag which can be used in difficult situations
	 	// bit lv_product_is_subnormal = 0;

		bit lv_sticky = lv_product_mantissa[0];
	 	$display("and thus the sticky bit = %b", lv_sticky);

	 	/*
	 	if exponent is > 127 then obviously none of the numbers are subnormal
	 	so the product is of the form 1x.xxxx or 01.xxxx
	 	the overflow conditions are handled in the following if condition accordingly
		*/

	  if(lv_actual_product_exponent > zeroExtend(unpack(bias)) || (msb_zeros == 0 && lv_actual_product_exponent == zeroExtend(unpack(bias)))) begin
	 		lv_product_overflow = 1;
	 		//When the product overflows, the FMA result is an overflow
	 		$display("lv_product_overflow!!!");
	 	end

	 	/*
		-151 = -126 -23 -2
		-2 is for the implicit bit and the carry bit
		i.e. if all the bits are shifted out then its an underflow
	 	*/

  	  else if(lv_actual_product_exponent_temp < unpack(-bias_temp-fromInteger(fPMAN)-1) ) begin
	 		lv_product_underflow = 1;
	 		lv_product_mantissa 	= 1;
	 		lv_product_exponent 	= 0;
	 		//When the exponent is < -151, sticky bit is automatically set to one
	 		$display("lv_product_underflow!!!");
	 	end
	 	
	 	else begin
	 		/*
	 		if msb of product is 1 then the case is 1x.xxxx
	 		product is shifted right once to make it 01.xxxx
	 		we don't care what is the exponent, just increase it by one
	 		actual exponent is also increased by one since exponent is increased by one
	 		this increasing of exponent leading to overflow is handled in the overflow case
	 		msb_zeros is increased for further arising conditions
	 		*/
	 		if(msb_zeros == 0) begin
	 			lv_product_mantissa 			= 	lv_product_mantissa >> 1;	 			
	 			lv_product_exponent 			= 	lv_product_exponent + 1;
	 			lv_actual_product_exponent = 	lv_actual_product_exponent + 1;
	 			msb_zeros 						= 	msb_zeros + 1;
	 		end

 			// possible shift is positive when exponent is lesser than -126
 			Int#(fpexp2) possible_shift 	= 	1-zeroExtend(unpack(bias))-(lv_actual_product_exponent);

 			/*
 			msb_zeros = 1 when
 			i)  the product is 1x.xxxx and shifted right once
 			ii) the product is 01.xxxx already
 			if possible_shift is negative or zero, it means that exponent is -126 or greater
 			and thus the product is already normalized
 			but if possible_shift is positive, it means that exponent is < -126
 			and thus product is shifted right to make exponent -126 and the result is subnormal
 			*/
	 		if((msb_zeros == 1 || msb_zeros != 1) && possible_shift > 0) begin

	 			//Setting sticky if all lsb zeros are removed out
	 			lsb_zeros = pack(countZerosLSB(lv_product_mantissa));

	 			if(possible_shift > unpack(zeroExtend(lsb_zeros)) || lv_product_mantissa[0] == 1) 
						  lv_sticky = 1;

	 			//Handling sticky
 				lv_product_mantissa 	= 	lv_product_mantissa >> pack(possible_shift);
 				lv_product_mantissa 	= 	{lv_product_mantissa[iMPFPMAN2:1], lv_product_mantissa[0] | lv_sticky};
 				lv_sticky 			  	= 	lv_product_mantissa[0];
 				$display("lv_product_mantissa = %b since exp < -126", lv_product_mantissa);
 				$display("and thus the sticky bit = %b", lv_sticky);
 				lv_product_exponent 	=  lv_product_exponent + pack(possible_shift);
 				// lv_product_is_subnormal = 1;
 			end

 			/*
 			msb_zeros != 1 means product is of the form 00.xxxx, important case
 			*/
	 		else if(msb_zeros != 'b1) begin
	 			/*
				if possible shift is < the number of leading zeros then the number can't be made normal
	 			*/
	 			if((~pack(possible_shift)+1) < zeroExtend(msb_zeros - 1)) begin

	 				lv_product_mantissa = lv_product_mantissa << (~pack(possible_shift)+1);
	 				lv_product_exponent = lv_product_exponent - (~pack(possible_shift)+1);
	 				// lv_product_is_subnormal = 1;
	 			end
	 			/*
	 			if exponent affords to give away enough such that shifting left leads to 01.xxxx and exponent >= -126
	 			*/
	 			else begin
		 			lv_product_mantissa = lv_product_mantissa << (msb_zeros - 1);
		 			lv_product_exponent = lv_product_exponent - (zeroExtend(msb_zeros) - 1);
		 			// lv_product_is_subnormal = 0;
		 		end
	 		end
	 	end

 		// if(lv_product_is_subnormal == 1) lv_product_exponent = 0;
		
		Bit#(1) sign2 				= 	lv_product_sign ^ lv_negate;
		Bit#(fpexp2) exponent2 	= 	lv_product_exponent;
		Bit#(fmaman) mantissa2 	= 	zeroExtendLSB(lv_product_mantissa);
		Bit#(1) sign3 				= 	lv_operand3[fPINP-1] ^ lv_negate;     
		Bit#(fpexp2) exponent3 	= 	{2'b0, lv_operand3[fPINP-2:fPMAN]};
		Bit#(fmaman) mantissa3 	= 	0;
      Bit#(fpman) lv_man3 		= 	lv_operand3[fPMAN-1:0];
      Bit#(fpexp) lv_exp_max 	= 	'1;
		bit lv_op3_is_invalid 	= 	0;
		bit lv_op3_is_infinity 	= 	0;
		bit lv_op3_is_zero 		= 	0;
      bit man3_is_zero 			= 	~(|(lv_man3));
      bit exp3_is_zero	 		= 	~(|(exponent3));
		bit op3_is_subnormal 	= ~man3_is_zero & exp3_is_zero;


		if(exponent3 == zeroExtend(lv_exp_max)) begin
			if(lv_man3 == 0) 	
				lv_op3_is_infinity = 1;
			else 				
				lv_op3_is_invalid = 1;
		end
		else if(exponent3 == '0) begin
			if(lv_man3 == 0) 	
				lv_op3_is_zero = 1;
			else 				
				mantissa3 = zeroExtendLSB({2'b0, lv_man3});
		end
		else 
			mantissa3 = zeroExtendLSB({2'b01, lv_man3});

      exponent3 = exponent3 + zeroExtend(op3_is_subnormal);

		Bit#(1) lv_result_is_invalid 	= 0;
		Bit#(2) lv_result_is_infinity = 0;
		Bit#(2) lv_result_is_zero 		= 0;

    	//Result is invalid cases
		if(lv_op3_is_invalid == 1 || lv_product_is_invalid == 1)
			lv_result_is_invalid = 1;

		//Result is zero cases
		else if(lv_op3_is_zero == 1 && lv_product_is_zero == 1) begin
			if(mul==0) begin
				if((lv_rounding_mode == 'b010) && (sign2 | (operation ^ sign3)) == 1)
                lv_result_is_zero = 2'b11; 
				else if((lv_rounding_mode != 'b010) && (sign2 & (operation ^ sign3)) == 1) 	
                lv_result_is_zero = 2'b11;
         	else begin
       			 $display("Oops");
        			 if(sign2 == 0)
         				lv_result_is_zero = 2'b01;
        			 else
         				lv_result_is_zero = {operation ^ sign3,1'b1};
				end
			end
			else
				  lv_result_is_zero = {sign2,1};
		end

		//Result is infinity cases
		else if(lv_product_is_infinity == 1 && lv_op3_is_infinity == 1) begin
			lv_result_is_infinity 	= {sign2, ~(sign2 ^ (operation ^ sign3))};
			lv_result_is_invalid 	= ~lv_result_is_infinity[0];
		end
		else if(lv_product_is_infinity == 1 || lv_op3_is_infinity == 1) begin
			lv_result_is_infinity 	= {((lv_product_is_infinity & ~lv_op3_is_infinity) & sign2) | ((~lv_product_is_infinity & lv_op3_is_infinity) & (operation ^ sign3)), 1};
		end
		if(lv_product_is_zero == 1) begin
			exponent2 = '0;
			mantissa2 = '0;
		end

		$display("sign2 = %b exponent2 = %b mantissa2 = %b", sign2, exponent2, mantissa2);
		$display("sign3 = %b exponent3 = %b mantissa3 = %b", sign3, exponent3, mantissa3);
		$display("lv_result_is_zero = %b lv_result_is_infinity = %b lv_result_is_invalid = %b", lv_result_is_zero, lv_result_is_infinity, lv_result_is_invalid);

		ff_stage3 <= tagged Valid Stage3_data_type{ 	
																	sign2						: 		sign2,
    																sign3						: 		sign3,
    																exponent2				:		exponent2, 
  							  										exponent3				: 		exponent3,
						    										mantissa2				: 		mantissa2,
						    										mantissa3				: 		mantissa3,
																	rounding_mode			:		lv_rounding_mode,
						    										operation				: 		operation,
																	_negate					: 		lv_negate,
						    										result_is_invalid		: 		lv_result_is_invalid,
						    										result_is_infinity	: 		lv_result_is_infinity,
						    										result_is_zero			: 		lv_result_is_zero,
						    										product_overflow		: 		lv_product_overflow,
						    										product_underflow		: 		lv_product_underflow,
																	mul 						: 		mul
    															};

	endrule:rl_stage_3_after_integer_multiplier_stage

	rule rl_stage_4(ff_stage3 matches tagged Valid .stage3_data);

	    bit lv_sign2							=     stage3_data.sign2;
	    bit lv_sign3							=     stage3_data.sign3;
	    Bit#(fmaman) lv_mantissa2			=     stage3_data.mantissa2;
	    Bit#(fmaman) lv_mantissa3			=     stage3_data.mantissa3;
	    Bit#(fpexp2) lv_exponent2			=     stage3_data.exponent2;
	    Bit#(fpexp2) lv_exponent3			=     stage3_data.exponent3;
	    Bit#(3) lv_rounding_mode			=     stage3_data.rounding_mode;
	    bit lv_operation						=     stage3_data.operation;
	    bit lv_negate 						=     stage3_data._negate;
	    bit lv_result_is_invalid			=     stage3_data.result_is_invalid;
	    Bit#(2) lv_result_is_infinity	=     stage3_data.result_is_infinity;
	    Bit#(2) lv_result_is_zero			=     stage3_data.result_is_zero;
	    bit lv_product_overflow			=     stage3_data.product_overflow;
	    bit lv_product_underflow			=     stage3_data.product_underflow;
       let mul = stage3_data.mul;
	    ff_stage3 <= tagged Invalid;

	    Bit#(fpexp2) lv_minuend, lv_subtrahend;
	    Bit#(fpexp2) exponent_difference = '0;
	    Bit#(fpexp2) resultant_exponent = '0;
	    bit op2_gt_op3 = 0;

	    Bit#(fmaman) mantissa_to_shift;
	    let lv_zeros_on_right;
	    bit lv_sticky = 0;
	    
	    if(lv_exponent2 > lv_exponent3) begin
	    	lv_minuend = lv_exponent2;
	    	lv_subtrahend = lv_exponent3;
	    	mantissa_to_shift = lv_mantissa3;
	    	op2_gt_op3 = 1;
	    end
	    else begin
	    	lv_minuend = lv_exponent3;
	    	lv_subtrahend = lv_exponent2;
	    	mantissa_to_shift = lv_mantissa2;
	    	op2_gt_op3 = 0;
	    end

	    resultant_exponent = lv_minuend;
	    exponent_difference = lv_minuend - lv_subtrahend;

		lv_zeros_on_right = zeroExtend(pack(countZerosLSB(mantissa_to_shift)));

		Bit#(1) shifted_operand_zero = mantissa_to_shift == '0 ? 1:0;
		
		mantissa_to_shift = mantissa_to_shift >> exponent_difference;

		//Handling sticky
		if(((lv_zeros_on_right < exponent_difference) || (mantissa_to_shift[0] == 1)) && shifted_operand_zero != 1)
			lv_sticky = 1;

		mantissa_to_shift = {mantissa_to_shift[fMAMAN-1:1], lv_sticky};

	    if(op2_gt_op3 == 1) begin
	    	lv_mantissa3 = mantissa_to_shift;
	    end
	    else begin
	    	lv_mantissa2 = mantissa_to_shift;
	    end

	    $display("lv_sign2 = %b lv_exponent2 = %b lv_mantissa2 = %b", lv_sign2, resultant_exponent, lv_mantissa2);
		$display("lv_sign3 = %b lv_exponent3 = %b lv_mantissa3 = %b", lv_sign3, resultant_exponent, lv_mantissa3);
		$display();

		bit man2_gt_man3 = 0;

	    if(lv_mantissa2 > lv_mantissa3) man2_gt_man3 = 1;

	    Bit#(fmaman) resultant_mantissa = 0;
	    bit resultant_sign = (man2_gt_man3 & lv_sign2) | (~man2_gt_man3 & (lv_operation ^ lv_sign3)); 	// Using Karnaugh maps
	    bit actual_operation = lv_sign2 ^ (lv_operation ^ lv_sign3); 									// 0 for addition 1 for subtraction


		if(actual_operation == 0) 	
				  resultant_mantissa = lv_mantissa2 + lv_mantissa3;
		else if(man2_gt_man3 == 1) 	
				  resultant_mantissa = lv_mantissa2 - lv_mantissa3;
		else						
				  resultant_mantissa = lv_mantissa3 - lv_mantissa2;

		//Case when Mantissa2 = Mantissa3 and hence the result is zero
		Bit#(2) add_sub_is_zero = 0;

		if(resultant_mantissa == '0) begin
			if(lv_rounding_mode == 3'b010) begin	
					  add_sub_is_zero = 2'b11;
			end
			else begin							
					  add_sub_is_zero = 2'b01;			// checks the resultant mantissa for zero
			end
		end

		$display("resultant_sign = %b resultant_exponent = %b resultant_mantissa = %b", resultant_sign, resultant_exponent, resultant_mantissa);
		$display();

		lv_sticky = resultant_mantissa[0];
		let lv_zeros_on_left = pack(countZerosMSB(resultant_mantissa));

		if(resultant_mantissa[fMAMAN-1] == 1'b1) begin
			resultant_mantissa = resultant_mantissa >> 1;
			resultant_mantissa = {resultant_mantissa[fMAMAN-1:1], lv_sticky | resultant_mantissa[0]};
			resultant_exponent = resultant_exponent + 1;
		end

		else if(resultant_mantissa[fMAMAN-2] != 1'b1) begin
			if((zeroExtend(lv_zeros_on_left) - 1) > (resultant_exponent - 1)) begin

				resultant_mantissa = resultant_mantissa << (resultant_exponent - 1);
				resultant_exponent = 0;
				$display("add_sub subnormal!!!");
			end
			else begin

				resultant_mantissa = resultant_mantissa << (lv_zeros_on_left - 1);
				resultant_exponent = resultant_exponent - (zeroExtend(lv_zeros_on_left) - 1);
			end
		end

		$display("resultant_sign = %b resultant_exponent = %b resultant_mantissa = %b", resultant_sign, resultant_exponent, resultant_mantissa);
		$display();
		

		ff_stage4<= tagged Valid Stage4_data_type{	
										resultant_sign			:				resultant_sign,
										resultant_exponent	:				resultant_exponent,
										resultant_mantissa	:				resultant_mantissa,
										rounding_mode			:				lv_rounding_mode,
										_negate					:				lv_negate,
										result_is_invalid		:				lv_result_is_invalid,
										result_is_infinity	:				lv_result_is_infinity,
										result_is_zero			:				lv_result_is_zero,
										add_sub_is_zero		:				add_sub_is_zero,
										product_overflow		:				lv_product_overflow,
										product_underflow		:				lv_product_underflow,
										mul 						: 				mul
										};

	endrule:rl_stage_4

	rule rl_stage_5_final_stage(ff_stage4 matches tagged Valid .stage4_data);

	    bit lv_resultant_sign						= 	stage4_data.resultant_sign;
	    Bit#(fpexp2) lv_resultant_exponent		= 	stage4_data.resultant_exponent;
	    Bit#(fmaman) lv_resultant_mantissa		= 	stage4_data.resultant_mantissa;
	    Bit#(3) lv_rounding_mode					= 	stage4_data.rounding_mode;
	    bit lv_result_is_invalid					= 	stage4_data.result_is_invalid;
	    Bit#(2) lv_result_is_infinity			= 	stage4_data.result_is_infinity;
	    Bit#(2) lv_result_is_zero					= 	stage4_data.result_is_zero;
	    Bit#(2) lv_add_sub_is_zero				= 	stage4_data.add_sub_is_zero;
	    bit lv_product_overflow					= 	stage4_data.product_overflow;
	    bit lv_product_underflow					= 	stage4_data.product_underflow;
		 bit lv_negate 								= 	stage4_data._negate;
		 let mul = stage4_data.mul;


	    ff_stage4 <= tagged Invalid;

    	Bit#(TAdd#(fpman,2)) lv_rounded_mantissa = 	lv_resultant_mantissa[fMAMAN-1:iMPFPMAN2];
      Bit#(2) lv_res_man 							  = 	lv_resultant_mantissa[fMAMAN-1:fMAMAN-2];
      Bit#(TSub#(impfpman2,2)) lv_res1 		  = 	lv_resultant_mantissa[iMPFPMAN2-3:0];
	   bit lv_guard 									  = 	lv_resultant_mantissa[iMPFPMAN2-1];
    	bit lv_round 									  = 	lv_resultant_mantissa[iMPFPMAN2-2];
    	bit lv_sticky 									  = 	|lv_res1;
	   bit lv_round_up	 							  = 	0;
    	bit lv_inexact 								  = 	lv_guard | lv_round | lv_sticky;

  		if(lv_rounding_mode == 'b000) 		
				  lv_round_up = lv_guard & (lv_resultant_mantissa[iMPFPMAN2] | lv_round | lv_sticky);
	    else if(lv_rounding_mode == 'b100) 	
				  lv_round_up = lv_guard ;//& (lv_round | lv_sticky | ~lv_resultant_sign);
	    else if(lv_rounding_mode == 'b010) 	
				  lv_round_up = lv_inexact & (lv_resultant_sign);
	    else if(lv_rounding_mode == 'b011) 	
				  lv_round_up = lv_inexact & (~lv_resultant_sign);

		$display("lv_guard = %b lv_round = %b lv_sticky = %b", lv_guard, lv_round, lv_sticky);
		$display("lv_round_up = %b", lv_round_up);
		$display("lv_rounded_mantissa = %b", lv_rounded_mantissa);

	    if(lv_round_up == 1) 
					lv_rounded_mantissa = lv_rounded_mantissa + 1;

		$display("lv_rounded_mantissa = %b after roundup", lv_rounded_mantissa);

		if(lv_rounded_mantissa[fPMAN+1] == 1) begin
			lv_resultant_exponent = lv_resultant_exponent + 1;
			lv_rounded_mantissa 	 = lv_rounded_mantissa >> 1;
		end
		else if(lv_res_man == 'b0 && lv_rounded_mantissa[fPMAN] == 1) begin
			lv_resultant_exponent = lv_resultant_exponent + 1;
		end


	    if(lv_product_underflow == 1) begin
	    end

        Bit#(fpexp) lv_res_exp_temp 		  = lv_resultant_exponent[fPEXP-1:0];
        Bit#(fpman) man_all_zeros 			  = '0;
        Bit#(TSub#(fpman,1)) man1_all_zeros = '0;
        Bit#(fpman) man_all_ones 			  = '1;
        Bit#(fpexp) exp_all_zeros 			  = '0;
        Bit#(TSub#(fpexp,1)) exp_all_ones_1 = '1;
        Bit#(fpinp) lv_final_output 		  =  0;
        Bit#(fpexp) exp_all_ones 			  = '1;
        Bit#(fpexp) out_exp                 = lv_resultant_exponent[fPEXP-1:0];
        Bit#(fpman) out_man                 = lv_rounded_mantissa[fPMAN-1:0];

	    if(lv_result_is_invalid == 1) begin
	    	lv_final_output = {1'b0, exp_all_ones,1'b1, man1_all_zeros};
	    end
		else if(lv_result_is_infinity[0] == 1) begin
	    	lv_final_output = {lv_result_is_infinity[1], exp_all_ones, man_all_zeros};
	    end
		 else if(lv_result_is_zero[0] == 1) begin
		    lv_final_output = {lv_result_is_zero[1],exp_all_zeros, man_all_zeros};
  		end
	    else if(lv_add_sub_is_zero[0] == 1) begin
	    	lv_final_output = {lv_add_sub_is_zero[1], exp_all_zeros , man_all_zeros};
	    end

      else if(lv_product_overflow == 1 || lv_res_exp_temp == '1) begin

        if(lv_rounding_mode == 'b001) 									
          lv_final_output={lv_resultant_sign,{exp_all_ones_1,1'b0},man_all_ones}; //??
        else if(lv_rounding_mode == 'b010 && lv_resultant_sign == 0)	
          lv_final_output={lv_resultant_sign,{exp_all_ones_1,1'b0},man_all_ones}; //??
        else if(lv_rounding_mode == 'b011 && lv_resultant_sign == 1)	
          lv_final_output={lv_resultant_sign,{exp_all_ones_1,1'b0},man_all_ones}; //??
		  else begin															
          lv_final_output={lv_resultant_sign,exp_all_ones,man_all_zeros};
        end
      end
	   else begin
          lv_final_output = {lv_resultant_sign, out_exp, out_man};
	    end
	    
     Bit#(5) fflags={lv_result_is_invalid,0,lv_product_overflow,lv_product_underflow,lv_inexact};
		 ff_final_out <= Floating_output{
	    									final_result		:			lv_final_output,
	    									fflags			:			fflags
								};

      $display("FMA: Result: %h",lv_final_output);
	endrule

	method Action _start(Bit#(fpinp) _operand1,Bit#(fpinp) _operand2, Bit#(fpinp) _operand3, Bit#(3) rounding_mode, bit operation, bit _negate, bit mul);
		

    	Bit#(TSub#(fpexp,1)) bias       =  '1;                             				             //Bias for the exponent: 127 for SP and 1023 for DP
    	Bit#(fpexp) lv_exponent1        =  _operand1[fPINP-2:fPMAN];
    	Bit#(fpexp) lv_exponent2        =  _operand2[fPINP-2:fPMAN];
    	Bit#(fpexp) lv_exponent3        =  _operand3[fPINP-2:fPMAN];
    	Bit#(fpman) lv_mantissa1        =  _operand1[fPMAN-1:0];
    	Bit#(fpman) lv_mantissa2        =  _operand2[fPMAN-1:0];
    	Bit#(fpman) lv_mantissa3        =  _operand3[fPMAN-1:0];            
    	Bit#(1) lv_exp1                 =  lv_exponent1 == '1 ? 1:0;										 //1 if all the bits of exponent are set; used to check if op1 is infinity or NaN
		Bit#(1) lv_exp2                 =  lv_exponent2 == '1 ? 1:0;										 //1 if all the bits of exponent are set; used to check if op2 is infinity or NaN
		Bit#(1) lv_man1_is_zero         =  lv_mantissa1 == '0 ? 1:0;							 			 //1 if mantissa of op1 is 0
		Bit#(1) lv_man2_is_zero         =  lv_mantissa2 == '0 ? 1:0;							          //1 if mantissa of op2 is 0
		Bit#(1) lv_exp1_is_zero         =  lv_exponent1 == '0? 1:0;								 			 //1 if exponent of operand1 is 0
		Bit#(1) lv_exp2_is_zero         =  lv_exponent2 == '0? 1:0;								 			 //1 if exponent of operand2 is 0
		Bit#(1) lv_op1_is_zero          =  lv_man1_is_zero & lv_exp1_is_zero;				    		 //1 when operand1=0
		Bit#(1) lv_op2_is_zero          =  lv_man2_is_zero & lv_exp2_is_zero;					 		 //1 when operand2=0
		Bit#(1) lv_op1_infinity         =  lv_exp1 & lv_man1_is_zero;							 			 //1 when operand1=inf
		Bit#(1) lv_op2_infinity         =  lv_exp2 & lv_man2_is_zero;							 			 //1 when operand2=inf
		Bit#(1) lv_op1_subnormal        =  lv_exp1_is_zero ;										 			 //1 when operand1 is subnormal
		Bit#(1) lv_op2_subnormal        =  lv_exp2_is_zero ;										 			 //1 when operand2 is subnormal
		Bit#(1) lv_product_sign         =  _operand1[fPINP-1]^_operand2[fPINP-1];
		Bit#(1) lv_inf                  =  0;                                                      //Bit indicating infinity
		Bit#(1) lv_inv                  =  0; 																		 //Invalid Bit
		Bit#(1) lv_zero                 =  0;																		 //Zero bit
		Int#(fpexp) actual_exponent1    =  unpack(lv_exponent1 - zeroExtend(bias));  				    //Unbiased Exponent1
		Int#(fpexp) actual_exponent2    =  unpack(lv_exponent2 - zeroExtend(bias));  					 //Unbiased Exponent2
		Int#(fpexp) actual_exponent3    =  unpack(lv_exponent3 - zeroExtend(bias));  					 //Unbiased Exponent3

	
	$display($time,"\top1 is subnormal = %b , op2 is subnormal = %b", lv_op1_subnormal, lv_op2_subnormal);
	$display($time,"\tsign1 = %b exponent1 = %b actual_exponent1 = %0d mantissa1 = %b.%b", _operand1[fPINP-1], lv_exponent1, actual_exponent1, ~lv_op1_subnormal, lv_mantissa1);
	$display($time,"\tsign2 = %b exponent2 = %b actual_exponent2 = %0d mantissa2 = %b.%b", _operand2[fPINP-1], lv_exponent2, actual_exponent2, ~lv_op2_subnormal, lv_mantissa2);
	$display($time,"\tsign3 = %b exponent3 = %b actual_exponent3 = %0d mantissa3 = %0b.%b", _operand3[fPINP-1],lv_exponent3, actual_exponent3, lv_exponent3 == '0? 1'b0:1'b1, lv_mantissa3);

	

	if((lv_exp1 == 1 && lv_man1_is_zero == 0) || (lv_exp2 == 1 && lv_man2_is_zero == 0))			// either of the operands are NaN
		lv_inv=1;
	else if(lv_op1_infinity == 1 || lv_op2_infinity == 1) begin												// checks if op1 or op2 are infinite
	   if(lv_op1_is_zero == 1 || lv_op2_is_zero == 1)                                            // if either op1 or op2 are zero, then 0*infinity results in NaN 
				  lv_inv=1; 
		else 
				  lv_inf=1;				  																				   //  else if both are infinite, result is infinite
	end
	else if(lv_op1_is_zero == 1 || lv_op2_is_zero == 1)
		lv_zero=1;

 	$display("lv_inv : %h lv_inf : %h lv_zero : %h",lv_inv,lv_inf,lv_zero);	

	
/*
	When normal and denormal number is multiplied, exponent is
	(biased_exponent - bias) + (1 - bias) + bias = biased_exponent - bias + 1;
	either _operand1[30:23] == 0 or _operand2[30:23] == 0 for the above if condition so no harm in adding both
*/

		Bit#(fpexp2) exp1_temp 			  =  {2'b0,_operand1[fPINP-2:fPMAN]};
   	Bit#(fpexp2) exp2_temp 			  =  {2'b0,_operand2[fPINP-2:fPMAN]};
		Bit#(fpexp2) lv_summed_exponent =  exp1_temp + exp2_temp - zeroExtend(bias) + zeroExtend(lv_op1_subnormal) + zeroExtend(lv_op2_subnormal);
		Bit#(1) lv_sign 					  =  _operand1[fPINP-1] ^ _operand2[fPINP-1];
		Int#(fpexp1) lv_actual_exponent =  unpack(lv_summed_exponent[fPEXP:0] - zeroExtend(bias));

	$display("lv_summed_exponent = %b, lv_actual_exponent = %0d", lv_summed_exponent, lv_actual_exponent);


	ff_input_register<= tagged Valid Input_data_type{	
																		mantissa1				:			{~lv_op1_subnormal, _operand1[fPMAN-1:0]},				//implicit bit is 0 since op1 is denormal
																		mantissa2				:			{~lv_op2_subnormal, _operand2[fPMAN-1:0]},				//implicit bit is 0 since op2 is denormal
																		lv_summed_exponent	: 			lv_summed_exponent,
																		sign						:			lv_sign,
																		_operand3				:			_operand3,
                                								rounding_mode 			: 			rounding_mode,
																		infinity					:			lv_inf,
																		invalid					:			lv_inv,
																		zero						:			lv_zero,
																		_operation				:			operation,
																		_negate					:			_negate,
								         							mul 						: 			mul         
															      };
	endmethod


	method Floating_output#(fpinp) get_result();
		return ff_final_out;
	endmethod

endmodule

//module mkTb_fpu_fm_add_sub(Empty);
//
//	Ifc_fpu_fm_add_sub#(32,23,8,32) uut <- mkfpu_fm_add_sub();
//
//    Reg#(Bit#(32)) rg_clock <-mkReg(0);
//    Reg#(Bit#(32)) operand1 <- mkReg(32'h3f800000);
//    Reg#(Bit#(32)) operand2 <- mkReg(32'h08998996);
//    Reg#(Bit#(32)) operand3 <- mkReg(32'h0a033439);
//
//    rule rl_count_clock ;
//      	rg_clock<=rg_clock+1;
//      	if(rg_clock=='d20) $finish(0);
//    endrule
//
//    rule rl_input1(rg_clock==1);
//        $display("giving inputs at %0d", rg_clock);
//        uut._start(operand1, operand2, operand3, 0, 3'b100, 1, 0,1);
//
//    endrule
//
//    rule rl_finish;
//        let res = uut.get_result();
//        $display("Output = %h at %0d",res.final_result[31:0], rg_clock);
//    endrule
//
//endmodule

//module mkTb_fpu_fm_add_sub_2 (Empty);
//	
////	RegFile #(Bit #(16), Bit #(100))  input_data <- mkRegFileFullLoad("./testcases/fma_inp_nor.txt");
////    RegFile #(Bit #(16), Bit #(68))  input_data <- mkRegFileFullLoad("./testcases/mul_denormal_testcases.txt");
//    RegFile #(Bit #(16), Bit #(68))  input_data <- mkRegFileFullLoad("./testcases/Add_normal_testcases.hex");
//	Reg #(Bit #(16)) index <- mkReg(0);
// 
//	Ifc_fpu_fm_add_sub#(32,23,8,16) multiplier <- mkfpu_fm_add_sub();
//	Reg #(Bit #(32)) state_clock <- mkReg(1);
//    Reg #(Bit #(1))  rg_state <- mkReg(0);
//
//	Reg#(int) cnt <- mkReg(0);                  //File Variable
//	let fh <- mkReg(InvalidFile) ;				//File handler		
//
//	//rule for file creation
//	rule open (cnt == 0 ) ;
//		File tb_mul_output <- $fopen("tb_madd_output.hex", "w+"); 
//		fh <= tb_mul_output;
//		cnt <= 1 ;
//	endrule
//
//	rule state_clock_count;
//		state_clock <= state_clock + 1;
//	endrule
//
//	rule take_input_in (rg_state == 0);
//	//	multiplier._start(input_data.sub(index)[99:68],input_data.sub(index)[67:36],input_data.sub(index)[35:4],0,input_data.sub(index)[2:0],0,0);
//	//	multiplier._start(input_data.sub(index)[67:36],input_data.sub(index)[35:4],32'b0,0,input_data.sub(index)[2:0],0,0);
//		multiplier._start(32'h3f800000, input_data.sub(index)[67:36],input_data.sub(index)[35:4],0,input_data.sub(index)[2:0],0,0);
//		index <= index + 1;
//	    rg_state <= 1;
//	endrule
//
//	rule display_output (rg_state == 1);
//        let abc = multiplier.get_result();
//		$fwrite(fh, "%h\n", abc.final_result[31:0]);
//		rg_state <= 0;
//	endrule
//
//	rule end_testing (index == 16562);
//		$finish(0);
//	endrule : end_testing
//
//endmodule

endpackage
