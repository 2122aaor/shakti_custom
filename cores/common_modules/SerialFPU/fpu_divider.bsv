/*

Copyright (c) 2013-2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name     : Single Precision Floating Point Divider
Author Name     : Arjun C. Menon, Vinod.G, Aditya Govardhan
Email ID        : c.arjunmenon@gmail.com, g.vinod1993@gmail.com, dtgovardhan@gmail.com
Last updated on : 11th August, 2016

    This module implements single and double precision floating point division operation. The fpu_divider is fully pipelined whereas the integer mantissa divider is sequential and hence the module is semi-pipelined. This semi pipelined nature of the module makes it more energy and area efficient. Moreover, hardware denormal support is also given to the module by the use of an extra exponent bit.  

    Division of floating point number can create the following exceptions (mentioned is
decreasing priority order): Invalid, Underflow, Overflow, Divide by zero and Inexact. Based on these
exceptions, the result and also the combinations of the inputs appropriate flags (invalid,underflow, 
overflow, divide by zero, inexact) in the fsr are also set.

TODO 1. Make latency optimizations on integer divider unit.
     2. Redundant variables are present as a workaround for handling provisos, without which bluespec compiler is unable to effectively identify the type information, need to be changed.
*/
package fpu_divider;
    import DReg::*;
    import defined_types::*;                              //contains typedef of exceptions which are generated here
    import integer_divider::*;               //divider module
    import RegFile::*;

    typedef struct{
    	Bit#(TAdd#(fpexp,2)) exponent;                              
    	Bit#(TAdd#(fpman,1)) dividend;
    	Bit#(TAdd#(fpman,1)) divisor;
        bit sign;
    	bit invalid;
    	bit infinity;
    	bit dz;
    	bit zero;
		Bit#(3) rounding_mode;
    } Stage1_type#(numeric type fpman,numeric type fpexp) deriving (Bits,Eq);                   //Structure of 1st Stage of the pipeline

     typedef struct {
            Bit#(fpexp2) exponent;
            Bit#(1) sign;
            Bit#(1) infinity;
            Bit#(1) invalid;
            Bit#(1) dz;
            Bit#(1) zero;
            Bit#(3) rounding_mode;
     } Stage2_type#(numeric type fpexp2) deriving (Bits,Eq);


    interface Ifc_fpu_divider#(numeric type fpinp, numeric type fpman, numeric type fpexp);
    	method Action _start(Bit#(fpinp) operand1, Bit#(fpinp) operand2, Bit#(3) rounding_mode); // input method to start the floating point operation
    	method Floating_output#(fpinp) final_result_();				 // Output method
    endinterface

//(*synthesize*)
module mkfpu_divider(Ifc_fpu_divider#(fpinp,fpman,fpexp))
       provisos(
                Add#(TAdd#(fpman,fpexp),1,fpinp), // fpman -23 fpexp=8 fpinp = 32
                Add#(fpman,2,fpman2), // fpman2 = 25
                Add#(fpman2,2,fpman4), // fpman4 = 27
                Add#(fpman4,1,fpman5), // fpman5 = 28
                Add#(fpexp,2,fpexp2), // fpexp2 = 10
                Add#(fpman5,1,fpman6), // fpman6 = 29
                Add#(fpman4,fpman6,acc_bits), // acc_bits= 56
                Add#(fpexp2,b__,fpman),
                Add#(TSub#(fpexp,1),c__,fpman),
                //per request of bsc
                Add#(d__,1,fpexp2),
                Add#(1, e__, fpman2),
                Add#(TDiv#(TSub#(TAdd#(TMul#(fpman4, 3), 2), 2), 3),
                     TAdd#(TDiv#(TSub#(TAdd#(TMul#(fpman4, 3), 2), 2), 3), 
                     TDiv#(TSub#(TAdd#(TMul#(fpman4, 3), 2), 2), 3)), 
                     TMul#(fpman4, 3)),
                Add#(f__, 1, fpman4),
                Add#(g__, TLog#(fpman5), fpexp2),
                Add#(TAdd#(fpman,1),3,fpman4),
                Add#(3,TSub#(fpexp,1),fpexp2),
                Add#(TDiv#(TSub#(TAdd#(TMul#(fpman4, 3), 2), 2), 3),
    						TAdd#(TDiv#(TSub#(TAdd#(TMul#(fpman4, 3), 2), 2), 3),
    						TAdd#(TDiv#(TSub#(TAdd#(TMul#(fpman4, 3), 2), 2), 3), 2)),
    						TAdd#(TMul#(fpman4, 3), 2)),
    				 Add#(h__, TLog#(TAdd#(1, TAdd#(c__, fpexp))), fpexp),
    				 Add#(c__, fpexp, TAdd#(fpman, 1)),
     				 Add#(a__, TLog#(TAdd#(1, TAdd#(c__, fpexp))), fpexp2)
               );

    Ifc_integer_divider#(fpman4) int_div <- mkinteger_divider();    // instantiation of divider module

	Wire#(Floating_output#(fpinp)) wr_final_out <- mkWire();			// instantiation of output FIFO whose structure definition is given in riscv_types.bsv
	Reg#(Maybe#(Stage1_type#(fpman,fpexp))) rg_stage1 <- mkDReg(tagged Invalid);       // instantiation of Stage 1 FIFO
    Reg#(Maybe#(Stage2_type#(fpexp2))) rg_stage2 <- mkReg(tagged Invalid); 
    let fPINP = valueOf(fpinp);
    let fPMAN = valueOf(fpman);
    let fPMAN5 = valueOf(fpman5);
    let fPEXP = valueOf(fpexp);
    let aCC   = valueOf(acc_bits);

    //This is the second stage of the pipe. Here the division of the two mantissas take place. Rest of the data are enqueued in another FIFO.
	rule rl_stage2 (rg_stage1 matches tagged Valid .stage1_data);
	      int_div._inputs({stage1_data.divisor,3'd0},
	    				{stage1_data.dividend,3'd0}
	    				);
        $display("Dividing Op1: %h (%d) Op2: %h (%d)",{stage1_data.dividend,3'd0},{stage1_data.dividend,3'd0},{stage1_data.divisor,3'd0},{stage1_data.divisor,3'd0});
        rg_stage2 <= tagged Valid Stage2_type { exponent : stage1_data.exponent,
                                     sign     : stage1_data.sign,
                                     infinity : stage1_data.infinity,
                                     invalid  : stage1_data.invalid,
                                     dz       : stage1_data.dz,
                                     zero     : stage1_data.zero,
                                     rounding_mode : stage1_data.rounding_mode
                                 };
	    

	endrule

	rule rl_stage3(rg_stage2 matches tagged Valid .stage2_data);
    let int_out = int_div.result_();
    Bit#(TSub#(fpexp,1)) bias = '1;
    $display("Int Data %h", int_out);
		Bit#(fpman4) lv_quotient 	= int_out[fPMAN+3:0];	//Quotient from the integer divider
		Bit#(fpman5) lv_remainder 	= int_out[aCC-1:fPMAN5]; //Remainder from the integer divider
		Bit#(fpexp2) lv_exponent 	= stage2_data.exponent;
		Bit#(1)  lv_sign			= stage2_data.sign;
		Bit#(1)  lv_infinity    	= stage2_data.infinity;
		Bit#(1)  lv_invalid 		= stage2_data.invalid;
		Bit#(1)  lv_dz 				= stage2_data.dz;
		Bit#(1)  lv_zero 			= stage2_data.zero;
		Bit#(3)  lv_rounding_mode	= stage2_data.rounding_mode;

    rg_stage2 <= tagged Invalid;

		bit lv_underflow = 0;
		bit lv_overflow = 0;

		Int#(fpexp2) lv_actual_exponent = unpack(lv_exponent - {3'b0,bias});
    Int#(fpman) lv_actual_exponent_temp = zeroExtend(lv_actual_exponent); 
		let msb_zeros = pack(countZerosMSB(lv_quotient));
    $display("MSB Zeros: %d",msb_zeros);
		let lsb_zeros = 0;

		// lv_quotient_is_subnormal construct is like a flag which can be used in difficult situations
		bit lv_quotient_is_subnormal = 0;
		bit lv_sticky = lv_quotient[0];
        Bit#(fpman) bias_temp = zeroExtend(bias);
		/*
		if exponent is > 128 then obviously none of the numbers are subnormal
		so the product is of the form 1x.xxxx or 01.xxxx
		the overflow conditions are handled in the following if condition accordingly
		*/
		if(lv_actual_exponent > unpack({3'b0,bias} + 1)) begin
			lv_overflow = 1;
			$display("lv_overflow!!!");
		end
		/*     
                -bias -fPMAN 
		-150  = -126 -23 -1
        -1075 = -1022 -52 -1 //for DP?
		-1 is for the implicit bit
		i.e. if all the bits are shifted out then its an underflow
		*/
		else if(lv_actual_exponent_temp < unpack(-bias_temp-fromInteger(fPMAN)-1)) begin            //TODO What here? TODO Check <-150 or <-151
            $display("lv_actual_exponent : %d bias-fpman-1 :  %d", lv_actual_exponent, -bias_temp-fromInteger(fPMAN-1));
			lv_underflow = 1;
			lv_quotient = 1;
			lv_exponent = 0;
			//When the exponent is < -151, sticky bit is automatically set to one
			$display("lv_underflow!!!");
		end
		 	
		else begin

			// possible shift is positive when exponent is lesser than -126
			Int#(fpexp2) possible_shift = 1-unpack({3'b0,bias})-lv_actual_exponent;
			$display("possible_shift = %0d", possible_shift);

			if(possible_shift > 0) begin

				//Setting sticky if all lsb zeros are removed out
				lsb_zeros = pack(countZerosLSB(lv_quotient));

				if(possible_shift > unpack(zeroExtend(lsb_zeros)) || lv_quotient[0] == 1) 
          lv_sticky = 1;

				//Handling sticky
				lv_quotient = lv_quotient >> pack(possible_shift);
				lv_quotient = {lv_quotient[fPMAN+3:1], lv_quotient[0] | lv_sticky};
				lv_sticky = lv_quotient[0];

				$display("lv_quotient = %h since exp < -126", lv_quotient);
				$display("and thus the sticky bit = %b", lv_sticky);

				lv_exponent = lv_exponent + pack(possible_shift);
				lv_quotient_is_subnormal = 1;
			end

			/*
			msb_zeros != 1 means product is of the form 00.xxxx, important case
			*/
			else if(msb_zeros != 0) begin
				/*
				if possible shift is < the number of leading zeros then the number can't be made normal
				*/
				if((~pack(possible_shift)+1) < zeroExtend(msb_zeros)) begin
					
					lv_quotient = lv_quotient << (~pack(possible_shift)+1);
					lv_exponent = lv_exponent - (~pack(possible_shift)+1);
					lv_quotient_is_subnormal = 1;
				end
				/*
				if exponent affords to give away enough such that shifting left leads to 01.xxxx and exponent >= -126
				*/
				else begin
					lv_quotient = lv_quotient << (msb_zeros);
					lv_exponent = lv_exponent - (zeroExtend(msb_zeros));
					lv_quotient_is_subnormal = 0;
	 			end
			end
		end

		if(lv_quotient_is_subnormal == 1) 
      lv_exponent = 0;

		$display();
		$display("lv_quotient = %h, lv_remainder = %h, lv_exponent = %h", lv_quotient, lv_remainder, lv_exponent);

		bit lv_guard = lv_quotient[2];  
		bit lv_round = lv_quotient[1];			
 		bit lv_inexact = 0;				
		bit lv_round_up = 0;						
           
		if(lv_remainder!=0 || lv_quotient[0] == 1) // if the remainder is zero, sticky bit is set to 1.
			lv_sticky = 1;

		if((lv_sticky | lv_guard | lv_round) == 1)// if any of the sticky,guard or round bit is set, the value is inexact.
			lv_inexact = 1;

		// Following if-else condition determine the value of lv_round_up. If set, the mantissa needs to be incremented, else the mantissa remains unchanged.
		if(lv_rounding_mode == 'b000) 
			lv_round_up = lv_guard & (lv_round|lv_sticky|lv_quotient[3]);
		else if(lv_rounding_mode == 'b100)
			lv_round_up = lv_guard & (lv_round|lv_sticky|lv_sign);
		else if(lv_rounding_mode == 'b011) 
			lv_round_up = (lv_guard|lv_round|lv_sticky) & ~lv_sign;
		else if(lv_rounding_mode == 'b010)
            lv_round_up = (lv_guard|lv_round|lv_sticky) & lv_sign;

        // otherwise if round to zero mode, then do nothing

		Bit#(fpman2) lv_rounded_quotient = {1'b0,lv_quotient[fPMAN+3:3]};

		if( lv_round_up == 1) begin
			lv_rounded_quotient = lv_rounded_quotient + 1;
		end

		if(lv_rounded_quotient[fPMAN+1] == 1 ) begin
      $display("Exponent Incremented 1");
			lv_exponent = lv_exponent + 1;
			lv_rounded_quotient = lv_rounded_quotient >> 1;
		end
		if(lv_quotient[fPMAN+3] == 0 && lv_rounded_quotient[fPMAN] == 1) begin
      $display("Exponent Incremented 2");
			lv_exponent = lv_exponent + 1;
		end
    Bit#(fpexp) out_exp = lv_exponent[fPEXP-1:0];
    Bit#(fpman) out_man = lv_rounded_quotient[fPMAN-1:0];
    Bit#(fpexp) exp_all_zeros = '0;
		Bit#(fpexp) exp_all_ones = '1;
    Bit#(TSub#(fpexp,1)) exp_all_ones_1 = '1;
    Bit#(fpman) man_all_zeros = '0;
    Bit#(fpman) man_all_ones = '1;
    Bit#(TSub#(fpman,1)) man1_all_zeros = '0;
    Bit#(TSub#(fpman,1)) man_all_ones_1 = '1;
		Bit#(fpinp) lv_final_output= 0;
		Exception e = None;  
     
		// result is infinity
		if(lv_infinity == 1)              
			lv_final_output = {lv_sign, exp_all_ones, man_all_zeros};

		// the result is invalid
		else if(lv_invalid == 1) begin              
      lv_final_output = {1'b0, exp_all_ones,1'b1, man1_all_zeros};
    	e = Invalid;
		end
		// operation is divide by zero
    else if(lv_dz==1) begin
      lv_final_output= {lv_sign, exp_all_ones, man_all_zeros};
      e = Divide_by_Zero;
    end
		// result is zero
    else if(lv_zero == 1)                  
      lv_final_output={lv_sign,exp_all_zeros,man_all_zeros};
        // result is underflow
    else if(lv_underflow == 1) begin
      lv_final_output= {lv_sign,exp_all_zeros,lv_rounded_quotient[fPMAN-1:0]};       	//TODO to verify if it needs to be lv_rounded_quotient[22:1] and lv_inexact bit.
    	e = Underflow;
    end
        // result is overflow
    else if(lv_overflow == 1 || out_exp == '1) begin
      e = Overflow;
      if(lv_rounding_mode == 'b001)
				lv_final_output = {lv_sign,{exp_all_ones_1,1'b0},man_all_ones};
			else if(lv_rounding_mode == 'b010 && lv_sign ==0)
				lv_final_output = {lv_sign,{exp_all_ones_1,1'b0},man_all_ones};
			else if(lv_rounding_mode == 'b011 && lv_sign==1)
				lv_final_output = {lv_sign,{exp_all_ones_1,1'b0},man_all_ones};
			else 
				lv_final_output ={lv_sign,exp_all_ones,man_all_zeros};
    end
    else begin
      lv_final_output = {lv_sign,out_exp,out_man};
			if(lv_inexact==1)
    			e = Inexact;
		end
    // Forming the new Floating point Status Register
		Bit#(32) lv_fsr_ ={0,lv_infinity,lv_zero,lv_rounding_mode,lv_invalid,lv_dz,lv_overflow,lv_underflow,lv_inexact}; 		
        // Enqueing the final result into the output FIFO
    wr_final_out <= Floating_output{ 
    		                   	  	  fsr             : lv_fsr_,
    		                     	  final_result    : lv_final_output,//Appending zeros at the MSB since the result is a Single Precision number which is 32-bits wide whereas the rob entries are 64-bits.
    		                     	  exception       : e
                                    };

	endrule

	method Action _start(Bit#(fpinp) _operand1, Bit#(fpinp) _operand2, Bit#(3) rounding_mode);

        Bit#(TSub#(fpexp,1)) bias = '1; 
        Bit#(fpexp) all_ones = '1;
        Bit#(fpexp) lv_exponent1 = _operand1[fPINP-2:fPMAN];
        Bit#(fpexp) lv_exponent2 = _operand2[fPINP-2:fPMAN];
        Bit#(fpman) lv_mantissa1 = _operand1[fPMAN-1:0];
        Bit#(fpman) lv_mantissa2 = _operand2[fPMAN-1:0];
        Int#(fpexp) actual_exponent1 = unpack(_operand1[fPINP-2:fPMAN] - {1'b0,bias});
		Int#(fpexp) actual_exponent2 = unpack(_operand2[fPINP-2:fPMAN] - {1'b0,bias});
    $display("Exp1: %h, Man1: %h, Exp2: %h Man2: %h",lv_exponent1,lv_mantissa1,lv_exponent2,lv_mantissa2);

		Bit#(1) lv_inf = 0;
		Bit#(1) lv_inv = 0;
		Bit#(1) lv_zero = 0;
		Bit#(1) lv_dz = 0;

		Bit#(1) lv_exp1 = lv_exponent1 == all_ones ? 1:0;				//1 if all the bits of exponent are set; used to check if op1 is infinity or NaN
		Bit#(1) lv_exp2 = lv_exponent2 == all_ones ? 1:0;				//1 if all the bits of exponent are set; used to check if op2 is infinity or NaN
		
		Bit#(1) lv_man1_is_zero = lv_mantissa1 == '0 ? 1:0;				//1 if mantissa of op1 is 0
		Bit#(1) lv_man2_is_zero = lv_mantissa2 == '0 ? 1:0;				//1 if mantissa of op2 is 0
		
		Bit#(1) lv_exp1_is_zero = lv_exponent1 == '0? 1:0;					//1 if exponent of operand1 is 0
		Bit#(1) lv_exp2_is_zero = lv_exponent2 == '0? 1:0;					//1 if exponent of operand2 is 0
		
		Bit#(1) lv_op1_is_zero = lv_man1_is_zero & lv_exp1_is_zero;			//1 when operand1=0
		Bit#(1) lv_op2_is_zero = lv_man2_is_zero & lv_exp2_is_zero;			//1 when operand2=0

		Bit#(1) lv_op1_subnormal = lv_exp1_is_zero & ~lv_man1_is_zero;		//1 when operand1 is subnormal
		Bit#(1) lv_op2_subnormal = lv_exp2_is_zero & ~lv_man2_is_zero;		//1 when operand2 is subnormal

		Bit#(1) lv_op1_is_infinity = lv_exp1 & lv_man1_is_zero;
		Bit#(1) lv_op2_is_infinity = lv_exp2 & lv_man2_is_zero;

		$display("op1 is subnormal = %b , op2 is subnormal = %b", lv_op1_subnormal, lv_op2_subnormal);
	//	$display("sign1 = %b exponent1 = %b actual_exponent1 = %0d mantissa1 = %b.%b", _operand1[31], _operand1[fPINP-2:fPMAN], actual_exponent1, ~lv_op1_subnormal, _operand1[fPMAN-1:0]);
//		$display("sign2 = %b exponent2 = %b actual_exponent2 = %0d mantissa2 = %b.%b", _operand2[31], _operand2[fPEXP-1:fPMAN], actual_exponent2, ~lv_op2_subnormal, _operand2[fPMAN-1:0]);


	  if((lv_exp1 == 1 && lv_man1_is_zero == 0) || (lv_exp2 == 1 && lv_man2_is_zero == 0) || (lv_op1_is_infinity == 1 && lv_op2_is_infinity == 1) || (lv_op1_is_zero == 1 && lv_op2_is_zero == 1)) begin  	//op1 or op2 are NaN (or) both are infinity (or) both are zero
	  	lv_inv = 1;                           					//result is invalid
		end
		else if(lv_op1_is_infinity ==1) begin       				//op 2 is neither NaN nor infinity, and op1 is infinity
	    lv_inf=1;                          						//result is infinity
		end
	  else if(lv_op2_is_zero==1) begin            				//op 1 is neither NaN nor infinity, and op2 is zero
      lv_inf=1;                          						//result is infinity
     	lv_dz=1;                                				//setting the divide by zero flag
    end
    else if(lv_op2_is_infinity == 1 || lv_op1_is_zero == 1)   	//{op1 and op2 are not NaN} (and) {op1 is zero and op2 is not zero (or) op2 is infinity and op1 is not infinity}
      lv_zero=1;                              				//result is zero

    Bit#(1) lv_sign= _operand1[fPINP-1] ^ _operand2[fPINP-1];

    let man1 ={~lv_op1_subnormal,lv_mantissa1};
    let man2 ={~lv_op2_subnormal,lv_mantissa2};
    let zeros1 =countZerosMSB(man1);
    let zeros2 =countZerosMSB(man2);
    man1=man1<<zeros1;
    man2=man2<<zeros2;
    Bit#(fpexp2) exp1={2'b0,lv_exponent1};
    Bit#(fpexp2) exp2={2'b0,lv_exponent2};
    exp1=exp1- zeroExtend(unpack(pack(zeros1)));
    exp2=exp2- zeroExtend(unpack(pack(zeros2)));
        /*
        total_baised_exponent = (biased_exponent - bias) - (1 - bias) + bias 					in the case of normal divided by subnormal
        total_biased_exponent = (1 - bias) - (biased_exponent - bias) + bias 					in the case of subnormal divided by normal
        total_biased_exponent = (biased_exponent - bias) - (biased_exponent - bias) + bias 		in the case of normal divided by normal
        total_biased_exponent = (1 - bias) - (1 - bias) + bias 									in the case of subnormal divided by normal

		SO equivalently to handle all the cases:
		total_biased_exponent = bias + (op1_biased_expo + is_op1_denorm) - (op2_biased_expo + is_op2_denorm)
        */

    Bit#(fpexp2) lv_exponent = {3'b0,bias} + ((exp1 + zeroExtend(lv_op1_subnormal)) - (exp2 + zeroExtend(lv_op2_subnormal))); //error will come obv


		Int#(fpexp2) lv_actual_exponent = unpack(lv_exponent - {3'b0,bias});
		$display("lv_sign: %h lv_exponent = %h, lv_actual_exponent = %h",lv_sign, lv_exponent, lv_actual_exponent);

	  rg_stage1 <= tagged Valid   Stage1_type  {	    exponent		: lv_exponent,
	  			               			           	dividend		: man1,
				       	                    			divisor			: man2,
                                         	sign        	: lv_sign,
        				        	            		invalid			: lv_inv,
        				        	            		infinity		: lv_inf,
        				        		            	dz	    		: lv_dz,
        				        		            	zero			: lv_zero,
										            	rounding_mode 	: rounding_mode
                                                 };
    
	endmethod

    // Output method which send the result
	method Floating_output#(fpinp) final_result_();
	    return wr_final_out;
	endmethod
	
    // This method needs to be called whenever the method final_result is called.
    // This method frees the final FIFO and resets the ready signal.

endmodule

		/*		TEST BENCH 		*/

//(*synthesize*)
//module mkTb_fpu_divider(Empty);
//	//Reg#(Bit#(32)) rg_operand1<-mkReg(32'h4A800BF6); 
//	//Reg#(Bit#(32)) rg_operand2<-mkReg(32'h4A3FFFFC);
//    Reg#(Bit#(64)) rg_operand1<-mkReg(64'h4150017EC0000000);    //876.25
//   	Reg#(Bit#(64)) rg_operand2<-mkReg(64'h4147FFFF80000000);    //728.125
//	Reg#(Bit#(32)) rg_clock<-mkReg(0); 
//	Ifc_fpu_divider#(64,52,11) divider<-mkfpu_divider();
//
//	Reg#(Bit#(32)) rg_arbit <-mkReg(0);
//
//	rule rl_clk_count;
//		rg_clock<=rg_clock+1;
//	endrule
//
//
//	rule rl_start_1(rg_clock=='d0);
//		 $display("Giving inputs rg_operand 1 : %h rg_operand 2 : %h through testbench",rg_operand1,rg_operand2,$time);
//		divider._start(rg_operand1,rg_operand2,{'b0,3'b000,5'b0},3'b010);
//	endrule
//
//	rule rl_display_result;
//         let abc = divider.final_result_();
//		 $display("Final result= %h, fsr: %h", abc.final_result, abc.fsr,$time);
//	endrule
//
//	rule rl_finish_(rg_clock=='d60);
//		$finish(0);
//	endrule
//
//endmodule:mkTb_fpu_divider



//module mkTb_fpu_divider_2(Empty);
//	
//	RegFile #(Bit #(10), Bit #(68))  input_data <- mkRegFileFullLoad("./testcases/Div_denormal_testcases.hex");
//	Reg #(Bit #(10)) index <- mkReg(0);
//	
//	Reg #(Bit #(32)) state_clock <- mkReg(1);
//    Reg #(Bit #(32)) rg_state <- mkReg(0);
// 	/*****************Module Instantiation******************************/
//	Ifc_fpu_divider#(32,23,8) divider <- mkfpu_divider();
//
//
//	/******************File Creation************************************/
//	Reg#(int) cnt <- mkReg(0);                  //File Creation counter
//	let fh <- mkReg(InvalidFile) ;				//File Handler
//	rule open (cnt == 0 ) ;
//		File tb_mul_output <- $fopen("tb_div_output.hex", "w+"); 
//		fh <= tb_mul_output;
//		cnt <= 1 ;
//	endrule
//
//	/******************clock_count**************************************/
//	rule state_clock_count;
//		state_clock <= state_clock + 1;
//	endrule
//
//	/*******************input******************************************/
//	rule take_input_in (rg_state == 0);
//		divider._start(input_data.sub(index)[67:36],input_data.sub(index)[35:4],0,input_data.sub(index)[2:0]);
//		index <= index + 1;
//        rg_state <= 1;
//	endrule
//
//	/*******************output*****************************************/
//	rule display_output (rg_state == 1);
//        let abc = divider.final_result_();
//		$fwrite(fh, "%h\n", abc.final_result[31:0]);
//        rg_state <= 0;
//	endrule
//
//	/******************end testing*************************************/
//	rule end_testing (index == 407);
//		$finish(0);
//	endrule
//
//endmodule
endpackage
