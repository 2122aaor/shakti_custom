/*

-------------------------------------------------------------------------------

Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
-------------------------------------------------------------------------------

Module Name     : Double Precision Floating Point Divider
Author Name     : Arjun C. Menon
Email ID        : c.arjunmenon@gmail.com
Last updated on : 3rd January, 2014

    This unit carries out division of two Double Precision(DP) Floating Point(FP) numbers. A pipelined architecture is used implement this module. The algorithm strictly follows the IEEE 754 standard. This unit takes 32 clock cycles to calculate the result; in which 29 clocks are taken to divide the two mantissa. Also, the integer divider module is not pipelined. Therefore, after it gets a set of inputs, the module will not take any further inputs for next 29 cycles. The inputs "rob_number" and "pc" do not affect the working of the multiplier. They have been kept for use when integrating this code with that of a microprocessor. Also specific care has been taken to ensure that the ff_output register holds the value of the result until it is read by the top module. This may lead to stalling the pipeline at certain instance. Thus once the top module reads the ff_output register value through the method 'final_result' it then asserts the method deque_buffer_reset_ready_signal. This method empties the ff_output register and resets the ready signal. Thus, the pipeline continues execution. Division of floating point number can create the following exceptions (mentioned is decreasing priority order): Invalid, Underflow, Overflow, Divide by zero and Inexact. Based on these exceptions, the result and also the combinations of the inputs appropriate flags (invalid,underflow, overflow, divide by zero, inexact) in the fsr are also set.

*********Performance****************:
Using UMCIP 65nm library in SYNOPSYS
Critical Path Length     :    0.64 ns
Max. Operating Frequency :    1.56 GHz
Combinational Cell Count :    62610
Sequential Cell Count    :    5602

The critical path is every stage of integer_divider_for_dpfdiv.
*/

package fpu_dpfloating_divider;

    import FIFO::*;                                     //importing FIFO library
    import SpecialFIFOs::*;                             //library which contains PipelineFIFO
    import riscv_types::*;                              //contains typedef of exceptions which are generated here
    import lead_zero_detect64::*;						//Function to calculate the leading number of zeros in a 32-bit number
	import integer_divider_for_dpfdiv::*;               //divider module
    import FIFOF::*;

    typedef struct{
    	Bit#(13) exponent;                              
    	Bit#(53) dividend;
    	Bit#(53) divisor;
        bit sign;
    	bit invalid;
    	bit infinity;
    	bit dz;
    	bit zero;
    	Bit#(5) destination;
        Bit#(4) rob;
    	Bit#(32) fsr;
    	Bit#(32) pc;
    } Stage1_type deriving (Bits,Eq);                   //Structure of 1st Stage of the pipeline

    typedef struct{
    	Bit#(13) exponent;
    	bit sign;
    	bit invalid;
    	bit infinity;
    	bit dz;
    	bit zero;
    	Bit#(5) destination;
        Bit#(4) rob;
    	Bit#(32) fsr;
    	Bit#(32) pc;
    } Stage2_type deriving (Bits,Eq);                   //Structure of 2nd Stage of the pipeline

    typedef struct{
        Bit#(55) mantissa;
        Bit#(6) shr;
    	Bit#(13) exponent;
    	bit sign;
    	bit invalid;
    	bit infinity;
        bit underflow;
        bit sticky;
    	bit dz;
    	bit zero;
    	Bit#(5) destination;
        Bit#(4) rob;
    	Bit#(32) fsr;
    	Bit#(32) pc;
    } Stage3_type deriving (Bits,Eq);                   //Structure of 2nd Stage of the pipeline

    //The interface to the module mkdpfloating_divider
    interface Ifc_dpfloating_divider;
    	method Action _start(Bit#(64) operand1, Bit#(64) operand2, Bit#(5) destination, Bit#(4) rob_number, Bit#(32) fsr, Bit#(32) pc); // input method to start the floating point operation
    	method Output_type final_result_();				 // Output method
        method Action _deque_buffer_reset_ready_signal();// input method to deque output buffer and reset the ready signal
    	method Action _set_flush(Bool f);				 // Input method to initiate the flush routine
    endinterface

(*synthesize*)
module mkfpu_dpfloating_divider(Ifc_dpfloating_divider);

	Ifc_integer_divider_for_dpfdiv int_div <- mkinteger_divider_for_dpfdiv();    // instantiation of divider module

	FIFOF#(Output_type) ff_final_out <- mkFIFOF();			// instantiation of output FIFO whose structure definition is given in riscv_types.bsv
	Wire#(Bool) wr_flush<- mkDWire(False);				    // wire that indicates when to flush the output FIFO in case of a processor pipeline flush by firing the rule rl_flush
	FIFO#(Stage1_type) ff_stage1 <- mkPipelineFIFO();       // instantiation of Stage 1 FIFO
	FIFO#(Stage2_type) ff_stage2 <- mkPipelineFIFO();       // instantiation of Stage 2 FIFO
	FIFO#(Stage3_type) ff_stage3 <- mkPipelineFIFO();       // instantiation of Stage 2 FIFO
	Reg#(Bool) rg_ready_signal   <- mkReg(False);

	rule rl_flush_output_fifo(wr_flush);					//rule that clears the contents of the FIFO in case of a flush
	    ff_final_out.clear();
        ff_stage1.clear();
        ff_stage2.clear();
        ff_stage3.clear();
        int_div._set_flush(True);
	endrule
	
    //This is the second stage of the pipe. Here the division of the two mantissas take place. Rest of the data are enqueued in another FIFO.
	rule rl_stage2(!wr_flush);
	    int_div._inputs({ff_stage1.first().divisor,3'd0},{ff_stage1.first().dividend,3'd0});        //sending inputs to the divider module
	    ff_stage2.enq(Stage2_type{  exponent    : ff_stage1.first().exponent,                        
                                    sign        : ff_stage1.first().sign,
                                    invalid     : ff_stage1.first().invalid,
                                    infinity    : ff_stage1.first().infinity,
                                    dz          : ff_stage1.first().dz,
                                    zero        : ff_stage1.first().zero,
                                    destination : ff_stage1.first().destination,
                                    rob         : ff_stage1.first().rob,
                                    fsr         : ff_stage1.first().fsr,
                                    pc          : ff_stage1.first().pc
                                 });
	    ff_stage1.deq();        //Freeing the previous FIFO so that it can accept new inputs
	endrule
	
	rule rl_stage3(!wr_flush);
		Bit#(56) lv_quotient= int_div.output_()[113:58];     // local variable which stores the quotient
	    Bit#(57) lv_remainder= int_div.output_()[56:0];     // local variable which stores the remainder
	    int_div._remove_last_entry();                       // freeing integer divider module
	  	Bit#(13) lv_v_shr=0;                                // local variable which stores the amount of left shift. 10bits are used cos the exponent is 10bits and this value is obtained by performing 1 - exponent
	  	Bit#(6) lv_shr=0;                                   // local variable which stores the actual amount of left shifts to be done
        bit lv_underflow= 0;                                // local variable which indicates if there is an underflow
		Bit#(13) lv_exponent= ff_stage2.first().exponent;   //  local variable which stores the value of the exponent. If the MSB of the result is 0, then the exponent needs to be subtracted by 1.
		//$display("lv_quotient: %b",lv_quotient);

		//when Numerator>Denominator, the MSB of the result is 0. Therefore we left shift the quotient once
		//so that the MSB i.e. the implicit bit becomes 1 in case of a normal(not denormal) result.
		//When Numerator<Denominator, the MSB will already be one. The function integer_divider_for_spfdiv
		//calculates one more extra quotient bit. Therefore the sticky bit in this case will be set if either
		//the remainder is not zero or if the LSB of the quotient is set.
		//Moreover, in both cases we can use the same condition to set the sticky bit since in the first case
		//left shifting the quotient will make the LSB zero and won't affect the sticky bit since 0 is the
		//non-controlling value of an 'or' gate.
        if(lv_quotient[55]==0) begin
            lv_quotient= lv_quotient<<1;
            lv_exponent= lv_exponent-1;
        end

		if((lv_exponent[12] | ~(|(lv_exponent[11:0])))==1) begin    //if exponent is zero or goes negative, then underflow
			lv_underflow= 1;
            lv_v_shr= 'd1 - lv_exponent;//-{'d0,lv_quotient[55]};   // amount of left shift is calculated based on the value of the exponent.
			//$display("lv_v_shr= %d",lv_v_shr);
		end
        if(lv_v_shr>'d54)                                           // if more than 54 shifts, then amount of shift set as 54 as beyond that the result is 'd0 (or 'd1 in some rounding modes).
            lv_shr='d54;                                            // actually need to shift by 48 but shifting by anything more yields the same result anyways zeros are appended at the MSB
        else
			lv_shr= lv_v_shr[5:0];                                  // calculated amount of shifting to be done
		//$display("quotient is %b rem is %b ", lv_quotient, lv_remainder);

	    //The quotient is 56 bits wide. We append 53 zeros to the right so that while shifting we do not loose any bits and thus the guard, round and sticky bits
        //can be computed perfectly. When the amount of shifting is more than 53, the MSB of the original mantissa comes to the round position of the result mantissa
        //in which case the result will be zero ( or result will be 64'd1 if rounding mode is round-up) and underflow flag will be set.
        //Therefore we choose the size of the local variable mantissa to be 56+53= 109 bits.
		Bit#(109) lv_mantissa= {lv_quotient,53'd0};                 // We are appending zeros at the LSB since we do not want to loose precision while shifting                                                                    
		lv_mantissa= lv_mantissa>>lv_shr;                           // Shifting mantissa by the calculated amount
        //$display("lv_mantissa= %b",lv_mantissa);
		bit lv_sticky=0;                                            // Declaring the sticky bit

		if(lv_mantissa[53:0]!=0 || lv_remainder!=0)                 // if lower bits of the lv_mantissa are set or if remainder is not zero
			lv_sticky= 1;                                           // set the sticky bit

        ff_stage3.enq(Stage3_type{  mantissa    : lv_mantissa[108:54],
                                    shr         : lv_shr,
                                    exponent    : lv_exponent,                        
                                    sign        : ff_stage2.first().sign,
                                    invalid     : ff_stage2.first().invalid,
                                    infinity    : ff_stage2.first().infinity,
                                    underflow   : lv_underflow,
                                    sticky      : lv_sticky,
                                    dz          : ff_stage2.first().dz,
                                    zero        : ff_stage2.first().zero,
                                    destination : ff_stage2.first().destination,
                                    rob         : ff_stage2.first().rob,
                                    fsr         : ff_stage2.first().fsr,
                                    pc          : ff_stage2.first().pc  
                                 });
	    ff_stage2.deq();        //Freeing the previous FIFO so that it can accept new inputs
    endrule

    rule rl_stage4(!wr_flush);
        let lv_mantissa= ff_stage3.first().mantissa;
        bit lv_guard= lv_mantissa[1];                                   // Setting the guard bit
        bit lv_round= lv_mantissa[0];                                   // Setting the round bit
        bit lv_sticky= ff_stage3.first().sticky;                        // Setting the sticky bit
        Bit#(64) lv_final_output= 0;                                    // local variable which stores the final result
        Exception lv_exception = tagged No_exception;               // local variable which indicates which exception is generated. By default, no exceptions are generated.
        bit lv_overflow= 0;
     	//$display("G= %b R= %b S= %b", lv_guard, lv_round, lv_sticky); 	
     
        bit lv_roundup=0;                                               // local variables which tells whether the mantissa needs to be rounded, i.e. incremented by 1, or not
        bit lv_inexact= lv_guard | lv_round | ff_stage3.first().sticky; // result is inexact if either of guard, round or sticky bits are set.

        // Following if-else condition determine the value of lv_roundup. If set, the mantissa needs to be incremented, else the mantissa remains unchanged.
        if(ff_stage3.first().fsr[7:5] == 'b000)				// round to nearest, ties to even
			lv_roundup = lv_guard & (lv_round|lv_sticky|lv_mantissa[2]);
        else if(ff_stage3.first().fsr[7:5] == 'b100)		// round to nearest, ties to max magnitude
            lv_roundup = lv_guard & (lv_round | lv_sticky |ff_stage3.first().sign);
        else if(ff_stage3.first().fsr[7:5] == 'b011)		// round up 
	   		lv_roundup = (lv_guard|lv_round|lv_sticky)&~ff_stage3.first().sign;
        else if(ff_stage3.first().fsr[7:5] == 'b010)		// round down			
            lv_roundup = (lv_guard|lv_round|lv_sticky)&ff_stage3.first().sign;
        // otherwise if round to zero mode, then do nothing
	
        Bit#(54) lv_rounded_mantissa;                       // declaring a local variable to store the value of rounded mantissa
        if(lv_roundup==1)                                   // if mantissa needs to be rounded up
            lv_rounded_mantissa={1'b0,lv_mantissa[54:2]}+1;
		else
            lv_rounded_mantissa={1'b0,lv_mantissa[54:2]};
		
        let lv_exponent= ff_stage3.first().exponent;
        if(lv_rounded_mantissa[53]==1)                      // if there is a mantissa overflow, then the exponent needs to be incremented by one.
            lv_exponent= lv_exponent+1;
        
        
		if(ff_stage3.first().invalid==1) begin              // the result is invalid
            lv_final_output= 64'h7fff_ffff_ffff_ffff;
    		lv_exception = tagged Invalid True;             // generating an invalid exception
		end			
        else if(ff_stage3.first().zero==1)                  // result is zero
            lv_final_output={ff_stage3.first().sign,63'd0};
        else if(ff_stage3.first().dz==1) begin              // if operand 2 is zero and operand 1 is neither zero nor NaN
            lv_exception = tagged Divide_by_Zero True;      // generating a divide by zero exception
            lv_final_output= {ff_stage3.first().sign,11'd-1,52'd0};
        end
        else if(ff_stage3.first().infinity==1)              // result is infinity
            lv_final_output={ff_stage3.first().sign,11'd-1,52'd0};
        else if(ff_stage3.first().underflow==1) begin                      // Underflow condition
            if(ff_stage3.first().fsr[7:5] == 'b001)                                     // round to nearest
				lv_final_output={ff_stage3.first().sign,'d1};
			else if(ff_stage3.first().fsr[7:5] == 'b010 && ff_stage3.first().sign==0)   // round down a positive number
				lv_final_output={ff_stage3.first().sign,'d1};
			else if(ff_stage3.first().fsr[7:5] == 'b011 && ff_stage3.first().sign==1)   // round up a negative number
				lv_final_output={ff_stage3.first().sign,'d1};
			else                                                                        // rest of the cases the result is infinity
                lv_final_output= {ff_stage3.first().sign,11'd0,lv_rounded_mantissa[51:0]};
    		lv_exception = tagged Underflow True;                       // generating an underflow exception
        end
        else if(lv_exponent[11]==1 || lv_exponent[10:0]=='d-1) begin    // Overflow condition. 
            lv_overflow= 1;
    		lv_exception = tagged Overflow True;                        // generating an overflow exception

            // In some cases the result should be the max magnitude number. In other cases the result is infinity
			if(ff_stage3.first().fsr[7:5] == 'b001)                                     // round to nearest
				lv_final_output={ff_stage3.first().sign,'h7FEF_FFFF_FFFF_FFFF};
			else if(ff_stage3.first().fsr[7:5] == 'b010 && ff_stage3.first().sign==0)   // round down a positive number
				lv_final_output={ff_stage3.first().sign,'h7FEF_FFFF_FFFF_FFFF};
			else if(ff_stage3.first().fsr[7:5] == 'b011 && ff_stage3.first().sign==1)   // round up a negative number
				lv_final_output={ff_stage3.first().sign,'h7FEF_FFFF_FFFF_FFFF};
			else                                                                        // rest of the cases the result is infinity
            	lv_final_output={ff_stage3.first().sign,11'd-1,52'd0};
        end
        
       	else begin                                      // if all the above conditions are false, the result is a normal number
            lv_final_output= {ff_stage3.first().sign,lv_exponent[10:0], lv_rounded_mantissa[51:0]};
			if(lv_inexact==1)                           // checking if the result is inexact.
    			lv_exception = tagged Inexact True;     // generating an inexact exception
		end

        // Forming the new Floating point Status Register
        Bit#(32) lv_fsr_ ={ff_stage3.first().fsr[31:10],ff_stage3.first().infinity,ff_stage3.first().zero,ff_stage3.first().fsr[7:5],ff_stage3.first().invalid,ff_stage3.first().dz,lv_overflow,ff_stage3.first().underflow,lv_inexact}; 		
        
        rg_ready_signal<=True;      // Ready signal is made true to indicate that the result is ready.
                                    // Wherever value is of ff_final_out is read, the implicit condition of that rule includes checking if ff_final_out is not Empty.
                                    // But still a ready signal is required when we are having multiple execution units returning results simultaneously and we need to give priority to one over the other.
        
        // Enqueing the final result into the output FIFO
    	ff_final_out.enq(Output_type{ destination     : ff_stage3.first().destination,
    		                   	  	  fsr             : lv_fsr_,
    		                     	 
    		                     	
    		                     	  final_result    : lv_final_output,           
    		                     	  exception       : lv_exception
                                    });
	    ff_stage3.deq();
            
	endrule
	
	
	method Action _start(Bit#(64) operand1, Bit#(64) operand2, Bit#(5) destination, Bit#(4) rob_number,  Bit#(32) fsr, Bit#(32) pc);
		    
	    bit lv_is_op1_mantissa_zero=~(|(operand1[51:0]));	// =1 if mantissa of operand 1 is zero
	    bit lv_op1_is_infinity= 0;                          // =1 if operand 2 is infinity
	    bit lv_op1_is_nan= 0;                               // =1 if operand 2 is Not a Number
	    bit lv_op1_is_zero= 0;                              // =1 if operand 2 is zero
	    bit lv_op1_is_denormal= 0;                          // =1 if operand 2 is denormal
	    bit lv_op1_is_normal= 0;                            // =1 if operand 2 is normal

	    bit lv_is_op2_mantissa_zero=~(|(operand2[51:0]));	// =1 if mantissa of operand 2 is zero
	    bit lv_op2_is_infinity= 0;                          // =1 if operand 2 is infinity
	    bit lv_op2_is_nan= 0;                               // =1 if operand 2 is Not a Number
	    bit lv_op2_is_zero= 0;                              // =1 if operand 2 is zero
	    bit lv_op2_is_denormal= 0;                          // =1 if operand 2 is denormal
	    bit lv_op2_is_normal= 0;                            // =1 if operand 2 is normal

	    bit lv_invalid= 0;                                  // =1 if result is invalid
	    bit lv_infinity= 0;                                 // =1 if result in infinity
	    bit lv_dz= 0;                                       // =1 if there si a divide by zero error
	    bit lv_zero= 0;                                     // =1 if result is zero
        bit lv_sign= operand1[63] ^ operand2[63];           // sign bit

	    if(&(operand1[62:52])==1) begin			        	//if all bits of exp are 1
			if(lv_is_op1_mantissa_zero==1)		        	//if mantissa=0
	 		    lv_op1_is_infinity= 1;
			else					                    	//if mantissa!=0
		    	lv_op1_is_nan= 1;
		end
	    else if(|(operand1[62:52])==0) begin	        	//if exponent=0
	        if(lv_is_op1_mantissa_zero==1)		        	//if mantissa=0
		  		lv_op1_is_zero= 1;
			else				                        	//if mantissa!=0
		    	lv_op1_is_denormal=1;
	    end
	    else
	        lv_op1_is_normal=1;
	    	   
	    if(&(operand2[62:52])==1) begin			        	//if all bits of exp are 1
			if(lv_is_op2_mantissa_zero==1)		        	//if mantissa=0
	 	    	lv_op2_is_infinity= 1;
			else					                    	//if mantissa!=0
		    	lv_op2_is_nan= 1;
	    end
	    else if(|(operand2[62:52])==0) begin	        	//if exponent=0
	        if(lv_is_op2_mantissa_zero==1)		        	//if mantissa=0
	   	   		lv_op2_is_zero= 1;
			else				                        	//if mantissa!=0
		    	lv_op2_is_denormal=1;
	    end
	    else
	        lv_op2_is_normal=1;
	    

		if(((lv_op1_is_nan | lv_op2_is_nan) | (lv_op1_is_infinity & lv_op2_is_infinity) | (lv_op1_is_zero & lv_op2_is_zero))==1) begin  //op1 or op2 are NaN (or) both are infinity (or) both are zero
			lv_invalid=1;                           //result is invalid
			//$display("invalid!");
		end
		else if(lv_op1_is_infinity ==1) begin       //op 2 is neither NaN nor infinity, and op1 is infinity
	        lv_infinity=1;                          //result is infinity
			//$display("inf!");
		end
	    else if(lv_op2_is_zero==1) begin            //op 1 is neither NaN nor infinity, and op2 is zero
            lv_infinity=1;                          //result is infinity
         	lv_dz=1;                                //setting the divide by zero flag
        end
        else if((lv_op2_is_infinity | lv_op1_is_zero)==1)   //{op1 and op2 are not NaN} (and) {op1 is zero and op2 is not zero (or) op2 is infinity and op1 is not infinity}
            lv_zero=1;                              //result is zero

	    Bit#(53) lv_dividend= {~lv_op1_is_denormal, operand1[51:0]};                // forming the dividend
	    Bit#(53) lv_divisor= {~lv_op2_is_denormal, operand2[51:0]};                 // forming the divisor

	    Bit#(6) lv_dividend_lead_zeros= fn_lead_zeros64({lv_dividend,'d0})[5:0];    // calculating number of leading zeros of the dividend
	    Bit#(6) lv_divisor_lead_zeros= fn_lead_zeros64({lv_divisor,'d0})[5:0];      // calculating number of leading zeros of he divisor
	    
	    lv_dividend= lv_dividend<<lv_dividend_lead_zeros;                           // left shifting the dividend by calculated amount of zeros
	    lv_divisor= lv_divisor<<lv_divisor_lead_zeros;                              // left shifting the divisor by calculated amount of zeros
        
        // forming the new exponent. 
	    Bit#(13) lv_new_exponent= ({2'd0,operand1[62:52]} + {12'd0,lv_op1_is_denormal}) - ({2'd0,operand2[62:52]} + {12'd0,lv_op2_is_denormal}) + 13'b0001111111111 - {'d0,lv_dividend_lead_zeros} + {'d0,lv_divisor_lead_zeros};
        //$display("%d + %d - %d + %d + %d -%d + %d",operand1[62:52],lv_op1_is_denormal,operand2[62:52],lv_op2_is_denormal,13'b0001111111111,lv_dividend_lead_zeros,lv_divisor_lead_zeros);
        //$display("exp= %d %b",lv_new_exponent, lv_new_exponent);
	    ff_stage1.enq( Stage1_type  {	    exponent	: lv_new_exponent,
	  			               				dividend	: lv_dividend,
				       	        			divisor		: lv_divisor,
                                        	sign        : lv_sign,
        				        			invalid		: lv_invalid,
        				        			infinity	: lv_infinity,
        				        			dz	    	: lv_dz,
        				        			zero		: lv_zero,
        				        			destination	: destination,
                                            rob         : rob_number,
        				        			fsr		    : fsr,
        				        			pc		    : pc
                                     });
	endmethod
	
    // Output method which send the result
	method Output_type final_result_();
	    return ff_final_out.first();
	endmethod
	
    // This method needs to be called whenever the method final_result is called.
    // This method frees the final FIFO and resets the ready signal.
    method Action _deque_buffer_reset_ready_signal()if(!wr_flush);
        ff_final_out.deq();
        if(ff_final_out.notFull())
	        rg_ready_signal<=False;
    endmethod
    // when a wr_flush is initiated in the processor this method will also be called.
    // it sets the flsuh wire to True, hence no rule other flush_all_fifos will
    // fire and hence clear all the buffer and intermediate register/ wires ...
    method Action _set_flush(Bool _flush);
		wr_flush<=_flush;
    endmethod


endmodule


//Testbench
module mkTb_fpu_dpfloating_divider(Empty);
  	Reg#(Bit#(32)) rg_clock<-mkReg(0); 
  	
    //Reg#(Bit#(64)) rg_operand1<-mkReg(64'h0000_de00_0000_0000);    //42
   	//Reg#(Bit#(64)) rg_operand2<-mkReg(64'h4090_0078_0000_0000);    //21

    //Reg#(Bit#(64)) rg_operand1<-mkReg(64'h0008_0000_0000_0001);    
    //Reg#(Bit#(64)) rg_operand2<-mkReg(64'h4330_0000_0000_0000);    
  
    Reg#(Bit#(64)) rg_operand1<-mkReg(64'h408B620000000000);    //876.25
   	Reg#(Bit#(64)) rg_operand2<-mkReg(64'h4086C10000000000);    //728.125    
    


    Reg#(Bit#(3)) rg_inst_count<-mkReg(1);
   	Ifc_dpfloating_divider divider<-mkfpu_dpfloating_divider();
    
	rule rl_clock_count;
		rg_clock<=rg_clock+1;
		$display("Clock= %d", rg_clock);
    if(rg_clock=='d32)
        $finish(0);
	endrule

	rule rl_display_result;
        //if(multiplier.ready_() == True)begin
 	    let abc = divider.final_result_();
        divider._deque_buffer_reset_ready_signal();
        $display("Final result= %h, fsr: %h", abc.final_result, abc.fsr);
        
		if(abc.exception matches tagged No_exception.*)
   		  	$display("NO EXCEPTION");
        else if(abc.exception matches tagged Invalid .*)
       		$display("INVALID EXCEPTION");
        else if(abc.exception matches tagged Inexact.*)
     		$display("INEXACT EXCEPTION");
        else if(abc.exception matches tagged Overflow.*)
     		$display("OVERFLOW EXCEPTION");
	endrule:rl_display_result

	rule rl_start1(rg_inst_count==1);
    	divider._start(rg_operand1, rg_operand2,0,0,0,0);       
        $display("Giving first inputs");
        //rg_operand2<= 32'h44000000;
        rg_inst_count<=2;
	endrule
/*
	rule rl_start2(rg_inst_count==2);
    	divider._start(rg_operand1, rg_operand2,0,0,0,0);
        $display("Giving second inputs");
        rg_operand2<= 32'h48000000;
        rg_inst_count<=3;
	endrule

    rule rl_start3(rg_inst_count==3);
    	divider._start(rg_operand1, rg_operand2,0,0,0,0);
        $display("Giving third inputs");
        rg_operand2<= 32'h40000000;
        rg_inst_count<=4;
	endrule

    rule rl_start4(rg_inst_count==4);
    	divider._start(rg_operand1, rg_operand2,0,0,0,0);
        $display("Giving fourth inputs");
        rg_inst_count<=5;
	endrule
*/


endmodule

endpackage
