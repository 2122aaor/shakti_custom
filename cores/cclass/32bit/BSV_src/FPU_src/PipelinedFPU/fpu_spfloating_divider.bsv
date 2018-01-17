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
Last updated on : 30th May, 2016

    This unit carries out division of two floating point numbers. A pipelined architecture is
used implement this module. The algorithm strictly follows the IEEE 754 standard. This unit takes
18 clock cycles to calculate the result; in which 16 clocks are taken to divide the two mantissa. 
Also, the integer divider module is not pipelined. Therefore, after it gets a set of inputs, the module
will not take any further inputs for next 16 cycles.
    The inputs "rob_number" and "pc" do not affect the working of the multiplier.
They have been kept for use when integrating this code with that of a microprocessor.
Also specific care has been taken to ensure that the ff_output register holds the value of the result
until it is read by the top module. This may lead to stalling the pipeline at certain instance. Thus
once the top module reads the ff_output register value through the method 'final_result' it then asserts
the method deque_buffer_reset_ready_signal. This method empties the ff_output register and resets the
ready signal. Thus, the pipeline continues execution.
    Division of floating point number can create the following exceptions (mentioned is
decreasing priority order): Invalid, Underflow, Overflow, Divide by zero and Inexact. Based on these
exceptions, the result and also the combinations of the inputs appropriate flags (invalid,underflow, 
overflow, divide by zero, inexact) in the fsr are also set.
*/
package fpu_spfloating_divider;



    import FIFO::*;                                     //importing FIFO library
	import FIFOF::*;
    import SpecialFIFOs::*;                             //library which contains PipelineFIFO
    import defined_types::*;                              //contains typedef of exceptions which are generated here
    import integer_divider_for_spdiv::*;               //divider module
    import RegFile::*;

    typedef struct{
    	Bit#(10) exponent;                              
    	Bit#(24) dividend;
    	Bit#(24) divisor;
        bit sign;
    	bit invalid;
    	bit infinity;
		bit lv_NaN;
    	bit dz;
    	bit zero;
    	Bit#(32) fsr;
		Bit#(3) rounding_mode;
    } Stage1_type deriving (Bits,Eq);                   //Structure of 1st Stage of the pipeline

    typedef struct{
    	Bit#(10) exponent;
    	bit sign;
    	bit invalid;
    	bit infinity;
		bit lv_NaN;
    	bit dz;
    	bit zero;
    	Bit#(32) fsr;
		Bit#(3) rounding_mode;
    } Stage2_type deriving (Bits,Eq);                   //Structure of 2nd Stage of the pipeline

    interface Ifc_fpu_spfloating_divider;
    	method Action _start(Bit#(32) operand1, Bit#(32) operand2, Bit#(32) fsr, Bit#(3) rounding_mode); // input method to start the floating point operation
    	method Floating_output final_result_();				 // Output method
        method Action _deque_buffer_reset_ready_signal();// input method to deque output buffer and reset the ready signal
    endinterface

(*synthesize*)
module mkfpu_spfloating_divider(Ifc_fpu_spfloating_divider);

	Ifc_integer_divider_for_spdiv int_div <- mkinteger_divider_for_spdiv();    // instantiation of divider module

	FIFOF#(Floating_output) ff_final_out <- mkFIFOF();			// instantiation of output FIFO whose structure definition is given in riscv_types.bsv
	FIFO#(Stage1_type) ff_stage1 <- mkPipelineFIFO();       // instantiation of Stage 1 FIFO
	FIFO#(Stage2_type) ff_stage2 <- mkPipelineFIFO();       // instantiation of Stage 2 FIFO

    //This is the second stage of the pipe. Here the division of the two mantissas take place. Rest of the data are enqueued in another FIFO.
	rule rl_stage2;

	    int_div._inputs({ff_stage1.first().divisor,3'd0},{ff_stage1.first().dividend,3'd0});        //sending inputs to the divider module
	    ff_stage2.enq(Stage2_type{  exponent    : ff_stage1.first().exponent,                        
                                    sign        : ff_stage1.first().sign,
                                    invalid     : ff_stage1.first().invalid,
                                    infinity    : ff_stage1.first().infinity,
									lv_NaN         : ff_stage1.first().lv_NaN,
                                    dz          : ff_stage1.first().dz,
                                    zero        : ff_stage1.first().zero,
                                    fsr         : ff_stage1.first().fsr,
									rounding_mode : ff_stage1.first().rounding_mode
                                 });
	    ff_stage1.deq();        //Freeing the previous FIFO so that it can accept new inputs
	endrule

	rule rl_stage3;

		Bit#(27) lv_quotient = int_div.output_quotient();	//Quotient from the integer divider
		Bit#(28) lv_remainder = int_div.output_remainder(); //Remainder from the integer divider
        let lv_NaN = ff_stage2.first().lv_NaN;
		let lv_infinity = ff_stage2.first().infinity;
		let rounding_mode = ff_stage2.first().rounding_mode;
		int_div._remove_last_entry();						//Frees the integer divider
		bit lv_decr_exp= 0;
		if(lv_quotient[26]==0) begin
			lv_quotient=lv_quotient<<1;
			lv_decr_exp=1;
		end

		//////////////////////////////Rounding stage///////////////////////////////////////////////////////////////////

		bit lv_guard = lv_quotient[2];  
		bit lv_round = lv_quotient[1];  
		bit lv_sticky=0;				
 		bit lv_inexact=0;				
           
		if(lv_remainder!=0) // if the remainder is zero, sticky bit is set to 1.
			lv_sticky = 1;

		if((lv_sticky | lv_guard | lv_round)==1)// if any of the sticky,guard or round bit is set, the value is inexact.
			lv_inexact= 1;


		bit lv_roundup=0;						

		// Following if-else condition determine the value of lv_roundup. If set, the mantissa needs to be incremented, else the mantissa remains unchanged.
		if(rounding_mode == 'b000) 
			lv_roundup = lv_guard & (lv_round|lv_sticky|lv_quotient[3]);
		else if(rounding_mode == 'b100)
			lv_roundup = lv_guard & (lv_round|lv_sticky|ff_stage2.first().sign);
		else if(rounding_mode == 'b011) 
			lv_roundup = (lv_guard|lv_round|lv_sticky)&~ff_stage2.first().sign;
		else if(rounding_mode == 'b010)
            lv_roundup = (lv_guard|lv_round|lv_sticky)&ff_stage2.first().sign;
		else if(rounding_mode == 'b111) begin
        if(ff_stage2.first().fsr[7:5] == 'b000)				// round to nearest, ties to even
			lv_roundup = lv_guard & (lv_round|lv_sticky|lv_quotient[3]);
        else if(ff_stage2.first().fsr[7:5] == 'b100)		// round to nearest, ties to max magnitude
            lv_roundup = lv_guard & (lv_round | lv_sticky |ff_stage2.first().sign);
        else if(ff_stage2.first().fsr[7:5] == 'b011)		// round up 
	   		lv_roundup = (lv_guard|lv_round|lv_sticky)&~ff_stage2.first().sign;
        else if(ff_stage2.first().fsr[7:5] == 'b010)		// round down			
            lv_roundup = (lv_guard|lv_round|lv_sticky)&ff_stage2.first().sign;
		end
        // otherwise if round to zero mode, then do nothing

		Bit#(25) lv_rounded_quotient;

        if(lv_roundup==1)
			lv_rounded_quotient={1'b0,lv_quotient[26:3]}+ 'd1;
		else
			lv_rounded_quotient={1'b0,lv_quotient[26:3]};

		/////////////////Determining shift in exponent/////////////////////////////////////////

		Bit#(10) lv_to_add_to_exp;
		if(lv_rounded_quotient[24]==1)begin
			if(lv_decr_exp==1)
				lv_to_add_to_exp=0;
			else
				lv_to_add_to_exp=1;
		end
		else begin
			if(lv_decr_exp==1)
				lv_to_add_to_exp=-1;
			else
				lv_to_add_to_exp=0;
		end
		Bit#(10) lv_new_exponent = ff_stage2.first().exponent + lv_to_add_to_exp;


		if(&(lv_new_exponent[7:0]) == 1 && lv_NaN == 0)   //Inferred from corner test case, where infinity can occur even when both the inputs are normal like when exp1 = +112 and exp2 = -16.
		    lv_infinity = 1;	

		///////////////////////Determining overflow and underflow/////////////////////////////////

		bit lv_overflow=0;
		bit lv_underflow=0;

		// if the 9th bit is set, there is an overflow in the result.
		// if the 10th bit is set, there is an underflow in the result.
		if(lv_new_exponent[9]==1)begin
			lv_underflow=1;
			// $display("underflow.....");
		end
		else if(lv_new_exponent[8]==1)	begin
			lv_overflow=1;
			// $display("overflow.....");
		end

		////////////////////Determining the final result///////////////////////////////////////

		
		Bit#(32) lv_final_output= 0;                        // local variable which stores the final result
		Exception lv_exception = None;       // local variable which indicates which exception is generated. By default, no exceptions are generated.

		if(ff_stage2.first().invalid==1) begin              // the result is invalid
            lv_final_output= 32'h7fffffff;
    		lv_exception = Invalid;             // generating an invalid exception
		end			
        else if(ff_stage2.first().zero==1)                  // result is zero
            lv_final_output={ff_stage2.first().sign,31'd0};
        else if(ff_stage2.first().dz==1) begin              // if operand 2 is zero and operand 1 is neither zero nor NaN
            lv_exception = Divide_by_Zero;      // generating a divide by zero exception
            lv_final_output= {ff_stage2.first().sign,8'd-1,23'd0};
        end
        else if(lv_infinity==1)              // result is infinity
            lv_final_output={ff_stage2.first().sign,8'd-1,23'd0};
        else if(lv_underflow==1) begin                      // Underflow condition
            lv_final_output= {ff_stage2.first().sign,8'd0,lv_rounded_quotient[22:0]};       //TODO to verify if it needs to be lv_rounded_mantissa[22:1] and lv_inexact bit.
    		lv_exception = Underflow;                       // generating an underflow exception
        end
        else if(lv_overflow == 1) begin      							// Overflow condition. 
    		lv_exception = Overflow;                        // generating an overflow exception
            // In some cases the result should be the max magnitude number. In other cases the result is infinity.
			if(rounding_mode == 'b001)
				lv_final_output = {ff_stage2.first().sign,'h7f7fffff};
			else if(rounding_mode == 'b010 && ff_stage2.first().sign ==0)
				lv_final_output = {ff_stage2.first().sign,'h7f7fffff};
			else if(rounding_mode == 'b011 && ff_stage2.first().sign==1)
				lv_final_output = {ff_stage2.first().sign,'h7f7fffff};
			else if(rounding_mode == 'b111) begin
				if(ff_stage2.first().fsr[7:5] == 'b001)                                     // round to nearest
					lv_final_output={ff_stage2.first().sign,'h7f7fffff};
				else if(ff_stage2.first().fsr[7:5] == 'b010 && ff_stage2.first().sign==0)   // round down a positive number
					lv_final_output={ff_stage2.first().sign,'h7f7fffff};
				else if(ff_stage2.first().fsr[7:5] == 'b011 && ff_stage2.first().sign==1)   // round up a negative number
					lv_final_output={ff_stage2.first().sign,'h7f7fffff};
				else                                                                        // rest of the cases the result is infinity
            		lv_final_output={ff_stage2.first().sign,8'd-1,23'd0};
			end
			else 
				lv_final_output ={ff_stage2.first().sign,8'd-1,23'd0};
        end
       	else begin                                      // if all the above conditions are false, the result is a normal number
            lv_final_output= {ff_stage2.first().sign,lv_new_exponent[7:0], lv_rounded_quotient[22:0]};
			if(lv_inexact==1)                           // checking if the result is inexact.
    			lv_exception = Inexact;     // generating an inexact exception
		end

        // Forming the new Floating point Status Register
		Bit#(32) lv_fsr_ ={ff_stage2.first().fsr[31:10],ff_stage2.first().infinity,ff_stage2.first().zero,ff_stage2.first().fsr[7:5],ff_stage2.first().invalid,ff_stage2.first().dz,lv_overflow,lv_underflow,lv_inexact}; 		
        
        //rg_ready_signal<=True;      // Ready signal is made true to indicate that the result is ready.
                                    // Wherever value is of ff_final_out is read, the implicit condition of that rule includes checking if ff_final_out is not Empty.
                                    // But still a ready signal is required when we are having multiple execution units returning results simultaneously and we need to give priority to one over the other.
        
        // Enqueing the final result into the output FIFO
    	ff_final_out.enq(Floating_output{ 
    		                   	  	  fsr             : lv_fsr_,
    		                     	  final_result    : {'d0,lv_final_output},            //Appending zeros at the MSB since the result is a Single Precision number which is 32-bits wide whereas the rob entries are 64-bits.
    		                     	  exception       : lv_exception
                                    });
	    ff_stage2.deq();

	endrule

	method Action _start(Bit#(32) operand1, Bit#(32) operand2,  Bit#(32) fsr, Bit#(3) rounding_mode);

		//Operand 1 Variables
	    bit lv_is_op1_mantissa_zero=~(|(operand1[22:0]));	// =1 if mantissa of operand 1 is zero
	    bit lv_op1_is_infinity= 0;                          // =1 if operand 1 is infinity
	   // bit lv_op1_is_nan= 0;                               // =1 if operand 1 is Not a Number
	    bit lv_op1_is_zero= 0;                              // =1 if operand 1 is zero
	    bit lv_op1_is_denormal= 0;                          // =1 if operand 1 is denormal
	    bit lv_op1_is_normal= 0;                            // =1 if operand 1 is normal

	    //Operand 2 Variables
	    bit lv_is_op2_mantissa_zero=~(|(operand2[22:0]));	// =1 if mantissa of operand 2 is zero
	    bit lv_op2_is_infinity= 0;                          // =1 if operand 2 is infinity
	   // bit lv_op2_is_nan= 0;                               // =1 if operand 2 is Not a Number
	    bit lv_op2_is_zero= 0;                              // =1 if operand 2 is zero
	    bit lv_op2_is_denormal= 0;                          // =1 if operand 2 is denormal
	    bit lv_op2_is_normal= 0;                            // =1 if operand 2 is normal
        bit lv_NaN = 0;
	    //Output Variables
	    bit lv_invalid= 0;                                  // =1 if result is invalid
	    bit lv_infinity= 0;                                 // =1 if result in infinity
	    bit lv_dz= 0;                                       // =1 if there is a divide by zero error
	    bit lv_zero= 0;                                     // =1 if result is zero
        bit lv_sign= operand1[31] ^ operand2[31];           // sign bit


        //////////////////////////// SETTING OPERAND 1 AND OPERAND 2 VARIABLES ///////////////////////////////////////////
        if(&(operand1[30:23])==1) begin			        	//if all bits of exp are 1
			if(lv_is_op1_mantissa_zero==1)		        	//if mantissa=0 => operand1 is infinite
	 		    lv_op1_is_infinity= 1;
				else					                    	//if mantissa!=0 => operand1 is NaN
		        lv_NaN = 1;
		end
	    else if(|(operand1[30:23])==0) begin	        	//if exponent=0
	        if(lv_is_op1_mantissa_zero==1)		        	//if mantissa=0 => operand1 is zero
		  		lv_op1_is_zero= 1;
			else				                        	//if mantissa!=0 => operand1 is denormal
		    	lv_op1_is_denormal=1;
	    end
	    else
	        lv_op1_is_normal=1;
	    	   
	    if(&(operand2[30:23])==1) begin			        	//if all bits of exp are 1
			if(lv_is_op2_mantissa_zero==1)		        	//if mantissa=0 => operand2 is infinite
	 	    	lv_op2_is_infinity= 1;
			else					                    	//if mantissa!=0 => operand2 is NaN
		    	lv_NaN = 1;
	    end
	    else if(|(operand2[30:23])==0) begin	        	//if exponent=0
	        if(lv_is_op2_mantissa_zero==1)		        	//if mantissa=0 => operand2 is zero
	   	   		lv_op2_is_zero= 1;
			else				                        	//if mantissa!=0 => operand2 is denormal
		    	lv_op2_is_denormal=1;
	    end
	    else
	        lv_op2_is_normal=1;

	    //////////////////////////////////SETTING OUTPUT VARIABLES////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	    if(((lv_NaN) | (lv_op1_is_infinity & lv_op2_is_infinity) | (lv_op1_is_zero & lv_op2_is_zero))==1) begin  //op1 or op2 are NaN (or) both are infinity (or) both are zero
			lv_invalid=1;                           //result is invalid

		end
		else if(lv_op1_is_infinity ==1) begin       //op 2 is neither NaN nor infinity, and op1 is infinity
	        lv_infinity=1;                          //result is infinity

		end
	    else if(lv_op2_is_zero==1) begin            //op 1 is neither NaN nor infinity, and op2 is zero
            lv_infinity=1;                          //result is infinity
         	lv_dz=1;                                //setting the divide by zero flag
        end
        else if((lv_op2_is_infinity | lv_op1_is_zero)==1)   //{op1 and op2 are not NaN} (and) {op1 is zero and op2 is not zero (or) op2 is infinity and op1 is not infinity}
            lv_zero=1;                              //result is zero

        Bit#(10) lv_new_exponent = 10'b0001111111 + ({2'b0,operand1[30:23]} - {2'b0,operand2[30:23]});

	    Bit#(24) lv_dividend= {~lv_op1_is_denormal, operand1[22:0]};                // forming the dividend mantissa
	    Bit#(24) lv_divisor= {~lv_op2_is_denormal, operand2[22:0]};                 // forming the divisor mantissa
		$display("lv_dividend : %h lv_divisor : %h \n",lv_dividend,lv_divisor);

	    ff_stage1.enq( Stage1_type  {	    exponent		: lv_new_exponent,
	  			               				dividend		: lv_dividend,
				       	        			divisor			: lv_divisor,
                                        	sign        	: lv_sign,
        				        			invalid			: lv_invalid,
        				        			infinity		: lv_infinity,
											lv_NaN      	: lv_NaN,
        				        			dz	    		: lv_dz,
        				        			zero			: lv_zero,
        				        			fsr		    	: fsr,
											rounding_mode 	: rounding_mode
                                     });
    
	endmethod

    // Output method which send the result
	method Floating_output final_result_();
	    return ff_final_out.first();
	endmethod
	
    // This method needs to be called whenever the method final_result is called.
    // This method frees the final FIFO and resets the ready signal.
    method Action _deque_buffer_reset_ready_signal();
        ff_final_out.deq();
    endmethod

endmodule

// //Testbench
// module mkTb_fpu_spfloating_divider_file(Empty);

//   	Reg#(Bit#(32)) rg_clock				<-mkReg(0);

//    	Ifc_fpu_spfloating_divider	divider <- mkfpu_spfloating_divider();

//    	RegFile #(Bit #(10), Bit #(68))  input_data <- mkRegFileFullLoad("Div_testcases.hex");
// 	Reg #(Bit #(10)) index <- mkReg(0);
    	
// 	Reg #(Bit #(32)) state_clock <- mkReg(1);

// 	Reg#(int) cnt <- mkReg(0);                  //File Variable
// 	let fh <- mkReg(InvalidFile) ;				//File handler		

// 	//rule for file creation
// 	rule open (cnt == 0 ) ;
// 		File tb_mul_output <- $fopen("tb_div_output.hex", "w+"); 
// 		fh <= tb_mul_output;
// 		cnt <= 1 ;
// 	endrule

// 	rule state_clock_count;
// 		//$display("------------------ Inside the rule state_clock_count at %0d ----------------------", $time);
// 		state_clock <= state_clock + 1;
// 	endrule : state_clock_count

// 	rule take_input_in (state_clock <= 100000);

// 		//$display("The input %h and %h is given at %0d", input_data.sub(index)[67:36], input_data.sub(index)[35:4], $time);
// 		 $display("Giving Inputs ",$time);
// 		divider._start(input_data.sub(index)[67:36],input_data.sub(index)[35:4],{'b0,input_data.sub(index)[2:0],5'b0},input_data.sub(index)[2:0]);
// 		index <= index + 1;
	
// 	endrule : take_input_in

// 	rule display_output;

// 		let abc = divider.final_result_();
// 		divider._deque_buffer_reset_ready_signal();
// 		//$display("The ouptput is available at %0d", $time);
// 		//$display("%d %h", srno, abc.final_result[31:0]);
// 		$fwrite(fh, "%h\n", abc.final_result[31:0]);
// 		$display("Final result= %h, fsr: %h", abc.final_result, abc.fsr,$time);

		
// 		if(abc.exception matches tagged Invalid .*)
// 			$display("INVALID EXCEPTION");
// 		else if(abc.exception matches tagged Inexact.*)
// 			$display("INEXACT EXCEPTION");
// 		else if(abc.exception matches tagged Overflow.*)
// 			$display("OVERFLOW EXCEPTION");
		

// 	endrule : display_output

// 	rule end_testing (state_clock == 100000);
// 		$finish(0);

// 	endrule : end_testing
// endmodule

// module mkTb_fpu_spfloating_divider(Empty);
//   	Reg#(Bit#(32)) rg_clock<-mkReg(0); 
//   	Reg#(Bit#(32)) rg_operand1<-mkReg(32'h420c0000);
//    	Reg#(Bit#(32)) rg_operand2<-mkReg(32'h40d00000);
//     Reg#(Bit#(3)) rg_inst_count<-mkReg(1);
//    	Ifc_fpu_spfloating_divider divider<-mkfpu_spfloating_divider();
    
// 	rule rl_clock_count;
// 		rg_clock<=rg_clock+1;
// 		$display("Clock= %d", rg_clock);
//     if(rg_clock=='d100)
//         $finish(0);
// 	endrule

// 	rule rl_display_result;
//  	    let abc = divider.final_result_();
//         divider._deque_buffer_reset_ready_signal();
//         $display("Final result= %b, fsr: %b", abc.final_result[31:0], abc.fsr);
        
// 		// if(abc.exception matches tagged No_exception.*)
//   //  		  	$display("NO EXCEPTION");
//   //       else if(abc.exception matches tagged Invalid .*)
//   //      		$display("INVALID EXCEPTION");
//   //       else if(abc.exception matches tagged Inexact.*)
//   //    		$display("INEXACT EXCEPTION");
//   //       else if(abc.exception matches tagged Overflow.*)
//   //    		$display("OVERFLOW EXCEPTION");
// 	endrule:rl_display_result

// 	rule rl_start1(rg_inst_count==1);
//     	divider._start(rg_operand1, rg_operand2,0,0);
//         $display("Giving first inputs = %b , %b ", rg_operand1, rg_operand2);
//         rg_inst_count<=2;
// 	endrule


// endmodule
endpackage
