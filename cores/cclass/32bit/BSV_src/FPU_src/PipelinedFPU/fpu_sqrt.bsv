/*

Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name 	: Floating Point square root single precision 
Author Name 	: Rishi Naidu, Vinod.G, Aditya Govardhan
e-mail Id	: rishinaidu.rn@gmail.com, g.vinod1993@gmail.com, dtgovardhan@gmail.com
Last updated on : 20th January 2013

This is a 26 cycle FLoating point square root module which is designed in form of a Finite State Machine(FSM). The implementation is NON-RESTORING SQUARE ROOT ALGORITHM
Design in the form of FSM which helps to reduce the cost and area of the module, but the throughput is compromised. Now instead of a result every cycle we will get output after 26 cycles.
As the use of square root won't be that frequent, we are in a safe zone to make such a design decision.

Here the module is divided into 4 stages. 
1st stage : Start stage, where one iteration is carried out and the output is enqued in a FIFO.
2nd stage : This is an intermediate stage which is like a buffer before the 3rd stage as the 3rd stage is the recursive stage. 
            The FIFO before stage 2 helps to control the input to this module.The output from this stage is put into a register
3rd Stage : This is the recursive stage which has to perform the iteration 23 times. Putting the data in a REGISTER helps to read and write in the same rule
4th Stage : Its the final stage which perform the final iteration.(26th Iteration). The final output is then stored in the final output FIFO.

Here the FIFO Stage1 acts like a buffer for the whole module and doesn't allow any other input till the 26 cycle iteration is complete.
A direct use of register will interfere with iteration if the next input is immediately after the first input.
Here we only need 24 cycles to generate the 24 bits of mantissa, but 2 extra iterations are needed to get the GUARD BIT and ROUND BIT for rounding operation.
The STICKY bit is determined by the remainder. In a NON RESTORING SQUARE ROOT ALOGRITHM the remainder is 0 for a perfect square.
Else the remainder will have some value. If reminder !=0 then STICY BIT =1


Implementation is based on a IEEE paper Titled:
"Implementation of Single Precision Floating Point Square Root on FPGAs"


*********Performance****************:
Using UMCIP 65nm library in SYNOPSYS
Critical Path Length    :    0.66 ns
Frequency 	        :    1.5 GHz
Combinational Cell Count:    2207
Sequential Cell Count   :    481

*/

package fpu_sqrt;
import defined_types::*;
import FIFO::*;
import SpecialFIFOs::*;
import RegFile::*;

typedef struct{
		Bit#(52) mantissa;        //Holds the extended mantissa 
                Bit#(26) result_mantissa; //Holds the Output mantissa
                Bit#(8) exponent;         //Holds the Output exponent
                bit sign;                 //Final sign bit
                Bit#(29) remainder;       //Remainder after eact iteration
                Bit#(26) root;            //Root after each iteration
                Bit#(32) fsr;             //the file status register containing all the flags and control bits
		}Stage_data deriving(Bits,Eq); //Data structure of interstage FIFO and register			

interface Ifc_fpu_sqrt;
	//Input Methods
	method Action _start(Bit#(32) _operand1, Bit#(32) fsr);  
	//Output Methods
	method Action deque_buffer();
	method Floating_output get_result();
	
endinterface

(*synthesize*)
module mkfpu_sqrt(Ifc_fpu_sqrt);

    FIFO#(Stage_data) ff_stage1 <- mkPipelineFIFO();     //Pipeline FIFO stage1
    FIFO#(Floating_output) ff_final_out <- mkPipelineFIFO(); //Final Output FIFO

    Reg#(Stage_data) rg_inter_stage <- mkRegU();         //Inter Stage register 
    Reg#(Bit#(32)) rg_state <-mkReg(0);                  //State counter of the module
   //***********ITERATION :2********************// 
    rule rl_stage2 (rg_state==1);
        let lv_stage1_data = ff_stage1.first();                   //Get data from FIFO stage1          
        let lv_remainder = lv_stage1_data.remainder;              //Get remainder data from stage1 
        Bit#(26) lv_root = lv_stage1_data.root;                   //Get root value from stage1      
        let mantissa = lv_stage1_data.mantissa;                   //Updated mantissa               
        Bit#(26) result_mantissa = lv_stage1_data.result_mantissa;//Get result value                    
        //Determining remainder
        if (lv_remainder[28]==1) begin //When r <0
           lv_remainder = {lv_remainder[26:0],mantissa[51],mantissa[50]} + {1'b0,lv_root[25:0],1'b1,1'b1};  
        end
        else begin
            lv_remainder = {lv_remainder[26:0],mantissa[51],mantissa[50]} - {1'b0,lv_root[25:0],1'b0,1'b1};
        end
        //Determining quotient
        if (lv_remainder[28]==1) begin //When r <0
            lv_root = {lv_root[24:0],0};
    
        end
        else begin
            lv_root = {lv_root[24:0],1}; 
        end
        result_mantissa[0]= lv_root[0];        //Storing the next bit in result_mantissa 
        mantissa = mantissa <<2;               //Shifting mantissa to get next 2 bits
        result_mantissa = result_mantissa <<1; //Shifting result_mantissa to make space to store the next bit
        rg_state <= rg_state +1;               //Incrementing state counter
        // $display("****************************************State = %d", rg_state);
        // $display("Remainder =%h", lv_remainder);
        // $display("Mantissa = %h",result_mantissa);
        //Storing the required values in register
        rg_inter_stage <= Stage_data{mantissa : mantissa, 
                                     result_mantissa : result_mantissa,
                                     root: lv_root ,
                                     fsr :lv_stage1_data.fsr,
                                     remainder:lv_remainder,
                                     sign : lv_stage1_data.sign,
                                     exponent : lv_stage1_data.exponent};
    endrule
    //********************ITERATION : 3 TO 25**************
    //RECURSIVE STAGE (saves hardware)
    rule rl_inter_stage (rg_state>1 && rg_state <25);               
        //Here register is used instead of FIFO as we have to read and write in the same cycle
        let lv_remainder = rg_inter_stage.remainder;                //Getting remainder
        Bit#(26) lv_root = rg_inter_stage.root;                     //Getting root value
        let mantissa = rg_inter_stage.mantissa;                     //Getting updated mantissa value 
        Bit#(26) result_mantissa = rg_inter_stage.result_mantissa;  //Getting the result bit of the square root
        //Determining the remainder
        if (lv_remainder[28]==1) begin //When r <0
           lv_remainder = {lv_remainder[26:0],mantissa[51],mantissa[50]} + {1'b0,lv_root[25:0],1'b1,1'b1};  
        end
        else begin
            lv_remainder = {lv_remainder[26:0],mantissa[51],mantissa[50]} - {1'b0,lv_root[25:0],1'b0,1'b1};
        end
        //Determining quotient
        if (lv_remainder[28]==1) begin //When r <0
            lv_root = {lv_root[24:0],0};
    
        end
        else begin
            lv_root = {lv_root[24:0],1}; 
        end
        result_mantissa[0] = lv_root[0];            //Storing the result bit from root
        mantissa = mantissa <<2;                    //Shifting mantissa to get the next 2 bits
        result_mantissa = result_mantissa <<1;      //Making space for the next bit
        rg_state <= rg_state +1;                    //Incrementing state counter
        // $display("****************************************State = %d", rg_state);
        // $display("Remainder =%h", lv_remainder);
        // $display("Mantissa = %h",result_mantissa);
        //Storing required values in register for next iteration
        rg_inter_stage <= Stage_data { mantissa:mantissa ,
                                       result_mantissa : result_mantissa,
                                       fsr :rg_inter_stage.fsr,
                                       root:lv_root , 
                                       remainder:lv_remainder,
                                       sign : rg_inter_stage.sign,
                                       exponent: rg_inter_stage.exponent};
        
       
    endrule
    //*****************ITERATION :26 ***********************//
    rule rl_final_stage (rg_state==25);
        let lv_remainder = rg_inter_stage.remainder;               //Getting remainder value for iteration
        Bit#(26) lv_root = rg_inter_stage.root;                    //Getting root value for iteration
        let mantissa = rg_inter_stage.mantissa;                    //Getting  shifted mantissa value
        Bit#(26) result_mantissa = rg_inter_stage.result_mantissa; //Getting the result bits
        let result_exponent = rg_inter_stage.exponent;             //Getting the final result exponent value
        //Determining the remainder
        if (lv_remainder[28]==1) begin //When r <0
           lv_remainder = {lv_remainder[26:0],mantissa[51],mantissa[50]} + {1'b0,lv_root[25:0],1'b1,1'b1};  
        end
        else begin
            lv_remainder = {lv_remainder[26:0],mantissa[51],mantissa[50]} - {1'b0,lv_root[25:0],1'b0,1'b1};
        end
        //Determining quotient
        if (lv_remainder[28]==1) begin //When r <0
            lv_root = {lv_root[24:0],0};
    
        end
        else begin
            lv_root = {lv_root[24:0],1}; 
        end
        result_mantissa[0]= lv_root[0];
        //**********Restoring the remainder if the remainder<0***********//
        //mantissa = mantissa <<2;
        if (lv_remainder[28] ==1) begin
            //lv_remainder = lv_remainder + {3'b0,lv_root[24:0],1'b1};
            lv_remainder = lv_remainder + zeroExtend({lv_root[25:0],1'b1});
        end
        
        //********Carrying out the rounding operation**************//
        Bit#(3) rounding_mode = rg_inter_stage.fsr[7:5];
        
        bit lv_roundup =0;                      //Declaring roundup bit
        bit lv_guard = result_mantissa[1];      //Setting the guard bit
        bit lv_round = result_mantissa[0];      //Setting the round bit
        bit lv_sticky = |(lv_remainder);        //Setting the sticky bit
        bit lv_sign = rg_inter_stage.sign;      //Getting sign bit 
	if(rounding_mode== 'b000)	    	// round to nearest, ties to even
			 lv_roundup = lv_guard & (result_mantissa[2] | lv_round | lv_sticky);
	else if(rounding_mode == 'b100)		// round to nearest, ties to max magnitude
	         	 lv_roundup = lv_guard & (lv_round | lv_sticky | ~lv_sign);
	else if(rounding_mode == 'b011 )	// round up
		         lv_roundup = (lv_guard | lv_round | lv_sticky) & (~lv_sign);
	else if(rounding_mode == 'b010)		// round down		
			 lv_roundup = (lv_guard | lv_round | lv_sticky) & (lv_sign);

        Bit#(27) lv_extended_mantissa = {1'b0,result_mantissa};
        if (lv_roundup==1) begin
                lv_extended_mantissa = lv_extended_mantissa + 27'd4;    //If roundup then add 4 as the LSB for final mantissa is 3rd bit 
                if (lv_extended_mantissa[26]==1)                        //When mantissa overflows
                        result_exponent = result_exponent +1;           //Increment exponent by 1
        end
        
        //Here most exceptions are taken care of in first stage, so module doesn't perform all iterations
	Exception lv_exception = None;                   //Setting exception variable
        // $display("****************************************State = %d", rg_state);
        // $display("Remainder =%h", lv_remainder);
        // $display("Mantissa = %h",lv_extended_mantissa);

        Bit#(64) final_result = zeroExtend({lv_sign, result_exponent, lv_extended_mantissa[24:2]});  //Setting the final result 

        ff_final_out.enq (Floating_output{
                                      fsr: rg_inter_stage.fsr,
                                      final_result:final_result,
                                      exception : lv_exception});
    
    endrule
    //START METHOD
    //*******************ITERATION :1 *********************************//
    method Action _start(Bit#(32) _operand1, Bit#(32) fsr);
        // $display("start_mantissa = %d", mantissa);
        // $display("start_mantissa = %b", mantissa);
        



        bit lv_is_invalid =0;                                               //Invalid Flag
        Exception lv_exception = None;                                      //Declaring exception

        bit sign = _operand1[31];                                           //Assigning sign bit
        Bit#(8) exponent = _operand1[30:23];                                //INput exponent
        Bit#(52) mantissa = {1'b0,1'b1,_operand1[22:0],27'b0};              //Extend mantissa to 48 bits as we need 24 bit output mantissa (Each iteartion use 2 bits of the opearand)

        if (exponent[0]==0)                                                 //If the exponent is even
                mantissa = mantissa <<1;                                    //Mantissa is left shifted so that Exponent-127 is even
        
        Bit#(29) lv_remainder = 0;                                          //Declaring local remainder variable
        Bit#(26) lv_root = 0;                                               //Declaring local root/quotient variable
        Bit#(26) result_mantissa = 0;                                       //Will store the square root answer 
        
        Bit#(8) result_exponent = (exponent >>1) +'d63 + zeroExtend(exponent[0]);  //Calculating the result exponent
        
        //Determining remainder
        if (lv_remainder[28]==1) begin //When r <0
           lv_remainder = {lv_remainder[26:0],mantissa[51],mantissa[50]} + {1'b0,lv_root[25:0],1'b1,1'b1};  
        end
        else begin
            lv_remainder = {lv_remainder[26:0],mantissa[51],mantissa[50]} - {1'b0,lv_root[25:0],1'b0,1'b1};
        end
        //Determining quotient
        if (lv_remainder[28]==1) begin //When r <0
            lv_root = {lv_root[24:0],0};
        end
        else begin
            lv_root = {lv_root[24:0],1}; 
        end
    
        result_mantissa[0] = lv_root [0];       //Setting the LSB of the result
        mantissa = mantissa <<2;                //Shifting the mantissa to get the next 2 bits of next iteration
        result_mantissa = result_mantissa <<1;  //Shifting the result mantissa to make space for the next bit
        
        Bit#(1) lv_exp1_is_all_ones= _operand1[30:23]=='b11111111 ? 1:0;    //1 if all the bits of exponent are set; used to check if op1 is infinity or NaN
        Bit#(1) lv_exp1_is_zero= _operand1[30:23]==0? 1:0;                  //1 if exponent of operand1 is 0
        Bit#(1) lv_man1_is_zero= _operand1[22:0]== 0 ? 1:0;                 //1 if mantissa of op1 is 0
        Bit#(1) lv_op1_is_zero= lv_man1_is_zero & lv_exp1_is_zero;          //1 when operand1=0
        
        Bit#(1) lv_inf=0;
        Bit#(1) lv_inv=0;
        Bit#(1) lv_zero=0;
            
        if(lv_exp1_is_all_ones == 1 && lv_man1_is_zero == 0)        //operand is NaN
            lv_inv=1;
        else if(lv_exp1_is_all_ones == 1 && lv_man1_is_zero == 1)   // check if operand is infinite
        begin               
            if(sign == 1)                                           // if -inf then result is NaN                                                  
                lv_inv=1;
            else                                                    // if +inf then result is +inf                                                       
                lv_inf=1;
        end
        else if(lv_op1_is_zero == 1)
            lv_zero=1;
        
        
        //*************Checking some exceptional conditions******************//
        if (lv_inv == 1 || (sign == 1 && lv_zero == 0) )// when the INput is NAN or Negative => Invalid flag is raised
                 ff_final_out.enq (Floating_output{ final_result:zeroExtend({1'b0, 8'd-1, 23'b10000000000000000000000}), //Quite Nan
                                                    fsr:{fsr[31:10],1'b0,1'b0,fsr[7:5],2'b0,1'b0,1'b0,1'b0}, //{fsr[31:10],1'b0,lv_is_zero[1],fsr[7:5],2'b0,lv_overflow,lv_underflow,lv_final_inexact}
                                                    exception : lv_exception});
                
        else if (lv_inf == 1)                         //Infinity
                ff_final_out.enq (Floating_output{  final_result:zeroExtend({1'b0, 8'd-1, 23'b0}),
                                                    fsr:{fsr[31:10],1'b0,1'b0,fsr[7:5],2'b0,1'b0,1'b0,1'b0}, //{fsr[31:10],1'b0,lv_is_zero[1],fsr[7:5],2'b0,lv_overflow,lv_underflow,lv_final_inexact}
                                                    exception : lv_exception});
        else if (lv_zero == 1) begin //Zeros
                // $display ("Zeros");
                ff_final_out.enq (Floating_output{  final_result:signExtend({sign, 31'b0}),
                                                    fsr:{fsr[31:10],1'b0,1'b1,fsr[7:5],2'b0,1'b0,1'b0,1'b0}, //{fsr[31:10],1'b0,lv_is_zero[1],fsr[7:5],2'b0,lv_overflow,lv_underflow,lv_final_inexact}
                                                    exception : lv_exception});
        end   
        else begin
            //State counter incremented only when it does not meet any above exceptional cases
            rg_state <= rg_state +1;  //Increment the State_counter for next iteration
        end
        // $display("****************************************State = %d", rg_state);
        // $display("Remainder = %b", lv_remainder);
        // $display("Mantissa = %h",result_mantissa);
        //Storing required data in FIFO stage1 for next iteration
        ff_stage1.enq (Stage_data{    mantissa : mantissa,
                                      fsr : fsr,
                                      result_mantissa : result_mantissa,
                                      root : lv_root,
                                      remainder : lv_remainder,
                                      sign : sign,
                                      exponent : result_exponent });
    endmethod

    method Action deque_buffer();
    		ff_final_out.deq();
                ff_stage1.deq();
                rg_state <=0;
    endmethod
    	
    method Floating_output get_result();
    		return ff_final_out.first();
    endmethod
    

endmodule




// //*************Test bench******************//
(*synthesize*)
module mkTb_fpu_sqrt(Empty);
	Reg#(Bit#(32)) rg_clock <-mkReg(0);     //Clock register
//      Reg#(Bit#(32)) rg__operand1ut1 <- mkReg(32'h441c4000);//625
//      Reg#(Bit#(32)) rg__operand1ut1 <- mkReg(32'h3f800000);//1
//      Reg#(Bit#(32)) rg__operand1ut1 <- mkReg(32'h46192400); //9801 output 99
        	
        Reg#(Bit#(32)) rg__operand1ut1 <- mkReg(32'h13cbbf1a);//435.55
	Ifc_fpu_sqrt square_root <- mkfpu_sqrt;

	rule rl_clock;
		
		rg_clock<=rg_clock+1;
	//	$display("CLOCK=%d",rg_clock);

                if(rg_clock=='d50) begin
		    $finish(0);
                end
	endrule
	
	rule give__operand1ut(rg_clock==2);
        $display("Giving _operand1ut at %d", rg_clock);
		square_root._start(rg__operand1ut1,{'b0,3'b001,5'b0}); 
        endrule
		
	rule get_output;
		let lv_output = square_root.get_result();
        $display("taking output at %d", rg_clock);
		$display("Output= %h" , lv_output.final_result);
		square_root.deque_buffer();
	endrule
endmodule

module mkTb_fpu_sqrt_2 (Empty);
    
    RegFile #(Bit #(16), Bit #(36))  input_data <- mkRegFileFullLoad("sqrt_inputs.hex");
    Reg #(Bit #(16)) index <- mkReg(0);
 
    Ifc_fpu_sqrt sqrt <- mkfpu_sqrt();
    Reg #(Bit #(32)) state_clock <- mkReg(1);

    Reg#(int) cnt <- mkReg(0);                  //File Variable
    let fh <- mkReg(InvalidFile) ;              //File handler      

    //rule for file creation
    rule open (cnt == 0 ) ;
        File tb_mul_output <- $fopen("tb_sqrt_output.hex", "w+"); 
        fh <= tb_mul_output;
        cnt <= 1 ;
    endrule

    rule state_clock_count;
        // $display("------------------ Inside the rule state_clock_count at %0d ----------------------", $time);
        state_clock <= state_clock + 1;
    endrule

    rule take__operand1ut_in (state_clock <= 1650);
        // $display("The _operand1ut1 %h and _operand1ut2 %h with rounding %b", input_data.sub(index)[67:36], input_data.sub(index)[35:4], input_data.sub(index)[2:0],$time);
        sqrt._start(input_data.sub(index)[35:4],{'b0, input_data.sub(index)[2:0], 5'b0});
        index <= index + 1;
    
    endrule

    rule display_output;

        let abc = sqrt.get_result();
        sqrt.deque_buffer();
        $display("The ouptput is available at %0d", $time);
        $display("%h",abc.final_result[31:0]);
        $fwrite(fh, "%h\n", abc.final_result[31:0]);
        //srno <= srno + 1;
        // $display("Final result= %h, fsr: %h", abc.final_result, abc.fsr);

    endrule

    rule end_testing (state_clock == /*18274*/ 5000);

        // $display("Inside the rule end_testing at %0d", $time);
        $finish(0);

    endrule : end_testing

endmodule


endpackage