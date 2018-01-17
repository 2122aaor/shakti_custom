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
Last updated on : 14th August 2016

This is a 26 cycle FLoating point square root module which is designed in form of a Finite State Machine(FSM). The implementation is NON-RESTORING SQUARE ROOT ALGORITHM
Design in the form of FSM which helps to reduce the cost and area of the module, but the throughput is compromised. Now instead of a result every cycle we will get output after 26 cycles.
As the use of square root won't be that frequent, we are in a safe zone to make such a design decision.

Here the module is divided into 4 stages. 
1st stage : Start stage, where one iteration is carried out and the output is enqued in a FIFO.
2nd stage : This is an intermediate stage which is like a buffer before the 3rd stage as the 3rd stage is the recursive stage. 
            The FIFO before stage 2 helps to control the _operand1ut to this module.The output from this stage is put into a register
3rd Stage : This is the recursive stage which has to perform the iteration 23 times. Putting the data in a REGISTER helps to read and write in the same rule
4th Stage : Its the final stage which perform the final iteration.(26th Iteration). The final output is then stored in the final output FIFO.

Here the FIFO Stage1 acts like a buffer for the whole module and doesn't allow any other _operand1ut till the 26 cycle iteration is complete.
A direct use of register will interfere with iteration if the next _operand1ut is immediately after the first _operand1ut.
Here we only need 24 cycles to generate the 24 bits of mantissa, but 2 extra iterations are needed to get the GUARD BIT and ROUND BIT for rounding operation.
The STICKY bit is determined by the remainder. In a NON RESTORING SQUARE ROOT ALOGRITHM the remainder is 0 for a perfect square.
Else the remainder will have some value. If reminder !=0 then STICY BIT =1

Implementation is based on a IEEE paper Titled:
"Implementation of Single Precision Floating Point Square Root on FPGAs"
*/

package fpu_sqrt;
import defined_types::*;
import RegFile::*;
import FIFO::*;
import SpecialFIFOs::*;
typedef struct{
	Bit#(TMul#(TAdd#(fpman,3),2)) mantissa;        //Holds the extended mantissa 
    Bit#(TAdd#(fpman,3)) result_mantissa; //Holds the Output mantissa
    Bit#(TAdd#(fpexp,1)) exponent;
    bit sign;                 //Final sign bit
    Bit#(TAdd#(fpman,6)) remainder;       //Remainder after eact iteration
    Bit#(TAdd#(fpman,3)) root;            //Root after each iteration
    Bit#(32) fsr;             //the file status register containing all the flags and control bits
    Bit#(3) rounding_mode;
}Stage_data#(numeric type fpman, numeric type fpexp) deriving(Bits,Eq); //Data structure of interstage FIFO and register			

interface Ifc_fpu_sqrt#(numeric type fpinp, numeric type fpman, numeric type fpexp);
	//Input Methods
	method Action _start(Bit#(fpinp) _operand1, Bit#(32) fsr, Bit#(3) rounding_mode); 

	//Output Methods
	method Action deque_buffer();
	method Maybe#(Floating_output#(fpinp)) get_result();
	
endinterface

//(*synthesize*)
module mkfpu_sqrt(Ifc_fpu_sqrt#(fpinp,fpman,fpexp))
      provisos(
              Add#(TAdd#(fpman,fpexp),1,fpinp),
              Add#(fpman,3,fpman3),
              Add#(fpman3,2,fpman5),
              Add#(fpman,5,fpman5),
              Add#(fpman5,1,fpman6),
              Mul#(fpman3,2,ext_fpman),
              Add#(fpexp,1,fpexp1),
              Log#(TAdd#(1,ext_fpman),ext_fplog),
              //per request of bsc
              Add#(1, a__, fpexp),
              Add#(2, b__, fpman6),
              Add#(c__, 1, fpman3),
              Add#(d__, ext_fplog, fpexp1),
              Add#(e__, 1, b__)
   //           Add#(d__, 1, b__)
              //Add#(a__, TAdd#(1, TAdd#(fpexp, TAdd#(1, TSub#(fpman, 1)))), 64),
              //Add#(b__, ext_fplog, fpexp1),
              //Add#(1,c__,fpexp),
              //Add#(d__,TAdd#(fpman3,1),e__),
              //Add#(1,e__,f__),
              //Add#(1,f__,ext_fpman),
              //Add#(3,g__,fpman6),
              //Add#(h__,1,g__),
              //Add#(i__,1,fpman3),
              //Add#(j__,2,fpman6),
              //Add#(k__,1,j__)
              );

    let fPMAN = valueOf(fpman);
    let fPMAN3 = valueOf(fpman3);
    let fPMAN5 = valueOf(fpman5);
    let fPMAN6 = valueOf(fpman6);
    let fPEXP = valueOf(fpexp);
    let fPINP = valueOf(fpinp);
    let eXT   = valueOf(ext_fpman);

    Reg#(Maybe#(Floating_output#(fpinp))) ff_final_out <- mkReg(tagged Invalid); //Final Output FIFO

    Reg#(Stage_data#(fpman,fpexp)) rg_inter_stage <- mkRegU();         //Inter Stage register 
    Reg#(Bit#(32)) rg_state <-mkReg(0);                  //State counter of the module

   //***********ITERATION :2********************// 
    rule rl_stage2 (rg_state==1);         
        let lv_remainder = rg_inter_stage.remainder;              //Get remainder data from stage1 
        Bit#(fpman3) lv_root = rg_inter_stage.root;                   //Get root value from stage1      
        let mantissa = rg_inter_stage.mantissa;                   //Updated mantissa 
        let rounding_mode = rg_inter_stage.rounding_mode;        
        Bit#(fpman3) result_mantissa = rg_inter_stage.result_mantissa;//Get result value
        Bit#(fpman6) lv_remainder_temp = {lv_remainder[fPMAN3:0],mantissa[eXT-1],mantissa[eXT-2]};
        Bit#(fpman6) lv_root_temp_1 = {1'b0,lv_root[fPMAN3-1:0],1'b1,1'b1};
        Bit#(fpman6) lv_root_temp_2 = {1'b0,lv_root[fPMAN3-1:0],1'b0,1'b1};
        //Determining remainder
        if (lv_remainder[fPMAN5]==1) begin //When r <0
           lv_remainder = lv_remainder_temp + lv_root_temp_1;  
        end
        else begin
            lv_remainder = lv_remainder_temp - lv_root_temp_2;
        end

        //Determining quotient
        if(lv_remainder[fPMAN5]==1'b1) begin //When r <0
            lv_root = {lv_root[fPMAN3-2:0],1'b0};
        end
        else begin
            lv_root = {lv_root[fPMAN3-2:0],1'b1}; 
        end

        result_mantissa[0]= lv_root[0];        //Storing the next bit in result_mantissa 
        mantissa = mantissa <<2;               //Shifting mantissa to get next 2 bits
        result_mantissa = result_mantissa <<1; //Shifting result_mantissa to make space to store the next bit
        rg_state <= rg_state +1;               //Incrementing state counter

        $display("****************************************State = %d", rg_state);
        $display("Remainder =%h", lv_remainder);
        $display("Mantissa = %h",result_mantissa);

        //Storing the required values in register

        rg_inter_stage <= Stage_data{mantissa : mantissa, 
                                     result_mantissa : result_mantissa,
                                     root: lv_root ,
                                     fsr :rg_inter_stage.fsr,
                                     remainder:lv_remainder,
                                     sign : rg_inter_stage.sign,
                                     exponent : rg_inter_stage.exponent,
                                     rounding_mode : rounding_mode 
                                    };
    endrule

    //********************ITERATION : 3 TO 25**************
    //RECURSIVE STAGE (saves hardware)
    rule rl_inter_stage (rg_state>1 && rg_state < fromInteger(fPMAN3-1) );               
        //Here register is used instead of FIFO as we have to read and write in the same cycle

        let lv_remainder = rg_inter_stage.remainder;                //Getting remainder
        Bit#(fpman3) lv_root = rg_inter_stage.root;                     //Getting root value
        let mantissa = rg_inter_stage.mantissa;                     //Getting updated mantissa value 
        let rounding_mode = rg_inter_stage.rounding_mode;
        Bit#(fpman3) result_mantissa = rg_inter_stage.result_mantissa;  //Getting the result bit of the square root
        Bit#(fpman6) lv_remainder_temp = {lv_remainder[fPMAN3:0],mantissa[eXT-1],mantissa[eXT-2]};
        Bit#(fpman6) lv_root_temp_1 = {1'b0,lv_root[fPMAN3-1:0],1'b1,1'b1};
        Bit#(fpman6) lv_root_temp_2 = {1'b0,lv_root[fPMAN3-1:0],1'b0,1'b1};

        //Determining the remainder
        if (lv_remainder[fPMAN5]==1'b1) begin //When r <0
           lv_remainder = lv_remainder_temp + lv_root_temp_1;  
        end
        else begin
            lv_remainder = lv_remainder_temp - lv_root_temp_2;
        end

        //Determining quotient
        if (lv_remainder[fPMAN5]==1'b1) begin //When r <0
            lv_root = {lv_root[fPMAN3-2:0],1'b0};
        end
        else begin
            lv_root = {lv_root[fPMAN3-2:0],1'b1}; 
        end
        result_mantissa[0] = lv_root[0];            //Storing the result bit from root
        mantissa = mantissa <<2;                    //Shifting mantissa to get the next 2 bits
        result_mantissa = result_mantissa <<1;      //Making space for the next bit
        rg_state <= rg_state +1;                    //Incrementing state counter

        $display("****************************************State = %d", rg_state);
        $display("Remainder =%h", lv_remainder);
        $display("Mantissa = %h",result_mantissa);

        //Storing required values in register for next iteration
        rg_inter_stage <= Stage_data { mantissa:mantissa ,
                                       result_mantissa : result_mantissa,
                                       fsr :rg_inter_stage.fsr,
                                       root:lv_root , 
                                       remainder:lv_remainder,
                                       sign : rg_inter_stage.sign,
                                       exponent: rg_inter_stage.exponent,
                                       rounding_mode : rounding_mode};
    endrule

    //*****************ITERATION :26 ***********************//
    rule rl_final_stage (rg_state==fromInteger(fPMAN3-1));
        let lv_remainder = rg_inter_stage.remainder;               //Getting remainder value for iteration
        Bit#(fpman3) lv_root = rg_inter_stage.root;                    //Getting root value for iteration
        let mantissa = rg_inter_stage.mantissa;                    //Getting  shifted mantissa value
        Bit#(fpman3) result_mantissa = rg_inter_stage.result_mantissa; //Getting the result bits
        let result_exponent = rg_inter_stage.exponent;             //Getting the final result exponent value
        Bit#(fpman6) lv_remainder_temp = {lv_remainder[fPMAN3:0],mantissa[eXT-1],mantissa[eXT-2]};
        Bit#(fpman6) lv_root_temp_1 = {1'b0,lv_root[fPMAN3-1:0],1'b1,1'b1};
        Bit#(fpman6) lv_root_temp_2 = {1'b0,lv_root[fPMAN3-1:0],1'b0,1'b1};
        //Determining the remainder
        if (lv_remainder[fPMAN5]==1'b1) begin //When r <0
           lv_remainder = lv_remainder_temp + lv_root_temp_1;  
        end
        else begin
            lv_remainder = lv_remainder_temp - lv_root_temp_2;
        end
        //Determining quotient
        if(lv_remainder[fPMAN5]==1) begin //When r <0
            lv_root = {lv_root[fPMAN3-2:0],1'b0};
        end
        else begin
            lv_root = {lv_root[fPMAN3-2:0],1'b1}; 
        end

        result_mantissa[0]= lv_root[0];
        Bit#(fpman6) lv_root_rem  = {2'b0,lv_root[fPMAN3-1:0],1'b1};
        //**********Restoring the remainder if the remainder<0***********//
        //mantissa = mantissa <<2;
        if (lv_remainder[fPMAN5] == 1'b1) begin
            //lv_remainder = lv_remainder + {3'b0,lv_root[24:0],1'b1};
            lv_remainder = lv_remainder + lv_root_rem;
        end

        //********Carrying out the rounding operation**************//
        Bit#(3) rounding_mode = rg_inter_stage.rounding_mode;
        let fsr = rg_inter_stage.fsr;
        
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
            else if(rounding_mode == 'b111) begin 
            	if(fsr[7:5]== 'b000)	    	// round to nearest, ties to even
			        lv_roundup = lv_guard & (result_mantissa[2] | lv_round | lv_sticky);
		        else if(fsr[7:5] == 'b100)		// round to nearest, ties to max magnitude
			        lv_roundup = lv_guard & (lv_round | lv_sticky | ~lv_sign);
		        else if(fsr[7:5] == 'b011 )	// round up
			        lv_roundup = (lv_guard | lv_round | lv_sticky) & (~lv_sign);
		        else if(fsr[7:5] == 'b010)		// round down		
			        lv_roundup = (lv_guard | lv_round | lv_sticky) & (lv_sign);
            end

        Bit#(TAdd#(fpman3,1)) lv_extended_mantissa = {1'b0,result_mantissa};
		if (lv_roundup==1) begin
			lv_extended_mantissa = lv_extended_mantissa + 'd4;    //If roundup then add 4 as the LSB for final mantissa is 3rd bit 
			if (lv_extended_mantissa[fPMAN3]==1)                        //When mantissa overflows
				result_exponent = result_exponent +1;           	//Increment exponent by 1
        end
        
        //Here most exceptions are taken care of in first stage, so module doesn't perform all iterations
		Exception lv_exception = None;                   //Setting exception variable

        $display("****************************************State = %d", rg_state);
        $display("Remainder =%h", lv_remainder);
        $display("Mantissa = %h",lv_extended_mantissa);
        Bit#(fpexp) exp_out = result_exponent[fPEXP-1:0];
        Bit#(fpman) man_out = lv_extended_mantissa[fPMAN+1:2];
        Bit#(fpinp) final_result = {lv_sign, exp_out, man_out};  //Setting the final result 

        ff_final_out <= tagged Valid Floating_output{
                                      fsr: rg_inter_stage.fsr,
                                      final_result:final_result,
                                      exception : lv_exception};
    
    endrule

    //START METHOD
    //*******************ITERATION :1 *********************************//
    method Action _start(Bit#(fpinp) _operand1, Bit#(32) fsr, Bit#(3) rounding_mode);

        bit lv_is_invalid =0;                                               //Invalid Flag
        Exception lv_exception = None;                                      //Declaring exception

        bit sign = _operand1[fPINP-1];                                           //Assigning sign bit
        // Bit#(8) exponent = _operand1[30:23];                                //Input exponent
        Bit#(fpexp1) exponent = {1'b0, _operand1[fPINP-2:fPMAN]};							//Input exponent
        Bit#(ext_fpman) mantissa = '0;
        Bit#(fpman) lv_mantissa = _operand1[fPMAN-1:0];
        Bit#(TAdd#(fpman3,1)) man4_zeros = '0;
        if(exponent == '0 && lv_mantissa != '0) begin						//Subnormal input
        	exponent = exponent + 1;// a tweak to make exponent -126 since 8'b0000000 represents -127 which is not the real exponent of subnormal numbers
        	mantissa = {1'b0,1'b0,lv_mantissa,man4_zeros};
        end
        else
        	mantissa = {1'b0,1'b1,lv_mantissa,man4_zeros};              //Extend mantissa to 48 bits as we need 24 bit output mantissa (Each iteartion use 2 bits of the opearand)
        
     //   $display("sign = %b exponent = %b mantissa = %b.%b", sign, exponent, mantissa[eXT-1], _operand1[fPMAN-1:0]);
        // Int#(9) actual_exponent = unpack(exponent - 'b001111111);
       // $display("actual_exponent = %0d", actual_exponent);
        
        /******************subnormal support*********************/
        Bit#(ext_fplog) lv_leading_zeros = pack(countZerosMSB(mantissa));
        mantissa = mantissa << (lv_leading_zeros - 1);
        exponent = exponent - (zeroExtend(lv_leading_zeros) - 1);           //possibility for a proviso problem

        if (exponent[0]==0)                                                 //If the exponent is even
                mantissa = mantissa <<1;                                    //Mantissa is left shifted so that Exponent-127 is even
        
        Bit#(fpman6) lv_remainder = '0;                                          //Declaring local remainder variable
        Bit#(fpexp) bias = {1'b0,'1};
        Bit#(fpexp) exp_all_ones = '1;
        Bit#(fpexp) exp_all_zeros = '0;
        Bit#(fpman) man_all_zeros = '0;
        Bit#(fpman) man_all_ones  = '1;
        Bit#(fpman3) lv_root = '0;                                               //Declaring local root/quotient variable
        Bit#(fpman3) result_mantissa = 0;                                       //Will store the square root answer 
        
        // Bit#(8) result_exponent = (exponent >>1) +'d63 + zeroExtend(exponent[0]);  //Calculating the result exponent
        Bit#(fpexp1) result_exponent = (exponent >> 1) + (zeroExtend((bias-1)>>1)) + zeroExtend(exponent[0]);  //Calculating the result exponent
        $display("Result_exponent %h bias %d exponent >> 1 %h exponent[0] %h",result_exponent,(bias-1) >> 1,exponent >> 1, exponent[0]);
        //Determining remainder
        if (lv_remainder[fPMAN5]==1) begin //When r <0
            lv_remainder = {lv_remainder[fPMAN3:0],mantissa[eXT-1],mantissa[eXT-2]} + {1'b0,lv_root[fPMAN3-1:0],1'b1,1'b1};  
        end
        else begin
            lv_remainder = {lv_remainder[fPMAN3:0],mantissa[eXT-1],mantissa[eXT-2]} - {1'b0,lv_root[fPMAN3-1:0],1'b0,1'b1};
        end
        $display("lv_remainder: %h",lv_remainder);

        //Determining quotient
        if (lv_remainder[fPMAN5]==1) begin //When r <0
            lv_root = {lv_root[fPMAN+1:0],1'b0};
        end
        else begin
            lv_root = {lv_root[fPMAN+1:0],1'b1}; 
        end
    
        result_mantissa[0] = lv_root [0];       //Setting the LSB of the result
        mantissa = mantissa << 2;                //Shifting the mantissa to get the next 2 bits of next iteration
        result_mantissa = result_mantissa << 1;  //Shifting the result mantissa to make space for the next bit
        
        Bit#(1) lv_exp1_is_all_ones= _operand1[fPINP-2:fPMAN]== exp_all_ones ? 1:0;    //1 if all the bits of exponent are set; used to check if op1 is infinity or NaN
        Bit#(1) lv_exp1_is_zero= _operand1[fPINP-2:fPMAN]== exp_all_zeros ? 1:0;                  //1 if exponent of operand1 is 0
        Bit#(1) lv_man1_is_zero= _operand1[fPMAN-1:0] == man_all_zeros ? 1:0;                 //1 if mantissa of op1 is 0
        Bit#(1) lv_op1_is_zero= lv_man1_is_zero & lv_exp1_is_zero;          //1 when operand1=0
        Bit#(TSub#(fpman,1)) man1_all_zeros = '0;
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
        
        
        if (lv_inv == 1 || (sign == 1 && lv_zero == 0)) begin                                                       // when the input is NAN or Negative => Invalid flag is raised
            lv_exception = Invalid;
            ff_final_out <= tagged Valid Floating_output{   final_result:{1'b0, exp_all_ones , {1'b1,man1_all_zeros}},    //Quite Nan
                                                fsr:{fsr[31:10],1'b0,1'b0,fsr[7:5],2'b0,1'b0,1'b0,1'b0},            //{fsr[31:10],1'b0,lv_is_zero[1],fsr[7:5],2'b0,lv_overflow,lv_underflow,lv_final_inexact}
                                                exception : lv_exception};
        end                
        else if(lv_inf == 1) begin                                                                          
            ff_final_out <= tagged Valid Floating_output{   final_result:{1'b0, exp_all_ones , man_all_zeros},                      //Infinity
                                                fsr:{fsr[31:10],1'b0,1'b0,fsr[7:5],2'b0,1'b0,1'b0,1'b0},            //{fsr[31:10],1'b0,lv_is_zero[1],fsr[7:5],2'b0,lv_overflow,lv_underflow,lv_final_inexact}
                                                exception : lv_exception};
        end
        else if (lv_zero == 1) begin 
            ff_final_out <= tagged Valid Floating_output{   final_result:{sign, exp_all_zeros,man_all_zeros},                             //Zeros
                                                fsr:{fsr[31:10],1'b0,1'b1,fsr[7:5],2'b0,1'b0,1'b0,1'b0},            //{fsr[31:10],1'b0,lv_is_zero[1],fsr[7:5],2'b0,lv_overflow,lv_underflow,lv_final_inexact}
                                                exception : lv_exception};
        end   
        else begin
            //State counter incremented only when it does not meet any above exceptional cases
            rg_state <= rg_state+1;  //Increment the State_counter for next iteration
        end

        $display("****************************************State = %0d", rg_state);
        $display("Remainder = %b", lv_remainder);
        $display("Mantissa = %b",result_mantissa);

        //Storing required data in FIFO stage1 for next iteration
        rg_inter_stage <= Stage_data{    mantissa : mantissa,
                                      fsr : fsr,
                                      result_mantissa : result_mantissa,
                                      root : lv_root,
                                      remainder : lv_remainder,
                                      sign : sign,
                                      exponent : result_exponent,
                                      rounding_mode : rounding_mode };
    endmethod

    method Action deque_buffer();
        rg_state <=0;
        ff_final_out <= tagged Invalid;
    endmethod
    	
    method Maybe#(Floating_output#(fpinp)) get_result();
		return ff_final_out;
    endmethod
    

endmodule




// //*************Test bench******************//
//(*synthesize*)
module mkTb_fpu_sqrt(Empty);

	Reg#(Bit#(32)) rg_clock <-mkReg(0);
    Reg#(Bit#(32)) rg__operand1ut1 <- mkReg(32'h76af0cb2);
    //Reg#(Bit#(64)) rg__operand1ut1 <- mkReg(64'h019000000000000);

	Ifc_fpu_sqrt#(32,23,8) square_root <- mkfpu_sqrt;

	rule rl_clock;
        rg_clock<=rg_clock+1;
        if(rg_clock=='d60) begin
    	    $finish(0);
        end
	endrule
        
	rule give__operand1ut(rg_clock==2);
        $display("Giving input %h at %0d", rg__operand1ut1, rg_clock,$time);
		square_root._start(rg__operand1ut1,{'b0,3'b011,5'b0}, 3'b011); 
    endrule

	rule get_output(square_root.get_result matches tagged Valid .lv_output);
        $display("taking output at %0d", rg_clock);
		$display("Output= %h" , lv_output.final_result,$time);
		square_root.deque_buffer();
	endrule
    
endmodule
        

module mkTb_fpu_sqrt_2 (Empty);
    
    	RegFile #(Bit #(10), Bit #(36))  input_data <- mkRegFileFullLoad("./testcases/fpgen_testcases/Sqrt_testcases.hex");
	Reg #(Bit #(10)) index <- mkReg(0);
	Reg #(Bit #(32)) state_clock <- mkReg(1);
    Reg #(Bit #(32)) rg_state <- mkReg(0);
 	/*****************Module Instantiation******************************/
	Ifc_fpu_sqrt#(32,23,8) sqrt <- mkfpu_sqrt;
	/******************File Creation************************************/
	Reg#(int) cnt <- mkReg(0);                  //File Creation counter
	let fh <- mkReg(InvalidFile) ;				//File Handler
	rule open (cnt == 0 ) ;
		File tb_sqrt_output <- $fopen("tb_sqrt_output.hex", "w+"); 
		fh <= tb_sqrt_output;
		cnt <= 1 ;
	endrule
	/*******************input******************************************/
	rule take_input_in (rg_state == 0);
		sqrt._start(input_data.sub(index)[35:4],{'b0,input_data.sub(index)[2:0],5'b0}, input_data.sub(index)[2:0]);
		index <= index + 1;
        rg_state <= 1;
	endrule

	/*******************output*****************************************/
	rule display_output (rg_state == 1 &&& sqrt.get_result matches tagged Valid .abc);
		$fwrite(fh, "%h\n", abc.final_result[31:0]);
        rg_state <= 0;
        sqrt.deque_buffer();
	endrule

	/******************end testing*************************************/
	rule end_testing (index == 65);
		$finish();
	endrule


endmodule


endpackage
