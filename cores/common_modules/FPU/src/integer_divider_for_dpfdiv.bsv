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

Module Name     : Integer Divider used in DP FP division 
Author Name     : Arjun C. Menon
Email Id        : c.arjunmenon@gmail.com
Last updaed on  : 2nd January, 2014.

This module performs the integer division used in the DP FP division calculation. The dividend is 114 bits wide and the divisor is 56 bits.
This division is performed in 28 steps or 28 clock cycles. During every clock cycle, we perform two steps of division by subtraction.
This is performed using the function fn_divide_step. At each cycle the copmuted value is enqueued in the next FIFO in the pipe.

*/

package integer_divider_for_dpfdiv;
import FIFO::*;
import SpecialFIFOs::*;

//function to perform two steps of division by subtraction
(*noinline*)
function Bit#(170) fn_divide_step(Bit#(56) denominator, Bit#(114) quotient_and_remainder);
	let temp=quotient_and_remainder;
    let x= temp[56:0]-{1'b0,denominator};
	if(quotient_and_remainder[56:0]<{1'b0,denominator})         // if Dr > Nr
		temp= {temp[113:58],1'b0,temp[56:0]};     // Quotient bit is 0
	else
		temp = {temp[113:58],1'b1,x};             // Quotient bit is 1 and new remainder is calculated by subtracting denominator from numerator
	temp = temp<<1;                               // shifting {Partial Quotient, Partial Remainder} which append a zero at the LSB of the partial remainder
    
   let y= temp[56:0]-{1'b0,denominator};          // if Dr > Nr
   if(temp[56:0]<{1'b0,denominator})              // Quotient bit is 0
       temp = {temp[113:58],1'b0,temp[56:0]};                                                                                                                
   else                                           // Quotient bit is 1 and new remainder is calculated by subtracting denominator from numerator
       temp = {temp[113:58],1'b1,y};              // shifting {Partial Quotient, Partial Remainder} which append a zero at the LSB of the partial remainder
    temp = temp<<1;
    
	return {denominator,temp};
endfunction

interface Ifc_integer_divider_for_dpfdiv;

		/* Input Methods */
	method Action _inputs(Bit#(56) _denominator, Bit#(56) _numerator);
	method Action _remove_last_entry();
    method Action _set_flush(Bool _flush);

		/* Output Methods */
	method Bit#(170) output_();
endinterface:Ifc_integer_divider_for_dpfdiv

(*synthesize*)
module mkinteger_divider_for_dpfdiv(Ifc_integer_divider_for_dpfdiv);

FIFO#(Bit#(170)) ff_stage1 <-mkFIFO;   //FIFO is used since PipelineFIFO causes compilation error due to dependency between two rules
FIFO#(Bit#(170)) ff_stage2 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage3 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage4 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage5 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage6 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage7 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage8 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage9 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage10 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage11 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage12 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage13 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage14 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage15 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage16 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage17 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage18 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage19 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage20 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage21 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage22 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage23 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage24 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage25 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage26 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage27 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage28 <-mkPipelineFIFO();
Wire#(Bool) wr_flush<- mkDWire(False);			// wire that indicates when to flush the output FIFO in case of a processor pipeline flush by firing the rule rl_flush

rule rl_flush_(wr_flush);				    	//rule that clears the contents of the FIFO in case of a flush
    ff_stage1.clear();
    ff_stage2.clear();
    ff_stage3.clear();
    ff_stage4.clear();
    ff_stage5.clear();
    ff_stage6.clear();
    ff_stage7.clear();
    ff_stage8.clear();
    ff_stage9.clear();
    ff_stage10.clear();
    ff_stage11.clear();
    ff_stage12.clear();
    ff_stage13.clear();
    ff_stage14.clear();
    ff_stage15.clear();
    ff_stage16.clear();
    ff_stage17.clear();
    ff_stage18.clear();
    ff_stage19.clear();
    ff_stage20.clear();
    ff_stage21.clear();
    ff_stage22.clear();
    ff_stage23.clear();
    ff_stage24.clear();
    ff_stage25.clear();
    ff_stage26.clear();
    ff_stage27.clear();
    ff_stage28.clear();
endrule

/* The following set of rules perform the computation. Each rule fires only when the previous FIFO is
filled. and the next FIFO is empty. 
Each rule calls the function fn_divide_step to perform the single step of division.
*/

rule rl_ff_stage_2(!wr_flush);
	ff_stage2.enq(fn_divide_step(ff_stage1.first()[169:114],ff_stage1.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage1.first()[113:56],ff_stage1.first()[57:0]);
	ff_stage1.deq();
endrule:rl_ff_stage_2

rule rl_ff_stage_3(!wr_flush);
	ff_stage3.enq(fn_divide_step(ff_stage2.first()[169:114],ff_stage2.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage2.first()[113:56],ff_stage2.first()[57:0]);
	ff_stage2.deq();
endrule:rl_ff_stage_3

rule rl_ff_stage_4(!wr_flush);
	ff_stage4.enq(fn_divide_step(ff_stage3.first()[169:114],ff_stage3.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage3.first()[113:56],ff_stage3.first()[57:0]);
	ff_stage3.deq();
endrule:rl_ff_stage_4

rule rl_ff_stage_5(!wr_flush);
	ff_stage5.enq(fn_divide_step(ff_stage4.first()[169:114],ff_stage4.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage4.first()[113:56],ff_stage4.first()[57:0]);
	ff_stage4.deq();
endrule:rl_ff_stage_5

rule rl_ff_stage_6(!wr_flush);
	ff_stage6.enq(fn_divide_step(ff_stage5.first()[169:114],ff_stage5.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage5.first()[113:56],ff_stage5.first()[57:0]);
	ff_stage5.deq();
endrule:rl_ff_stage_6

rule rl_ff_stage_7(!wr_flush);
	ff_stage7.enq(fn_divide_step(ff_stage6.first()[169:114],ff_stage6.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage6.first()[113:56],ff_stage6.first()[57:0]);
	ff_stage6.deq();
endrule:rl_ff_stage_7

rule rl_ff_stage_8(!wr_flush);
	ff_stage8.enq(fn_divide_step(ff_stage7.first()[169:114],ff_stage7.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage7.first()[113:56],ff_stage7.first()[57:0]);
	ff_stage7.deq();
endrule:rl_ff_stage_8

rule rl_ff_stage_9(!wr_flush);
	ff_stage9.enq(fn_divide_step(ff_stage8.first()[169:114],ff_stage8.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage8.first()[113:56],ff_stage8.first()[57:0]);
	ff_stage8.deq();
endrule:rl_ff_stage_9

rule rl_ff_stage_10(!wr_flush);
	ff_stage10.enq(fn_divide_step(ff_stage9.first()[169:114],ff_stage9.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage9.first()[113:56],ff_stage9.first()[57:0]);
	ff_stage9.deq();
endrule:rl_ff_stage_10

rule rl_ff_stage_11(!wr_flush);
	ff_stage11.enq(fn_divide_step(ff_stage10.first()[169:114],ff_stage10.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage10.first()[113:56],ff_stage10.first()[57:0]);
	ff_stage10.deq();
endrule:rl_ff_stage_11

rule rl_ff_stage_12(!wr_flush);
	ff_stage12.enq(fn_divide_step(ff_stage11.first()[169:114],ff_stage11.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage11.first()[113:56],ff_stage11.first()[57:0]);
	ff_stage11.deq();
endrule:rl_ff_stage_12

rule rl_ff_stage_13(!wr_flush);
	ff_stage13.enq(fn_divide_step(ff_stage12.first()[169:114],ff_stage12.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage12.first()[113:56],ff_stage12.first()[57:0]);
	ff_stage12.deq();
endrule:rl_ff_stage_13

rule rl_ff_stage_14(!wr_flush);
	ff_stage14.enq(fn_divide_step(ff_stage13.first()[169:114],ff_stage13.first()[113:0]));
    //$display("quotient: %b\n rem: %b",ff_stage13.first()[113:56],ff_stage13.first()[57:0]);
	ff_stage13.deq();
endrule:rl_ff_stage_14

rule rl_ff_stage_15(!wr_flush);
	ff_stage15.enq(fn_divide_step(ff_stage14.first()[169:114],ff_stage14.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage14.first()[113:56],ff_stage14.first()[57:0]);
	ff_stage14.deq();
endrule:rl_ff_stage_15

rule rl_ff_stage_16(!wr_flush);
	ff_stage16.enq(fn_divide_step(ff_stage15.first()[169:114],ff_stage15.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage15.first()[113:56],ff_stage15.first()[57:0]);
	ff_stage15.deq();
endrule:rl_ff_stage_16

rule rl_ff_stage_17(!wr_flush);
	ff_stage17.enq(fn_divide_step(ff_stage16.first()[169:114],ff_stage16.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage16.first()[113:56],ff_stage16.first()[57:0]);
	ff_stage16.deq();
endrule:rl_ff_stage_17

rule rl_ff_stage_18(!wr_flush);
	ff_stage18.enq(fn_divide_step(ff_stage17.first()[169:114],ff_stage17.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage17.first()[113:56],ff_stage17.first()[57:0]);
	ff_stage17.deq();
endrule:rl_ff_stage_18

rule rl_ff_stage_19(!wr_flush);
	ff_stage19.enq(fn_divide_step(ff_stage18.first()[169:114],ff_stage18.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage18.first()[113:56],ff_stage18.first()[57:0]);
	ff_stage18.deq();
endrule:rl_ff_stage_19

rule rl_ff_stage_20(!wr_flush);
	ff_stage20.enq(fn_divide_step(ff_stage19.first()[169:114],ff_stage19.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage19.first()[113:56],ff_stage19.first()[57:0]);
	ff_stage19.deq();
endrule:rl_ff_stage_20

rule rl_ff_stage_21(!wr_flush);
	ff_stage21.enq(fn_divide_step(ff_stage20.first()[169:114],ff_stage20.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage20.first()[113:56],ff_stage20.first()[57:0]);
	ff_stage20.deq();
endrule:rl_ff_stage_21

rule rl_ff_stage_22(!wr_flush);
	ff_stage22.enq(fn_divide_step(ff_stage21.first()[169:114],ff_stage21.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage21.first()[113:56],ff_stage21.first()[57:0]);
	ff_stage21.deq();
endrule:rl_ff_stage_22

rule rl_ff_stage_23(!wr_flush);
	ff_stage23.enq(fn_divide_step(ff_stage22.first()[169:114],ff_stage22.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage22.first()[113:56],ff_stage22.first()[57:0]);
	ff_stage22.deq();
endrule:rl_ff_stage_23

rule rl_ff_stage_24(!wr_flush);
	ff_stage24.enq(fn_divide_step(ff_stage23.first()[169:114],ff_stage23.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage23.first()[113:56],ff_stage23.first()[57:0]);
	ff_stage23.deq();
endrule:rl_ff_stage_24

rule rl_ff_stage_25(!wr_flush);
	ff_stage25.enq(fn_divide_step(ff_stage24.first()[169:114],ff_stage24.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage24.first()[113:56],ff_stage24.first()[57:0]);
	ff_stage24.deq();
endrule:rl_ff_stage_25

rule rl_ff_stage_26(!wr_flush);
	ff_stage26.enq(fn_divide_step(ff_stage25.first()[169:114],ff_stage25.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage25.first()[113:56],ff_stage25.first()[57:0]);
	ff_stage25.deq();
endrule:rl_ff_stage_26

rule rl_ff_stage_27(!wr_flush);
	ff_stage27.enq(fn_divide_step(ff_stage26.first()[169:114],ff_stage26.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage26.first()[113:56],ff_stage26.first()[57:0]);
	ff_stage26.deq();
endrule:rl_ff_stage_27

rule rl_ff_stage_28(!wr_flush);
	ff_stage28.enq(fn_divide_step(ff_stage27.first()[169:114],ff_stage27.first()[113:0]));
	//$display("quotient: %b\n rem: %b",ff_stage27.first()[113:56],ff_stage27.first()[57:0]);
	ff_stage27.deq();
endrule:rl_ff_stage_28

method Action _inputs(Bit#(56) _denominator, Bit#(56) _numerator);
    //$display("integer_divider_for_dpfdiv: Got inputs");
	//$display("Nr: %b \nDr: %b",_numerator,_denominator);
	ff_stage1.enq(fn_divide_step(_denominator,{58'd0,_numerator}));
endmethod

method Bit#(170) output_();
	return ff_stage28.first();
endmethod

method Action _remove_last_entry();
	ff_stage28.deq();
endmethod

// when a wr_flush is initiated in the processor this method will also be called.
// it sets the flsuh wire to True, hence no rule other flush_all_fifos will
// fire and hence clear all the buffer and intermediate register/ wires ...
method Action _set_flush(Bool _flush);
    wr_flush<=_flush;
endmethod

endmodule:mkinteger_divider_for_dpfdiv


		/* 	TEST BENCH 	*/

//(*synthesize*)
module mkTb(Empty);

Reg#(Bit#(32)) rg_clock <-mkReg(0);

Ifc_integer_divider_for_dpfdiv instance_divider <-mkinteger_divider_for_dpfdiv();

rule rl_count_clock ;
	rg_clock<=rg_clock+1;
	$display("Clock=%d",rg_clock);
	if(rg_clock=='d35)
		$finish(0);
endrule:rl_count_clock

rule rl_input1(rg_clock==1);
	instance_divider._inputs({2'b11,'d0},{1'b1,'d0});
endrule:rl_input1

rule rl_check;
	instance_divider._remove_last_entry();
	$display("Quotient=%b \nRemainder=%b",instance_divider.output_[113:58],instance_divider.output_[56:0]);
endrule
endmodule
endpackage:integer_divider_for_dpfdiv
