/*

Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



Module Name     : Integer Divider for Single Precision Floating Point Divider
Author Name     : Arjun C. Menon, Aditya Govardhan, Vinod.G
Email ID        : c.arjunmenon@gmail.com, dtgovardhan@gmail.com, g.vinod1993@gmail.com
Last updated on : 30th May, 2016

*/

package integer_divider_for_spdiv;


import FIFO :: *;
import SpecialFIFOs :: *;

(* noinline *)
function Bit#(83) fn_divide_step (Bit#(27) _divisor, Bit#(29) _remainder, Bit#(27) _dividend, Bool final_stage);
	if(final_stage == False) begin
		Bit#(56) accumulator = 0;

	    if(_remainder[28]==1'b0) begin
			accumulator = ({_remainder,_dividend}<<1) - {1'b0,_divisor,1'b0,27'b0} ;
			accumulator[0] = 1'b1;
		end
		else begin
			accumulator = ({_remainder,_dividend}<<1) + {1'b0,_divisor,1'b0,27'b0} ;
			accumulator[0] = 1'b0;
		end
		_remainder = accumulator[55:27];
		_dividend = accumulator[26:0];
	end
	else begin
	    _dividend = _dividend - (_dividend ^ ('d-1));

	    if(_remainder[28] == 1'b1) begin
		    _remainder = _remainder + {1'b0,_divisor,1'b0};
		    _dividend = _dividend - 1;
	    end
	end
	return {_divisor, _remainder, _dividend};
endfunction

interface Ifc_integer_divider_for_spdiv;

	method Action _inputs(Bit#(27) _denominator, Bit#(27) _numerator);
	method Bit#(27) output_quotient();
	method Bit#(28) output_remainder();
	method Action _remove_last_entry();

endinterface

(* synthesize *)
module mkinteger_divider_for_spdiv(Ifc_integer_divider_for_spdiv);

	FIFO#(Bit#(83)) ff_stage1 <-mkSizedFIFO(1);         //SizedFIFO is used since PipelineFIFO causes compilation error due to dependency between two rules
	FIFO#(Bit#(83)) ff_stage2 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage3 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage4 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage5 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage6 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage7 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage8 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage9 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage10 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage11 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage12 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage13 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage14 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage15 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage16 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage17 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage18 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage19 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage20 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage21 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage22 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage23 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage24 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage25 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage26 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage27 <-mkPipelineFIFO();
	FIFO#(Bit#(83)) ff_stage28 <-mkPipelineFIFO();

	rule rl_ff_stage_2;
		ff_stage2.enq(fn_divide_step(ff_stage1.first()[82:56],ff_stage1.first()[55:27],ff_stage1.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage1.first()[55:29],ff_stage1.first()[28:0]);
		ff_stage1.deq();
	endrule:rl_ff_stage_2

	rule rl_ff_stage_3;
		ff_stage3.enq(fn_divide_step(ff_stage2.first()[82:56],ff_stage2.first()[55:27],ff_stage2.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage2.first()[55:29],ff_stage2.first()[28:0]);
		ff_stage2.deq();
	endrule:rl_ff_stage_3

	rule rl_ff_stage_4;
		ff_stage4.enq(fn_divide_step(ff_stage3.first()[82:56],ff_stage3.first()[55:27],ff_stage3.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage3.first()[55:29],ff_stage3.first()[28:0]);
		ff_stage3.deq();
	endrule:rl_ff_stage_4

	rule rl_ff_stage_5;
		ff_stage5.enq(fn_divide_step(ff_stage4.first()[82:56],ff_stage4.first()[55:27],ff_stage4.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage4.first()[55:29],ff_stage4.first()[28:0]);
		ff_stage4.deq();
	endrule:rl_ff_stage_5

	rule rl_ff_stage_6;
		ff_stage6.enq(fn_divide_step(ff_stage5.first()[82:56],ff_stage5.first()[55:27],ff_stage5.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage5.first()[55:29],ff_stage5.first()[28:0]);
		ff_stage5.deq();
	endrule:rl_ff_stage_6

	rule rl_ff_stage_7;
		ff_stage7.enq(fn_divide_step(ff_stage6.first()[82:56],ff_stage6.first()[55:27],ff_stage6.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage6.first()[55:29],ff_stage6.first()[28:0]);
		ff_stage6.deq();
	endrule:rl_ff_stage_7

	rule rl_ff_stage_8;
		ff_stage8.enq(fn_divide_step(ff_stage7.first()[82:56],ff_stage7.first()[55:27],ff_stage7.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage7.first()[55:29],ff_stage7.first()[28:0]);
		ff_stage7.deq();
	endrule:rl_ff_stage_8

	rule rl_ff_stage_9;
		ff_stage9.enq(fn_divide_step(ff_stage8.first()[82:56],ff_stage8.first()[55:27],ff_stage8.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage8.first()[55:29],ff_stage8.first()[28:0]);
		ff_stage8.deq();
	endrule:rl_ff_stage_9

	rule rl_ff_stage_10;
		ff_stage10.enq(fn_divide_step(ff_stage9.first()[82:56],ff_stage9.first()[55:27],ff_stage9.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage9.first()[55:29],ff_stage9.first()[28:0]);
		ff_stage9.deq();
	endrule:rl_ff_stage_10

	rule rl_ff_stage_11;
		ff_stage11.enq(fn_divide_step(ff_stage10.first()[82:56],ff_stage10.first()[55:27],ff_stage10.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage10.first()[55:29],ff_stage10.first()[28:0]);
		ff_stage10.deq();
	endrule:rl_ff_stage_11

	rule rl_ff_stage_12;
		ff_stage12.enq(fn_divide_step(ff_stage11.first()[82:56],ff_stage11.first()[55:27],ff_stage11.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage11.first()[55:29],ff_stage11.first()[28:0]);
		ff_stage11.deq();
	endrule:rl_ff_stage_12

	rule rl_ff_stage_13;
		ff_stage13.enq(fn_divide_step(ff_stage12.first()[82:56],ff_stage12.first()[55:27],ff_stage12.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage12.first()[55:29],ff_stage12.first()[28:0]);
		ff_stage12.deq();
	endrule:rl_ff_stage_13

	rule rl_ff_stage_14;
		ff_stage14.enq(fn_divide_step(ff_stage13.first()[82:56],ff_stage13.first()[55:27],ff_stage13.first()[26:0], False));
	    //$display("quotient: %b\n rem: %b",ff_stage13.first()[55:29],ff_stage13.first()[28:0]);
		ff_stage13.deq();
	endrule:rl_ff_stage_14

	rule rl_ff_stage_15;
		ff_stage15.enq(fn_divide_step(ff_stage14.first()[82:56],ff_stage14.first()[55:27],ff_stage14.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage14.first()[55:29],ff_stage14.first()[28:0]);
		ff_stage14.deq();
	endrule:rl_ff_stage_15

	rule rl_ff_stage_16;
		ff_stage16.enq(fn_divide_step(ff_stage15.first()[82:56],ff_stage15.first()[55:27],ff_stage15.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage15.first()[55:29],ff_stage15.first()[28:0]);
		ff_stage15.deq();
	endrule:rl_ff_stage_16

	rule rl_ff_stage_17;
		ff_stage17.enq(fn_divide_step(ff_stage16.first()[82:56],ff_stage16.first()[55:27],ff_stage16.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage1.first()[55:29],ff_stage1.first()[28:0]);
		ff_stage16.deq();
	endrule:rl_ff_stage_17

	rule rl_ff_stage_18;
		ff_stage18.enq(fn_divide_step(ff_stage17.first()[82:56],ff_stage17.first()[55:27],ff_stage17.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage2.first()[55:29],ff_stage2.first()[28:0]);
		ff_stage17.deq();
	endrule:rl_ff_stage_18

	rule rl_ff_stage_19;
		ff_stage19.enq(fn_divide_step(ff_stage18.first()[82:56],ff_stage18.first()[55:27],ff_stage18.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage3.first()[55:29],ff_stage3.first()[28:0]);
		ff_stage18.deq();
	endrule:rl_ff_stage_19

	rule rl_ff_stage_20;
		ff_stage20.enq(fn_divide_step(ff_stage19.first()[82:56],ff_stage19.first()[55:27],ff_stage19.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage4.first()[55:29],ff_stage4.first()[28:0]);
		ff_stage19.deq();
	endrule:rl_ff_stage_20

	rule rl_ff_stage_21;
		ff_stage21.enq(fn_divide_step(ff_stage20.first()[82:56],ff_stage20.first()[55:27],ff_stage20.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage5.first()[55:29],ff_stage5.first()[28:0]);
		ff_stage20.deq();
	endrule:rl_ff_stage_21

	rule rl_ff_stage_22;
		ff_stage22.enq(fn_divide_step(ff_stage21.first()[82:56],ff_stage21.first()[55:27],ff_stage21.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage6.first()[55:29],ff_stage6.first()[28:0]);
		ff_stage21.deq();
	endrule:rl_ff_stage_22

	rule rl_ff_stage_23;
		ff_stage23.enq(fn_divide_step(ff_stage22.first()[82:56],ff_stage22.first()[55:27],ff_stage22.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage7.first()[55:29],ff_stage7.first()[28:0]);
		ff_stage22.deq();
	endrule:rl_ff_stage_23

	rule rl_ff_stage_24;
		ff_stage24.enq(fn_divide_step(ff_stage23.first()[82:56],ff_stage23.first()[55:27],ff_stage23.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage8.first()[55:29],ff_stage8.first()[28:0]);
		ff_stage23.deq();
	endrule:rl_ff_stage_24

	rule rl_ff_stage_25;
		ff_stage25.enq(fn_divide_step(ff_stage24.first()[82:56],ff_stage24.first()[55:27],ff_stage24.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage9.first()[55:29],ff_stage9.first()[28:0]);
		ff_stage24.deq();
	endrule:rl_ff_stage_25

	rule rl_ff_stage_26;
		ff_stage26.enq(fn_divide_step(ff_stage25.first()[82:56],ff_stage25.first()[55:27],ff_stage25.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage10.first()[55:29],ff_stage10.first()[28:0]);
		ff_stage25.deq();
	endrule:rl_ff_stage_26

	rule rl_ff_stage_27;
		ff_stage27.enq(fn_divide_step(ff_stage26.first()[82:56],ff_stage26.first()[55:27],ff_stage26.first()[26:0], False));
		//$display("quotient: %b\n rem: %b",ff_stage11.first()[55:29],ff_stage11.first()[28:0]);
		ff_stage26.deq();
	endrule:rl_ff_stage_27

	rule rl_ff_stage_28;
		ff_stage28.enq(fn_divide_step(ff_stage27.first()[82:56],ff_stage27.first()[55:27],ff_stage27.first()[26:0], True));
		//$display("quotient: %b\n rem: %b",ff_stage11.first()[55:29],ff_stage11.first()[28:0]);
		ff_stage27.deq();
	endrule:rl_ff_stage_28

	// rule rl_ff_stage_29;
	// 	ff_stage29.enq(fn_divide_step(ff_stage28.first()[82:56],ff_stage28.first()[55:27],ff_stage28.first()[26:0], True));
	// 	//$display("quotient: %b\n rem: %b",ff_stage11.first()[55:29],ff_stage11.first()[28:0]);
	// 	ff_stage28.deq();
	// endrule:rl_ff_stage_29

	method Action _inputs(Bit#(27) _denominator, Bit#(27) _numerator);
		ff_stage1.enq(fn_divide_step(_denominator, {2'b0,_numerator}, 27'b0, False));
	endmethod

	method Bit#(27) output_quotient();
        //return dividend;
        return ff_stage28.first()[26:0];
    endmethod

    method Bit#(28) output_remainder();
        //return finalRem[28:1];
        return ff_stage28.first()[55:28];
    endmethod

	method Action _remove_last_entry();
		ff_stage28.deq();
	endmethod

endmodule

// (*synthesize*)
// module mkTb (Empty);

//     Reg#(Bit#(32)) rg_clock <-mkReg(0);

//     Ifc_integer_divider_for_spdiv instance_divider <-mkinteger_divider_for_spdiv();

//     rule rl_count_clock ;
//       	rg_clock<=rg_clock+1;
//       	if(rg_clock=='d45) $finish(0);
//     endrule

//     rule rl_input1(rg_clock==1);
//         $display("giving inputs at %0d", rg_clock);
//         instance_divider._inputs(27'd7, 27'd36 ); // divisor, dividend 
//     endrule

//     rule rl_finish;
//         $display("Quotient=%b Remainder=%b at %0d",instance_divider.output_quotient,instance_divider.output_remainder, rg_clock);
//         instance_divider._remove_last_entry();
//     endrule

// endmodule

endpackage
