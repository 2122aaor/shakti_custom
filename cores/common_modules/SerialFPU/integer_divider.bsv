/*

Copyright (c) 2013-2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name     : Integer Divider for Single Precision Floating Point Divider
Author Name     : Arjun C. Menon, Vinod.G, Aditya Govardhan
Email ID        : c.arjunmenon@gmail.com, g.vinod1993@gmail.com, dtgovardhan@gmail.com 
Last updated on : 12th July, 2016

*/

package integer_divider;

//(* noinline *)
function Bit#(55) fn_divide_neel (Bit#(55) remainder, Bit#(27) divisor);
  if(remainder[54]==1) begin // P is negative
    remainder[54:27]=remainder[54:27]+{1'b0,divisor};
    remainder=remainder<<1;
  end
  else begin
    remainder[54:27]=remainder[54:27]-{1'b0,divisor};
    remainder=remainder<<1;
    remainder[0]=1;
  end
  return remainder;
endfunction
function Bit#(op_fpman) fn_divide_step (Bit#(op_fpman) packed_div, Bit#(1) final_stage, Bit#(1) is_even)
    provisos(
             Div#(TSub#(op_fpman,2),3,fpman4),
             Add#(fpman4,2,fpman6),
             Add#(fpman4,fpman6,acc_bits),
             //per request of bsc
             Add#(fpman4,acc_bits,op_fpman)  
                    // fpman4 = 27
                    // fpman6 = 29
                    // opfpman = 83
                    // acc_btis = 56

            ); 
         let fPMAN4 = valueOf(fpman4);
         let aCC    = valueOf(acc_bits);
         let oP_FPMAN = valueOf(op_fpman);

         Bit#(fpman4) all_zeros = '0;
         Bit#(fpman4) _divisor   = packed_div[oP_FPMAN-1:aCC];
         Bit#(fpman6) _remainder = packed_div[aCC-1:fPMAN4];
         Bit#(fpman4) _dividend  = packed_div[fPMAN4-1:0];
         Bit#(acc_bits) accumulator = 0;

    for(Integer i = 0 ; i <=1  ; i=i+1) begin
      if(final_stage == 0 || (final_stage == 1 && i == 0)) begin
        if(_remainder[fPMAN4+1]==1'b0) begin
          accumulator = ({_remainder,_dividend}<<1) - {1'b0,_divisor,1'b0,all_zeros} ;
          accumulator[0] = 1'b1;
        end
        else begin
          accumulator = ({_remainder,_dividend}<<1) + {1'b0,_divisor,1'b0,all_zeros} ;
          accumulator[0] = 1'b0;
         end
        _remainder = accumulator[aCC-1:fPMAN4];
        _dividend = accumulator[fPMAN4-1:0];
    	end
    	else begin
        if(is_even == 0) begin
          if(_remainder[fPMAN4+1]==1'b0) begin
			      accumulator = ({_remainder,_dividend}<<1) - {1'b0,_divisor,1'b0,all_zeros} ;
		        accumulator[0] = 1'b1;
	        end
		      else begin
		    	  accumulator = ({_remainder,_dividend}<<1) + {1'b0,_divisor,1'b0,all_zeros} ;
		    	  accumulator[0] = 1'b0;
	        end
	    	  _remainder = accumulator[aCC-1:fPMAN4];
	    	  _dividend = accumulator[fPMAN4-1:0];
        end
        _dividend = _dividend - (_dividend ^ ('1));
        if(_remainder[fPMAN4+1] == 1'b1) begin
		      _remainder = _remainder + {1'b0,_divisor,1'b0};
		      _dividend = _dividend - 1;
	      end
	    end
    end
	return {_divisor, _remainder, _dividend};
endfunction



interface Ifc_integer_divider#(numeric type fpman4);

	method Action _inputs(Bit#(fpman4) _denominator, Bit#(fpman4) _numerator);
	method Bit#(TAdd#(TMul#(fpman4,3),2)) result_();

endinterface

//(* synthesize *)
module mkinteger_divider(Ifc_integer_divider#(fpman4))
      provisos(
               Add#(TMul#(fpman4,3),2,op_fpman),
               //per request of bsc
               Add#(TDiv#(TSub#(op_fpman, 2), 3), 
                    TAdd#(TDiv#(TSub#(op_fpman, 2), 3),
                    TAdd#(TDiv#(TSub#(op_fpman, 2), 3), 2)), op_fpman)
                  
              );
    let fPMAN4 = valueOf(fpman4);

	Reg#(Bit#(op_fpman)) rg_inter_stage <- mkRegU();
	Reg#(Bit#(6)) rg_state <- mkReg(0);
	Wire#((Bit#(op_fpman))) wr_final_out <- mkWire;


	rule stage_1(rg_state == 1);
		rg_state <= rg_state + 1;
        $display("Int Data %h rg_state %d",rg_inter_stage[55:0], rg_state);
		rg_inter_stage <= fn_divide_step(rg_inter_stage,0,0);
	endrule

	rule recursive_stage(rg_state > 1 && rg_state <= ((fromInteger(fPMAN4-5)>>1) +1) );
	rg_state <= rg_state + 1;
    $display($time,"\t Int Data %h rg_state %d", rg_inter_stage[55:0], rg_state);
    rg_inter_stage <=  fn_divide_step(rg_inter_stage,0,0);
	endrule

	rule end_stage(rg_state == ((fromInteger(fPMAN4-5)>>1)+2));
        rg_state <= 0;
        $display($time,"\t End stage Int Data %h rg_state %d fpman4[0] %d", rg_inter_stage[55:0], rg_state, fromInteger(fPMAN4)[0]);
		wr_final_out <= fn_divide_step(rg_inter_stage,1,fromInteger(fPMAN4)[0]);
	endrule

	method Action _inputs(Bit#(fpman4) _denominator, Bit#(fpman4) _numerator)if(rg_state==0);
		rg_state <= rg_state + 1;
        Bit#(fpman4) man_all_zeros = '0;
        Bit#(op_fpman) packed_div = {_denominator,{2'b0,_numerator},man_all_zeros};
    $display("Numerator: %h Denominator: %h",_numerator,_denominator);
		rg_inter_stage <= fn_divide_step(packed_div, 0,0);
	endmethod

	method Bit#(op_fpman) result_();
		return wr_final_out;
	endmethod

endmodule

 module mkTb (Empty);

    Reg#(Bit#(32)) rg_clock <-mkReg(0);
    Reg#(Bit#(55)) rg_remainder <-mkReg(0);
//     Ifc_integer_divider#(27) instance_divider <-mkinteger_divider();

     Ifc_integer_divider#(27) instance_divider <-mkinteger_divider();

     rule rl_input1(rg_clock==1);
      instance_divider._inputs(27'h6d74e20,27'h68a4e18); // divisor, dividend 
     endrule

     rule rl_finish;
      let temp = instance_divider.result_();
 		  $display("Quotient=%h remainder=%h at %0d",temp[26:0],temp[53:27], rg_clock);
     endrule
     
     rule rl_count_clock ;
       	rg_clock<=rg_clock+1;
       	if(rg_clock=='d35) 
          $finish(0);
     endrule

//     rule rl_input2(rg_clock==0);
//      rg_remainder<=fn_divide_neel({28'd0,27'h69a4890},27'h5c81d50); // divisor, dividend 
//     endrule
//
//     rule inter_stage(rg_clock>0);
//       rg_remainder<=fn_divide_neel(rg_remainder,27'h5c81d50);
//       $display("P: %h A: %h Clock: %d",rg_remainder[53:27],rg_remainder[26:0],rg_clock);
//       if(rg_clock==27)begin
//         let quo=rg_remainder[26:0];
//         let rem1=rg_remainder[53:27];
//         quo=quo-(quo ^ ('1));
//         if(rg_remainder[54]==1)
//           rem1=rem1+27'h5c81d50;
//          $display("Quotient: %h (%h) Remainder: %h",quo,rg_remainder[26:0],rem1);
//        end
//     endrule

 endmodule
// (*synthesize*)
// module mkTb (Empty);
//
//    Reg#(Bit#(32)) rg_clock <-mkReg(0);
//
//     Ifc_integer_divider#(27) instance_divider <-mkinteger_divider();
//
//     rule rl_count_clock ;
//       	rg_clock<=rg_clock+1;
//       	if(rg_clock=='d60) $finish(0);
//     endrule
//
//     rule rl_input1(rg_clock==1);
//		$display("giving inputat %0d", rg_clock);
//         instance_divider._inputs(27'd40,27'd800); // divisor, dividend 
//    $display("Expected Quotient: %d",27'd800/27'd40);
//     endrule
//
//     rule rl_finish;
//         let temp = instance_divider.result_();
// 		$display("Quotient=%h remainder=%h at %0d",temp[26:0],temp[53:27], rg_clock);
//     endrule
//
// endmodule

endpackage
