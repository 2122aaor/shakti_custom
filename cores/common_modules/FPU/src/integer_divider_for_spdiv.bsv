package integer_divider_for_spdiv;


interface Ifc_integer_divider_for_spfdiv;
       	/* Input Methods */
	method Action _inputs(Bit#(27) _denominator, Bit#(27) _numerator);
	//method Action _remove_last_entry();

        /* Output Methods */
	method Bit#(26) output_quotient();
        method Bit#(28) output_remainder();

endinterface:Ifc_integer_divider_for_spfdiv

(*synthesize*)
module mkinteger_divider_for_spfdiv(Ifc_integer_divider_for_spfdiv);
Reg#(Bit#(27)) numerator <- mkReg(0);
Reg#(Bit#(27)) divisor <-mkReg(0);
Reg#(Bit#(28)) remainder <-mkReg(0);
Reg#(Bit#(28)) finalRem <-mkReg(0);
Reg#(Bit#(5)) count <-mkReg(-1);
//Int#(5) count;//=28;
Reg#(Bit#(1)) check <- mkReg(0);

 rule rl_ff_stage(count>='d0 && count<'d27);
      let x;
      //$display("Remainder(P): %b Dividend(A):%b",rem,num);
      if(remainder[27]==1'b0) begin
          x = ({remainder,numerator}<<1)-{1'b0,divisor,27'b0} ;
          //$display("After  - ");
      end
      else begin
          x= {{remainder,numerator}<<1}+{1'b0,divisor,27'b0} ;
          //$display("After  +");
       end
      remainder<=x[54:27];
      numerator<={x[26:1],~x[54]};
      count<=count+1;
endrule:rl_ff_stage

rule rl_ff_stage1(count==5'd27);
   //finalRem<={1'b0,rem}+{2'b00,denom};
   //$display("Remainder=%b & Final remainder:%b",rem,finalRem);
    if(remainder[27]==1'b1) begin
    //$display("one");
    finalRem<=remainder+{1'b0,divisor};
    end
    else begin finalRem<=remainder;
    end
   // $display("Im in");
    // count<=count+1;
   check<=1;
endrule:rl_ff_stage1

method Action _inputs(Bit#(27) _denominator, Bit#(27) _numerator);
        divisor<=_denominator;
        numerator <=_numerator;
	$display("Nr: %b \nDr: %b",_numerator,_denominator);
        count<=0;
endmethod

method Bit#(26) output_quotient()if(check==1'b1);
        //$display("\nRemainder: %b",finalRem);
	return numerator[25:0];
endmethod
method Bit#(28) output_remainder();
 /* let temp={rem}+{1'b0,denom};
  if(rem[27]=='b1) temp=rem;
     //$display("final Remainder=%d",rem);*/
   return finalRem;


endmethod

endmodule:mkinteger_divider_for_spfdiv



		/* 	TEST BENCH 	*/

(*synthesize*)
module mkTbinteger_divider_for_spfdiv (Empty);

Reg#(Bit#(32)) rg_clock <-mkReg(0);

Ifc_integer_divider_for_spfdiv instance_divider <-mkinteger_divider_for_spfdiv();

rule rl_count_clock ;
	rg_clock<=rg_clock+1;
	$display("Clock=%d",rg_clock);
	if(rg_clock=='d32)
		$finish(0);
endrule:rl_count_clock

rule rl_input1(rg_clock==1);
	instance_divider._inputs(27'b010010000101011011010100001, 27'b010000100110010110011110011); //res=0.10100101110010011000001
	 //instance_divider._inputs(27'b000001000000000000000000000, 27'b000110000000000000000000000); //res=1.00100110110010011011001
	//instance_divider._inputs(27'h6000000,27'h0000000); //res=1.0101...
	//instance_divider._inputs(4'h4000000,4'h6000000);
	//instance_divider._inputs(4'b100110000000000000000000000, 4'b100001000000000000000000000);
	//instance_divider._inputs(4'b100000000000000000000000000, 4'b110000000000000000000000000);

endrule:rl_input1

/*rule rl_input2(rg_clock==2);
	instance_divider._inputs(4'd8,4'd512);
endrule:rl_input2
*/
rule rl_check;
	//instance_divider._remove_last_entry();
	$display("Quotient=%b \nRemainder=%b",instance_divider.output_quotient,instance_divider.output_remainder);
endrule:rl_check
endmodule:mkTbinteger_divider_for_spfdiv
endpackage:integer_divider_for_spdiv
