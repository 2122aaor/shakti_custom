package fpu_int_to_fp;

import defined_types ::*;

interface Ifc_fpu_int_to_fp#(numeric type szint, numeric type fpinp, numeric type fpman, numeric type fpexp);
    method Action _start(Bit#(szint) inp_int, Bit#(1) unsigned_bit, Bit#(3) rounding_mode);
    method Floating_output#(fpinp) result_();
endinterface

module mkfpu_int_to_fp(Ifc_fpu_int_to_fp#(szint, fpinp,fpman,fpexp))
    provisos (
              Add#(fpman,2,fpman2),
              Add#(a__, TLog#(szint), fpexp),
              Add#(1, b__, fpman2),
              Add#(1, TAdd#(fpexp, fpman), fpinp),
              Add#(c__, TLog#(TAdd#(1, szint)), fpexp)
             );

Wire#(Floating_output#(fpinp)) wr_final_out <-mkWire();	// output FIFO
let sZINT = valueOf(szint);
let fPINP = valueOf(fpinp);
let fPMAN = valueOf(fpman);
let fPEXP = valueOf(fpexp);

method Action _start(Bit#(szint) inp_int, Bit#(1) unsigned_bit, Bit#(3) rounding_mode);

Bit#(TSub#(fpexp,1)) bias  = '1;
Bit#(fpexp) lv_exponent    =  zeroExtend(bias) + fromInteger(sZINT-1);
bit sign                   =  inp_int[sZINT-1];
Bit#(szint) lv_integer_num = inp_int;
Bit#(fpinp) final_out      = '0;
Bit#(fpexp) exp_all_zeros  = '0;
Bit#(fpman) man_all_zeros  = '0;
Bit#(fpexp) exp_all_ones   = '1;
Bit#(fpman) man_all_ones   = '1;
Bit#(5) fflags=0;
if(unsigned_bit == 0 && sign == 1) 
     lv_integer_num = ~inp_int + 1;

let lv_zeros = pack(countZerosMSB(lv_integer_num));
lv_integer_num = lv_integer_num << lv_zeros;

     
if(inp_int == '0)
    final_out = '0;
else if(lv_zeros == fromInteger(sZINT))
    final_out = {sign,exp_all_zeros,man_all_zeros};
else begin
    bit guard = lv_integer_num[fPINP-fPMAN-2];
    bit round = lv_integer_num[fPINP-fPMAN-3];
    bit sticky = 0;
    Bit#(TSub#(fpinp,TSub#(fpman,4))) lv_sticky_num_zeros = '0;

    if(lv_integer_num[fPINP-fPMAN-4:0] != lv_sticky_num_zeros)
        sticky = 1;

    bit lv_inexact = (guard | round | sticky);
    bit lv_roundup = 0;

    if(rounding_mode == 'b000) 
		lv_roundup = guard & (lv_integer_num[fPINP-fPMAN-1] | round | sticky);
	else if (rounding_mode == 'b100)
		lv_roundup = guard & (round | sticky | ~sign);
	else if (rounding_mode == 'b011)
		lv_roundup = (guard | round | sticky) & (~sign);
	else if (rounding_mode == 'b010)
		lv_roundup = (guard | round | sticky) & (sign);

    Bit#(fpman2) lv_rounded_mantissa;

    if(lv_roundup == 1)
        lv_rounded_mantissa = {1'b0,lv_integer_num[fPINP-1:fPINP-fPMAN-1]} + 1;
    else
        lv_rounded_mantissa = {1'b0,lv_integer_num[fPINP-1:fPINP-fPMAN-1]};

    if(lv_rounded_mantissa[fPMAN+1] == 1) begin
        lv_exponent = lv_exponent + 1;
        lv_rounded_mantissa = lv_rounded_mantissa >> 1;
    end
    
    Bit#(fpman) final_mantissa = truncate(lv_rounded_mantissa);
    lv_exponent = lv_exponent - zeroExtend(lv_zeros);
    final_out = {sign,lv_exponent,final_mantissa};
end

   wr_final_out <= Floating_output {
                    final_result : final_out,
                    fflags : fflags
                   };

endmethod

method Floating_output#(fpinp) result_();
   return wr_final_out;
endmethod

endmodule

module mkTb(Empty);
   Reg#(Bit#(32)) rg_operand1<-mkReg(32'd64); 
   Reg#(Bit#(32)) rg_clock<-mkReg(0); 
   Ifc_fpu_int_to_fp#(32,32,23,8) itof <- mkfpu_int_to_fp();
   Reg#(Bit#(32)) rg_arbit <-mkReg(0);

   rule rl_clk_count;
      rg_clock<=rg_clock+1;
   endrule

   rule rl_start_1(rg_clock=='d0);
       $display("Giving inputs rg_operand 1 : %h through testbench",rg_operand1,$time);
      itof._start(rg_operand1,1'b0,3'b000);
   endrule

   rule rl_display_result;
           let abc = itof.result_();
       $display("Final result= %h", abc.final_result,$time);
       $finish(0);
   endrule
endmodule
endpackage
