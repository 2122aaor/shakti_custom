package muldiv;
  `include "defined_parameters.bsv"
  import defined_types::*;
  import DReg::*;

  function Bit#(TAdd#(1,TMul#(2,`Reg_width))) restoring_step(Bit#(TAdd#(1,TMul#(2,`Reg_width))) remainder, Bit#(`Reg_width) divisor);
    for (Integer i=0;i<`Loop;i=i+1)begin
      remainder=remainder<<1;
//      Int#(TAdd#(1,`Reg_width)) upper=unpack(remainder[2*`Reg_width:`Reg_width]); // convert to signed number
      Bit#(33) sub=remainder[64:32]+signExtend(~divisor+1);
      if(remainder[63:32]>=divisor)begin // if subtraction is positive
				remainder[0]=1;
				remainder[2*`Reg_width:`Reg_width]=sub;
      end
    end
    return remainder;
  endfunction

  interface Ifc_muldiv;
    method ActionValue#(Maybe#(Bit#(`Reg_width))) _start(Bit#(`Reg_width) inp1, Bit#(`Reg_width) inp2, Bit#(3) funct, Bit#(1) word32); //_div_name 00 : DIV/REM 01: DIVU/REMU
  endinterface


  (*synthesize*)
  module mkmuldiv(Ifc_muldiv);
    Reg#(Bit#(TAdd#(1,(TMul#(`Reg_width,2))))) partial_prod <-mkReg(0);
    Reg#(Bit#(TAdd#(1,TLog#(`Reg_width)))) rg_state_counter <-mkDReg(0);//Register for state machine counter
    
    method ActionValue#(Maybe#(Bit#(`Reg_width))) _start(Bit#(`Reg_width) inp1, Bit#(`Reg_width) inp2, Bit#(3) funct, Bit#(1) word32); //_div_name 00 : DIV/REM 01: DIVU/REMU
      Bool lv_take_compl=False;
      Bit#(1) mul_or_div = funct[2];  // 0 = multiplication 1 = division
      Bit#(1) rs1_signed=(funct[2]==0)?funct[0]^funct[1]:~funct[0];
      Bit#(1) rs2_signed=(funct[2]==0)?~funct[1]&funct[0]:~funct[0];
      Bit#(TAdd#(`Reg_width,1)) in1={inp1[31],inp1};
      Bit#(TAdd#(`Reg_width,1)) in2={inp2[31],inp2};
     
				in1=(funct[0]==0 && in1[`Reg_width]==1)?~in1[`Reg_width-1:0]+1:in1[`Reg_width-1:0];
				in2=(funct[0]==0 && in2[`Reg_width]==1)?~in2[`Reg_width-1:0]+1:in2[`Reg_width-1:0];
				if(funct[1:0]==0 && in2!=0 && (in1[`Reg_width]^in2[`Reg_width])==1)
					lv_take_compl=True;

      if(rg_state_counter==0)begin
					partial_prod<=zeroExtend(in1[31:0]);
				rg_state_counter<=rg_state_counter+1;
				return tagged Invalid;
      end
      else begin
					Bit#(65) temp1=restoring_step(partial_prod,(in2[`Reg_width-1:0]));
					Bit#(33) sub=partial_prod[64:32]+signExtend(~in2[31:0]+1);
					$display("Division. Partial :%h Sub: %h Counter: %d",temp1,sub,rg_state_counter);
					if(rg_state_counter==32/`Loop)begin
						rg_state_counter<=0;
						if(funct[1]==1) // REM/REMU
							temp1=signExtend(temp1[63:32]);
						else // DIV/DIVU
							temp1=signExtend(temp1[31:0]);

						if(funct[1]==0 && lv_take_compl)// DIVU
							temp1=~temp1+1;
						else if(funct[1:0]=='b10 && temp1[63]!=in1[`Reg_width])  // REMU/REM
							temp1=~temp1+1;
						return tagged Valid temp1[31:0];	    
					end
					else begin
						partial_prod<=temp1;
						rg_state_counter<=rg_state_counter+1;
						return tagged Invalid;
					end
      end
    endmethod
  endmodule
endpackage
