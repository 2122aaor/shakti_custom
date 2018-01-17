package muldiv;
  `include "defined_parameters.bsv"
  import defined_types::*;
  import DReg::*;

  function Bit#(TAdd#(`Reg_width,1)) return_modified_operands(Bit#(`Reg_width) inp, Bit#(1) is_signed, Bit#(1) is_word);
    let sign = is_signed & (is_word==1?inp[`Reg_width/2-1]:inp[`Reg_width-1]);
    Bit#(32) upper_bits = is_word==1?signExtend(sign):inp[`Reg_width-1:`Reg_width/2];
    return {sign,upper_bits,inp[31:0]};
  endfunction

  function Bit#(TAdd#(1,TMul#(2,`Reg_width))) restoring_step(Bit#(TAdd#(1,TMul#(2,`Reg_width))) remainder, Bit#(`Reg_width) divisor);
    for (Integer i=0;i<`Loop;i=i+1)begin
      remainder=remainder<<1;
//      Int#(TAdd#(1,`Reg_width)) upper=unpack(remainder[2*`Reg_width:`Reg_width]); // convert to signed number
      Bit#(65) sub=remainder[128:64]+signExtend(~divisor+1);
      if(remainder[127:64]>=divisor)begin // if subtraction is positive
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
      $display("Taken inputs in multiplier. rs1: %h rs2: %h",inp1,inp2);
      Bool lv_take_compl=False;
      Bit#(1) mul_or_div = funct[2];  // 0 = multiplication 1 = division
      Bit#(1) rs1_signed=(funct[2]==0)?funct[0]^funct[1]:~funct[0];
      Bit#(1) rs2_signed=(funct[2]==0)?~funct[1]&funct[0]:~funct[0];
      Bit#(TAdd#(`Reg_width,1)) in1=return_modified_operands(inp1,rs1_signed,word32);
      Bit#(TAdd#(`Reg_width,1)) in2=return_modified_operands(inp2,rs2_signed,word32);
     
      if(funct[2]==0) begin// multiplication
	in1=((funct[0]^funct[1])==1 && in1[`Reg_width]==1)?~in1[`Reg_width-1:0]+1:in1[`Reg_width-1:0] ;//MULH/MULSU
	in2=(funct[1:0]=='b01 && in2[`Reg_width]==1)?~in2[`Reg_width-1:0]+1:in2[`Reg_width-1:0]; // MULH
	if((funct[1:0]=='b01 && (in1[`Reg_width]^in2[`Reg_width])==1) || (funct[1:0]=='b10 && in1[`Reg_width]==1))
	  lv_take_compl=True;
      end
      else begin
	in1=(funct[0]==0 && in1[`Reg_width]==1)?~in1[`Reg_width-1:0]+1:in1[`Reg_width-1:0];
	in2=(funct[0]==0 && in2[`Reg_width]==1)?~in2[`Reg_width-1:0]+1:in2[`Reg_width-1:0];
	if(funct[1:0]==0 && in2!=0 && (in1[`Reg_width]^in2[`Reg_width])==1)
	  lv_take_compl=True;
      end
      $display("Modified inputs in multiplier. rs1: %h rs2: %h",in1,in2);

      if(rg_state_counter==0)begin
	if(funct[2]==0)begin // start multiplication operation
	  partial_prod<=zeroExtend(in2[63:0]);
	end
	else begin // division operation
	  partial_prod<=zeroExtend(in1[63:0]);
	end
	rg_state_counter<=rg_state_counter+1;
	return tagged Invalid;
      end
      else begin
	if(funct[2]==0) begin // multiplication
	  Bit#(`Reg_width) temp=(partial_prod[`Loop-1:0])*in1[`Reg_width-1:0];
	  Bit#(TAdd#(`Reg_width,1)) accum=partial_prod[2*`Reg_width:`Reg_width]+zeroExtend(temp);
	  Bit#(TAdd#(1,(TMul#(`Reg_width,2)))) temp1 ={accum[`Reg_width:0],partial_prod[`Reg_width-1:0]}>>`Loop;
	  $display("multiplication. Partial :%h Counter: %d",temp1,rg_state_counter);
	  if(rg_state_counter==(word32==1?32/`Loop:64/`Loop))begin
	    rg_state_counter<=0;
	    if(word32==1) // MULW
	      return tagged Valid signExtend(temp1[63:32]);
	    else if(funct==0) // MUL 
	      return tagged Valid truncate(temp1);
	    else if(lv_take_compl) // for MULH/MULHSU
	      return tagged Valid ((~temp1+1)[2*`Reg_width-1:`Reg_width]);
	    else
	      return tagged Valid temp1[2*`Reg_width-1:`Reg_width];
	  end
	  else begin
	    partial_prod<=temp1;
	    rg_state_counter<=rg_state_counter+1;
	    return tagged Invalid;
	  end
	end
	else begin
	  Bit#(129) temp1=restoring_step(partial_prod,(in2[`Reg_width-1:0]));
	  Bit#(65) sub=partial_prod[128:64]+signExtend(~in2[63:0]+1);
	  $display("Division. Partial :%h Sub: %h Counter: %d",temp1,sub,rg_state_counter);
	  if(rg_state_counter==64/`Loop)begin
	    rg_state_counter<=0;
	    if(funct[1]==1) // REM/REMU
	      temp1=signExtend(temp1[127:64]);
	    else // DIV/DIVU
	      temp1=signExtend(temp1[63:0]);

	    if(funct[1]==0 && lv_take_compl)// DIVU
	      temp1=~temp1+1;
	    else if(funct[1:0]=='b10 && temp1[63]!=in1[`Reg_width])  // REMU/REM
	      temp1=~temp1+1;

	    return tagged Valid ((word32==1)?signExtend(temp1[31:0]):temp1[63:0]);	    
	  end
	  else begin
	    partial_prod<=temp1;
	    rg_state_counter<=rg_state_counter+1;
	    return tagged Invalid;
	  end
	end
      end
    endmethod
  endmodule
endpackage
