/*
Copyright (c) 2013-2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name   	: Sequential Integer Multiplier Unit
Author's Name 	: Neel Gala, Vinod.G
e-mail id	: neelgala@gmail.com, g.vinod1993@gmail.com
Last updated on : 5th August 2016

*/

package integermultiplier;
  import DReg::*;
  interface Ifc_integermultiplier#(numeric type regwidth, numeric type loop);
    method ActionValue#(Maybe#(Bit#(TMul#(2,regwidth)))) _start(Bit#(regwidth) inp1, Bit#(regwidth) inp2); //_div_name 00 : DIV/REM 01: DIVU/REMU
  endinterface
  //(*synthesize*)
  module mkintegermultiplier(Ifc_integermultiplier#(regwidth,loop))
     provisos ( Add#(regwidth,1,regwidth1),
                Add#(regwidth,regwidth,regwidth_twice),
                Add#(1,TMul#(2,regwidth),regwidth_twice1),
                Add#(1,TLog#(regwidth),regwidth_log1),
                //per request of bsc
                Add#(regwidth1,regwidth,regwidth_twice1)
              );

    Reg#(Bit#(regwidth_twice1)) partial_prod <-mkReg(0);
    Reg#(Bit#(regwidth_log1)) rg_state_counter <-mkDReg(0);//Register for state machine counter
    let rEGWIDTH = valueOf(regwidth);
    let lOOP = valueOf(loop); 
    
    method ActionValue#(Maybe#(Bit#(regwidth_twice))) _start(Bit#(regwidth) inp1, Bit#(regwidth) inp2); 
      $display("Taken inputs in multiplier. rs1: %h rs2: %h",inp1,inp2);
      $display("Register State Counter %h", rg_state_counter);
      $display("partial_prod %h", partial_prod);
      if(rg_state_counter==0)begin
	     partial_prod<=zeroExtend(inp2);
         rg_state_counter<=rg_state_counter+1;
         return tagged Invalid;
      end
      else begin
	      Bit#(regwidth) temp=(partial_prod[lOOP-1:0])*inp1[rEGWIDTH-1:0];
	      Bit#(regwidth1) accum=partial_prod[2*rEGWIDTH:rEGWIDTH]+zeroExtend(temp);
          Bit#(regwidth) partial_prod_temp = partial_prod[rEGWIDTH-1:0];
	      Bit#(regwidth_twice1) temp1 ={accum,partial_prod_temp}>>lOOP;
	      $display("multiplication. Partial :%h Counter: %d",temp1,rg_state_counter);
	      if(rg_state_counter==(fromInteger(rEGWIDTH)/fromInteger(lOOP)))begin
	         rg_state_counter<=0;
	         return tagged Valid temp1[2*rEGWIDTH-1:0];
	      end
	      else begin
	         partial_prod<=temp1;
	         rg_state_counter<=rg_state_counter+1;
	         return tagged Invalid;
	      end
	  end
      endmethod
  endmodule

  module mkTb(Empty);
   Ifc_integermultiplier#(8,4) mul <- mkintegermultiplier();
   Reg#(Bit#(8)) inp1 <- mkReg(8'b1100);
   Reg#(Bit#(8)) inp2 <- mkReg(8'b1010);

   rule give_inputs;
   let x <- mul._start(inp1,inp2);
   if(x matches tagged Valid .res) begin
       $display("Output is %b",res);
       $finish(0);
   end
   endrule

   endmodule
endpackage
