/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name 	: Leading zero detector for 32-bit numbers
Author's Name	: Neel Gala
e-mail id	: neelgala@gmail.com
Last updated on : 15th December 2013

This algorithm has been picked up from the paper Titled:		
An Algorith and Novel Design of a Leading Zero Detector Circuit:comparision with 	
Logic Synthesis - by Vojin G. Oklobdzija

*/

package lead_zero_detect32;

//(*noinline*)
function Bit#(3) lzd4 (bit p0, bit p1,bit p2, bit p3);
	return {(p0|p1|p2|p3) , (~p0)&(~p1) , ((~p0)&p1 | (~p0)&(~p2))};
endfunction

//(*noinline*)
function Bit#(4) lzd8(Bit#(2) p0, Bit#(2) p1, bit v0 , bit v1);
	if(v0 ==1)
		return {1'b1,1'b0,p0};
	else if(v1==1)
		return {1'b1,1'b1,p1};
	else return 0;
endfunction
//(*noinline*)
function Bit#(5) lzd16(Bit#(3) p0, Bit#(3) p1, bit v0 , bit v1);
	if(v0 ==1)
		return {1'b1,1'b0,p0};
	else if(v1==1)
		return {1'b1,1'b1,p1};
	else return 0;
endfunction
//(*noinline*)
function Bit#(6) lzd32(Bit#(4) p0, Bit#(4) p1, bit v0 , bit v1);
	if(v0 ==1)
		return {1'b1 , 1'b0 , p0};
	else if(v1 ==1)
		return {1'b1 , 1'b1 , p1};
	else 
		return 0;
endfunction

//(*noinline*)
 function Bit#(6) fn_lead_zeros32 (Bit#(32) mantissa);

 Bit#(2) p0_l1= lzd4(mantissa[31], mantissa[30], mantissa[29], mantissa[28])[1:0];
 bit v0_l1 = lzd4(mantissa[31], mantissa[30], mantissa[29], mantissa[28])[2];
 
 Bit#(2) p1_l1= lzd4(mantissa[27], mantissa[26], mantissa[25], mantissa[24])[1:0];
 bit v1_l1 = lzd4(mantissa[27], mantissa[26], mantissa[25], mantissa[24])[2];
 
 Bit#(2) p2_l1= lzd4(mantissa[23], mantissa[22], mantissa[21], mantissa[20])[1:0];
 bit v2_l1= lzd4(mantissa[23], mantissa[22], mantissa[21], mantissa[20])[2];
 
 Bit#(2) p3_l1= lzd4(mantissa[19], mantissa[18], mantissa[17], mantissa[16])[1:0];
 bit v3_l1 = lzd4(mantissa[19], mantissa[18], mantissa[17], mantissa[16])[2];
 
 Bit#(2) p4_l1= lzd4(mantissa[15], mantissa[14], mantissa[13], mantissa[12])[1:0];
 bit v4_l1 = lzd4(mantissa[15], mantissa[14], mantissa[13], mantissa[12])[2];
 
 Bit#(2) p5_l1= lzd4(mantissa[11], mantissa[10], mantissa[9], mantissa[8])[1:0];
 bit v5_l1 = lzd4(mantissa[11], mantissa[10], mantissa[9], mantissa[8])[2];
 
 Bit#(2) p6_l1= lzd4(mantissa[7], mantissa[6], mantissa[5], mantissa[4])[1:0];
 bit v6_l1 = lzd4(mantissa[7], mantissa[6], mantissa[5], mantissa[4])[2];
 
 Bit#(2) p7_l1= lzd4(mantissa[3], mantissa[2], mantissa[1], mantissa[0])[1:0];
 bit v7_l1 = lzd4(mantissa[3], mantissa[2], mantissa[1], mantissa[0])[2];
 
 ///////////////////////////////////////////////////////////////////////
 //////////////////////// LEVEL 1 OVER /////////////////////////////////
 ///////////////////////////////////////////////////////////////////////
 
  
 Bit#(3) p0_l2 = lzd8(p0_l1, p1_l1 , v0_l1, v1_l1)[2:0];
 bit v0_l2 = lzd8(p0_l1, p1_l1 , v0_l1, v1_l1)[3];
 
 Bit#(3) p1_l2 = lzd8(p2_l1, p3_l1 , v2_l1, v3_l1)[2:0];
 bit v1_l2 = lzd8(p2_l1, p3_l1 , v2_l1, v3_l1)[3];
 
 Bit#(3) p2_l2 = lzd8(p4_l1, p5_l1 , v4_l1, v5_l1)[2:0];
 bit v2_l2 = lzd8(p4_l1, p5_l1 , v4_l1, v5_l1)[3];
 
 Bit#(3) p3_l2 = lzd8(p6_l1, p7_l1 , v6_l1, v7_l1)[2:0];
 bit v3_l2 = lzd8(p6_l1, p7_l1 , v6_l1, v7_l1)[3];
 
 ///////////////////////////////////////////////////////////////////////
 //////////////////////// LEVEL 2 OVER /////////////////////////////////
 ///////////////////////////////////////////////////////////////////////


 Bit#(4) p0_l3 = lzd16(p0_l2,p1_l2,v0_l2, v1_l2)[3:0];
 bit v0_l3= lzd16(p0_l2,p1_l2,v0_l2, v1_l2)[4];
 
 Bit#(4) p1_l3 = lzd16(p2_l2,p3_l2,v2_l2, v3_l2)[3:0];
 bit v1_l3= lzd16(p2_l2,p3_l2,v2_l2, v3_l2)[4];
 
 return (lzd32 ( p0_l3, p1_l3, v0_l3, v1_l3 )[5:0]);
endfunction
endpackage
 
