/*

Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name 	: Leading Zeros Detector for 64-bit numbers
Author's Name 	: Arjun C. Menon
e-mail id	: c.arjunmenon@gmail.com
Last updated on : 16th October 2013

This algorithm has been picked up from the paper Titled:		
An Algorith and Novel Design of a Leading Zero Detector Circuit:comparision with 	
Logic Synthesis - by Vojin G. Oklobdzija

*/

package lead_zero_detect64;

//(*noinline*)
function Bit#(3) lzd4(bit p0, bit p1,bit p2, bit p3);
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
		return {1'b1,1'b0,p0};
	else if(v1==1)
		return {1'b1,1'b1,p1};
	else return 0;
endfunction
//(*noinline*)
function Bit#(7) lzd64(Bit#(5) p0, Bit#(5) p1, bit v0 , bit v1);
	if(v0 ==1)
		return {1'b1,1'b0,p0};
	else if(v1==1)
		return {1'b1,1'b1,p1};
	else return 0;
endfunction
//(*noinline*)
 function Bit#(7) fn_lead_zeros64(Bit#(64) mantissa);
 Bit#(2) p0_l1= lzd4(mantissa[63], mantissa[62], mantissa[61], mantissa[60])[1:0];
 bit v0_l1 = lzd4(mantissa[63], mantissa[62], mantissa[61], mantissa[60])[2];
 
 Bit#(2) p1_l1= lzd4(mantissa[59], mantissa[58], mantissa[57], mantissa[56])[1:0];
 bit v1_l1 = lzd4(mantissa[59], mantissa[58], mantissa[57], mantissa[56])[2];
 
 Bit#(2) p2_l1= lzd4(mantissa[55], mantissa[54], mantissa[53], mantissa[52])[1:0];
 bit v2_l1 = lzd4(mantissa[55], mantissa[54], mantissa[53], mantissa[52])[2];
 
 Bit#(2) p3_l1= lzd4(mantissa[51], mantissa[50], mantissa[49], mantissa[48])[1:0];
 bit v3_l1 = lzd4(mantissa[51], mantissa[50], mantissa[49], mantissa[48])[2];
 
 Bit#(2) p4_l1= lzd4(mantissa[47], mantissa[46], mantissa[45], mantissa[44])[1:0];
 bit v4_l1 = lzd4(mantissa[47], mantissa[46], mantissa[45], mantissa[44])[2];
 
 Bit#(2) p5_l1= lzd4(mantissa[43], mantissa[42], mantissa[41], mantissa[40])[1:0];
 bit v5_l1 = lzd4(mantissa[43], mantissa[42], mantissa[41], mantissa[40])[2];
 
 Bit#(2) p6_l1= lzd4(mantissa[39], mantissa[38], mantissa[37], mantissa[36])[1:0];
 bit v6_l1= lzd4(mantissa[39], mantissa[38], mantissa[37], mantissa[36])[2];
 
 Bit#(2) p7_l1= lzd4(mantissa[35], mantissa[34], mantissa[33], mantissa[32])[1:0];
 bit v7_l1 = lzd4(mantissa[35], mantissa[34], mantissa[33], mantissa[32])[2];

 Bit#(2) p8_l1= lzd4(mantissa[31], mantissa[30], mantissa[29], mantissa[28])[1:0];
 bit v8_l1 = lzd4(mantissa[31], mantissa[30], mantissa[29], mantissa[28])[2];
 
 Bit#(2) p9_l1= lzd4(mantissa[27], mantissa[26], mantissa[25], mantissa[24])[1:0];
 bit v9_l1 = lzd4(mantissa[27], mantissa[26], mantissa[25], mantissa[24])[2];
 
 Bit#(2) p10_l1= lzd4(mantissa[23], mantissa[22], mantissa[21], mantissa[20])[1:0];
 bit v10_l1 = lzd4(mantissa[23], mantissa[22], mantissa[21], mantissa[20])[2];
 
 Bit#(2) p11_l1= lzd4(mantissa[19], mantissa[18], mantissa[17], mantissa[16])[1:0];
 bit v11_l1 = lzd4(mantissa[19], mantissa[18], mantissa[17], mantissa[16])[2];
 
 Bit#(2) p12_l1= lzd4(mantissa[15], mantissa[14], mantissa[13], mantissa[12])[1:0];
 bit v12_l1 = lzd4(mantissa[15], mantissa[14], mantissa[13], mantissa[12])[2];
 
 Bit#(2) p13_l1= lzd4(mantissa[11], mantissa[10], mantissa[9], mantissa[8])[1:0];
 bit v13_l1 = lzd4(mantissa[11], mantissa[10], mantissa[9], mantissa[8])[2];
 
 Bit#(2) p14_l1= lzd4(mantissa[7], mantissa[6], mantissa[5], mantissa[4])[1:0];
 bit v14_l1= lzd4(mantissa[7], mantissa[6], mantissa[5], mantissa[4])[2];
 
 Bit#(2) p15_l1= lzd4(mantissa[3], mantissa[2], mantissa[1], mantissa[0])[1:0];
 bit v15_l1 = lzd4(mantissa[3], mantissa[2], mantissa[1], mantissa[0])[2];
 
 ///////////////////////////////////////////////////////////////////////
 //////////////////////// LEVEL 1 OVER /////////////////////////////////
 ///////////////////////////////////////////////////////////////////////
 
 

 Bit#(3) p0_l3 = lzd8(p0_l1, p1_l1 , v0_l1, v1_l1)[2:0];
 bit v0_l3 = lzd8(p0_l1, p1_l1 , v0_l1, v1_l1)[3];
 
 Bit#(3) p1_l3 = lzd8(p2_l1, p3_l1 , v2_l1, v3_l1)[2:0];
 bit v1_l3 = lzd8(p2_l1, p3_l1 , v2_l1, v3_l1)[3];
 
 Bit#(3) p2_l3 = lzd8(p4_l1, p5_l1 , v4_l1, v5_l1)[2:0];
 bit v2_l3 = lzd8(p4_l1, p5_l1 , v4_l1, v5_l1)[3];
 
 Bit#(3) p3_l3 = lzd8(p6_l1, p7_l1 , v6_l1, v7_l1)[2:0];
 bit v3_l3 = lzd8(p6_l1, p7_l1 , v6_l1, v7_l1)[3];
 
 Bit#(3) p4_l3 = lzd8(p8_l1, p9_l1 , v8_l1, v9_l1)[2:0];
 bit v4_l3 = lzd8(p8_l1, p9_l1 , v8_l1, v9_l1)[3];

 Bit#(3) p5_l3 = lzd8(p10_l1, p11_l1 , v10_l1, v11_l1)[2:0];
 bit v5_l3 = lzd8(p10_l1, p11_l1 , v10_l1, v11_l1)[3];
 
 Bit#(3) p6_l3 = lzd8(p12_l1, p13_l1 , v12_l1, v13_l1)[2:0];
 bit v6_l3 = lzd8(p12_l1, p13_l1 , v12_l1, v13_l1)[3];
 
 Bit#(3) p7_l3 = lzd8(p14_l1, p15_l1 , v14_l1, v15_l1)[2:0];
 bit v7_l3 = lzd8(p14_l1, p15_l1 , v14_l1, v15_l1)[3];
 
 
 ///////////////////////////////////////////////////////////////////////
 //////////////////////// LEVEL 2 OVER /////////////////////////////////
 ///////////////////////////////////////////////////////////////////////


 Bit#(4) p0_l4 = lzd16(p0_l3,p1_l3,v0_l3, v1_l3)[3:0];
 bit v0_l4= lzd16(p0_l3,p1_l3,v0_l3, v1_l3)[4];
 
 Bit#(4) p1_l4 = lzd16(p2_l3,p3_l3,v2_l3, v3_l3)[3:0];
 bit v1_l4= lzd16(p2_l3,p3_l3,v2_l3, v3_l3)[4];

 Bit#(4) p2_l4 = lzd16(p4_l3,p5_l3,v4_l3, v5_l3)[3:0];
 bit v2_l4= lzd16(p4_l3,p5_l3,v4_l3, v5_l3)[4];
 
 Bit#(4) p3_l4 = lzd16(p6_l3,p7_l3,v6_l3, v7_l3)[3:0];
 bit v3_l4= lzd16(p6_l3,p7_l3,v6_l3, v7_l3)[4];
 
 ///////////////////////////////////////////////////////////////////////
 //////////////////////// LEVEL 3 OVER /////////////////////////////////
 ///////////////////////////////////////////////////////////////////////


 Bit#(5) p0_l5 = lzd32(p0_l4,p1_l4,v0_l4, v1_l4)[4:0];
 bit v0_l5= lzd32(p0_l4,p1_l4,v0_l4, v1_l4)[5];
 
 Bit#(5) p1_l5 = lzd32(p2_l4,p3_l4,v2_l4, v3_l4)[4:0];
 bit v1_l5= lzd32(p2_l4,p3_l4,v2_l4, v3_l4)[5];

 ///////////////////////////////////////////////////////////////////////
 //////////////////////// LEVEL 4 OVER /////////////////////////////////
 ///////////////////////////////////////////////////////////////////////

 return (lzd64( p0_l5, p1_l5,  v0_l5, v1_l5)[6:0]);
endfunction
endpackage
