/*

Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



Module Name     : Trailing zero detector module
Author Name     : Arjun C. Menon
Email ID        : c.arjunmenon@gmail.com
Last updated on : 21th December, 2013

*/

package trail_zero_detect32;

//(*noinline*)
function Bit#(3) tzd4 (bit p0, bit p1,bit p2, bit p3);
	return {(p0|p1|p2|p3) , (~p0)&(~p1) , ((~p0)&p1 | (~p0)&(~p2))};
endfunction

//(*noinline*)
function Bit#(4) tzd8(Bit#(2) p0, Bit#(2) p1, bit v0 , bit v1);
	if(v0 ==1)
		return {1'b1,1'b0,p0};
	else if(v1==1)
		return {1'b1,1'b1,p1};
	else return 0;
endfunction
//(*noinline*)
function Bit#(5) tzd16(Bit#(3) p0, Bit#(3) p1, bit v0 , bit v1);
	if(v0 ==1)
		return {1'b1,1'b0,p0};
	else if(v1==1)
		return {1'b1,1'b1,p1};
	else return 0;
endfunction
//(*noinline*)
function Bit#(6) tzd32(Bit#(4) p0, Bit#(4) p1, bit v0 , bit v1);
	if(v0 ==1)
		return {1'b1 , 1'b0 , p0};
	else if(v1 ==1)
		return {1'b1 , 1'b1 , p1};
	else 
		return 0;
endfunction

//(*noinline*)
 function Bit#(6) count_trailing_zeros32 (Bit#(32) significand);

 Bit#(2) p0_l1= tzd4(significand[0], significand[1], significand[2], significand[3])[1:0];
 bit v0_l1 = tzd4(significand[0], significand[1], significand[2], significand[3])[2];
 
 Bit#(2) p1_l1= tzd4(significand[4], significand[5], significand[6], significand[7])[1:0];
 bit v1_l1 = tzd4(significand[4], significand[5], significand[6], significand[7])[2];
 
 Bit#(2) p2_l1= tzd4(significand[8], significand[9], significand[10], significand[11])[1:0];
 bit v2_l1= tzd4(significand[8], significand[9], significand[10], significand[11])[2];
 
 Bit#(2) p3_l1= tzd4(significand[12], significand[13], significand[14], significand[15])[1:0];
 bit v3_l1 = tzd4(significand[12], significand[13], significand[14], significand[15])[2];
 
 Bit#(2) p4_l1= tzd4(significand[16], significand[17], significand[18], significand[19])[1:0];
 bit v4_l1 = tzd4(significand[16], significand[17], significand[18], significand[19])[2];
 
 Bit#(2) p5_l1= tzd4(significand[20], significand[21], significand[22], significand[23])[1:0];
 bit v5_l1 = tzd4(significand[20], significand[21], significand[22], significand[23])[2];
 
 Bit#(2) p6_l1= tzd4(significand[24], significand[25], significand[26], significand[27])[1:0];
 bit v6_l1 = tzd4(significand[24], significand[25], significand[26], significand[27])[2];
 
 Bit#(2) p7_l1= tzd4(significand[28], significand[29], significand[30], significand[31])[1:0];
 bit v7_l1 = tzd4(significand[28], significand[29], significand[30], significand[31])[2];
 
 ///////////////////////////////////////////////////////////////////////
 //////////////////////// LEVEL 1 OVER /////////////////////////////////
 ///////////////////////////////////////////////////////////////////////
 
  
 Bit#(3) p0_l2 = tzd8(p0_l1, p1_l1 , v0_l1, v1_l1)[2:0];
 bit v0_l2 = tzd8(p0_l1, p1_l1 , v0_l1, v1_l1)[3];
 
 Bit#(3) p1_l2 = tzd8(p2_l1, p3_l1 , v2_l1, v3_l1)[2:0];
 bit v1_l2 = tzd8(p2_l1, p3_l1 , v2_l1, v3_l1)[3];
 
 Bit#(3) p2_l2 = tzd8(p4_l1, p5_l1 , v4_l1, v5_l1)[2:0];
 bit v2_l2 = tzd8(p4_l1, p5_l1 , v4_l1, v5_l1)[3];
 
 Bit#(3) p3_l2 = tzd8(p6_l1, p7_l1 , v6_l1, v7_l1)[2:0];
 bit v3_l2 = tzd8(p6_l1, p7_l1 , v6_l1, v7_l1)[3];
 
 ///////////////////////////////////////////////////////////////////////
 //////////////////////// LEVEL 2 OVER /////////////////////////////////
 ///////////////////////////////////////////////////////////////////////


 Bit#(4) p0_l3 = tzd16(p0_l2,p1_l2,v0_l2, v1_l2)[3:0];
 bit v0_l3= tzd16(p0_l2,p1_l2,v0_l2, v1_l2)[4];
 
 Bit#(4) p1_l3 = tzd16(p2_l2,p3_l2,v2_l2, v3_l2)[3:0];
 bit v1_l3= tzd16(p2_l2,p3_l2,v2_l2, v3_l2)[4];
 
 return (tzd32 ( p0_l3, p1_l3, v0_l3, v1_l3 )[5:0]);
endfunction
endpackage
 
