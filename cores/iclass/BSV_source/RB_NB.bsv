/*Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


Module Name: RB_NB
Author Name: Rishi Naidu
Email id:    rishinaidu.rn@gmail.com
Last updated on : 6th October 2013

Converts the Redundant binary number to Normal binary number by an efficient method mentioned in an IEEE paper. (Name mentioned in top file)

*/

package RB_NB;

`include "defined_parameters.bsv"
//Conversion of one bit redundant binary number based on logic from reference paper
(*noinline*)
function Bit#(2) c_z_mux ( bit fp, bit fm, bit cp);
	bit x;
	Bit#(2) res=0;
	x=fp|fm;
	if(cp==1)
		res={x,~fm};
	else
		res={~x,fp};
	return res;

endfunction

(*noinline*)	
// 64 bit RB to NB converter
function Bit#(TMul#(`REG_WIDTH,2)) rb_nb( Bit#(TMul#(`REG_WIDTH,4)) rb);
	// z is the resultant binary number
	// c is the carry
	// fp and fm are the positive and negative components of the Redundant Binary number which is to be converted into normal binary
	Bit#(TMul#(`REG_WIDTH,2)) fp=rb[(`REG_WIDTH*4)-1:`REG_WIDTH*2],fm=rb[(`REG_WIDTH*2)-1:0];
	Bit#(TMul#(`REG_WIDTH,2)) z=0;
	Bit#(2) ans;
	bit c=1;

	for(int i=0;i<`REG_WIDTH*2;i=i+1)
	begin
		ans=c_z_mux(fp[i],fm[i],c);
		z[i]=ans[1];
		c=ans[0];	
	end
	return z;
endfunction


endpackage
