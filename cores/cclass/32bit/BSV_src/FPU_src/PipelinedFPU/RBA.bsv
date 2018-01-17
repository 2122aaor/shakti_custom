/*

Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name: Interger Multiplier used in Floating Point Unit.
Author Name: Neel Gala , Kalyan Kumar , Aditya Govardhan , Vinod.G
Email Id: neelgala@gmail.com kalyavkumar.suvvada@gmail.com dtgovardhan@gmail.com g.vinod1993@gmail.com
Last updated on : 30th September, 2013

This file is the part of redundant binary integer multiplier. It does the addition part after the reduction of partial product by booth encoding second algorithm
*/


package RBA;
import Vector::*;

// Adding two one bit redundant binary numbers
(*noinline*)
function Bit#(4) rba(bit ap, bit am, bit bp, bit bm, bit betap, bit hp);
	bit dp,dm,alpha,beta,h,k,l,x,y;
	Bit#(4) ans=0;
	x=ap|am;
	y=bp|bm;
	k=ap|bp;
	h=am|bm;
	l=x^y;
	beta=(x&y&k)|((~x)&y&(~hp))|(x&(~y)&(~hp));
	alpha=x^y^hp;
	dm=alpha&(~betap);
	dp=(~alpha)&betap;
	ans={beta,h,dp,dm};
	return ans;

endfunction

// 64 bit rbAdder
(*noinline*)
function Bit#(128)  rbAdder(Bit#(64) ap, Bit#(64) am, Bit#(64) bp, Bit#(64) bm);
	Bit#(64) dp=0,dm=0;
	bit beta=0,h=0;
	Bit#(128) ans=0;
	Bit#(4) res=0;
	for(int i=0;i<64;i=i+1)
	begin
		if(ap[i]==1 && am[i]==1)
		begin
			ap[i]=0;
			am[i]=0;
		end
		
		if(bp[i]==1 && bm[i]==1)
		begin
			bp[i]=0;
			bm[i]=0;
		end
		res=rba(ap[i],am[i],bp[i],bm[i],beta,h);		
		beta=res[3];
		h=res[2];
		dp[i]=res[1];
		dm[i]=res[0];
	end
	ans={dp,dm};

	return ans;
endfunction

// Wallace tree structure with 4 leaf RB Adders and two RB Adders in the next level
(*noinline*)
function Bit#(256) wallace_rba_part1(Vector#(16,Bit#(64)) inp);
	// si_jp = positive part of the j th redundant binary pp from stage i ,si_jm = negative component of the j th redundant binary pp from stage i
	Bit#(64) s1_1p,s1_2p,s1_3p,s1_4p,s1_1m,s1_2m,s1_3m,s1_4m,s2_1p,s2_2p,s2_1m,s2_2m;
	Bit#(128) res1,res2,res3,res4;
	
	//stage1
	res1=rbAdder(inp[0],inp[1],'d0,{35'b0,29'b10001000100010001000100010001});
	s1_1m=res1[63:0];
	s1_1p=res1[127:64];
	
	res2=rbAdder(inp[2],inp[3],inp[4],inp[5]);
	s1_2m=res2[63:0];
	s1_2p=res2[127:64];
	
	res3=rbAdder(inp[6],inp[7],inp[8],inp[9]);
	s1_3m=res3[63:0];
	s1_3p=res3[127:64];
	
	res4=rbAdder(inp[10],inp[11],inp[12],inp[13]);
	s1_4m=res4[63:0];
	s1_4p=res4[127:64];

	//step2
	res1=rbAdder(s1_1p,s1_1m,s1_2p,s1_2m);
	s2_1m=res1[63:0];
	s2_1p=res1[127:64];

	res2=rbAdder(s1_3p,s1_3m,s1_4p,s1_4m);
	s2_2m=res2[63:0];
	s2_2p=res2[127:64];

	return {s2_1p,s2_1m,s2_2p,s2_2m};

endfunction

// Two RB Adders Adding three RB numbers, two of which are the results from the previous stage and one is the initial input.
(*noinline*)
function Bit#(128) wallace_rba_part2( Bit#(64) s2_1p, Bit#(64) s2_1m, Bit#(64) s2_2p, Bit#(64) s2_2m, Bit#(64) inp1, Bit#(64) inp2);

	//step3
	
	let res2=rbAdder(s2_1p,s2_1m,s2_2p,s2_2m);
	let s3_1m=res2[63:0];
	let s3_1p=res2[127:64];
	
	//step4
	
	res2=rbAdder(s3_1p,s3_1m,inp1,inp2);
	
	return res2;

endfunction

endpackage
