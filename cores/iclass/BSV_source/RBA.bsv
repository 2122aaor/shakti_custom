/*Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name: RBA (Redundant Binary Adder)
Author Name: Rishi Naidu, Neel
Email id:    rishinaidu.rn@gmail.com, neelgala@gmail.com
Last updated on : 25th October 2013

This module performs wallace tree addition using RBA adders. The RBA adders are generated using an efficient method as mentioned in IEEE paper(Named mentioned in top file)

Function **wallace_rba** is used for addition in 3 levels. Takes 16 partial products and give out a RB number. It is used in stage 2 and stage 3

Function **wallace_rba_final** is used for addition of 3 rb numbers in 2 levels. Two RB numbers generated in stage 2 and stage 3 respectively. The 3rd rb number is added due to a logic in Redundant binary addition.
*/
package RBA;

import Vector::*;

`include "defined_parameters.bsv"
(*noinline*)
// Adding two 1 bit redundant binary numbers.
// As per the RBA from reference paper
function Bit#(4) rba(bit ap, bit am, bit bp, bit bm, bit betap, bit hp);
	bit dp,dm,alpha,beta,h,k,l,x,y;
	Bit#(4) ans=0;
	x=ap|am;
	y=bp|bm;
	k=ap|bp;
	h=am|bm;	// Required for next addition 
//	l=((~x)&y)|((~y)&x);
	l=x^y;
//	beta=((~l)&k)|(l&(~hp));
//	beta=(x&y&k)|(l&(~hp));
	beta=(x&y&k)|((~x)&y&(~hp))|(x&(~y)&(~hp)); // Required for computation of next addition
//	beta=l^k;
	alpha=x^y^hp;
	dm=alpha&(~betap); //sum answer minus
	dp=(~alpha)&betap; //sum answer positive
	ans={beta,h,dp,dm}; 
	return ans;

endfunction

(*noinline*)
// 128 bit rbAdder. Adds two RB numbers 
function Bit#(TMul#(`REG_WIDTH,4))  rbAdder(Bit#(TMul#(`REG_WIDTH,2)) ap, Bit#(TMul#(`REG_WIDTH,2)) am, Bit#(TMul#(`REG_WIDTH,2)) bp, Bit#(TMul#(`REG_WIDTH,2)) bm);
	Bit#(TMul#(`REG_WIDTH,2)) dp=0,dm=0;
	bit beta=0,h=0;
	Bit#(TMul#(`REG_WIDTH,4)) ans=0;
	Bit#(4) res=0;
	for(int i=0;i< `REG_WIDTH*2;i=i+1)
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
	ans={dp,dm}; // Result of addition of two 128 bit redundant binary numbers.

	return ans;
endfunction

//Stage 2/3
//Wallace tree structure with 3 levels 
(*noinline*) 
function  Bit#(TMul#(`REG_WIDTH,4)) wallace_rba(Vector#(TDiv#(`REG_WIDTH,4),Bit#(TMul#(`REG_WIDTH,2))) inp) ;
	
	//Step1. Adding 16 partial products
	`ifdef Multiplier16
		Bit#(TMul#(`REG_WIDTH,4)) res1; //Temp varialbes
		Bit#(TMul#(`REG_WIDTH,2)) s1_1p,s1_1m; //Temp variables for stage 1
		res1=rbAdder(inp[0],inp[1],inp[2],inp[3]);
		s1_1m=res1[(`REG_WIDTH*2)-1:0];
		s1_1p=res1[(`REG_WIDTH*4)-1:`REG_WIDTH*2];
	`endif
	
	`ifdef Multiplier32 
		Bit#(TMul#(`REG_WIDTH,4)) res2; //Temp varialbes
		Bit#(TMul#(`REG_WIDTH,2)) s1_2m,s1_2p; //Temp variables for stage 1
		res2=rbAdder(inp[4],inp[5],inp[6],inp[7]);
		s1_2m=res2[(`REG_WIDTH*2)-1:0];
		s1_2p=res2[(`REG_WIDTH*4)-1:`REG_WIDTH*2];
	`endif
	
	`ifdef Multiplier64
		Bit#(TMul#(`REG_WIDTH,4)) res3,res4; //Temp varialbes
		Bit#(TMul#(`REG_WIDTH,2)) s1_3m,s1_3p,s1_4m,s1_4p; //Temp variables for stage 1
		res3=rbAdder(inp[8],inp[9],inp[10],inp[11]);
		s1_3m=res3[(`REG_WIDTH*2)-1:0];
		s1_3p=res3[(`REG_WIDTH*4)-1:`REG_WIDTH*2];
		
		res4=rbAdder(inp[12],inp[13],inp[14],inp[15]);
		s1_4m=res4[(`REG_WIDTH*2)-1:0];
		s1_4p=res4[(`REG_WIDTH*4)-1:`REG_WIDTH*2];
	`endif

	//Step2. Adding the result from step 1

	`ifdef  Multiplier32
		Bit#(TMul#(`REG_WIDTH,2)) s2_1p,s2_1m; //Temp variable stage 2
		res1=rbAdder(s1_1p,s1_1m,s1_2p,s1_2m);
		s2_1m=res1[(`REG_WIDTH*2)-1:0];
		s2_1p=res1[(`REG_WIDTH*4)-1:`REG_WIDTH*2];
	`endif

	`ifdef  Multiplier64
		Bit#(TMul#(`REG_WIDTH,2)) s3_p, s3_m; // Temp varible for stage 3. Store final rb number after 3 steps of addition
		Bit#(TMul#(`REG_WIDTH,2)) s2_2p,s2_2m; //Temp variable stage 2
		res2=rbAdder(s1_3p,s1_3m,s1_4p,s1_4m);
		s2_2m=res2[(`REG_WIDTH*2)-1:0];
		s2_2p=res2[(`REG_WIDTH*4)-1:`REG_WIDTH*2];
	//Step 3 Adding the result from step 2
		res1 =  rbAdder(s2_1p,s2_1m,s2_2p,s2_2m);
		s3_m = res1[(`REG_WIDTH*2)-1:0];
		s3_p = res1[(`REG_WIDTH*4)-1:`REG_WIDTH*2];
	`endif

	`ifdef Multiplier64
		return {s3_p,s3_m};
	`elsif Multiplier32
		return {s2_1p,s2_1m};
	`else
		return {s1_1p, s1_1m};
	`endif

	
endfunction

//Stage 4
//Wallace tree structure with 2 levels 
(*noinline*) 
function Bit#(TMul#(`REG_WIDTH,4)) wallace_rba_final(Bit#(TMul#(`REG_WIDTH,4)) in1, Bit#(TMul#(`REG_WIDTH,4)) in2);

	Bit#(TMul#(`REG_WIDTH,2)) s1_1p,s1_1m; //Temp variables
	Bit#(TMul#(`REG_WIDTH,2)) s2_p, s2_m; //Temp variable for final result
	Bit#(TMul#(`REG_WIDTH,4)) res1,res2;
	//Step1. Adding the result of additon of two sets of 16 partial products
	res1=rbAdder(in1[(`REG_WIDTH*4)-1:`REG_WIDTH*2], in1[(`REG_WIDTH*2)-1:0], in2[(`REG_WIDTH*4)-1:`REG_WIDTH*2],in2[(`REG_WIDTH*2)-1:0]);
	s1_1m=res1[(`REG_WIDTH*2)-1:0];
	s1_1p=res1[(`REG_WIDTH*4)-1:`REG_WIDTH*2];
	
	//Step2. Adding the result from step 2 with a rb number, which is to be added due to logic in redundant binary number.
	Bit#(TAdd#(`REG_WIDTH,3)) zero_=0;
	`ifdef Multiplier64
		Bit#(TSub#(`REG_WIDTH,3)) inter = 'b1000100010001000100010001000100010001000100010001000100010001;
	`elsif Multiplier32
		Bit#(TSub#(`REG_WIDTH,3)) inter = 'b10001000100010001000100010001;
	`endif
	
	res2=rbAdder(s1_1p,s1_1m,'d0,{zero_,inter});
	s2_m=res2[(`REG_WIDTH*2)-1:0];
	s2_p=res2[(`REG_WIDTH*4)-1:`REG_WIDTH*2];

	return {s2_p, s2_m}; 

endfunction

endpackage
