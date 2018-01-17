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
Author Name: Neel Gala , Kalyan Kumar
Email Id: neelgala@gmail.com kalyavkumar.suvvada@gmail.com
Last updated on : 30th September, 2013

This package converts a redundant binary number to a normal binary number*/

package RB_NB;

	//Conversion of one bit redundant binary number
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
	function Bit#(17) rb_nb1 ( Bit#(128) rb);
		// z is the resultant binary number
		// c is the carry
		// fp and fm are the positive and negative components of the Redundant Binary number which is to be converted into normal binary
		Bit#(64) fp=rb[127:64],fm=rb[63:0];
		Bit#(16) z=0;
		Bit#(2) ans;
		Bit#(17) out=0;
		bit c=1;

		for(int i=0;i<16;i=i+1)
		begin
			ans=c_z_mux(fp[i],fm[i],c);
			z[i]=ans[1];
			c=ans[0];
		end
		out={z,c};
		return out;
	endfunction
	
	(*noinline*)
	// 64 bit RB to NB converter
	function Bit#(33) rb_nb2 ( Bit#(128) rb, Bit#(17) fwd);
		// z is the resultant binary number
		// c is the carry
		// fp and fm are the positive and negative components of the Redundant Binary number which is to be converted into normal binary
		Bit#(64) fp=rb[127:64],fm=rb[63:0];
		Bit#(32) z={16'h0,fwd[16:1]};
		Bit#(2) ans;
		bit c=fwd[0];

		for(int i=16;i<32;i=i+1)
		begin
			ans=c_z_mux(fp[i],fm[i],c);
			z[i]=ans[1];
			c=ans[0];
		end

		return {z,c};
	endfunction
	
	(*noinline*)
	// 64 bit RB to NB converter
	function Bit#(49) rb_nb3 ( Bit#(128) rb, Bit#(33) fwd);
		// z is the resultant binary number
		// c is the carry
		// fp and fm are the positive and negative components of the Redundant Binary number which is to be converted into normal binary
		Bit#(64) fp=rb[127:64],fm=rb[63:0];
		Bit#(48) z={16'h0,fwd[32:1]};
		Bit#(2) ans;
		bit c=fwd[0];

		for(int i=32;i<48;i=i+1)
		begin
			ans=c_z_mux(fp[i],fm[i],c);
			z[i]=ans[1];
			c=ans[0];
		end

		return {z,c};
	endfunction
	
	(*noinline*)
	// 64 bit RB to NB converter
	function Bit#(64) rb_nb4 ( Bit#(128) rb, Bit#(49) fwd);
		// z is the resultant binary number
		// c is the carry
		// fp and fm are the positive and negative components of the Redundant Binary number which is to be converted into normal binary
		Bit#(64) fp=rb[127:64],fm=rb[63:0];
		Bit#(64) z={16'h0,fwd[48:1]};
		Bit#(2) ans;
		bit c=fwd[0];

		for(int i=48;i<64;i=i+1)
		begin
			ans=c_z_mux(fp[i],fm[i],c);
			z[i]=ans[1];
			c=ans[0];
		end

		return z;
	endfunction
endpackage
