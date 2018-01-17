/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

Authors' Names : Rahul Bodduna, N Sireesh.
E-mail : rahul.bodduna@gmail.com
*/

package  Encoder_tree;
/* Functionality of encoder tree is explained in 'Issue_stage' PDF in repo.
   This file needs to be modified if the issue queue size is changed */

`include "defined_parameters.bsv"


(* noinline *)

/* This function defines the working of priority encoder with 4 bit inputs */
function Bit#(4) priority_encoder(Bit#(4) inp, Bool alu_free);
   Bit#(4) outp = 0;
   if(alu_free)
	  begin			
		 if(inp[0]==1)
			outp[0] = 1'b1;
		 else if(inp[1]==1)
			outp[1] = 1'b1;
		 else if(inp[2]==1)
			outp[2] = 1'b1;
		 else if(inp[3]==1)
			outp[3] = 1'b1;
	  end
   return outp;
endfunction



function bit any_req(Bit#(4) inp);
   return inp[0] | inp[1] | inp[2] | inp[3];
endfunction

/* Encodes the grant vector */
function Bit#(TAdd#(1,TLog#(`ENTRY_ROB_SIZE))) encoder(Bit#(`ENTRY_ROB_SIZE) inp);
   Bit#(TLog#(`ENTRY_ROB_SIZE)) outp = 0;
   bit outp_valid = 1'b1;
   case(inp) matches
	  16'h0001: outp = 4'h0;
	  16'h0002: outp = 4'h1;
	  16'h0004: outp = 4'h2;
	  16'h0008: outp = 4'h3;
	  16'h0010: outp = 4'h4;
	  16'h0020: outp = 4'h5;
	  16'h0040: outp = 4'h6;
	  16'h0080: outp = 4'h7;
	  16'h0100: outp = 4'h8;
	  16'h0200: outp = 4'h9;
	  16'h0400: outp = 4'hA;
	  16'h0800: outp = 4'hB;
	  16'h1000: outp = 4'hC;
	  16'h2000: outp = 4'hD;
	  16'h4000: outp = 4'hE;
	  16'h8000: outp = 4'hF;
	  default: outp_valid = 1'b0;
   endcase
   return {outp_valid,outp};
endfunction

/* Request vectors are passed down the tree and the grants are given back */
function Bit#(`ENTRY_ROB_SIZE) encoder_tree(Bit#(`ENTRY_ROB_SIZE) inp, Bool alu_free);
   Bit#(`ENTRY_ROB_SIZE) outp = 0;
   if(alu_free)
	  begin
		 //request to root
		 Bit#(4) root_reqs;

		 //grant from root
		 Bit#(4) root_grants;
		 
		 for(Integer i=0;i<4;i=i+1)
			root_reqs[i] = any_req(inp[4*fromInteger(i)+3:4*fromInteger(i)]);
		 
		 root_grants = priority_encoder(root_reqs, alu_free);
		 
		 //grants are passed back to leaves
		 for(Integer i=0;i<4;i=i+1)
			outp[4*fromInteger(i)+3:4*fromInteger(i)] = priority_encoder(inp[4*fromInteger(i)+3:4*fromInteger(i)], unpack(root_grants[i]));
	  end
   return outp;
endfunction			

endpackage 
