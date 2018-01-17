/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


Module Name: GEN_PP
Author Name: Rishi Naidu
Email id:    rishinaidu.rn@gmail.com
Last updated on : 6th October 2013

Generates partial products based on Booth Encoded Radix-4 algorithm.

*/

package Booth2_pp_gen;

`include "defined_parameters.bsv"

//(*noinline*)
// Function to generate the Booth's partial products.
//Radix 4
function Bit#(TMul#(`REG_WIDTH,2)) gen_pp(Bit#(TMul#(`REG_WIDTH,2)) _multp, Bit#(TMul#(`REG_WIDTH,2)) _twice_multp, Bit#(3) sel, Bit#(1) inv);
	
	Bit#(TMul#(`REG_WIDTH,2)) _result='d0;
	case(sel)   //sel is set of 3 bits of multiplier
		'd0: _result=0; 	        //zero
		'd1: _result=_multp; 	        // M
		'd2: _result=_multp; 	        // M
		'd3: _result=_twice_multp;	// 2M
		'd4: _result=~_twice_multp+1;	// -2M			
		'd5: _result=~_multp+1;	        // -M
		'd6: _result=~_multp+1;	        // -M
		'd7: _result=0;	        // zero
	endcase

	//This inversion of alternate partial products is to obtain the redundant binary numbers from this and the previous partial product.
	if(inv==1)
		_result=~_result;
	
	return _result;

endfunction

endpackage
