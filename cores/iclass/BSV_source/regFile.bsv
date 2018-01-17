/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

Author Names : Rahul Bodduna, N Sireesh.
Email ID : rahul.bodduna@gmail.com
*/

package regFile;

`include "defined_parameters.bsv"
import riscv_types::*;

import ConfigReg::*;
import Vector::*;

interface Ifc_regFile;
   method Bit#(`REG_WIDTH) read(UInt#(TLog#(`PRF_SIZE)) address);
   method Action _write(UInt#(TLog#(`PRF_SIZE)) address, Bit#(`REG_WIDTH) data);
   method Action display_contents();
endinterface
   
module mkRegFile(Ifc_regFile);

	Reg#(Bit#(`REG_WIDTH)) prf[`PRF_SIZE];

	for(Integer i=0; i<`PRF_SIZE; i=i+1) begin
		if(i==5)
			prf[i] <- mkReg('h80000000);
		else
			prf[i] <- mkReg(0);
	end

	//Vector#(`PRF_SIZE, Reg#(Bit#(`REG_WIDTH))) prf <- genWithM(reset_prf);
	
	rule rl_display;
	 	$display("prf_value %d", prf[11]);
	endrule
	
	method Bit#(`REG_WIDTH) read(UInt#(TLog#(`PRF_SIZE)) address);
	   return prf[address];
	endmethod
	
	method Action _write(UInt#(TLog#(`PRF_SIZE)) address, Bit#(`REG_WIDTH) data);
	 	$display("writing into PRF at address %d with data %h", address, data);
	   prf[address] <= data;
	endmethod
	
	method Action display_contents();
	   
	   for(Integer i=0;i<`PRF_SIZE;i=i+1)
	 	 $display("%d %d", i, prf[i]);
	
	endmethod
   	  
endmodule

endpackage 
