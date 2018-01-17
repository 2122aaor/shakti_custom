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

package fRAM;

import riscv_types::*;
import Vector::*;
import ConfigReg::*;
import GetPut::*;
`include "defined_parameters.bsv" 

interface  IfcPrf_fRAM;

    //Input method 
    method Action update_fRAM_1(Bit#(TLog#(`REGFILE_SIZE)) entry_slot, RAT_entry entry_val);
    method Action update_fRAM_2(Bit#(TLog#(`REGFILE_SIZE)) entry_slot, RAT_entry entry_val);
    method Action update_whole_fRAM(Vector#(`REGFILE_SIZE, RAT_entry) entries_fRAM);

    //output method
	interface Get#(Vector#(`REGFILE_SIZE, RAT_entry)) to_map;

/******************* VERIFICATION ENVIRONMENT ******************/
    method Vector#(`REGFILE_SIZE, RAT_entry) return_whole_fRAM();

endinterface

(*synthesize*)
module mkPrf_fRAM(IfcPrf_fRAM);
    ConfigReg#(RAT_entry) fRAM[`REGFILE_SIZE];
    
    //Initial mapping Ri -> Ti for FRAM
    for(Integer i=0; i<`REGFILE_SIZE; i=i+1)
           begin
         	 fRAM[i] <- mkConfigReg(fromInteger(i));
           end
    

    method Action update_fRAM_1(Bit#(TLog#(`REGFILE_SIZE)) entry_slot, RAT_entry entry_val);
         fRAM[entry_slot] <= entry_val;
    endmethod
    
    method Action update_fRAM_2(Bit#(TLog#(`REGFILE_SIZE)) entry_slot, RAT_entry entry_val);
         fRAM[entry_slot] <= entry_val;
    endmethod
    
    method Action update_whole_fRAM(Vector#(`REGFILE_SIZE, RAT_entry) entries_fRAM);
	 for(Integer i = 0; i < `REGFILE_SIZE; i=i+1)
	     fRAM[i] <= entries_fRAM[i];
    endmethod

	interface to_map = interface Get 
    							method ActionValue#(Vector#(`REGFILE_SIZE, RAT_entry)) get();
								 		Vector#(`REGFILE_SIZE, RAT_entry) fRAM_return;
								 		for(Integer i = 0; i < `REGFILE_SIZE; i=i+1)
								 		   fRAM_return[i] = fRAM[i];
								 		return fRAM_return;
    							endmethod
							endinterface;

    method Vector#(`REGFILE_SIZE, RAT_entry) return_whole_fRAM();
		Vector#(`REGFILE_SIZE, RAT_entry) fRAM_return;
		for(Integer i = 0; i < `REGFILE_SIZE; i=i+1)
		   fRAM_return[i] = fRAM[i];
		return fRAM_return;
    endmethod

endmodule : mkPrf_fRAM
endpackage  : fRAM
