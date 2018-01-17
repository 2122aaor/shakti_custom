/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

Author Names : Rahul Bodduna
Email ID : rahul.bodduna@gmail.com
*/

package rRAM;

import riscv_types::*;
import Vector::*;
import ConfigReg::*;
`include "defined_parameters.bsv" 

interface  IfcPrf_rRAM;
    //Input method 
    method Action update_rRAM_1(Bit#(TLog#(`REGFILE_SIZE)) entry_slot, RAT_entry entry_val);
    method Action update_rRAM_2(Bit#(TLog#(`REGFILE_SIZE)) entry_slot, RAT_entry entry_val);
    //output method
    method RAT_entry return_val_in_rRAM_1(Bit#(TLog#(`REGFILE_SIZE)) entry_slot);
    method RAT_entry return_val_in_rRAM_2(Bit#(TLog#(`REGFILE_SIZE)) entry_slot);
    method Vector#(`REGFILE_SIZE, RAT_entry) return_whole_rRAM();

endinterface

(*synthesize*)
module mkPrf_rRAM(IfcPrf_rRAM);
    ConfigReg#(RAT_entry) rRAM[`REGFILE_SIZE];
    
    //Initial mapping Ri -> Ti for RRAM 
    for(Integer i=0; i<`REGFILE_SIZE; i=i+1)
           begin
         	 rRAM[i] <- mkConfigReg(fromInteger(i));
           end

    method Action update_rRAM_1(Bit#(TLog#(`REGFILE_SIZE)) entry_slot, RAT_entry entry_val);
         rRAM[entry_slot] <= entry_val;
    endmethod
    
    method Action update_rRAM_2(Bit#(TLog#(`REGFILE_SIZE)) entry_slot, RAT_entry entry_val);
	 rRAM[entry_slot] <= entry_val;
    endmethod

    method RAT_entry return_val_in_rRAM_1(Bit#(TLog#(`REGFILE_SIZE)) entry_slot);
         return rRAM[entry_slot];
    endmethod 

    method RAT_entry return_val_in_rRAM_2(Bit#(TLog#(`REGFILE_SIZE)) entry_slot);
         return rRAM[entry_slot];
    endmethod 

    method Vector#(`REGFILE_SIZE, RAT_entry) return_whole_rRAM();
	 Vector#(`REGFILE_SIZE, RAT_entry) rRAM_return;
	 for(Integer i = 0; i < `REGFILE_SIZE; i=i+1)
	    rRAM_return[i] = rRAM[i];
	 return rRAM_return;
    endmethod

endmodule : mkPrf_rRAM
endpackage  : rRAM
