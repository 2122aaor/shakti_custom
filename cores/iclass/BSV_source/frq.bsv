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

package frq;

import riscv_types::*;
import Vector::*;
import ConfigReg::*;
import GetPut::*;
`include "defined_parameters.bsv" 

interface IfcPrf_frq;

//input methods
method Action update_frq_1(FRQ_entry entry);
method Action update_frq_2(FRQ_entry entry);
method Action update_tail(Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)) tail);
method Action update_head(Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)) head);
method Action get_rob_head(Bit#(TLog#(`ENTRY_ROB_SIZE)) rob_head);
method Action reset_head();
method Action reset_tail();
method Action reset_entries_of_FRQ();

//output methods
interface Get#(FRQ_to_map) to_map;
method Bool if_frq_empty;
//method FRQ_entry return_frq_entries_1;
//method FRQ_entry return_frq_entries_2;

/****************** VERIFICATION ENVIRONMENT ******************/
method Vector#(`ENTRY_ROB_SIZE, FRQ_entry) return_whole_frq;
method Bit#(TLog#(`ENTRY_ROB_SIZE)) return_frq_head;
method Bit#(TLog#(`ENTRY_ROB_SIZE)) return_frq_tail;
/****************** VERIFICATION ENVIRONMENT ******************/

endinterface

(*synthesize*)
module mkPrf_frq(IfcPrf_frq);

   //FRQ is a circular buffer. We enqueue at tail and dequeue at head
   //Initially, FRQ has all the registers except the default mapped ones
   //T0-T31 full and T32 till the end are free. 
   ConfigReg#(FRQ_entry) frq[`ENTRY_ROB_SIZE];
   for(Integer i=0; i<`ENTRY_ROB_SIZE;i=i+1)
		 frq[i] <- mkConfigReg(FRQ_entry {
										  free_reg: fromInteger(i + `REGFILE_SIZE),
										  valid: True});
   //Head at T0
   ConfigReg#(Bit#(TLog#(`ENTRY_ROB_SIZE))) rg_frq_head <- mkConfigReg(0);
   //Tail at T0
   ConfigReg#(Bit#(TLog#(`ENTRY_ROB_SIZE))) rg_frq_tail <- mkConfigReg(0);
   
   //wire to check if frq is empty
   Wire#(Bool) wr_frq_empty <- mkDWire(False);
   Wire#(Bit#(TLog#(`ENTRY_ROB_SIZE))) wr_entry_rob_head <- mkDWire(0);

       rule rl_check_frq_empty;
             
             if((rg_frq_head==rg_frq_tail && !frq[wr_entry_rob_head].valid) ||
           	 (rg_frq_head+1==rg_frq_tail))
           	 begin
           		wr_frq_empty <= True;
           		
           		$display("Time%d\nFRQ is empty",$time);
           		
           	 end
             
      endrule
//input methods
method Action update_frq_1(FRQ_entry entry);
    if(entry.valid == True) begin
	frq[rg_frq_tail] <= entry;
    end
endmethod 

method Action update_frq_2(FRQ_entry entry);
    if(entry.valid == True) begin
	frq[rg_frq_tail+1] <= entry;
    end
endmethod

method Action update_tail(Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)) tail);
    rg_frq_tail <= rg_frq_tail + zeroExtend(tail);
endmethod

method Action update_head(Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)) head);
    rg_frq_head <= rg_frq_head + zeroExtend(head);
endmethod

method Action get_rob_head(Bit#(TLog#(`ENTRY_ROB_SIZE)) rob_head);
    wr_entry_rob_head <= rob_head;
endmethod

method Action reset_head();
    rg_frq_head <= 0;
endmethod

method Action reset_tail();
    rg_frq_tail <= 0;
endmethod

method Action reset_entries_of_FRQ();
    for(Integer i = 0; i <`ENTRY_ROB_SIZE; i=i+1)
	frq[i].valid <= True;
endmethod

//output methods
interface to_map = interface Get
					method ActionValue#(FRQ_to_map) get();
						let lv_FRQ_to_map = FRQ_to_map { entry_1  : frq[rg_frq_head],
														 entry_2  : frq[rg_frq_head+1]
														};
						return lv_FRQ_to_map;
					endmethod
				   endinterface;

method Bool if_frq_empty;
    return wr_frq_empty;
endmethod

//method FRQ_entry return_frq_entries_1;
//    return frq[rg_frq_head];
//endmethod
//
//method FRQ_entry return_frq_entries_2;
//    return frq[rg_frq_head+1];
//endmethod

/******************** VERIFICATION ENVIRONMENT *********************/
method Vector#(`ENTRY_ROB_SIZE, FRQ_entry) return_whole_frq;
    Vector#(`ENTRY_ROB_SIZE, FRQ_entry) frq_values; 
    for(Integer i = 0; i < `ENTRY_ROB_SIZE; i = i +1)
	frq_values[i] = frq[i];
    return frq_values;
endmethod
	
method Bit#(TLog#(`ENTRY_ROB_SIZE)) return_frq_head;
    return rg_frq_head;
endmethod

method Bit#(TLog#(`ENTRY_ROB_SIZE)) return_frq_tail;
    return rg_frq_tail;
endmethod

endmodule
endpackage 
