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

package wakeup;

import riscv_types ::*;
import Vector ::*;
import DefaultValue ::*;
`include "defined_parameters.bsv"

interface IfcPrf_wakeup;

//input methods
method Action get_erob_entries(Vector#(`ENTRY_ROB_SIZE, Entry_rob_type) entry_rob);
method Action broadcast_of_functional_units(Vector#(`FUN_UNITS, Broadcast_type) fun_unit_broadcast); 
method Action get_squash_pc(Bit#(`REG_WIDTH) squash_pc);
method Action get_if_squash(Bool squash);
method Action get_if_exception(TrapCause exception);
method Action result_from_fununits(Vector#(`FUN_UNITS, Maybe#(Result_bypass_type)) _result);

//output methods 
method Vector#(`ENTRY_ROB_SIZE, Bool)  if_execute_done();  
method Vector#(`ENTRY_ROB_SIZE, Bool)  if_op1_ready_in_erob();
method Vector#(`ENTRY_ROB_SIZE, Bool)  if_op2_ready_in_erob();
method Vector#(`ENTRY_ROB_SIZE, Bool)  if_entry_rob_squash();
method Vector#(`ENTRY_ROB_SIZE, TrapCause)  if_entry_rob_exception();  
method Vector#(`ENTRY_ROB_SIZE, Maybe#(Bit#(`REG_WIDTH)))  send_squash_value();

endinterface

(*synthesize*)
module mkPrf_wakeup(IfcPrf_wakeup);

Wire#(Entry_rob_type)  wr_entry_rob[`ENTRY_ROB_SIZE];
    for(Integer i = 0; i < `ENTRY_ROB_SIZE; i = i + 1)  
	wr_entry_rob[i] <- mkDWire(defaultValue);					
Wire#(Broadcast_type) wr_broadcast[`FUN_UNITS];
    for(Integer i = 0; i < `FUN_UNITS; i=i+1)
	wr_broadcast[i] <- mkDWire(
			    Broadcast_type{          //TODO: change this
				valid: False,
		 		dest_tag: ?});
    
Vector#(`FUN_UNITS, Wire#(Maybe#(Result_bypass_type)))   wr_result_bypass <- replicateM(mkDWire(tagged Invalid));

Vector#(`ENTRY_ROB_SIZE, Wire#(Maybe#(Bit#(`REG_WIDTH))))   wr_squash_buf <- replicateM(mkDWire(tagged Invalid));

Wire#(Bit#(`REG_WIDTH)) wr_squash_pc <- mkDWire(0);
Wire#(Bool) wr_squash <- mkDWire(False);

Wire#(TrapCause)	wr_load_store_exception  <- mkDWire(No_trap);

Vector#(`ENTRY_ROB_SIZE, Wire#(Bool)) wr_erob_execute_done <- replicateM(mkDWire(False));
		 
Vector#(`ENTRY_ROB_SIZE, Wire#(Bool))   wr_erob_op1_ready <- replicateM(mkDWire(False));

Vector#(`ENTRY_ROB_SIZE, Wire#(Bool))   wr_erob_op2_ready <- replicateM(mkDWire(False));

Vector#(`ENTRY_ROB_SIZE, Wire#(Bool))   wr_erob_squash <- replicateM(mkDWire(False));

Vector#(`ENTRY_ROB_SIZE, Wire#(TrapCause))   wr_erob_exception <- replicateM(mkDWire(No_trap));

Rules rs_wk = emptyRules;
   
for(Integer i=0;i<`ENTRY_ROB_SIZE;i=i+1)
       begin
	    Rules r_wk = (rules 
		rule rl_wakeup;
			
			//this vector does the primary matching of validities, thread id and instr type
			Vector#(`FUN_UNITS, Bool) lv_primary_match;

			
			//Match thread_id, validity of erob entry, validity of 
			//broadcast packet and instruction type.
			for(Integer j=0;j<`FUN_UNITS;j=j+1)
   
			    lv_primary_match[j]  = wr_broadcast[j].valid &&
							    wr_entry_rob[i].valid;

			//Match destination tag, mark the instruction as completed.
			//this should be changed if number of FUs increased in the design.
			//dest_op of store and conditional branch instructions is not used.
			//So, they should be excluded from setting execute_done.
			if((isValid(wr_result_bypass[0]) && wr_entry_rob[i].dest_op==validValue(wr_result_bypass[0]).dest_tag) ||
			   (isValid(wr_result_bypass[1]) && wr_entry_rob[i].dest_op==validValue(wr_result_bypass[1]).dest_tag) ||
			   (isValid(wr_result_bypass[2]) && wr_entry_rob[i].dest_op==validValue(wr_result_bypass[2]).dest_tag) ||
			   (isValid(wr_result_bypass[3]) && wr_entry_rob[i].dest_op==validValue(wr_result_bypass[3]).dest_tag) || 
			   (isValid(wr_result_bypass[4]) && wr_entry_rob[i].dest_op==validValue(wr_result_bypass[4]).dest_tag)) 
			   begin
				 wr_erob_execute_done[i] <= True;
			
				  $display("Time:%d\nentry_rob %d execution done", $time, i);
			
			   end
			
			if(isValid(wr_result_bypass[3]) && wr_entry_rob[i].dest_op==validValue(wr_result_bypass[3]).dest_tag)
			   begin
				  wr_squash_buf[i] <= tagged Valid wr_squash_pc;
				  wr_erob_squash[i] <= wr_squash;
				  wr_erob_exception[i] <= wr_load_store_exception;
				  $display("writing into %d squash %d", i, wr_squash);
			   end


			//Match op1 and op2 tags.
			if((wr_broadcast[0].valid && wr_entry_rob[i].op_1==wr_broadcast[0].dest_tag) ||
			   (wr_broadcast[1].valid && wr_entry_rob[i].op_1==wr_broadcast[1].dest_tag) ||
			   (wr_broadcast[2].valid && wr_entry_rob[i].op_1==wr_broadcast[2].dest_tag) ||
			   (wr_broadcast[3].valid && wr_entry_rob[i].op_1==wr_broadcast[3].dest_tag) ||
			   (wr_broadcast[4].valid && wr_entry_rob[i].op_1==wr_broadcast[4].dest_tag)) 
			   begin
				  wr_erob_op1_ready[i] <= True;

				  $display(" entry_rob %d op_1 woke up", i);

			   end
			
			if((wr_broadcast[0].valid && wr_entry_rob[i].op_2==wr_broadcast[0].dest_tag) ||
			   (wr_broadcast[1].valid && wr_entry_rob[i].op_2==wr_broadcast[1].dest_tag) ||
			   (wr_broadcast[2].valid && wr_entry_rob[i].op_2==wr_broadcast[2].dest_tag) ||
			   (wr_broadcast[3].valid && wr_entry_rob[i].op_2==wr_broadcast[3].dest_tag) ||
			   (wr_broadcast[4].valid && wr_entry_rob[i].op_2==wr_broadcast[4].dest_tag)) 
			   begin
				  wr_erob_op2_ready[i] <= True;

				  $display(" entry_rob %d op_2 woke up", i);

			   end
		    endrule
		endrules);
	rs_wk = rJoinConflictFree(rs_wk, r_wk);
end
addRules(rs_wk);

//input methods
method Action get_erob_entries(Vector#(`ENTRY_ROB_SIZE, Entry_rob_type) entry_rob);
    for(Integer i = 0; i < `ENTRY_ROB_SIZE; i = i + 1) begin
	wr_entry_rob[i]  <=  entry_rob[i]; 
	end
endmethod

method Action broadcast_of_functional_units(Vector#(`FUN_UNITS, Broadcast_type) fun_unit_broadcast); 
    for(Integer i = 0; i < `FUN_UNITS; i = i + 1)
	wr_broadcast[i]  <=  fun_unit_broadcast[i];
endmethod

method Action get_squash_pc(Bit#(`REG_WIDTH) squash_pc);
	wr_squash_pc  <=  squash_pc; 
endmethod
    
method Action get_if_squash(Bool squash);
	wr_squash <= squash;
endmethod

method Action get_if_exception(TrapCause exception);
	wr_load_store_exception <= exception;
endmethod

method Action result_from_fununits(Vector#(`FUN_UNITS, Maybe#(Result_bypass_type)) _result);
	for(Integer i = 0; i < `FUN_UNITS; i = i + 1)
		wr_result_bypass[i] <= _result[i];
endmethod
 
//output methods 

method Vector#(`ENTRY_ROB_SIZE, Bool)  if_execute_done();  
    Vector#(`ENTRY_ROB_SIZE, Bool) execute_slots;
    for(Integer i = 0; i < `ENTRY_ROB_SIZE; i=i+1)
	execute_slots[i] = wr_erob_execute_done[i];
    return execute_slots;
endmethod

method Vector#(`ENTRY_ROB_SIZE, Bool)  if_op1_ready_in_erob();
    Vector#(`ENTRY_ROB_SIZE, Bool) op1_slots;
    for(Integer i = 0; i < `ENTRY_ROB_SIZE; i=i+1)
	op1_slots[i] = wr_erob_op1_ready[i];
    return op1_slots;
endmethod

method Vector#(`ENTRY_ROB_SIZE, Bool)  if_op2_ready_in_erob();
    Vector#(`ENTRY_ROB_SIZE, Bool) op2_slots;
    for(Integer i = 0; i < `ENTRY_ROB_SIZE; i=i+1)
	op2_slots[i] = wr_erob_op2_ready[i];
    return op2_slots;
endmethod

method Vector#(`ENTRY_ROB_SIZE, Bool)  if_entry_rob_squash();  
    Vector#(`ENTRY_ROB_SIZE, Bool) if_squash_slots;
    for(Integer i = 0; i < `ENTRY_ROB_SIZE; i=i+1)
	if_squash_slots[i] = wr_erob_squash[i];
    return if_squash_slots;
endmethod

method Vector#(`ENTRY_ROB_SIZE, TrapCause)  if_entry_rob_exception();  
    Vector#(`ENTRY_ROB_SIZE, TrapCause) exception_slots;
    for(Integer i = 0; i < `ENTRY_ROB_SIZE; i=i+1)
	exception_slots[i] = wr_erob_exception[i];
    return exception_slots;
endmethod

method Vector#(`ENTRY_ROB_SIZE, Maybe#(Bit#(`REG_WIDTH)))  send_squash_value();
    Vector#(`ENTRY_ROB_SIZE, Maybe#(Bit#(`REG_WIDTH))) squash_value_slots;
    for(Integer i = 0; i < `ENTRY_ROB_SIZE; i=i+1)
	squash_value_slots[i] = wr_squash_buf[i];
    return squash_value_slots;
endmethod

endmodule

endpackage

