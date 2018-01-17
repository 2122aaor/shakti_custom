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

package IQ;

import riscv_types::*;
import Vector::*;
import ConfigReg::*;
import DefaultValue:: *;
import GetPut:: *;

`include "defined_parameters.bsv" 

interface Ifc_Map_to_IQ;

method Action update_entry_rob_execute_done(Bool entry);
method Action update_if_op1_ready_in_erob(Bool entry);
method Action update_if_op2_ready_in_erob(Bool entry);
method Action update_if_entry_rob_squash(Bool entry);
method Action update_squash_value(Bit#(`REG_WIDTH) entry);
method Action update_selected_for_execution(Bool entry);
method Action update_entry_rob_exception(TrapCause exception);

endinterface 

interface Ifc_broadcast_update;
    method Action update_Prf_valid(Bit#(TLog#(`PRF_SIZE)) valid_entry);
endinterface

interface IfcPrf_IQ;
//input interface
interface Vector#(`ENTRY_ROB_SIZE, Ifc_Map_to_IQ) map_to_IQ_ifc;
interface Vector#(`ALU_UNITS, Ifc_broadcast_update) update_broadcast_ifc;
//input methods
method Action fill_imm_entries_1(Bit#(`REG_WIDTH) imm_entry);
method Action fill_imm_entries_2(Bit#(`REG_WIDTH) imm_entry);
method Action update_imm_tail(Bit#(TLog#(`IMM_BUF_SIZE)) imm_tail);
method Action  fill_entry_rob_1(Entry_rob_type entry_rob_entry); 
method Action  fill_entry_rob_2(Entry_rob_type entry_rob_entry); 
method Action fill_entry_rob_op_1_ready_1(Prf_info entry_rob_op_1_ready_entry);
method Action fill_entry_rob_op_1_ready_2(Prf_info entry_rob_op_1_ready_entry);
method Action fill_entry_rob_op_2_ready_1(Prf_info entry_rob_op_2_ready_entry);
method Action fill_entry_rob_op_2_ready_2(Prf_info entry_rob_op_2_ready_entry);
method Action fill_entry_rob_execute_done_1(Bool entry_rob_execute_done_entry);
method Action fill_entry_rob_execute_done_2(Bool entry_rob_execute_done_entry);
method Action fill_entry_rob_squash_1(Bool entry_rob_squash_entry);
method Action fill_entry_rob_squash_2(Bool entry_rob_squash_entry);
method Action fill_squash_buf_1(Bit#(`REG_WIDTH) squash_buf_entry);
method Action fill_squash_buf_2(Bit#(`REG_WIDTH) squash_buf_entry);
method Action fill_selected_for_exec_1(Bool selected_for_exec_entry);
method Action fill_selected_for_exec_2(Bool selected_for_exec_entry);
method Action fill_entry_rob_execution_1(TrapCause exception);
method Action fill_entry_rob_execution_2(TrapCause exception);
method Action update_rob_tail(Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)) rob_tail);
method Action invalidate_erob_1;
method Action invalidate_erob_2;
method Action invalidate_imm_1(Bit#(TLog#(`IMM_BUF_SIZE)) invalid_imm);
method Action invalidate_imm_2(Bit#(TLog#(`IMM_BUF_SIZE)) invalid_imm);
method Action invalidate_prf_valid_1(Update_map_prf prf_slot);
method Action invalidate_prf_valid_2(Update_map_prf prf_slot);
method Action update_entry_rob_head(Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)) add_to_head);
method Action update_imm_buf_head(Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)) add_to_head);
method Action update_imm_head(Bit#(TLog#(`IMM_BUF_SIZE)) imm_head);
method Action invalidate_imm(Bool invalidate);
method Action update_Prf_valid_1(Bit#(TLog#(`PRF_SIZE)) valid_entry);
method Action update_Prf_valid_2(Bit#(TLog#(`PRF_SIZE)) valid_entry);
method Action update_Prf_valid_3(Bit#(TLog#(`PRF_SIZE)) valid_entry);
method Action reset_rob_head();
method Action reset_rob_tail();
method Action reset_entries_of_EROB();

//output methods
interface Get#(IQ_to_map) to_map;
method Bool if_erob_empty;
method Bool if_erob_full;
method Bool if_imm_buf_full;
method Vector#(`ENTRY_ROB_SIZE, Entry_rob_type) rob_entries;
method Vector#(`IMM_BUF_SIZE, Imm_buf_entry) imm_entries;
method Vector#(`ENTRY_ROB_SIZE, Bool) selected_for_execution;
method Vector#(`ENTRY_ROB_SIZE, Bit#(`MAX_LATENCY)) op1_ready_info;
method Vector#(`ENTRY_ROB_SIZE, Bit#(`MAX_LATENCY)) op2_ready_info;
method Vector#(`ENTRY_ROB_SIZE, Bool) squash_buf_entries;
method Vector#(`FETCH_WIDTH, Entry_rob_type) send_entry_rob_head_entries;
method Vector#(`FETCH_WIDTH, Imm_buf_entry) send_imm_buf_head_entries;
method Vector#(`FETCH_WIDTH, Bool) send_heads_execute_done;
method Vector#(`FETCH_WIDTH, TrapCause) send_heads_exception;
method Vector#(`FETCH_WIDTH, Bool) send_heads_rob_squash; 
method Vector#(`FETCH_WIDTH, Bit#(`REG_WIDTH)) send_heads_squash_value; 

/********************* VERIFICATION ENVIRONMENT ********************/
method Bit#(TLog#(`ENTRY_ROB_SIZE)) send_entry_rob_head; 
method Bit#(TLog#(`ENTRY_ROB_SIZE)) send_entry_rob_tail; 
method Bit#(TLog#(`IMM_BUF_SIZE)) send_imm_buf_head;
method Bit#(TLog#(`IMM_BUF_SIZE)) send_imm_buf_tail;
method Vector#(`PRF_SIZE, Bool) send_prf_entries;
/********************* VERIFICATION ENVIRONMENT ********************/


endinterface
//(*conflict_free = "invalidate_erob_1, invalidate_erob_2, fill_imm_entries_1, fill_imm_entries_2"*)
(*synthesize*)
module mkPrf_IQ(IfcPrf_IQ);

   Vector#(`ENTRY_ROB_SIZE, Reg#(Entry_rob_type)) entry_rob <- replicateM(mkConfigReg(defaultValue));
	  
   

   //Holds the selected_for_exec bool   
   Vector#(`ENTRY_ROB_SIZE, Reg#(Bool)) selected_for_exec <- replicateM(mkConfigReg(False));

   //'operand ready' bits are put in separate register due to bluespec constraints
   Vector#(`ENTRY_ROB_SIZE, Reg#(Bool)) entry_rob_match_op_1 <- replicateM(mkConfigReg(False));
   Vector#(`ENTRY_ROB_SIZE, Reg#(Bool)) entry_rob_match_op_2 <- replicateM(mkConfigReg(False));
   
   //shift holds the result delay in inverted one's complement form. 
   //6 here is the number of cycles for multiplication
   Vector#(`ENTRY_ROB_SIZE, Reg#(Bit#(`MAX_LATENCY))) entry_rob_shift_op_1 <- replicateM(mkConfigReg(0));
   Vector#(`ENTRY_ROB_SIZE, Reg#(Bit#(`MAX_LATENCY))) entry_rob_shift_op_2 <- replicateM(mkConfigReg(0));
   
   //once there is a match between tag broadcast and operand tag, 
   //value in the delay field is copied to shift field and shifting is enabled.
   Vector#(`ENTRY_ROB_SIZE, Reg#(Bit#(`MAX_LATENCY))) entry_rob_delay_op_1 <- replicateM(mkConfigReg(0));
   Vector#(`ENTRY_ROB_SIZE, Reg#(Bit#(`MAX_LATENCY))) entry_rob_delay_op_2 <- replicateM(mkConfigReg(0));


   Vector#(`ENTRY_ROB_SIZE, Reg#(Bool)) entry_rob_execute_done <- replicateM(mkConfigReg(False));
   Vector#(`ENTRY_ROB_SIZE, Reg#(Bool)) entry_rob_squash <- replicateM(mkConfigReg(False));
   Vector#(`ENTRY_ROB_SIZE, Reg#(TrapCause)) entry_rob_exception <- replicateM(mkConfigReg(No_trap));
   
   
   //Stores the squash pc of each instruction
   Vector#(`ENTRY_ROB_SIZE, Reg#(Bit#(`REG_WIDTH))) squash_buf <- replicateM(mkConfigReg(0));
   
   Reg#(Bit#(TLog#(`ENTRY_ROB_SIZE))) rg_erob_tail <- mkConfigReg(0);
   Reg#(Bit#(TLog#(`ENTRY_ROB_SIZE))) rg_erob_head <- mkConfigReg(0);
  

   Vector#(`PRF_SIZE, Array#(Reg#(Bool))) prf_match <- replicateM(mkCReg(1,False));
   Vector#(`PRF_SIZE, Array#(Reg#(Bit#(`MAX_LATENCY)))) prf_shift <- replicateM(mkCReg(1,6'b111111));
   Vector#(`PRF_SIZE, Array#(Reg#(Bit#(`MAX_LATENCY)))) prf_delay <- replicateM(mkCReg(1,6'b111111));

   Wire#(Bool) wr_erob_full <- mkDWire(False);
   Wire#(Bool) wr_erob_empty <- mkDWire(False);
   
   Vector#(`ALU_UNITS, Wire#(Bit#(TLog#(`PRF_SIZE)))) wr_prf_slot <- replicateM(mkWire());
   Vector#(`FETCH_WIDTH, Wire#(Entry_rob_type)) wr_erob_fill <- replicateM(mkWire());
   Vector#(`FETCH_WIDTH, Wire#(Imm_buf_entry)) wr_imm_buf_fill <- replicateM(mkWire());
   Vector#(`FETCH_WIDTH, Wire#(Bool)) wr_seleted_exec <- replicateM(mkWire());

   //'operand ready' bits are put in separate register due to bluespec constraints
   Vector#(`FETCH_WIDTH, Wire#(Bool)) wr_entry_rob_match_op_1 <- replicateM(mkWire());
   Vector#(`FETCH_WIDTH, Wire#(Bool)) wr_entry_rob_match_op_2 <- replicateM(mkWire());
   
   //shift holds the result delay in inverted one's complement form. 
   //6 here is the number of cycles for multiplication
   Vector#(`FETCH_WIDTH, Wire#(Bit#(`MAX_LATENCY))) wr_entry_rob_shift_op_1 <- replicateM(mkWire());
   Vector#(`FETCH_WIDTH, Wire#(Bit#(`MAX_LATENCY))) wr_entry_rob_shift_op_2 <- replicateM(mkWire());
   
   //once there is a match between tag broadcast and operand tag, 
   //value in the delay field is copied to shift field and shifting is enabled.
   Vector#(`FETCH_WIDTH, Wire#(Bit#(`MAX_LATENCY))) wr_entry_rob_delay_op_1 <- replicateM(mkWire());
   Vector#(`FETCH_WIDTH, Wire#(Bit#(`MAX_LATENCY))) wr_entry_rob_delay_op_2 <- replicateM(mkWire());

   //A buffer to store immediate operands. Is a circular queue filled during dispatch
   //and an entry is reclaimed at the time of commit.
   Vector#(`IMM_BUF_SIZE, Array#(Reg#(Imm_buf_entry))) imm_buf <- replicateM(mkCReg(1, defaultValue));
	  
   
   //registers to store head and tail pointers of imm_buf
   Reg#(Bit#(TLog#(`IMM_BUF_SIZE))) rg_imm_buf_head <- mkConfigReg(0);
   Reg#(Bit#(TLog#(`IMM_BUF_SIZE))) rg_imm_buf_tail <- mkConfigReg(0);

   //wire to tell if imm_buf is full
   Wire#(Bool) wr_imm_buf_full <- mkDWire(False);

   rule rl_display_rob_head_and_tail;
    $display("the erob head %d %d", rg_erob_head, $time);
    $display("the erob tail %d %d", rg_erob_tail, $time);
	$display("the imm buf head %d %d", rg_imm_buf_head, $time);
	$display("the imm buf tail %d %d", rg_imm_buf_tail, $time);
   endrule

   for(Integer i = 0; i < `ALU_UNITS; i = i+1) begin 

   	rule rl_prf_valid_for_alu_broadcast;
		prf_match[wr_prf_slot[i]][0] <= True;
		prf_shift[wr_prf_slot[i]][0] <= prf_delay[wr_prf_slot[i]][0];
   	endrule

   end


   for(Integer i = 0; i < `FETCH_WIDTH; i = i+1) begin 

   	rule rl_entry_rob_fill;
   	 	entry_rob[rg_erob_tail+fromInteger(i)] <= wr_erob_fill[i];
   	endrule

	rule rl_fill_imm_entries;
   	 	imm_buf[rg_imm_buf_tail+fromInteger(i)][0] <= wr_imm_buf_fill[i];
	endrule

	rule rl_fill_selected_for_exec;
		selected_for_exec[rg_erob_tail+fromInteger(i)] <= wr_seleted_exec[i];
	endrule

	rule rl_fill_rob_op1_ready;
		entry_rob_match_op_1[rg_erob_tail+fromInteger(i)] <= wr_entry_rob_match_op_1[i]; 
		entry_rob_shift_op_1[rg_erob_tail+fromInteger(i)] <= wr_entry_rob_shift_op_1[i];
		entry_rob_delay_op_1[rg_erob_tail+fromInteger(i)] <= wr_entry_rob_delay_op_1[i];	
	endrule

	rule rl_fill_rob_op2_ready;
		entry_rob_match_op_2[rg_erob_tail+fromInteger(i)] <= wr_entry_rob_match_op_2[i]; 
		entry_rob_shift_op_2[rg_erob_tail+fromInteger(i)] <= wr_entry_rob_shift_op_2[i];
		entry_rob_delay_op_2[rg_erob_tail+fromInteger(i)] <= wr_entry_rob_delay_op_2[i];	
	endrule
   end


   //The three rules below in the conflict free attribute update 
   //entry_rob_shift_op registers without conflicts.
   Rules rs_shift_erob = emptyRules;

   for(Integer i=0;i<`ENTRY_ROB_SIZE;i=i+1)
	  begin
   
		 Rules r_shift_erob = (rules
								  
			rule rl_shift_erob(entry_rob[i].valid);
	  
			   if(entry_rob_match_op_1[i] && !unpack(entry_rob_shift_op_1[i][0]))
				  entry_rob_shift_op_1[i] <= {entry_rob_shift_op_1[i][`MAX_LATENCY-1], entry_rob_shift_op_1[i][`MAX_LATENCY-1:1]};
				  
			   if(entry_rob_match_op_2[i] && !unpack(entry_rob_shift_op_2[i][0]))
				  entry_rob_shift_op_2[i] <= {entry_rob_shift_op_2[i][`MAX_LATENCY-1], entry_rob_shift_op_2[i][`MAX_LATENCY-1:1]};
				  
			endrule
			endrules);
		 
		 rs_shift_erob = rJoinConflictFree(r_shift_erob, rs_shift_erob);

	  end
addRules(rs_shift_erob);

   
   Rules rs_shift_prf = emptyRules;
   
   for(Integer i=0;i<`PRF_SIZE;i=i+1)
	  begin
		 
		 Rules r_shift_prf = (rules 

			rule rl_shift_prf(prf_match[i][0] && !unpack(prf_shift[i][0][0]));
	  
			   prf_shift[i][0] <= {prf_shift[i][0][`MAX_LATENCY-1],prf_shift[i][0][`MAX_LATENCY-1:1]};
			
			endrule
			endrules);
		 
		 rs_shift_prf = rJoinConflictFree(r_shift_prf, rs_shift_prf);
		 
	  end
addRules(rs_shift_prf);

(*conflict_free = "rl_fill_imm_entries, rl_fill_imm_entries_1" *)
(*conflict_free = "rl_entry_rob_fill, rl_entry_rob_fill_1" *)
(*conflict_free = "rl_prf_valid_for_alu_broadcast, rl_prf_valid_for_alu_broadcast_1" *)
(*conflict_free = "rl_fill_selected_for_exec, rl_fill_selected_for_exec_1" *)
(*conflict_free = "rl_fill_rob_op1_ready, rl_fill_rob_op1_ready_1" *)
(*conflict_free = "rl_fill_rob_op2_ready, rl_fill_rob_op2_ready_1" *)

   rule rl_check_erob_status;
	  
	  if((rg_erob_head==rg_erob_tail && entry_rob[rg_erob_head].valid) ||
		 (rg_erob_tail+1==rg_erob_head))
		 begin
							  
			$display("EROB FULL");
		    
			wr_erob_full <= True;
		 end
			
	  if(rg_erob_head==rg_erob_tail && !entry_rob[rg_erob_head].valid)
		 begin
				  
			$display("EROB EMPTY");
			
			wr_erob_empty <= True;
				  
		 end
		 
		 
   endrule: rl_check_erob_status


   rule rl_check_imm_buf_full;
	  
	  if((rg_imm_buf_head==rg_imm_buf_tail && 
		  imm_buf[rg_imm_buf_head][0].valid) ||
		 (rg_imm_buf_tail+1==rg_imm_buf_head))
		 begin
			$display("Time:%d\nIMM_BUF FULL",$time);
			wr_imm_buf_full <= True;
		 end
	  
   endrule

Vector#(`ENTRY_ROB_SIZE, Ifc_Map_to_IQ) temp_map_to_IQ_ifc;
Vector#(`ALU_UNITS, Ifc_broadcast_update) temp_update_broadcast_ifc;

for(Integer i = 0; i < `ENTRY_ROB_SIZE;i=i+1) begin
    temp_map_to_IQ_ifc[i] =  interface Ifc_Map_to_IQ

				method Action update_entry_rob_execute_done(Bool entry);
				    $display("writing into slot %d %d", i, $time);
				  entry_rob_execute_done[i] <= entry;  
				endmethod
				
				method Action update_if_op1_ready_in_erob(Bool entry);
					if(entry == True) begin
				    	entry_rob_match_op_1[i] <= True;	    
						entry_rob_shift_op_1[i] <= entry_rob_delay_op_1[i];
					end
				endmethod

				method Action update_if_op2_ready_in_erob(Bool entry);
					if(entry == True) begin
				    	entry_rob_match_op_2[i] <= True;	    
						entry_rob_shift_op_2[i] <= entry_rob_delay_op_2[i];
					end
				endmethod

				method Action update_if_entry_rob_squash(Bool entry);
				    entry_rob_squash[i] <= entry;
				endmethod
    
				method Action update_squash_value(Bit#(`REG_WIDTH) entry);
				    squash_buf[i] <= entry; 
				endmethod

				method Action update_selected_for_execution(Bool entry);
				    selected_for_exec[i] <= entry;
				endmethod

				method Action update_entry_rob_exception(TrapCause exception);
					entry_rob_exception[i] <= exception;
				endmethod

			   endinterface;
end

for(Integer j = 0; j < `ALU_UNITS; j=j+1) begin
    temp_update_broadcast_ifc[j]  =  interface Ifc_broadcast_update	
					method Action update_Prf_valid(Bit#(TLog#(`PRF_SIZE)) valid_entry);
					    wr_prf_slot[j] <= valid_entry;
						$display("the_valid_entry %d %d", valid_entry, $time);
					endmethod
				     endinterface;
end

interface map_to_IQ_ifc = temp_map_to_IQ_ifc;
interface update_broadcast_ifc = temp_update_broadcast_ifc;

method Action fill_imm_entries_1(Bit#(`REG_WIDTH) imm_entry);
	wr_imm_buf_fill[0] <= Imm_buf_entry { valid : True, imm : imm_entry};
	//imm_buf[rg_imm_buf_tail[0]][0] <= Imm_buf_entry { valid : True, imm : imm_entry};
	$display("filling the entry at %d %d", rg_imm_buf_tail, $time);
endmethod

method Action fill_imm_entries_2(Bit#(`REG_WIDTH) imm_entry);
	wr_imm_buf_fill[1] <= Imm_buf_entry { valid : True, imm : imm_entry};
	//imm_buf[rg_imm_buf_tail[0] + 1][0] <= Imm_buf_entry { valid : True, imm : imm_entry};
	$display("filling the entry at %d %d", rg_imm_buf_tail+1, $time);
endmethod

method Action update_imm_tail(Bit#(TLog#(`IMM_BUF_SIZE)) imm_tail);
    rg_imm_buf_tail <= imm_tail;
endmethod
    
method Action fill_entry_rob_1(Entry_rob_type entry_rob_entry); 
	wr_erob_fill[0] <= entry_rob_entry;
endmethod
    
method Action fill_entry_rob_2(Entry_rob_type entry_rob_entry); 
	wr_erob_fill[1] <= entry_rob_entry;
endmethod
    
method Action fill_entry_rob_op_1_ready_1(Prf_info entry_rob_op_1_ready_entry);
    wr_entry_rob_match_op_1[0] <= entry_rob_op_1_ready_entry._match;
    wr_entry_rob_shift_op_1[0] <= entry_rob_op_1_ready_entry._shift;
    wr_entry_rob_delay_op_1[0] <= entry_rob_op_1_ready_entry._delay;
endmethod

method Action fill_entry_rob_op_1_ready_2(Prf_info entry_rob_op_1_ready_entry);
    wr_entry_rob_match_op_1[1] <= entry_rob_op_1_ready_entry._match;
    wr_entry_rob_shift_op_1[1] <= entry_rob_op_1_ready_entry._shift;
    wr_entry_rob_delay_op_1[1] <= entry_rob_op_1_ready_entry._delay;
endmethod

method Action fill_entry_rob_op_2_ready_1(Prf_info entry_rob_op_2_ready_entry);
    wr_entry_rob_match_op_2[0] <= entry_rob_op_2_ready_entry._match;
    wr_entry_rob_shift_op_2[0] <= entry_rob_op_2_ready_entry._shift;
    wr_entry_rob_delay_op_2[0] <= entry_rob_op_2_ready_entry._delay;
endmethod

method Action fill_entry_rob_op_2_ready_2(Prf_info entry_rob_op_2_ready_entry);
    wr_entry_rob_match_op_2[1] <= entry_rob_op_2_ready_entry._match;
    wr_entry_rob_shift_op_2[1] <= entry_rob_op_2_ready_entry._shift;
    wr_entry_rob_delay_op_2[1] <= entry_rob_op_2_ready_entry._delay;
endmethod

method Action fill_entry_rob_execute_done_1(Bool entry_rob_execute_done_entry);
    $display("filling_into_slot %d %d", rg_erob_tail, $time);
    entry_rob_execute_done[rg_erob_tail] <= entry_rob_execute_done_entry;
endmethod

method Action fill_entry_rob_execute_done_2(Bool entry_rob_execute_done_entry);
    $display("filling_into_slot %d %d", rg_erob_tail+1, $time);
    entry_rob_execute_done[rg_erob_tail+1] <= entry_rob_execute_done_entry;
endmethod

method Action fill_entry_rob_squash_1(Bool entry_rob_squash_entry);
    entry_rob_squash[rg_erob_tail] <= entry_rob_squash_entry;
endmethod

method Action fill_entry_rob_squash_2(Bool entry_rob_squash_entry);
    entry_rob_squash[rg_erob_tail+1] <= entry_rob_squash_entry;
endmethod

method Action fill_squash_buf_1(Bit#(`REG_WIDTH) squash_buf_entry);
    squash_buf[rg_erob_tail] <= squash_buf_entry;
endmethod

method Action fill_squash_buf_2(Bit#(`REG_WIDTH) squash_buf_entry);
    squash_buf[rg_erob_tail+1] <= squash_buf_entry;
endmethod

method Action fill_selected_for_exec_1(Bool selected_for_exec_entry);
     wr_seleted_exec[0] <= selected_for_exec_entry;
endmethod

method Action fill_selected_for_exec_2(Bool selected_for_exec_entry);
     wr_seleted_exec[1] <= selected_for_exec_entry;
endmethod

method Action fill_entry_rob_execution_1(TrapCause exception);
	entry_rob_exception[rg_erob_tail] <= exception;
endmethod

method Action fill_entry_rob_execution_2(TrapCause exception);
	entry_rob_exception[rg_erob_tail+1] <= exception;
endmethod

method Action update_rob_tail(Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)) rob_tail);
    rg_erob_tail <= rg_erob_tail + zeroExtend(rob_tail);
    $display("Entry rob tail updated at %d at time %d", rg_erob_tail, $time);  
endmethod

method Action invalidate_erob_1;
    entry_rob[rg_erob_head].valid <= False;
endmethod

method Action invalidate_erob_2;
    entry_rob[rg_erob_head+1].valid <= False;
endmethod

method Action invalidate_imm_1(Bit#(TLog#(`IMM_BUF_SIZE)) invalid_imm);
    imm_buf[invalid_imm][0].valid <= False;
endmethod

method Action invalidate_imm_2(Bit#(TLog#(`IMM_BUF_SIZE)) invalid_imm);
    imm_buf[invalid_imm][0].valid <= False;
endmethod

method Action invalidate_prf_valid_1(Update_map_prf prf_msd);
    prf_match[prf_msd.prf_slot][0] <= False;
    prf_shift[prf_msd.prf_slot][0] <= 0; 
	prf_delay[prf_msd.prf_slot][0] <= prf_msd.prf_delay;
endmethod

method Action invalidate_prf_valid_2(Update_map_prf prf_msd);
    prf_match[prf_msd.prf_slot][0] <= False;
    prf_shift[prf_msd.prf_slot][0] <= 0; 
	prf_delay[prf_msd.prf_slot][0] <= prf_msd.prf_delay;
endmethod

method Action update_entry_rob_head(Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)) add_to_head);
    rg_erob_head <= rg_erob_head + zeroExtend(add_to_head);
endmethod

method Action update_imm_buf_head(Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)) add_to_head);
    rg_imm_buf_head <= rg_imm_buf_head + zeroExtend(add_to_head);
endmethod

method Action invalidate_imm(Bool invalidate);
    if(invalidate == True) begin
	for(Integer i = 0; i < `IMM_BUF_SIZE; i = i + 1)
    	    imm_buf[i][0].valid <= False;
    end
endmethod

method Action update_imm_head(Bit#(TLog#(`IMM_BUF_SIZE)) imm_head);
    rg_imm_buf_head <= imm_head;
endmethod

method Action update_Prf_valid_1(Bit#(TLog#(`PRF_SIZE)) valid_entry);
	prf_match[valid_entry][0] <= True;
	prf_shift[valid_entry][0] <= prf_delay[valid_entry][0];
endmethod

method Action update_Prf_valid_2(Bit#(TLog#(`PRF_SIZE)) valid_entry);
	prf_match[valid_entry][0] <= True;
	prf_shift[valid_entry][0] <= prf_delay[valid_entry][0];
endmethod

method Action update_Prf_valid_3(Bit#(TLog#(`PRF_SIZE)) valid_entry);
	prf_match[valid_entry][0] <= True;
	prf_shift[valid_entry][0] <= prf_delay[valid_entry][0];
endmethod

method Action reset_rob_head();
    rg_erob_head <= 0;
endmethod

method Action reset_rob_tail();
    rg_erob_tail <= 0;
endmethod

method Action reset_entries_of_EROB();
    for(Integer i = 0; i < `ENTRY_ROB_SIZE; i=i+1)
	entry_rob[i].valid <= False;
endmethod

//output methods
interface to_map = interface Get
						method ActionValue#(IQ_to_map) get();
							Vector#(`PRF_SIZE, Prf_info) temp_prf;
    						for(Integer i = 0; i < `PRF_SIZE; i=i+1) begin
								temp_prf[i]._match = prf_match[i][0];
								temp_prf[i]._shift = prf_shift[i][0];
								temp_prf[i]._delay = prf_delay[i][0];
							end
							let lv_IQ_to_map = IQ_to_map { prf_entries : temp_prf,
														  imm_buf_tail : rg_imm_buf_tail}; 
							return lv_IQ_to_map;	
						endmethod
				   endinterface;
						
method Bool if_erob_empty;
    return wr_erob_empty;
endmethod

method Bool if_erob_full;
	return wr_erob_full;
endmethod

method Bool if_imm_buf_full;
	return wr_imm_buf_full;
endmethod

method Vector#(`ENTRY_ROB_SIZE, Entry_rob_type) rob_entries;
    Vector#(`ENTRY_ROB_SIZE, Entry_rob_type) temp_rob_entries;
    for(Integer i = 0; i < `ENTRY_ROB_SIZE; i=i+1)
	temp_rob_entries[i] = entry_rob[i];
    return temp_rob_entries;
endmethod
 
method Vector#(`IMM_BUF_SIZE, Imm_buf_entry) imm_entries;
    Vector#(`IMM_BUF_SIZE, Imm_buf_entry) temp_imm_entries;
    for(Integer i = 0; i < `IMM_BUF_SIZE;i=i+1)
	temp_imm_entries[i] = imm_buf[i][0];
    return temp_imm_entries;
endmethod

method Vector#(`ENTRY_ROB_SIZE, Bool) selected_for_execution;
    Vector#(`ENTRY_ROB_SIZE, Bool) temp_selected_slots;
    for(Integer i = 0; i < `ENTRY_ROB_SIZE; i=i+1)
	temp_selected_slots[i] = selected_for_exec[i];
    return temp_selected_slots;
endmethod

method Vector#(`ENTRY_ROB_SIZE, Bit#(`MAX_LATENCY)) op1_ready_info;
    Vector#(`ENTRY_ROB_SIZE, Bit#(`MAX_LATENCY)) temp_if_op1_ready;
    for(Integer i = 0; i < `ENTRY_ROB_SIZE; i=i+1) begin
		temp_if_op1_ready[i] = entry_rob_shift_op_1[i]; 
	end
    return temp_if_op1_ready;
endmethod
    
method Vector#(`ENTRY_ROB_SIZE, Bit#(`MAX_LATENCY)) op2_ready_info;
    Vector#(`ENTRY_ROB_SIZE, Bit#(`MAX_LATENCY)) temp_if_op2_ready;
    for(Integer i = 0; i < `ENTRY_ROB_SIZE; i=i+1) begin
		temp_if_op2_ready[i] = entry_rob_shift_op_2[i]; 
	end
    return temp_if_op2_ready;
endmethod
    
method Vector#(`ENTRY_ROB_SIZE, Bool) squash_buf_entries;
    Vector#(`ENTRY_ROB_SIZE, Bool) temp_entry_rob_squash;
    for(Integer i = 0; i < `ENTRY_ROB_SIZE; i=i+1)
	temp_entry_rob_squash[i] = entry_rob_squash[i]; 
    return temp_entry_rob_squash;
endmethod

method Vector#(`FETCH_WIDTH, Entry_rob_type) send_entry_rob_head_entries;
    Vector#(`FETCH_WIDTH, Entry_rob_type) temp_entry_rob_heads; 
    for(Integer i = 0; i < `FETCH_WIDTH; i=i+1)
	temp_entry_rob_heads[i] = entry_rob[rg_erob_head + fromInteger(i)];
    return temp_entry_rob_heads;
endmethod

method Vector#(`FETCH_WIDTH, Imm_buf_entry) send_imm_buf_head_entries;
	Vector#(`FETCH_WIDTH, Imm_buf_entry) temp_imm_heads; 
    for(Integer i = 0; i < `FETCH_WIDTH; i=i+1)
	temp_imm_heads[i] = imm_buf[rg_erob_head + fromInteger(i)][0];
    return temp_imm_heads;
endmethod

method Vector#(`FETCH_WIDTH, Bool) send_heads_execute_done;
    Vector#(`FETCH_WIDTH, Bool) temp_heads_execute_done;
    for(Integer i = 0; i < `FETCH_WIDTH; i=i+1)
	temp_heads_execute_done[i] = entry_rob_execute_done[rg_erob_head + fromInteger(i)];
    return temp_heads_execute_done;
endmethod
    
method Vector#(`FETCH_WIDTH, TrapCause) send_heads_exception;
    Vector#(`FETCH_WIDTH, TrapCause) temp_heads_exception;
    for(Integer i = 0; i < `FETCH_WIDTH; i=i+1)
	temp_heads_exception[i] = entry_rob_exception[rg_erob_head + fromInteger(i)];
    return temp_heads_exception;
endmethod
    
method Vector#(`FETCH_WIDTH, Bool) send_heads_rob_squash; 
    Vector#(`FETCH_WIDTH, Bool) temp_heads_rob_squash;
    for(Integer i = 0; i < `FETCH_WIDTH; i=i+1)
	temp_heads_rob_squash[i] = entry_rob_squash[rg_erob_head + fromInteger(i)];
    return temp_heads_rob_squash;
endmethod
    
method Vector#(`FETCH_WIDTH, Bit#(`REG_WIDTH)) send_heads_squash_value; 
    Vector#(`FETCH_WIDTH, Bit#(`REG_WIDTH)) temp_heads_squash_value;
    for(Integer i = 0; i < `FETCH_WIDTH; i=i+1)
	temp_heads_squash_value[i] = squash_buf[rg_erob_head + fromInteger(i)];
    return temp_heads_squash_value;
endmethod

/////////////////////////////////////////////////////////////////////////////////
///VERIFICATION ENVIRONMENT 
/////////////////////////////////////////////////////////////////////////////////

method Bit#(TLog#(`ENTRY_ROB_SIZE)) send_entry_rob_head; 
    return rg_erob_head;
endmethod

method Bit#(TLog#(`ENTRY_ROB_SIZE)) send_entry_rob_tail; 
    return rg_erob_tail;
endmethod

method Bit#(TLog#(`IMM_BUF_SIZE)) send_imm_buf_head;
    return rg_imm_buf_head;
endmethod

method Bit#(TLog#(`IMM_BUF_SIZE)) send_imm_buf_tail;
    return rg_imm_buf_tail;
endmethod

method Vector#(`PRF_SIZE, Bool) send_prf_entries;
    Vector#(`PRF_SIZE, Bool) temp_prf;
    for(Integer i = 0; i < `PRF_SIZE; i=i+1)
	temp_prf[i] = prf_match[i][0];
    return temp_prf; 
endmethod

/////////////////////////////////////////////////////////////////////////////////
///VERIFICATION ENVIRONMENT
/////////////////////////////////////////////////////////////////////////////////

endmodule
endpackage 

