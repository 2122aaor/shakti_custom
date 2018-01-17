/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

Author Names : Rahul Bodduna, N Sireesh.
Email ID : rahul.bodduna@gmail.com */

package select_grant;

import Encoder_tree:: *;
import riscv_types:: *;
import Vector :: *;
import FIFO:: *;
import DefaultValue ::*;

import SpecialFIFOs ::*;
`include "defined_parameters.bsv"


interface IfcPrf_select_grant;

//input methods
method Action get_rob_details(Vector#(`ENTRY_ROB_SIZE, Entry_rob_type)  erob_data);
method Action get_revert_front_end(Bool  revert_map);
method Action get_imm_value(Vector#(`IMM_BUF_SIZE, Imm_buf_entry) data_from_IMM_buf); 
method Action get_if_selected_for_exec(Vector#(`ENTRY_ROB_SIZE, Bool) selected_slots);
method Action get_if_operand1_ready(Vector#(`ENTRY_ROB_SIZE, Bit#(`MAX_LATENCY)) if_operand_ready);
method Action get_if_operand2_ready(Vector#(`ENTRY_ROB_SIZE, Bit#(`MAX_LATENCY)) if_operand_ready);
method Action get_regfile_values(Vector#(`PRF_SIZE, Bit#(`REG_WIDTH)) operand);
method Action bypass_forwarding(Vector#(`FUN_UNITS, Maybe#(Result_bypass_type)) op_value);
method Action ff_alu_data_read_0_clear(Bool clear);
method Action ff_alu_data_read_1_clear(Bool clear);
method Action ff_ls_data_read_clear(Bool clear);
method Action ff_branch_data_read_clear(Bool clear);
method Action ff_mul_div_data_read_clear(Bool clear);
method Action ff_alu_payload_0_clear(Bool clear);
method Action ff_alu_payload_1_clear(Bool clear);
method Action ff_ls_payload_clear(Bool clear);
method Action ff_branch_payload_clear(Bool clear);
method Action ff_mul_div_payload_clear(Bool clear);
method Action get_rob_head(Bit#(TLog#(`ENTRY_ROB_SIZE)) head);
method Action if_erob_empty(Bool if_empty);

//output methods
method ActionValue#(ALU_payload_type) send_ALU_0_inputs;
method ActionValue#(ALU_payload_type) send_ALU_1_inputs;
method ActionValue#(LS_payload_type) send_LS_inputs;            //TODO
method MUL_DIV_System_payload send_mul_div_system_inputs;
method Action release_mul_div_payload;
method ActionValue#(Branch_payload_type) send_Branch_input;
method Vector#(`ENTRY_ROB_SIZE, Bool) send_selected_slots_in_ROB;
method Vector#(TSub#(`FUN_UNITS,1), Broadcast_type) send_pre_exe_broadcast;
method ActionValue#(Broadcast_type) send_pre_exe_broadcast_mul;

endinterface

(*synthesize*)
module  mkPrf_select_grant(IfcPrf_select_grant);

    Vector#(`ENTRY_ROB_SIZE, Wire#(Entry_rob_type)) wr_entry_rob <-  replicateM(mkDWire(defaultValue));
				
   Vector#(`ENTRY_ROB_SIZE, Wire#(Bool)) wr_selected_for_exec <-  replicateM(mkDWire(False));
   Vector#(`ENTRY_ROB_SIZE, Wire#(Bool)) wr_update_select_for_exec <-  replicateM(mkDWire(False));
   Vector#(`ENTRY_ROB_SIZE, Wire#(Bit#(`MAX_LATENCY)))  wr_entry_rob_shift_op_1<-  replicateM(mkDWire(0));
   Vector#(`ENTRY_ROB_SIZE, Wire#(Bit#(`MAX_LATENCY)))  wr_entry_rob_shift_op_2<-  replicateM(mkDWire(0));

   //FIFOs to hold 'selected' ALU instructions
   Vector#(`ALU_UNITS, FIFO#(ALU_data_read)) ff_alu_data_read <- replicateM(mkPipelineFIFO());
   
   //FIFOs to hold the payload to ALUs
   Vector#(`ALU_UNITS, FIFO#(ALU_payload_type)) ff_alu_payload <- replicateM(mkPipelineFIFO());

   /* This rule selects the ready instructions to be sent to functional units.
      Selection policy used: position based.
      Handles ALU and LS instructions.
	  Contents of IQ it modifies: EROB:selected_for_exec */
	  
   //FIFO to hold selected mul and div instructions 
   FIFO#(MUL_DIV_System_data_read) ff_mds_data_read <- mkPipelineFIFO();

   //FIFO to hold mul div payload
   FIFO#(MUL_DIV_System_payload) ff_mds_payload <- mkPipelineFIFO();

   //FIFO to hold 'selected' LS instruction
   FIFO#(LS_data_read) ff_ls_data_read <- mkPipelineFIFO();
   
   //FIFO to hold LS unit payload 
   FIFO#(LS_payload_type) ff_ls_payload <- mkPipelineFIFO();
    
   //FIFO to hold 'selected' branch instructions
   FIFO#(Branch_data_read) ff_branch_data_read <- mkPipelineFIFO();

   //FIFO to hold Branch unit payload 
   FIFO#(Branch_payload_type) ff_branch_payload <- mkPipelineFIFO();

   Wire#(Bool) wr_revert_map <- mkDWire(False);

	 Wire#(Bit#(TLog#(`ENTRY_ROB_SIZE))) wr_erob_head <- mkDWire(0);

	 Wire#(Bool) wr_erob_empty <- mkDWire(True);

   Vector#(`ENTRY_ROB_SIZE, Wire#(Bool)) wr_grant_from_alu[`ALU_UNITS];
   for(Integer i=0; i<`ALU_UNITS;i=i+1)
	  wr_grant_from_alu[i] <- replicateM(mkWire());
   
   Vector#(`ENTRY_ROB_SIZE, Wire#(Bool)) wr_grant_from_ls_unit <- replicateM(mkWire());
   Vector#(`ENTRY_ROB_SIZE, Wire#(Bool)) wr_grant_from_branch_unit <- replicateM(mkWire());
   Vector#(`ENTRY_ROB_SIZE, Wire#(Bool)) wr_grant_from_mul_div_system <- replicateM(mkWire());

   Vector#(`PRF_SIZE, Wire#(Bit#(64))) wr_regfile_data <- replicateM(mkDWire(0));

   Vector#(`FUN_UNITS, Wire#(Maybe#(Result_bypass_type))) wr_result_bypass <- replicateM(mkDWire(tagged Invalid));

   Vector#(TSub#(`FUN_UNITS,1), Wire#(Broadcast_type)) wr_broadcast <- replicateM(mkDWire(Broadcast_type { valid : False, dest_tag : 0}));

	 FIFO#(Broadcast_type) ff_mul_broadcast <- mkBypassFIFO();
   Vector#(`IMM_BUF_SIZE, Wire#(Imm_buf_entry)) wr_imm_buf <- replicateM(mkDWire(
	  Imm_buf_entry {
					 valid: False,
					 imm: 0
   					 }));

   Vector#(`ALU_UNITS, Wire#(ALU_payload_type)) wr_ALU_input <- replicateM(mkWire);
   //Wire#(LS_payload_type)   wr_LS_input <- mkWire;
   //Wire#(Branch_payload_type) wr_Branch_input <- mkWire;

   rule rl_get_grant_from_select_logic(!wr_revert_map && !wr_erob_empty);
	   
	  Bit#(`ENTRY_ROB_SIZE) lv_req_vector_to_alu[`ALU_UNITS]; 
	  Bit#(`ENTRY_ROB_SIZE) lv_grant_vector_from_alu[`ALU_UNITS];
	  Bit#(`ENTRY_ROB_SIZE) lv_req_vector_to_mul_div_system;
	  Bit#(`ENTRY_ROB_SIZE) lv_grant_vector_from_mul_div_system;
	  Bit#(`ENTRY_ROB_SIZE) lv_req_vector_to_ls_unit;
	  Bit#(`ENTRY_ROB_SIZE) lv_grant_vector_from_ls_unit;
	  Bit#(`ENTRY_ROB_SIZE) lv_req_vector_to_branch_unit;
	  Bit#(`ENTRY_ROB_SIZE) lv_grant_vector_from_branch_unit;
	  
	  	  
	  //request vector for ALU 0, LS unit and branch unit
	  for(Integer i=0;i<`ENTRY_ROB_SIZE;i=i+1)
		 begin
			lv_req_vector_to_alu[0][i] = pack(wr_entry_rob[i].valid &&
			   (isALU(wr_entry_rob[i].inst_op)) &&
			   !wr_selected_for_exec[i] &&
			   unpack(wr_entry_rob_shift_op_1[i][0]) && 
			   unpack(wr_entry_rob_shift_op_2[i][0]));
			
			lv_req_vector_to_mul_div_system[i] = pack(wr_entry_rob[i].valid &&
			   ((isArithmetic(wr_entry_rob[i].inst_op) && !isALU(wr_entry_rob[i].inst_op)) ||
				 (isSystem(wr_entry_rob[i].inst_op) && wr_erob_head == fromInteger(i))) &&
			   !wr_selected_for_exec[i] &&
			   unpack(wr_entry_rob_shift_op_1[i][0]) && 
			   unpack(wr_entry_rob_shift_op_2[i][0]));

			lv_req_vector_to_ls_unit[i] = pack(wr_entry_rob[i].valid &&
			   (isMemory(wr_entry_rob[i].inst_op)) &&
			   !wr_selected_for_exec[i] &&
			   unpack(wr_entry_rob_shift_op_1[i][0]) &&
			   unpack(wr_entry_rob_shift_op_2[i][0]));
			
			lv_req_vector_to_branch_unit[i] = pack(wr_entry_rob[i].valid &&
			   (isBranch(wr_entry_rob[i].inst_op)) &&
			   !wr_selected_for_exec[i] &&
			   unpack(wr_entry_rob_shift_op_1[i][0]) &&
			   unpack(wr_entry_rob_shift_op_2[i][0]));

		 end
	  
	  $display("Time:%d\nreq_vector to ALUs:%b", $time, lv_req_vector_to_alu[0]);  
	  $display("req_vector to MUL or DIV:%b",lv_req_vector_to_mul_div_system);  
	  $display("req vector to LS_unit: %b", lv_req_vector_to_ls_unit);
	  $display("req vector to Branch: %b", lv_req_vector_to_branch_unit);
	  
	  $display("entry_rob_valid %d, selected_for_exec %d, op_1_ready %d, op_2_ready %d", wr_entry_rob[10].valid,
							wr_selected_for_exec[10], wr_entry_rob_shift_op_1[10][0], wr_entry_rob_shift_op_2[10][0]);

	  //Load store unit is assumed to execute one instruction every cycle
	  //in the current model (there is only one level d-cache)
	  lv_grant_vector_from_ls_unit = encoder_tree(lv_req_vector_to_ls_unit, True);

	  lv_grant_vector_from_branch_unit = encoder_tree(lv_req_vector_to_branch_unit, True);

	  lv_grant_vector_from_mul_div_system = encoder_tree(lv_req_vector_to_mul_div_system, True);

	  //stack encoder trees for ALUs
	  for(Integer i=1;i<`ALU_UNITS;i=i+1)
		 begin
    
			lv_grant_vector_from_alu[fromInteger(i)-1] = encoder_tree(lv_req_vector_to_alu[fromInteger(i)-1], 
															   True);
			
			lv_req_vector_to_alu[i] = lv_req_vector_to_alu[fromInteger(i)-1] & ~lv_grant_vector_from_alu[fromInteger(i)-1]; 

		 end
	  
	  lv_grant_vector_from_alu[fromInteger(`ALU_UNITS)-1] = encoder_tree(lv_req_vector_to_alu[fromInteger(`ALU_UNITS)-1], 
																		 True);
	  



	  for(Integer i=0;i<`ALU_UNITS;i=i+1)
		 $display("grant vector from ALU%d: %b", i, lv_grant_vector_from_alu[i]);
	  
	  $display("grant vector from mul div: %b", lv_grant_vector_from_mul_div_system);
	  $display("grant vector from LS: %b", lv_grant_vector_from_ls_unit);
	  $display("grant vector from Branch: %b", lv_grant_vector_from_branch_unit);

	  
	  //Write the grants on to the wires
	  for(Integer i=0;i<`ENTRY_ROB_SIZE;i=i+1)
		 begin
			
			for(Integer j=0;j<`ALU_UNITS;j=j+1)
			   wr_grant_from_alu[j][i] <= unpack(lv_grant_vector_from_alu[j][i]);
			wr_grant_from_ls_unit[i] <= unpack(lv_grant_vector_from_ls_unit[i]);
			wr_grant_from_branch_unit[i] <= unpack(lv_grant_vector_from_branch_unit[i]);
			wr_grant_from_mul_div_system[i] <= unpack(lv_grant_vector_from_mul_div_system[i]);

		 end
	  
	  
   endrule: rl_get_grant_from_select_logic
   
   Rules rs_all = emptyRules; 
   Rules rs_just = emptyRules;
   Rules rs_select = emptyRules;
   
   Rules rs_alu[`ALU_UNITS], r_alu[`ALU_UNITS];
   for(Integer i=0;i<`ALU_UNITS;i=i+1)
	  begin
		 rs_alu[i] = emptyRules;
		 r_alu[i] = emptyRules;
	  end


	  /* Select the ALU ready instructions and enqueue into data read fifo */
	  for(Integer j=0;j<`ALU_UNITS;j=j+1)
	  begin
		 
		 for(Integer i=0;i<`ENTRY_ROB_SIZE;i=i+1)
			begin

			   r_alu[j] = (rules

			   rule rl_select_for_alu(!wr_revert_map && 
				  wr_grant_from_alu[j][i] &&& 
					wr_entry_rob[i].inst_op matches tagged ALU .arith);
				  
						ff_alu_data_read[j].enq(ALU_data_read {
						   alu_op:arith.alu_func,
						   word_flag: wr_entry_rob[i].word_flag,
						   alu_type:arith.alu_type,
						   imm_valid: wr_entry_rob[i].imm_valid,
						   op_1: wr_entry_rob[i].op_1,
						   op_2: wr_entry_rob[i].op_2,
						   imm_index: wr_entry_rob[i].imm_index,
						   dest_op: wr_entry_rob[i].dest_op,
						   pc: wr_entry_rob[i].program_counter
						   });
				  
				  wr_update_select_for_exec[i] <= True;

				  wr_broadcast[j] <= Broadcast_type { valid : True,
												  dest_tag : wr_entry_rob[i].dest_op};
						
				  $display("data read fifo %d updated for alu", j);
				  $display(fshow(ALU_data_read {
					 alu_op: arith.alu_func,
					 word_flag: wr_entry_rob[i].word_flag,
					 alu_type:arith.alu_type,
					 imm_valid: wr_entry_rob[i].imm_valid,
					 op_1: wr_entry_rob[i].op_1,
					 op_2: wr_entry_rob[i].op_2,
					 imm_index: wr_entry_rob[i].imm_index,
					 dest_op: wr_entry_rob[i].dest_op,
					 pc: wr_entry_rob[i].program_counter
					 }));
				  endrule
				  endrules);
			   
			   rs_alu[j] = rJoinMutuallyExclusive(r_alu[j],rs_alu[j]);
			   
			end
		 
		rs_select = rJoinConflictFree(rs_alu[j], rs_select);

	  end

   Rules rs_ls = emptyRules;
   Rules rs_b = emptyRules;
   Rules rs_md = emptyRules;
   
   /* Select the LS and Branch ready instructions and enqueue into data read fifo */
   for(Integer i=0;i<`ENTRY_ROB_SIZE;i=i+1)
	  begin

		 Rules r_ls = (rules
			rule rl_select_for_ls_unit(!wr_revert_map && 
									   wr_grant_from_ls_unit[i] &&&
										 wr_entry_rob[i].inst_op matches tagged Memory .mem);
			
			   ff_ls_data_read.enq(LS_data_read {
				  base: wr_entry_rob[i].op_1,
				  offset: wr_entry_rob[i].imm_index,
				  op_2: wr_entry_rob[i].op_2,
				  mem_q_index: wr_entry_rob[i].mem_q_index,
				  dest_op: wr_entry_rob[i].dest_op,
				  mem_type: mem.mem_type,
				  mem_size: mem.mem_size
				  });
				  
			   wr_update_select_for_exec[i] <= True;
			
			   $display("data read fifo for LS unit updated");
			   $display(fshow(LS_data_read {
				  base: wr_entry_rob[i].op_1,
				  offset: wr_entry_rob[i].imm_index,
				  op_2: wr_entry_rob[i].op_2,
				  mem_q_index: wr_entry_rob[i].mem_q_index,
				  dest_op: wr_entry_rob[i].dest_op,
				  mem_type: mem.mem_type,
				  mem_size: mem.mem_size
				  }));
			endrule	
			endrules);

		 Rules r_b = (rules
			rule rl_select_for_branch_unit(!wr_revert_map && 
										   wr_grant_from_branch_unit[i] &&&
											 wr_entry_rob[i].inst_op matches tagged Branch .branch);
   
			   ff_branch_data_read.enq(Branch_data_read {
				  branch_op: branch.branch_func,
				  op_1: wr_entry_rob[i].op_1,
				  op_2: wr_entry_rob[i].op_2,
				  imm_index: wr_entry_rob[i].imm_index,
				  dest_op: wr_entry_rob[i].dest_op,
				  program_counter: wr_entry_rob[i].program_counter,
				  prediction: wr_entry_rob[i].prediction
				  });
			
			   wr_update_select_for_exec[i] <= True;
				  
			  wr_broadcast[3] <= Broadcast_type { valid : True,
											dest_tag : wr_entry_rob[i].dest_op};

			   $display("data read fifo for Branch unit updated");
			   $display(fshow(Branch_data_read {
				  branch_op: branch.branch_func,
				  op_1: wr_entry_rob[i].op_1,
				  op_2: wr_entry_rob[i].op_2,
				  imm_index: wr_entry_rob[i].imm_index,
				  dest_op: wr_entry_rob[i].dest_op,
				  program_counter: wr_entry_rob[i].program_counter,
				  prediction: wr_entry_rob[i].prediction
				  }));
			
			endrule		 
			endrules);

		Rules r_md = (rules                                                      //TODO
			rule rl_select_for_mul_div(!wr_revert_map && 
										   wr_grant_from_mul_div_system[i]);
											 
					 if(wr_entry_rob[i].inst_op matches tagged ALU .arith &&& arith.alu_type != ALU) begin
				
				   ff_mds_data_read.enq(tagged Mul_div_inst MUL_DIV_data_read { alu_op: arith.alu_func,
						   word_flag: wr_entry_rob[i].word_flag,
						   alu_type: arith.alu_type,
						   op_1: wr_entry_rob[i].op_1,
						   op_2: wr_entry_rob[i].op_2,
						   dest_op: wr_entry_rob[i].dest_op
						   });
						if(arith.alu_type == MUL)
						ff_mul_broadcast.enq(Broadcast_type { valid : True,
													dest_tag : wr_entry_rob[i].dest_op});
			   $display(fshow(MUL_DIV_data_read {
						   alu_op: arith.alu_func,
						   word_flag: wr_entry_rob[i].word_flag,
						   alu_type: arith.alu_type,
						   op_1: wr_entry_rob[i].op_1,
						   op_2: wr_entry_rob[i].op_2,
						   dest_op: wr_entry_rob[i].dest_op
				  		   }));
					end

					 else if(wr_entry_rob[i].inst_op matches tagged System .system_inst) begin
				   ff_mds_data_read.enq(tagged System_inst Interrupt_data_read { 
						   system_inst: system_inst,
							 imm_valid: wr_entry_rob[i].imm_valid,
							 imm_index: wr_entry_rob[i].imm_index,
						   op_1: wr_entry_rob[i].op_1,
						   dest_op: wr_entry_rob[i].dest_op
						   });
					 end
				   wr_update_select_for_exec[i] <= True;
				
				endrule	
				endrules);


		 	rs_md = rJoinMutuallyExclusive(r_md,rs_md);
		 	rs_ls = rJoinMutuallyExclusive(r_ls,rs_ls);
		 	rs_b = rJoinMutuallyExclusive(r_b, rs_b);

		end

    rs_select = rJoinConflictFree(rs_md, rs_select);
    rs_select = rJoinConflictFree(rs_ls, rs_select);
    rs_select = rJoinConflictFree(rs_b, rs_select);
addRules(rs_select);

   /* Read data from PRF to payload FIFO 0 */	  

   
   rule rl_data_read_from_0(!wr_revert_map );

	  
	  let lv_alu_data_read = ff_alu_data_read[0].first();
	  Bit#(`REG_WIDTH) lv_op_1 = wr_regfile_data[lv_alu_data_read.op_1];
	  Bit#(`REG_WIDTH) lv_op_2 = wr_regfile_data[lv_alu_data_read.op_2];
	  $display("data of op_2 %d", lv_op_2);
	  if(lv_alu_data_read.imm_valid) begin  
		 lv_op_2 = wr_imm_buf[lv_alu_data_read.imm_index].imm;
	  end 
		$display("the imm_index %h and imm_value %d",  lv_alu_data_read.imm_index, wr_imm_buf[lv_alu_data_read.imm_index].imm);

	  
			for(Integer i=0;i<`FUN_UNITS;i=i+1)
                begin
                   //if the tag on bypass wires matches the destination
                   //operand, forward the result
                   if(wr_result_bypass[i] matches tagged Valid .bypass)
                      begin
                         if(lv_alu_data_read.op_1==bypass.dest_tag)
                            begin
 
                               $display("data from %d to op1 forwarded", bypass.dest_tag);
 
                               lv_op_1 = bypass._result;
 
                            end
 
                         if(!lv_alu_data_read.imm_valid &&
                            lv_alu_data_read.op_2==bypass.dest_tag)
                            begin
 
                               lv_op_2 = bypass._result;
 
                               $display("data from %d to op2 forwarded", bypass.dest_tag);
 
                            end
                      end
                end
	  
	  $display("Time:%d\nff_alu_payload_to_alu_0 is enqueued", $time);
	  $display(fshow( ALU_payload_type {
		 alu_op  :  lv_alu_data_read.alu_op,
		 word_flag: lv_alu_data_read.word_flag,
		 alu_type     : lv_alu_data_read.alu_type,
		 src_1: lv_op_1,
		 src_2: lv_op_2,
		 dest_op: lv_alu_data_read.dest_op,
		 pc: lv_alu_data_read.pc
		 }));
	  
	  
	  ff_alu_data_read[0].deq();
	  ff_alu_payload[0].enq( ALU_payload_type {
		 alu_op  :  lv_alu_data_read.alu_op,
		 word_flag: lv_alu_data_read.word_flag,
		 alu_type     : lv_alu_data_read.alu_type,
		 src_1: lv_op_1,
		 src_2: lv_op_2,
		 dest_op: lv_alu_data_read.dest_op,
		 pc: lv_alu_data_read.pc
		 });
   
	  endrule: rl_data_read_from_0
   
   /* Read data from PRF to payload FIFO 1 */
   
   rule rl_data_read_from_1(!wr_revert_map );

	  
	  let lv_alu_data_read = ff_alu_data_read[1].first();
	   Bit#(`REG_WIDTH) lv_op_1 = wr_regfile_data[lv_alu_data_read.op_1];
	   Bit#(`REG_WIDTH) lv_op_2 = wr_regfile_data[lv_alu_data_read.op_2];
	  
	  if(lv_alu_data_read.imm_valid)
		 lv_op_2 = wr_imm_buf[lv_alu_data_read.imm_index].imm;
	  
			for(Integer i=0;i<`FUN_UNITS;i=i+1)
                begin
                   //if the tag on bypass wires matches the destination
                   //operand, forward the result
                   if(wr_result_bypass[i] matches tagged Valid .bypass)
                      begin
                         if(lv_alu_data_read.op_1==bypass.dest_tag)
                            begin
 
                               $display("data from %d to op1 forwarded", bypass.dest_tag);
 
                               lv_op_1 = bypass._result;
 
                            end
 
                         if(!lv_alu_data_read.imm_valid &&
                            lv_alu_data_read.op_2==bypass.dest_tag)
                            begin
 
                               lv_op_2 = bypass._result;
 
                               $display("data from %d to op2 forwarded", bypass.dest_tag);
 
                            end
                      end
                end

	  $display("Time:%d\nff_alu_payload_to_alu_1 is enqueued", $time);
	  $display(fshow(ALU_payload_type {
		 alu_op :  lv_alu_data_read.alu_op,
		 word_flag: lv_alu_data_read.word_flag,
		 alu_type : lv_alu_data_read.alu_type,
		 src_1: lv_op_1,
		 src_2: lv_op_2,
		 dest_op: lv_alu_data_read.dest_op,
		 pc: lv_alu_data_read.pc
		 }));

	  
	  ff_alu_data_read[1].deq();
	  ff_alu_payload[1].enq( ALU_payload_type {
		 alu_op :  lv_alu_data_read.alu_op,
		 word_flag: lv_alu_data_read.word_flag,
		 alu_type : lv_alu_data_read.alu_type,
		 src_1: lv_op_1,
		 src_2: lv_op_2,
		 dest_op: lv_alu_data_read.dest_op,
		 pc: lv_alu_data_read.pc
		 });
   
   endrule: rl_data_read_from_1
   
   /* To ALU 1 from payload FIFO 1 */
   
  
   /* Data read after selecting a LS instruction  */
   
   rule rl_data_read_ls_unit(!wr_revert_map);
	  
	  
	  let lv_ls_data_read = ff_ls_data_read.first();
	  ff_ls_data_read.deq();
	  
	  Maybe#(Bit#(`REG_WIDTH)) lv_str_data = tagged Invalid;
	  
	  if(lv_ls_data_read.mem_type==STR)
		 lv_str_data = tagged Valid wr_regfile_data[lv_ls_data_read.op_2];
					
	  Bit#(`REG_WIDTH) lv_base = wr_regfile_data[lv_ls_data_read.base];
	  Bit#(`REG_WIDTH) lv_offset = wr_imm_buf[lv_ls_data_read.offset].imm;

			for(Integer i=0;i<`FUN_UNITS;i=i+1)
                begin
                   //if the tag on bypass wires matches the destination
                   //operand, forward the result
                   if(wr_result_bypass[i] matches tagged Valid .bypass)
                      begin
                         if(lv_ls_data_read.base==bypass.dest_tag)
                            begin
 
                               $display("data from %d to op1 forwarded", bypass.dest_tag);
 
                               lv_base = bypass._result;
 
                            end
 
                         if(lv_ls_data_read.mem_type==STR && lv_ls_data_read.op_2==bypass.dest_tag)
                            begin
 
                               lv_str_data = tagged Valid bypass._result;
 
                               $display("data from %d to op2 forwarded", bypass.dest_tag);
 
                            end
                      end
                end


	  ff_ls_payload.enq(LS_payload_type {
		 base: lv_base,
		 offset: lv_offset,
		 mem_q_index: lv_ls_data_read.mem_q_index,
		 dest_op: lv_ls_data_read.dest_op,
		 str_data: lv_str_data,
		 mem_size: lv_ls_data_read.mem_size
		 });
	  
	  $display("Time:%d\nreading PRF for LS instruction", $time);
	  $display(fshow(LS_payload_type {
		 base: lv_base,
		 offset: lv_offset,
		 mem_q_index: lv_ls_data_read.mem_q_index,
		 dest_op: lv_ls_data_read.dest_op,
		 str_data: lv_str_data,
		 mem_size: lv_ls_data_read.mem_size
		 }));
	  
   endrule: rl_data_read_ls_unit


   rule rl_data_read_branch_unit(!wr_revert_map );
	  
	  let lv_branch_data_read = ff_branch_data_read.first();

	   Bit#(`REG_WIDTH) lv_op_1 = wr_regfile_data[lv_branch_data_read.op_1];
	   Bit#(`REG_WIDTH) lv_op_2 = wr_regfile_data[lv_branch_data_read.op_2];

	  
	  ff_branch_data_read.deq();
		 
			for(Integer i=0;i<`FUN_UNITS;i=i+1)
                begin
                   //if the tag on bypass wires matches the destination
                   //operand, forward the result
                   if(wr_result_bypass[i] matches tagged Valid .bypass)
                      begin
                         if(lv_branch_data_read.op_1==bypass.dest_tag)
                            begin
 
                               $display("data from %d to op1 forwarded", bypass.dest_tag);
 
                               lv_op_1 = bypass._result;
 
                            end
 
                         if(lv_branch_data_read.op_2==bypass.dest_tag)
                            begin
 
                               lv_op_2 = bypass._result;
 
                               $display("data from %d to op2 forwarded", bypass.dest_tag);
 
                            end
                      end
                end

	  ff_branch_payload.enq(Branch_payload_type {
		 branch_op: lv_branch_data_read.branch_op,
		 src_1: lv_op_1,
		 src_2: lv_op_2,
		 dest_op: lv_branch_data_read.dest_op,
		 imm: wr_imm_buf[lv_branch_data_read.imm_index].imm,
		 program_counter: lv_branch_data_read.program_counter,
		 prediction: lv_branch_data_read.prediction
		 });

	  
	  $display("Time:%d\nreading PRF for Branch instruction", $time);
	  
	  $display(fshow(Branch_payload_type {
		 branch_op: lv_branch_data_read.branch_op,
		 src_1: lv_op_1,
		 src_2: lv_op_2,
		 dest_op: lv_branch_data_read.dest_op,
		 imm: wr_imm_buf[lv_branch_data_read.imm_index].imm,
		 program_counter: lv_branch_data_read.program_counter,
		 prediction: lv_branch_data_read.prediction
		 }));
	  
   endrule

rule rl_data_read_from_mul_div(!wr_revert_map &&& 
				ff_mds_data_read.first() matches tagged Mul_div_inst .lv_md_data_read);

	  Bit#(`REG_WIDTH) lv_op_1 = wr_regfile_data[lv_md_data_read.op_1];
	  Bit#(`REG_WIDTH) lv_op_2 = wr_regfile_data[lv_md_data_read.op_2];
	  
			for(Integer i=0;i<`FUN_UNITS;i=i+1)
                begin
                   //if the tag on bypass wires matches the destination
                   //operand, forward the result
                   if(wr_result_bypass[i] matches tagged Valid .bypass)
                      begin
                         if(lv_md_data_read.op_1==bypass.dest_tag)
                            begin
 
                               $display("data from %d to op1 forwarded", bypass.dest_tag);
 
                               lv_op_1 = bypass._result;
 
                            end
 
                         if(lv_md_data_read.op_2==bypass.dest_tag)
                            begin
 
                               lv_op_2 = bypass._result;
 
                               $display("data from %d to op2 forwarded", bypass.dest_tag);
 
                            end
                      end
                end

	  ff_mds_data_read.deq();
	  ff_mds_payload.enq( tagged Mul_div_inst  MUL_DIV_payload_type{
		 alu_op  :  lv_md_data_read.alu_op,
		 word_flag: lv_md_data_read.word_flag,
		 alu_type     : lv_md_data_read.alu_type,
		 src_1: lv_op_1,
		 src_2: lv_op_2,
		 dest_op: lv_md_data_read.dest_op
		 });

	  $display("Time:%d\nff_alu_payload_to_alu_0 is enqueued", $time);
	  $display(fshow(MUL_DIV_payload_type {
		 alu_op  :  lv_md_data_read.alu_op,
		 word_flag: lv_md_data_read.word_flag,
		 alu_type     : lv_md_data_read.alu_type,
		 src_1: lv_op_1,
		 src_2: lv_op_2,
		 dest_op: lv_md_data_read.dest_op
		 }));
	  
	  endrule: rl_data_read_from_mul_div


rule rl_data_read_from_system(!wr_revert_map &&& 
				ff_mds_data_read.first() matches tagged System_inst .lv_mds_data_read);

		Bit#(`REG_WIDTH) lv_op_1 = 0;
		if(lv_mds_data_read.imm_valid)
			lv_op_1 = zeroExtend(lv_mds_data_read.op_1);
		else 
			lv_op_1 = wr_regfile_data[lv_mds_data_read.op_1];
	  
		for(Integer i=0;i<`FUN_UNITS;i=i+1)
                begin
                   //if the tag on bypass wires matches the destination
                   //operand, forward the result
                   if(wr_result_bypass[i] matches tagged Valid .bypass) begin
                         if(lv_mds_data_read.op_1==bypass.dest_tag) begin
 
                               $display("data from %d to op1 forwarded", bypass.dest_tag);
 
                               lv_op_1 = bypass._result;
 
                          end
                    end
				end

	  ff_mds_data_read.deq();
	  ff_mds_payload.enq( tagged System_inst Interrupt_payload_type {
		 system_inst			: lv_mds_data_read.system_inst,
		 src_1						: lv_op_1,
		 dest_op					: lv_mds_data_read.dest_op,
		 csr_address			: wr_imm_buf[lv_mds_data_read.imm_index].imm[11:0]
		 });

	  $display("Time:%d\nff_alu_payload_to_alu_0 is enqueued", $time);
	  $display(fshow( tagged Interrupt_payload_type {
		 system_inst			: lv_mds_data_read.system_inst,
		 src_1						: lv_op_1,
		 dest_op					: lv_mds_data_read.dest_op,
		 csr_address			: wr_imm_buf[lv_mds_data_read.imm_index].imm[11:0]
		 }));
	  
	  endrule: rl_data_read_from_system


   /* FIFO 0 is always given priority over FIFO 1 to use ALUs */
   
//   (* descending_urgency = "rl_drive_to_alu_0_from_0, rl_drive_to_alu_0_from_1" *)
//   (* descending_urgency = "rl_drive_to_alu_1_from_0, rl_drive_to_alu_1_from_1" *)
   
   /* Instruction should be driven to ALU only once, ALU 0 is given more
	  priority over ALU 1 */
   
//   (* preempts = "rl_drive_to_alu_0_from_0, rl_drive_to_alu_1_from_0" *)
   
   /* To ALU 0 from Payload FIFO 0 */
   
   //rule rl_drive_to_alu_0_from_0(!wr_revert_map);
   //   
   //   $display("Time:%d\nrl_drive_to_alu_0_from_0 is fired", $time);
   //   
   //   let lv_alu_0_payload = ff_alu_payload[0].first();
   //   ff_alu_payload[0].deq();
   //   wr_ALU_input[0] <= lv_alu_0_payload;

   //endrule: rl_drive_to_alu_0_from_0
   
   
   /* To ALU 1 from payload FIFO 0 */
/* -----\/----- EXCLUDED -----\/-----
   
   rule rl_drive_to_alu_1_from_0(!rg_revert_front_end && !rg_revert_back_end);

	  $display("Time:%d\nrl_drive_to_alu_1_from_0 is fired", $time);
		 
	  let lv_alu_1_payload = ff_alu_payload[0].first();
	  ff_alu_payload[0].deq();
	  alu[1]._inputs(lv_alu_1_payload.alu_type,lv_alu_1_payload.alu_op, lv_alu_1_payload.word_flag,
					 lv_alu_1_payload.src_1, lv_alu_1_payload.src_2, 
					 lv_alu_1_payload.dest_op, lv_alu_1_payload.pc, lv_alu_1_payload.thread_id);

   endrule: rl_drive_to_alu_1_from_0


   /-* Instruction should be driven to ALU only once, ALU 0 is given more
	  priority over ALU 1 *-/
   
   (* preempts = "rl_drive_to_alu_0_from_1, rl_drive_to_alu_1_from_1" *)
   
   /-* To ALU 0 from payload FIFO 1 *-/
   
   rule rl_drive_to_alu_0_from_1(!wr_revert_front_end && !wr_revert_back_end);
	  
	  $display("Time:%d\nrl_drive_to_alu_0_from_1 is fired", $time);
	  
	  let lv_alu_0_payload = ff_alu_payload[1].first();
	  ff_alu_payload[1].deq();
	  alu[0]._inputs(lv_alu_0_payload.alu_type,lv_alu_0_payload.alu_op,lv_alu_0_payload.word_flag,
					 lv_alu_0_payload.src_1, lv_alu_0_payload.src_2, 
					 lv_alu_0_payload.dest_op, lv_alu_0_payload.pc, lv_alu_0_payload.thread_id);

   endrule: rl_drive_to_alu_0_from_1

 -----/\----- EXCLUDED -----/\----- */

   /* To ALU 1 from payload FIFO 1 */
   
   //rule rl_drive_to_alu_1_from_1(!wr_revert_map);
   //   
   //   $display("Time:%d\nrl_drive_to_alu_1_from_1 is fired", $time);
   //   
   //   let lv_alu_1_payload = ff_alu_payload[1].first();
   //   ff_alu_payload[1].deq();
   //   wr_ALU_input[1] <= lv_alu_1_payload;

   //endrule: rl_drive_to_alu_1_from_1


   /* Drive the inputs to load store unit */

   //rule rl_drive_to_ls_unit(!wr_revert_map );
   //   
   //   $display("Time:%d\ninside drive_to_ls_unit", $time);
   //   
   //   let lv_ls_payload = ff_ls_payload.first();
   //   wr_LS_input <= lv_ls_payload;
   //   ff_ls_payload.deq();

   //endrule: rl_drive_to_ls_unit

    
   //rule rl_drive_to_branch_unit(!wr_revert_map );

   //   $display("Time:%d\nSent to branch unit for execution", $time);
   //   
   //   let lv_branch_payload = ff_branch_payload.first();
   //   ff_branch_payload.deq();
   //   wr_Branch_input <= lv_branch_payload;
   //endrule
    
    method Action get_rob_details(Vector#(`ENTRY_ROB_SIZE, Entry_rob_type)  erob_data);
	for(Integer i = 0; i < `ENTRY_ROB_SIZE; i = i + 1)
	    wr_entry_rob[i] <= erob_data[i];
    endmethod
    
    method Action get_revert_front_end(Bool  revert_map);
	wr_revert_map <= revert_map;
    endmethod
    
    method Action get_imm_value(Vector#(`IMM_BUF_SIZE, Imm_buf_entry) data_from_IMM_buf); 
	for(Integer i = 0; i < `IMM_BUF_SIZE; i = i + 1)
	    wr_imm_buf[i] <= data_from_IMM_buf[i];
    endmethod
    
    method Action get_if_selected_for_exec(Vector#(`ENTRY_ROB_SIZE, Bool) selected_slots);
	for(Integer i = 0; i < `ENTRY_ROB_SIZE; i = i + 1)
	    wr_selected_for_exec[i]  <= selected_slots[i];
    endmethod

    method Action get_if_operand1_ready(Vector#(`ENTRY_ROB_SIZE, Bit#(`MAX_LATENCY)) if_operand_ready);
	for(Integer i = 0; i < `ENTRY_ROB_SIZE; i = i + 1)
	    wr_entry_rob_shift_op_1[i] <= if_operand_ready[i];
    endmethod

    method Action get_if_operand2_ready(Vector#(`ENTRY_ROB_SIZE, Bit#(`MAX_LATENCY)) if_operand_ready);
	for(Integer i = 0; i < `ENTRY_ROB_SIZE; i = i + 1)
	    wr_entry_rob_shift_op_2[i] <= if_operand_ready[i];
    endmethod 

    method Action get_regfile_values(Vector#(`PRF_SIZE, Bit#(`REG_WIDTH)) operand);
	for(Integer i = 0; i < `PRF_SIZE; i = i + 1)
	    wr_regfile_data[i] <= operand[i];
    endmethod

	method Action bypass_forwarding(Vector#(`FUN_UNITS, Maybe#(Result_bypass_type)) op_value);
	for(Integer i = 0; i < `FUN_UNITS; i = i + 1)
		wr_result_bypass[i] <= op_value[i];
	endmethod

    method Action ff_alu_data_read_0_clear(Bool clear);
	if(clear == True)
	    ff_alu_data_read[0].clear();
    endmethod

    method Action ff_alu_data_read_1_clear(Bool clear);
	if(clear == True)
	    ff_alu_data_read[1].clear();
    endmethod

    method Action ff_ls_data_read_clear(Bool clear);
	if(clear == True)
	    ff_ls_data_read.clear();
    endmethod

    method Action ff_branch_data_read_clear(Bool clear);
	if(clear == True)
	    ff_branch_data_read.clear();
    endmethod

    method Action ff_alu_payload_0_clear(Bool clear);
	if(clear == True)
	    ff_alu_payload[0].clear();
    endmethod

    method Action ff_mul_div_data_read_clear(Bool clear);
	if(clear == True)
	    ff_mds_data_read.clear();
    endmethod

    method Action ff_alu_payload_1_clear(Bool clear);
	if(clear == True)
	    ff_alu_payload[1].clear();
    endmethod

    method Action ff_ls_payload_clear(Bool clear);
	if(clear == True)
	    ff_ls_payload.clear();
    endmethod

    method Action ff_branch_payload_clear(Bool clear);
	if(clear == True)
	    ff_branch_payload.clear();
    endmethod

    method Action ff_mul_div_payload_clear(Bool clear);
	if(clear == True)
	    ff_mds_payload.clear();
    endmethod

	method Action get_rob_head(Bit#(TLog#(`ENTRY_ROB_SIZE)) head);
		wr_erob_head <= head;
	endmethod

	method Action if_erob_empty(Bool if_empty);
		wr_erob_empty <= if_empty;
	endmethod

    //output methods
    method ActionValue#(ALU_payload_type) send_ALU_0_inputs if(!wr_revert_map);
	let lv_alu_0_payload = ff_alu_payload[0].first();
	ff_alu_payload[0].deq();
	return lv_alu_0_payload;
    endmethod

    method ActionValue#(ALU_payload_type) send_ALU_1_inputs if(!wr_revert_map);
	let lv_alu_1_payload = ff_alu_payload[1].first();
	ff_alu_payload[1].deq();
	return lv_alu_1_payload;
    endmethod

    method ActionValue#(LS_payload_type) send_LS_inputs if(!wr_revert_map);
	  let lv_ls_payload = ff_ls_payload.first();
	    ff_ls_payload.deq();
	return lv_ls_payload;
    endmethod

	method MUL_DIV_System_payload send_mul_div_system_inputs if(!wr_revert_map);
	  let lv_md_payload = ff_mds_payload.first();
	  return lv_md_payload;
	endmethod

	method Action release_mul_div_payload if(!wr_revert_map);
	  ff_mds_payload.deq();
	endmethod
    
    method ActionValue#(Branch_payload_type) send_Branch_input if(!wr_revert_map);
    let lv_branch_payload = ff_branch_payload.first();
    ff_branch_payload.deq();
	return lv_branch_payload;
	//return wr_Branch_input;
    endmethod

    method Vector#(`ENTRY_ROB_SIZE, Bool) send_selected_slots_in_ROB;
	Vector#(`ENTRY_ROB_SIZE, Bool) selected_slots;	
	for(Integer i = 0; i < `ENTRY_ROB_SIZE; i =i+1)
	    selected_slots[i] = wr_update_select_for_exec[i];
	return selected_slots;
    endmethod

	method Vector#(TSub#(`FUN_UNITS,1), Broadcast_type) send_pre_exe_broadcast;
	Vector#(TSub#(`FUN_UNITS,1), Broadcast_type) lv_broadcast;
	for(Integer i = 0; i < `FUN_UNITS - 1; i =i+1)
	    lv_broadcast[i] = wr_broadcast[i];
	return lv_broadcast;
	endmethod

	method ActionValue#(Broadcast_type) send_pre_exe_broadcast_mul;
		let lv_mul_broadcast = ff_mul_broadcast.first();
			ff_mul_broadcast.deq;
	return lv_mul_broadcast;
	endmethod

endmodule

endpackage

