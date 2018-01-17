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

package commit;

import riscv_types:: *;
import FIFO:: *;
import Vector:: *;
import DefaultValue:: *;
`include "defined_parameters.bsv"

interface IfcPrf_commit;
//Input methods
method Action flush_signals(Bool revert);
method Action erob_head_entry(Vector#(`FETCH_WIDTH, Entry_rob_type) entry);
method Action imm_head_entry(Vector#(`FETCH_WIDTH, Imm_buf_entry) entry);
method Action entry_rob_execute_done(Vector#(`FETCH_WIDTH, Bool) execute_done);
method Action entry_rob_exceptions(Vector#(`FETCH_WIDTH, TrapCause) exception);
method Action squash_buf_entry(Vector#(`FETCH_WIDTH, Bit#(`REG_WIDTH)) entry);
method Action get_rRAM_entry_1(Bit#(TLog#(`PRF_SIZE)) entry);
method Action get_rRAM_entry_2(Bit#(TLog#(`PRF_SIZE)) entry);
method Action commit_load(Vector#(`FETCH_WIDTH, Bool) load_commit);
method Action squash_buf_status(Vector#(`FETCH_WIDTH, Bool) squash);
//method Action to_stall(Bool stall);

//Output methods
method Bit#(TLog#(`REGFILE_SIZE)) send_rRAM_slot_1;
method Bit#(TLog#(`REGFILE_SIZE)) send_rRAM_slot_2;
method Bool erob_invalidate_1;
method Bool erob_invalidate_2;
method Maybe#(Bit#(TLog#(`IMM_BUF_SIZE))) invalidate_imm_slot_1;
method Maybe#(Bit#(TLog#(`IMM_BUF_SIZE))) invalidate_imm_slot_2;
method Bool commit_store_1;
method Bool commit_store_2;
method Vector#(`FETCH_WIDTH, Bool) is_inst_load;
method Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)) update_imm_head;
method Bool update_rRAM_1;
method Bool update_rRAM_2;
method Maybe#(Bit#(`REG_WIDTH)) squash_pc; 
method FRQ_entry frq_update_1; 
method FRQ_entry frq_update_2; 
method Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)) update_frq_tail;
method Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)) update_erob_head;
method TrapCause return_exception;
method Bit#(`REG_WIDTH) return_pc;
method Vector#(`FETCH_WIDTH, Bool) update_csr_registers; 
/**************************** VERIFICATION ENVIRONMENT *******************************/
method Action _register_values(Vector#(`PRF_SIZE, Bit#(`REG_WIDTH)) prf_entries);
method Action _rRAM_values(Vector#(`REGFILE_SIZE, RAT_entry) rRAM_entries);
endinterface

(*synthesize*)
module mkPrf_commit(IfcPrf_commit);

    Wire#(Bool) wr_revert_map <- mkDWire(False);
    Vector#(`FETCH_WIDTH, Wire#(Entry_rob_type)) wr_entry_rob <- replicateM( mkDWire(defaultValue));
    Vector#(`FETCH_WIDTH, Wire#(Imm_buf_entry)) wr_imm_buf <- replicateM(mkDWire(defaultValue));
    Vector#(`FETCH_WIDTH, Wire#(Bool)) wr_entry_rob_invalidate <- replicateM( mkDWire(False));
    Vector#(`FETCH_WIDTH, Wire#(Bool)) wr_entry_rob_execute_done <- replicateM( mkDWire(False));
    Vector#(`FETCH_WIDTH, Wire#(TrapCause)) wr_entry_rob_exception <- replicateM( mkDWire(No_trap));
    Vector#(`FETCH_WIDTH, Wire#(Bool)) wr_entry_rob_squash <- replicateM( mkDWire(False));
    Wire#(Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)))  wr_update_imm_buf_head <- mkDWire(0);
    Wire#(Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)))  wr_update_erob_head <- mkDWire(0);
    Wire#(Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)))  wr_update_frq_tail <- mkDWire(0);
    Vector#(`FETCH_WIDTH, Wire#(Bool)) wr_update_rRAM <- replicateM(mkDWire(False));
    Vector#(`FETCH_WIDTH, Wire#(Maybe#(Bit#(TLog#(`IMM_BUF_SIZE)))))  wr_invalidate_imm  <- replicateM(mkDWire(tagged Invalid));
    Vector#(`FETCH_WIDTH, Wire#(Bool)) wr_commit_store <- replicateM(mkDWire(False));
    Vector#(`FETCH_WIDTH, Wire#(Bool)) wr_commit_load  <- replicateM(mkDWire(False));
    Wire#(Vector#(`FETCH_WIDTH, Bool)) wr_is_load      <- mkDWire(replicate(False));
    Vector#(`FETCH_WIDTH, Wire#(FRQ_entry)) wr_frq <- replicateM(mkDWire(FRQ_entry {
									   free_reg : ?,
									      valid : False}));
    Vector#(`FETCH_WIDTH, Wire#(Bit#(TLog#(`REGFILE_SIZE)))) wr_rRAM_slot <- replicateM(mkDWire(0));
    Vector#(`FETCH_WIDTH, Wire#(Bit#(TLog#(`PRF_SIZE)))) wr_rRAM_entry <- replicateM(mkDWire(0));
    Vector#(`FETCH_WIDTH, Wire#(Bit#(`REG_WIDTH))) wr_squash_buf <- replicateM(mkDWire(0));
    Wire#(Maybe#(Bit#(`REG_WIDTH))) wr_squash_pc <- mkDWire(tagged Invalid);
	Vector#(`FETCH_WIDTH, Wire#(Bool)) wr_update_csr <- replicateM(mkDWire(False));
	Wire#(TrapCause) wr_cause_trap <- mkDWire(tagged No_trap);
	Wire#(Bit#(`REG_WIDTH)) wr_pc <- mkDWire(0);
	//Wire#(Bool) wr_to_stall <- mkDWire(False);

/*************************** VERIFICATION ENVIRONMENT *****************************/
    Vector#(`PRF_SIZE, Wire#(Bit#(`REG_WIDTH))) wr_ifc_regFile <- replicateM(mkDWire(0));
    Vector#(`REGFILE_SIZE, Wire#(Bit#(TLog#(`PRF_SIZE)))) wr_rRAM <- replicateM(mkDWire(0));
    Reg#(Bool) rg_open_dump_file_1 <- mkReg(True);
	Reg#(Bool) rg_stop <- mkReg(False);

    let rg_dump_file <- mkReg(InvalidFile);

rule rl_register_file_write(rg_open_dump_file_1);

      String dumpFile = "output.txt";
      File fl_dump <- $fopen( dumpFile, "w");
      rg_open_dump_file_1 <= False;
      rg_dump_file <= fl_dump;

endrule

rule rl_commit(!wr_revert_map && 
		 ((wr_entry_rob_execute_done[0] && isWireNoTrap(select(wr_entry_rob_exception,0))) 
		|| (!isWireNoTrap(wr_entry_rob_exception[0])  || isSystem(wr_entry_rob[0].inst_op)))  
		&& wr_entry_rob[0].valid /*&& !wr_to_stall*/);

	  $display("COMMIT:THE_COMMIT");
	  //register to be added to frq
	  Vector#(`FETCH_WIDTH, Bit#(TLog#(`PRF_SIZE))) lv_reg_to_free;
	  TrapCause lv_exception = tagged No_trap;
	  Bit#(64) lv_pc = 64'b0;

	  //in the case of store and conditional branch instructions, add back the
	  //dummy destination register allocated. Also, in the instrs with rd = 0,
	  //dest_op is a dummy one, which is added back to frq.
	  for(Integer i=0;i<`FETCH_WIDTH;i=i+1) begin

			if(isSTR(wr_entry_rob[i].inst_op) || isCond(wr_entry_rob[i].inst_op) ||
			   wr_entry_rob[i].dest_arch == 0)
			   lv_reg_to_free[i] = wr_entry_rob[i].dest_op;
			else begin
			   wr_rRAM_slot[i] <= wr_entry_rob[i].dest_arch;
			   lv_reg_to_free[i] = wr_rRAM_entry[i];
			end
	  end
	  
	  
	  
	  //If the dest_arch of instr1 is same as dest_arch of instr0, 
	  //add back the dest_op of instr0. Note that instr1 should not
	  //be a store or conditional branch instruction. instr0 should not
	  //be the one with invalid rd.
	  if(!(isSTR(wr_entry_rob[0].inst_op) || isCond(wr_entry_rob[0].inst_op)) && 
		 wr_entry_rob[0].dest_arch==wr_entry_rob[1].dest_arch &&
		!(isSTR(wr_entry_rob[1].inst_op) || isCond(wr_entry_rob[1].inst_op)) && 
		 wr_entry_rob[1].dest_arch != 0)
		 lv_reg_to_free[1] = wr_entry_rob[0].dest_op;
	  

	  Vector#(`FETCH_WIDTH, Bool) lv_squash = replicate(False);

	  Vector#(`FETCH_WIDTH, Bool) lv_is_load = replicate(False);
	  
	  Vector#(`FETCH_WIDTH, Bool) lv_squash_from_ls_unit = replicate(False);
	  
	  Vector#(`FETCH_WIDTH, Bool) lv_imm_valid = replicate(False);
	  
	  Vector#(`FETCH_WIDTH, Bool) lv_free_erob = replicate(False);
	
	  //free the erob entry
	  wr_entry_rob_invalidate[0] <= True;
	  
	  $display("COMMIT:Time:%d\nEntry rob index %d freed",$time, 0);
			
	  lv_free_erob[0] = True;
			
	  //retrive squash bit from IQ updated during branch broadcast
	  lv_squash[0] = wr_entry_rob_squash[0];
			
	  if(wr_entry_rob[0].imm_valid || isSystem(wr_entry_rob[0].inst_op)) begin
			//free the imm buffer
			lv_imm_valid[0] = True;
				  
			$display("COMMIT:imm_buf index %d freed", wr_entry_rob[0].imm_index);
				  
			wr_invalidate_imm[0] <= tagged Valid wr_entry_rob[0].imm_index;

	  end
			


	  //call the commit method in ls unit
		if(wr_entry_rob[0].inst_op matches tagged Memory .memx) begin
			if(memx.mem_type == STR) begin
				wr_commit_store[0] <= True; 
			end
			else if(memx.mem_type==LD) begin
        		lv_is_load[0] = True; 
        		lv_squash_from_ls_unit[0] = wr_commit_load[0];
			end
		end
	  if(isSystem(wr_entry_rob[0].inst_op))
				wr_update_csr[0] <= True;


	  //commit instr1 if it has completed execution. Note that only one store can be committed
	  //at a time. So, if instr0 and instr1 are 'stores', do not commit instr1. If instr0 is a
	  //taken branch, instr1 should not be committed.
	  if(wr_entry_rob[1].valid && (wr_entry_rob_execute_done[1] && !isSystem(wr_entry_rob[0].inst_op)) && 
		 wr_imm_buf[0] != wr_imm_buf[1] && wr_entry_rob[0].dest_op != wr_entry_rob[1].op_1 &&
		 !(isSTR(wr_entry_rob[0].inst_op) && isMemory(wr_entry_rob[1].inst_op)) &&
		 !lv_squash[0] && !isNoTrap(wr_entry_rob_exception[0]))
		 begin
			wr_entry_rob_invalidate[1] <= True;
			
			$display("COMMIT:Time:%d\nentry rob index %d freed",$time, 1);
				  
			lv_free_erob[1] = True;
				  
			lv_squash[1] = wr_entry_rob_squash[1];
			
			if(wr_entry_rob[1].imm_valid || isSystem(wr_entry_rob[1].inst_op))
			   begin
						
				  lv_imm_valid[1] = True;
				  
				  wr_invalidate_imm[1] <= tagged Valid wr_entry_rob[1].imm_index;
				  
				  $display("COMMIT:imm_buf index %d freed", wr_entry_rob[1].imm_index);
				  
			   end

		if(isLD(wr_entry_rob[1].inst_op) && isLD(wr_entry_rob[0].inst_op)) begin
		  lv_is_load[1] = True;
		  lv_squash_from_ls_unit[1] = wr_commit_load[1];
		end
		else if(isLD(wr_entry_rob[1].inst_op)) begin
		  lv_is_load[1] = True;
		  lv_squash_from_ls_unit[1] = wr_commit_load[0];
		end
	  end
		//if(wr_entry_rob[1].inst_op matches tagged System)
		//	wr_update_csr[1] <= True;
		//	
		//if(wr_entry_rob[1].inst_op matches tagged Memory .memy 
		//	&&& memy.mem_type == STR) begin
		//		wr_commit_store[1] <= True;				  
		//end
        //else if(memy.mem_type==LD) begin
		//			lv_is_load[1] = True;
        //  lv_squash_from_ls_unit[1] = wr_commit_load[1];
        //end
		//	end
		//end
		//else if(wr_entry_rob[1].inst_op matches tagged Memory .memy 
		//		&&& memy.mem_type == STR) begin
		//			wr_commit_store[1] <= True;				  
		//end
		//else if(wr_entry_rob[1].inst_op matches tagged Memory .memy 
		//		&&& memy.mem_type == LD) begin
		//		lv_is_load[1] = True;
      	//lv_squash_from_ls_unit[1] = wr_commit_load[0];
		//end
	  wr_is_load <= lv_is_load;
	    //for(Integer i = 0; i < `FETCH_WIDTH; i=i+1) begin
		// lv_squash_from_ls_unit[i] = wr_commit_load[i];
	    //end
	  
	  //update immediate buffer head pointer
	  if(lv_imm_valid[0] && lv_imm_valid[1])
		 wr_update_imm_buf_head <= 2'b10;
	  else if(lv_imm_valid[0] || lv_imm_valid[1])
		 wr_update_imm_buf_head <= 2'b01;
	  $display("COMMIT:imm_valid %d %d %d", lv_imm_valid[0], lv_imm_valid[1], $time);

	  


	  if(!lv_squash_from_ls_unit[0])
		 begin
				  
			//add back the register to free register queue
				  
			wr_frq[0] <= FRQ_entry {
			   free_reg: lv_reg_to_free[0],
			   valid: True
			   };
			
	  
			$display("COMMIT:%d lv_reg_to_free_0 added back to frq", lv_reg_to_free[0]);				  
				  
			//update RRAM				  
			//RRAM should be update only for the instructions with valid rd values. If dest_arch of instr0
			//and dest_arch of instr1 are equal, we should update rRAM only once. MAKE SURE that dest_arch of
			// instr1 is a valid one. Also, RRAM should
			//not be updated when dest_arch==0 as it is hardwired to 0.
		if(!(isSTR(wr_entry_rob[0].inst_op) || isCond(wr_entry_rob[0].inst_op)) && 
			   wr_entry_rob[0].dest_arch != 0 && 
			   !(lv_free_erob[1] && !lv_squash_from_ls_unit[1] &&  
			   !(isSTR(wr_entry_rob[1].inst_op) || isCond(wr_entry_rob[1].inst_op)) &&
				  (wr_entry_rob[0].dest_arch==wr_entry_rob[1].dest_arch)))
			   begin
				  wr_update_rRAM[0] <= True;
			   end
		 end
	  else
		 begin
			
			wr_frq[0] <= FRQ_entry {
			   free_reg: wr_entry_rob[0].dest_op,
			   valid: True
			   };
			
	  
			$display("COMMIT:%d lv_dest_op_0 added back to frq", wr_entry_rob[0].dest_op);
		 end
	  
	  if(lv_free_erob[1])
		 begin
			
			//if instr1 is squashed
			if(!lv_squash_from_ls_unit[1] && !lv_squash_from_ls_unit[0])
			   begin
				  
				  //add back the register to free register queue
			
				  wr_frq[1] <= FRQ_entry {
					 free_reg: lv_reg_to_free[1],
					 valid: True
					 };
				  
				  
				  $display("COMMIT:%d lv_reg_to_free_1 added back to frq", lv_reg_to_free[1]);
				  
					if(!(isSTR(wr_entry_rob[1].inst_op) || isCond(wr_entry_rob[1].inst_op)) && 
					 wr_entry_rob[1].dest_arch != 0)
					 wr_update_rRAM[1] <= True;
			   end
			else
			   begin
				  //add back the register to free register queue
				  
				  wr_frq[1] <= FRQ_entry {
					 free_reg: wr_entry_rob[1].dest_op,
					 valid: True
					 };
				  
				  
				  $display("COMMIT:%d lv_dest_op_1 added back to frq", lv_reg_to_free[1]);
			   end
		 end
		
	  //update the squash wire
	  if(!isNoTrap(wr_entry_rob_exception[0]))	
		 begin
			lv_exception = wr_entry_rob_exception[0];
			lv_pc = wr_entry_rob[0].program_counter;
		 end
	  else if(lv_squash[0] || lv_squash_from_ls_unit[0])
		 begin
			$display("COMMIT:wr_squash_pc written with %d", wr_squash_buf[0]);
			wr_squash_pc <= tagged Valid wr_squash_buf[0];
		 end
	  else if(!isNoTrap(wr_entry_rob_exception[1]))
		 begin
			lv_exception = wr_entry_rob_exception[1];
			lv_pc = wr_entry_rob[1].program_counter;
		 end
	  else if(lv_squash[1] || lv_squash_from_ls_unit[1])
		 begin
			wr_squash_pc <= tagged Valid wr_squash_buf[1];
			$display("COMMIT:wr_squash_pc written with %d", wr_squash_buf[1]);
		 end
	  wr_cause_trap <= lv_exception; 
	  wr_pc <= lv_pc;
	  
	  //update entry rob head
	  if(lv_free_erob[1])
		 begin
			wr_update_erob_head <= 2'b10;
			wr_update_frq_tail <=  2'b10;
		 end
	  else
		 begin
			wr_update_erob_head <= 2'b01;
			wr_update_frq_tail <=  2'b01;
		 end
	  //Bit#(64) lv_cause = 64'b0;  

	  //case(lv_exception)
	  //  
	  //  Instruction_misaligned : lv_cause = 64'b0;  

	  //  Instruction_access_fault : lv_cause = 64'b1;	

	  //  Illegal_instruction : lv_cause = {62'b0, 2'b10};

	  //  Breakpoint : lv_cause = {62'b0, 2'b11};

	  //  Load_address_misaligned : lv_cause = {61'b0, 3'b100};

	  //  Load_access_fault		: lv_cause = {61'b0, 3'b101};

	  //  Store_address_misaligned : lv_cause = {61'b0, 3'b110};

	  //  Store_access_fault 		 : lv_cause = {61'b0, 3'b111};

	  //  Environment_call_from_M_mode : lv_cause = {60'b0, 4'b1000};

	  //endcase

	  //if(lv_exception != No_exception)
	  //	wr_cause_trap <= tagged Valid lv_cause; 

	  //if(lv_exception == Instruction_misaligned || lv_exception == Instruction_access_fault)
	  //  wr_pc <= tagged Valid lv_pc;
	  
	  if(lv_exception matches tagged Exception .exception &&& exception == IllegalInst)
		$stop(0);
		
////////////////////////////////////////////////////////////////////////////////
///VERIFICATION FRAMEWORK
////////////////////////////////////////////////////////////////////////////////

	  Bit#(`REG_WIDTH) lv_reg_value_0[`REGFILE_SIZE];
	  Bit#(`REG_WIDTH) lv_reg_value_1[`REGFILE_SIZE];
	  
	  for(Integer i=0;i<`REGFILE_SIZE;i=i+1)
		 begin
			lv_reg_value_0[i] = wr_ifc_regFile[wr_rRAM[i]];
			lv_reg_value_1[i] = wr_ifc_regFile[wr_rRAM[i]];
		 end
	  
	  
	  //if dest_arch==0, RRAM won't be updated
	  if(!(isSTR(wr_entry_rob[0].inst_op) || isCond(wr_entry_rob[0].inst_op) || 
		 wr_entry_rob[0].dest_arch == 0))
		 begin
			lv_reg_value_0[wr_entry_rob[0].dest_arch] = wr_ifc_regFile[wr_entry_rob[0].dest_op];
			lv_reg_value_1[wr_entry_rob[1].dest_arch] = wr_ifc_regFile[wr_entry_rob[1].dest_op];
		 end

	  
	  if(!lv_squash_from_ls_unit[0])
		 begin
			
			$display("COMMIT:Time:%d\ninstruction with PC %h committed", $time, wr_entry_rob[0].program_counter);
			
			$fwrite(rg_dump_file, "PC = %h\n", wr_entry_rob[0].program_counter);
			for(Integer i=1;i<`REGFILE_SIZE;i=i+1)
			   $fwrite(rg_dump_file, "REG %d %h\n",i, lv_reg_value_0[i]);
			$fwrite(rg_dump_file, "\n");
		 end
	  
	  if(lv_free_erob[1])
		 begin
			
			if(isSTR(wr_entry_rob[1].inst_op) || isCond(wr_entry_rob[1].inst_op) || 
			   wr_entry_rob[1].dest_arch != 0 || !lv_squash_from_ls_unit[0])
			   begin
				  lv_reg_value_1[wr_entry_rob[1].dest_arch] = 
				  wr_ifc_regFile[wr_entry_rob[1].dest_op];
			   end
			
			if(!lv_squash_from_ls_unit[0] &&
			   !lv_squash_from_ls_unit[1])
			   begin
				  
				  $display("COMMIT:Time:%d\ninstruction with PC %h committed", $time, wr_entry_rob[1].program_counter);
				  
				  $fwrite(rg_dump_file, "PC = %d\n", wr_entry_rob[1].program_counter);
				  for(Integer i=0;i<`REGFILE_SIZE;i=i+1)
					 $fwrite(rg_dump_file, "REG %d %h\n",i, lv_reg_value_1[i]);
				  $fwrite(rg_dump_file, "\n");
			   end
		 end
		

////////////////////////////////////////////////////////////////////////////////
///VERIFICATION FRAMEWORK
////////////////////////////////////////////////////////////////////////////////

	 	 if(wr_entry_rob[0].program_counter[19:0] > 'd80000)
			$stop(0);
	 	 	//rg_stop <= True;	
	  
		 	  
   endrule: rl_commit

//Input methods

method Action flush_signals(Bool revert);
    wr_revert_map <= revert;
	$display("COMMIT:if_revert_map %d", revert);
endmethod

method Action erob_head_entry(Vector#(`FETCH_WIDTH, Entry_rob_type) entry);
    for(Integer i=0; i<`FETCH_WIDTH; i=i+1)
	wr_entry_rob[i] <= entry[i];	
endmethod

method Action imm_head_entry(Vector#(`FETCH_WIDTH, Imm_buf_entry) entry);
    for(Integer i=0; i<`FETCH_WIDTH; i=i+1)
	wr_imm_buf[i] <= entry[i];	
endmethod

method Action entry_rob_execute_done(Vector#(`FETCH_WIDTH, Bool) execute_done);
    for(Integer i=0; i<`FETCH_WIDTH; i=i+1)
	wr_entry_rob_execute_done[i] <= execute_done[i];
	$display("COMMIT:if_execute_done %d", execute_done[0]);
endmethod

method Action entry_rob_exceptions(Vector#(`FETCH_WIDTH, TrapCause) exception);
    for(Integer i=0; i<`FETCH_WIDTH; i=i+1)
	wr_entry_rob_exception[i] <= exception[i];
endmethod

method Action squash_buf_entry(Vector#(`FETCH_WIDTH, Bit#(`REG_WIDTH)) entry);
    for(Integer i=0; i<`FETCH_WIDTH; i=i+1)
	wr_squash_buf[i] <= entry[i];
endmethod
	
method Action get_rRAM_entry_1(Bit#(TLog#(`PRF_SIZE)) entry);
    wr_rRAM_entry[0] <= entry;
endmethod

method Action get_rRAM_entry_2(Bit#(TLog#(`PRF_SIZE)) entry);
    wr_rRAM_entry[1] <= entry;
endmethod

method Action commit_load(Vector#(`FETCH_WIDTH, Bool) load_commit);
    for(Integer i=0; i<`FETCH_WIDTH; i=i+1)
	wr_commit_load[i] <= load_commit[i];
endmethod

method Action squash_buf_status(Vector#(`FETCH_WIDTH, Bool) squash);
    for(Integer i=0; i<`FETCH_WIDTH; i=i+1)
	wr_entry_rob_squash[i] <= squash[i];
endmethod

//method Action to_stall(Bool stall);
//	wr_to_stall <= stall;
//	$display("COMMIT:if_stall %d", stall);
//endmethod

//Output methods
method Bit#(TLog#(`REGFILE_SIZE)) send_rRAM_slot_1;
    return wr_rRAM_slot[0];
endmethod

method Bit#(TLog#(`REGFILE_SIZE)) send_rRAM_slot_2;
    return wr_rRAM_slot[1];
endmethod

method Bool erob_invalidate_1;
    return wr_entry_rob_invalidate[0];
endmethod

method Bool erob_invalidate_2;
    return wr_entry_rob_invalidate[1];
endmethod

method Maybe#(Bit#(TLog#(`IMM_BUF_SIZE))) invalidate_imm_slot_1;
    return wr_invalidate_imm[0];
endmethod

method Maybe#(Bit#(TLog#(`IMM_BUF_SIZE))) invalidate_imm_slot_2;
    return wr_invalidate_imm[1];
endmethod

method Bool commit_store_1;
    return wr_commit_store[0];
endmethod

method Bool commit_store_2;
    return wr_commit_store[1];
endmethod

method Vector#(`FETCH_WIDTH, Bool) is_inst_load;
    return  wr_is_load;
endmethod

method Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)) update_imm_head;
    return wr_update_imm_buf_head;
endmethod

method  Bool update_rRAM_1;
    return wr_update_rRAM[0];
endmethod

method  Bool update_rRAM_2;
    return wr_update_rRAM[1];
endmethod

method Maybe#(Bit#(`REG_WIDTH)) squash_pc; 
    return wr_squash_pc;
endmethod

method FRQ_entry frq_update_1; 
    return wr_frq[0];
endmethod

method FRQ_entry frq_update_2; 
    return wr_frq[1];
endmethod

method Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)) update_frq_tail;
    return wr_update_frq_tail;
endmethod
 
method Bit#(TAdd#(TLog#(`FETCH_WIDTH),1)) update_erob_head;
    return wr_update_erob_head;
endmethod

method TrapCause return_exception;
	return wr_cause_trap;
endmethod

method Bit#(`REG_WIDTH) return_pc;
	return wr_pc;
endmethod

method Vector#(`FETCH_WIDTH, Bool) update_csr_registers;
	Vector#(`FETCH_WIDTH, Bool) lv_if_update;
	for(Integer i = 0; i < `FETCH_WIDTH; i = i+1)
		lv_if_update[i] = wr_update_csr[i];
	return lv_if_update;
endmethod

/**************************** VERIFICATION ENVIRONMENT *******************************/
method Action _register_values(Vector#(`PRF_SIZE, Bit#(`REG_WIDTH)) prf);
    for(Integer i = 0; i < `PRF_SIZE; i=i+1) 
	wr_ifc_regFile[i] <= prf[i];
endmethod

method Action _rRAM_values(Vector#(`REGFILE_SIZE, RAT_entry) rRAM_entries);
    for(Integer i = 0; i < `REGFILE_SIZE; i=i+1) 
	wr_rRAM[i] <= rRAM_entries[i];
endmethod

endmodule
endpackage 

