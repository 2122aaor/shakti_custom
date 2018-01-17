/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Names : Rahul Bodduna, N Sireesh.
Email ID : rahul.bodduna@gmail.com
*/

package  LS_unit;

`include "defined_parameters.bsv"
import riscv_types::*;

import Vector::*;
import ConfigReg::*;
import FIFO::*;
import SpecialFIFOs::*;
import GetPut::*;

interface Ifc_LS_unit;
   
   method Action inputs(Bit#(`REG_WIDTH) base, Bit#(`REG_WIDTH) offset, Bit#(TLog#(`MEMQ_SIZE)) mem_q_index, Bit#(TLog#(`PRF_SIZE)) dest_op, Maybe#(Bit#(`REG_WIDTH)) str_data,  Mem_size mem_size);
   method Action allot_mem_q(Vector#(`FETCH_WIDTH, Mem_type) req);
   method ActionValue#(Load_Broadcast_type) get_load_broadcast_packet();
   method ActionValue#(Broadcast_type) get_store_broadcast_packet();
   method Action commit_store();
   method Action commit_load(Vector#(`FETCH_WIDTH, Bool) vc_is_load);
   method Vector#(`FETCH_WIDTH,Bool) if_load_aliased;
   method Bool is_load_q_full();
   method Bool is_store_q_full();
   method Bit#(TLog#(`MEMQ_SIZE)) load_q_tail();
   method Bit#(TLog#(`MEMQ_SIZE)) store_q_tail();
   method Action clear_mem_queues();
   
   method ActionValue#(Load_request) get_dcache_read_req();
   method Store_request get_dcache_store_req();
   
   method Action put_load_data(Load_Broadcast_type load_data);
   
   method Bit#(`ADDRESS_WIDTH) return_head_store_address();
   method Bit#(`ADDRESS_WIDTH) return_head_load_address();
endinterface

   
(*synthesize*)
module mkLS_unit(Ifc_LS_unit);

 
   //Load and store queues are implemented as circular buffers.
   //Will be filled during dispatch and freed during commit.
   Vector#(`MEMQ_SIZE, Array#(Reg#(StoreQ_type))) store_q <- replicateM(mkCReg(1,
	  StoreQ_type {
		 filled: False,
		 valid: False,
		 str_addr: 0,
		 str_data: 0,
		 str_size: 0 
		 }));
   
   Vector#(`MEMQ_SIZE, Reg#(LoadQ_type)) load_q <- replicateM(mkConfigReg(
	  LoadQ_type {
		 filled: False,
		 valid: False,
		 store_mask: replicate(False),
		 ld_addr: 0,
		 ld_size: 0
		 }));
   
   Vector#(`MEMQ_SIZE, Reg#(Bool)) load_q_aliased <- replicateM(mkConfigReg(False));
   Vector#(`MEMQ_SIZE, Reg#(Bool)) load_q_forward_ack <- replicateM(mkConfigReg(False));
   
   //decouple 'forwarded' bit from load queue. It tells if the store
   //forwarding had occured.
   Vector#(`MEMQ_SIZE, Reg#(Bool)) load_q_forwarded <- replicateM(
	  mkConfigReg(False));
   
   Reg#(Bit#(TLog#(`MEMQ_SIZE))) rg_store_q_head <- mkConfigReg(0);
   Reg#(Bit#(TLog#(`MEMQ_SIZE))) rg_store_q_tail <- mkConfigReg(0);
   Reg#(Bit#(TLog#(`MEMQ_SIZE))) rg_load_q_head <- mkConfigReg(0);
   Reg#(Bit#(TLog#(`MEMQ_SIZE))) rg_load_q_tail <- mkConfigReg(0);

   //Broadcast wire
   Wire#(Broadcast_type) wr_broadcast <- mkDWire(
	  Broadcast_type {
					  valid: False,
					  dest_tag: 0
					  });

   //To stall execution if load and store queues are full
   Wire#(Bool) wr_store_q_full <- mkDWire(False);
   Wire#(Bool) wr_load_q_full <- mkDWire(False);
   
   
   //Inter stage buffer after effective address calculation (EAC)
   FIFO#(Load_FIFO) ff_load <- mkPipelineFIFO();
   
   //Buffers to store the final results
   FIFO#(Broadcast_type) ff_load_broadcast <- mkPipelineFIFO();
   FIFO#(Broadcast_type) ff_store_broadcast <- mkPipelineFIFO();
   FIFO#(Bit#(`REG_WIDTH)) ff_load_result <- mkPipelineFIFO();
   //FIFO to hold read request for D-cache
   FIFO#(Load_request) ff_load_request_to_cache <- mkPipelineFIFO();
   
   //wire to hold read request for D-cache
   //Wire#(Load_request) wr_read_req <- mkWire();
   
   //wire to hold store request for D-cache
   Wire#(Store_request) wr_store_req <- mkWire();
   
   //wire to hold the loaded value from D-cache
   Wire#(Load_Broadcast_type) wr_load_data <- mkWire();
   
   Wire#(Vector#(`FETCH_WIDTH, Mem_type)) wr_allot_mem_req <- mkWire();

   Wire#(Bit#(`REG_WIDTH)) wr_eff_addr <- mkWire();
   Wire#(Bit#(TLog#(`MEMQ_SIZE))) wr_mem_q_index <- mkWire();
   Wire#(Maybe#(Bit#(`REG_WIDTH))) wr_str_data <- mkWire();
   Wire#(Mem_size) wr_mem_size <- mkWire();
   
   Wire#(Bool) wr_commit_store <- mkWire();
   
   Wire#(Vector#(`FETCH_WIDTH, Bool)) wr_vc_is_load <- mkWire();
   
   
   Wire#(Bool) wr_clear_mem_queues <- mkWire();
   
   FIFO#(Bool) ff_ls_free <- mkPipelineFIFO();
   
////////////////////////////////////////////////////////////////////////////////
///VERIFICATION FRAMEWORK
////////////////////////////////////////////////////////////////////////////////

   Reg#(Bool) rg_open_dump_file <- mkReg(True);
   
   let rg_store_dump_file <- mkReg(InvalidFile);
   
   rule rl_open_ls_dump_file(rg_open_dump_file);
	  String store_dumpFile = "store.txt";
	  File fl_store_dump <- $fopen(store_dumpFile, "w");
	  if(fl_store_dump==InvalidFile)
		 $finish();
	  rg_open_dump_file <= False;
	  
	  rg_store_dump_file <= fl_store_dump;

   endrule
   
   
////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

   
   (* conflict_free = "rl_process_store, rl_process_load, rl_allot_mem_q, rl_commit_load, rl_commit_store, rl_clear_mem_queues" *)

   (* conflict_free = "rl_access_memory, rl_get_load_result, rl_allot_mem_q, rl_clear_mem_queues" *)
   
   //(* execution_order = "get_dcache_read_req, inputs" *)
   
   rule rl_allot_mem_q;
	  
	  Vector#(`MEMQ_SIZE, Bool) lv_store_mask[`FETCH_WIDTH];

	  $display("Time:%d\nallot_mem_q method called",$time);
	  //create store mask vector
	  for(Integer i=0;i<`FETCH_WIDTH;i=i+1)
		 for(Integer j=0;j<`MEMQ_SIZE;j=j+1)
			lv_store_mask[i][j] = store_q[j][0].filled;
   
	  //if instr0 is 'store', it should be added to mask
	  if(wr_allot_mem_req[0]==STR)
		 lv_store_mask[1][rg_store_q_tail] = True;
   
	  //if instr0 is 'load'
	  if(wr_allot_mem_req[0]==LD)
		 begin
			load_q[rg_load_q_tail] <= LoadQ_type {
					 filled: True,
					 valid: False,
					 store_mask: lv_store_mask[0],
					 ld_addr: 0,
			         ld_size: 0
					 };
			load_q_forwarded[rg_load_q_tail] <= False;
			load_q_aliased[rg_load_q_tail] <= False;
			load_q_forward_ack[rg_load_q_tail] <= False;
			
			//if instr0 and instr1 are 'loads'
			if(wr_allot_mem_req[1]==LD)
			   begin
				  load_q[rg_load_q_tail+1] <= LoadQ_type {
					 filled: True,
					 valid: False,
					 store_mask: lv_store_mask[1],
					 ld_addr: 0,
					 ld_size: 0
					 };
				  load_q_forwarded[rg_load_q_tail+1] <= False;
				  load_q_aliased[rg_load_q_tail+1] <= False;
				  load_q_forward_ack[rg_load_q_tail+1] <= False;
				  rg_load_q_tail <= rg_load_q_tail+2;
			   end
			//if instr0 is 'load' and instr1 is not load
			else
			   rg_load_q_tail <= rg_load_q_tail+1;
		 end
   
	  //if instr0 is not load and instr1 is 'load'
	  else if(wr_allot_mem_req[1]==LD)
		 begin
			load_q[rg_load_q_tail] <= LoadQ_type {
			   filled: True,
			   valid: False,
			   store_mask: lv_store_mask[1],
			   ld_addr: 0,
			   ld_size: 0
			   };
			load_q_forwarded[rg_load_q_tail] <= False;
			load_q_aliased[rg_load_q_tail] <= False;
			load_q_forward_ack[rg_load_q_tail] <= False;
			rg_load_q_tail <= rg_load_q_tail+1;
		 end
		 
	  
   
	  //if instr0 is 'store'
	  if(wr_allot_mem_req[0]==STR)
		 begin
			store_q[rg_store_q_tail][0] <= StoreQ_type {
			   filled: True,
			   valid: False,
			   str_addr: 0,
			   str_data: 0,
			   str_size: 0
			   };
			//if instr0 and instr1 are 'stores'
			if(wr_allot_mem_req[1]==STR)
			   begin
				  store_q[rg_store_q_tail+1][0] <= StoreQ_type {
					 filled: True,
					 valid: False,
					 str_addr: 0,
					 str_data: 0,
					 str_size: 0
					 };
				  rg_store_q_tail <= rg_store_q_tail+2;
			   end
			//if instr0 is 'store' and instr1 is not store
			else
			   rg_store_q_tail <= rg_store_q_tail+1;
		 end
	  
	  //if instr0 is not store and instr1 is 'store'
	  else if(wr_allot_mem_req[1]==STR)
		 begin
			store_q[rg_store_q_tail][0] <= StoreQ_type {
			   filled: True,
			   valid: False,
			   str_addr: 0,
			   str_data: 0,
			   str_size: 0
			   };
			rg_store_q_tail <= rg_store_q_tail+1;
		 end

   endrule
   
   rule rl_process_store(wr_str_data matches tagged Valid .data);

	  $display("eff_addr for mem_q_index %d is %d", wr_mem_q_index, wr_eff_addr);
	  
	  store_q[wr_mem_q_index][0] <= StoreQ_type {
		 filled: True,
		 valid: True,
		 str_addr: wr_eff_addr,
		 str_data: data,
		 str_size: wr_mem_size
		 };
			

	  $display("store q at index %d updated", wr_mem_q_index);
	  $display(fshow(StoreQ_type {
		 filled: True,
		 valid: True,
		 str_addr: wr_eff_addr,
		 str_data: data,
		 str_size: wr_mem_size
		 }));

   endrule
   
   rule rl_process_load(!isValid(wr_str_data));
	  	  
	  $display("eff_addr for mem_q_index %d is %d", wr_mem_q_index, wr_eff_addr);
	  
	  let lv_load_q_entry = load_q[wr_mem_q_index];
	  
	  lv_load_q_entry.valid = True;
	  lv_load_q_entry.ld_addr = wr_eff_addr;
	  
	  load_q[wr_mem_q_index] <= lv_load_q_entry;

	  $display("load q at index %d updated", wr_mem_q_index);
		    
	  
   endrule
	  
   (*descending_urgency = "rl_commit_store, rl_get_load_result, rl_access_memory"*) 
   /* Check for store-load forwarding and access memory for load instructions */
   rule rl_access_memory;
	  
	  let lv_load = ff_load.first();
	  Bit#(TLog#(`MEMQ_SIZE)) lv_forward_index = 0;
	  Bool lv_forward_valid = False;
	  ff_load.deq();
	  
	  $display("load being executed");
	  //CAM check store queue for store forwarding 
	  for(Integer i=0;i<`MEMQ_SIZE;i=i+1)
		 begin
			
			//Check each entry of store q for address match. Also, forward
			//the store only if the sizes are same.
			if(store_q[i][0].filled && store_q[i][0].valid && lv_load.store_mask[i]  
			   && lv_load.eff_addr==store_q[i][0].str_addr && 
			   lv_load.ld_size== store_q[i][0].str_size)
			   begin
				  lv_forward_valid = True;
				  lv_forward_index = fromInteger(i);
			   end
		 end
	  
	  //use the forwarded value if the address matches
	  if(lv_forward_valid)
		 begin
			load_q_forwarded[lv_load.load_q_index] <= True;
			
			$display("SL forward has occured from %d", lv_forward_index);
			
			ff_load_broadcast.enq(Broadcast_type {
			   valid: True,
			   dest_tag: lv_load.dest_reg
			   });
			ff_load_result.enq(store_q[lv_forward_index][0].str_data);
		 end
	  
	  //access d-cache
	  else
		 begin
			ff_load_request_to_cache.enq(Load_request {
			   address: lv_load.eff_addr,
			   ld_size: lv_load.ld_size,
			   data   : 0,
			   ld_st  : Load, 
			   dest_reg : lv_load.dest_reg
			   });
			
		 end
	  
   endrule
   
   
   rule rl_get_load_result;

	  let lv_load_broadcast_data = wr_load_data; 
	  ff_load_result.enq(lv_load_broadcast_data.result);
	  ff_load_broadcast.enq(Broadcast_type {
			   valid: True,
			   dest_tag: lv_load_broadcast_data.dest_tag
			   });
	  
   endrule
   
   
      rule rl_commit_store(wr_commit_store);
	  
	  //invalidate the store entry
	  store_q[rg_store_q_head][0].filled <= False;
	  rg_store_q_head <= rg_store_q_head + 1;


	  $display("Time: %d\nstore commit at %d", $time, rg_store_q_head);
   
	  //write the data to data cache
	   ff_load_request_to_cache.enq(Load_request {
		 address: store_q[rg_store_q_head][0].str_addr,
		 ld_size: store_q[rg_store_q_head][0].str_size,
		 data   : store_q[rg_store_q_head][0].str_data,
	     ld_st  : Store, 
		 dest_reg: 0
		 });   
   
	  $display("writing to d-cache address %d the data %d",store_q[rg_store_q_head][0].str_addr, 
		 store_q[rg_store_q_head][0].str_data);
   
   
	  //update status of load queue
	  /* We squash the instructions in the following cases:
	     1. If the store is not forwarded.
	     2. If the data forwarded from wrong store.	
	  */
	  for(Integer i=0;i<`MEMQ_SIZE;i=i+1)
		 begin
			
			//if there is an address match
			//we match the bits 63 to 3
			//TODO: Change to parameter
			$display("compare for alias %d %d %d",i, store_q[rg_store_q_head][0].str_addr, load_q[i].ld_addr);
			if(load_q[i].filled && load_q[i].valid &&
			   (store_q[rg_store_q_head][0].str_addr[63:3]==load_q[i].ld_addr[63:3]))
			   begin
				  //if the load is forwarded already
				  if(load_q_forwarded[i])
					 begin
						//if forward_ack is set, there is a chance that
						//the wrong data is forwarded. So, set flush.
						if(load_q_forward_ack[i])
						   begin
							  $display("Alias set for LQ index %d, multiple stores to same location", i);
							  load_q_aliased[i] <= True;
						   end
						else
						   //if not, acknowledge the forward.
						   begin
							  load_q_forward_ack[i] <= True;
							  $display("Store forward acknowledged for LQ index %d", i);
						   end
					 end
				  //if there was no load forward, set flush.
				  else
					 begin
						load_q_aliased[i] <= True;
						$display("Alias set for LQ index %d, No forward", i);
					 end
			   end
			
		 end
	  
   endrule
   
      rule rl_commit_load;

   
	  $display("Time:%d\nLoad commit method called", $time);
	  
	  if(wr_vc_is_load[0])
		 begin
			//free the load entry
			load_q[rg_load_q_head].filled <= False;
			if(load_q_aliased[rg_load_q_head])
			   begin
				  $display("SQASH THE PIPE");
			   end
			$display("Time:%d\nload at index %d committed", $time, rg_load_q_head);
			//if instr0 and instr1 are loads
			if(wr_vc_is_load[1])
			   begin
				  //free load entry
				  load_q[rg_load_q_head+1].filled <= False;
				  if(load_q_aliased[rg_load_q_head+1])
					 begin
						$display("SQASH THE PIPE");
					 end
				  rg_load_q_head <= rg_load_q_head + 2;
				  $display("Time:%d\nload at index %d committed", $time, rg_load_q_head+1);
			   end
			//if instr0 is load and instr1 is not load
			else
			   rg_load_q_head <= rg_load_q_head + 1;
		 end
   
	  //if instr0 is not load and instr1 is load
	  else if(wr_vc_is_load[1])
		 begin
			load_q[rg_load_q_head].filled <= False;
			if(load_q_aliased[rg_load_q_head])
			   begin
				  $display("SQUASH THE PIPE");
			   end
			rg_load_q_head <= rg_load_q_head + 1;
			$display("Time:%d\nload at index %d committed", $time, rg_load_q_head);
		 end
			
	  
   endrule


  
   /* checks if the load and store queues are full */
   /* since we are fetching two instructions per cycle, we consider
	  that memory queue is full if less than two entries are empty */
   
   rule rl_check_mem_q_full;
	  
	  if((rg_store_q_head==rg_store_q_tail && store_q[rg_store_q_head][0].filled) || 
		 (rg_store_q_tail+1==rg_store_q_head))
		 begin
			wr_store_q_full <= True;
			$display("Time:%d\n Store Q FULL", $time);
		 end
	  if((rg_load_q_head==rg_load_q_tail && load_q[rg_load_q_head].filled) ||
		 (rg_load_q_tail+1==rg_load_q_head))
		 begin
			wr_load_q_full <= True;
			$display("Time:%d\n Load Q FULL", $time);
		 end
	  
	  if(rg_store_q_head==rg_store_q_tail && !store_q[rg_store_q_head][0].filled)
		 $display("Time:%d\n Store Q EMPTY",$time);
	  if(rg_load_q_tail==rg_load_q_head && !load_q[rg_load_q_tail].filled)
		 $display("Time:%d\n Load Q EMPTY",$time);
	  
	  
   endrule


   rule rl_show_store_contents;
	  
	  $fwrite(rg_store_dump_file, "Time:%d\nSTORE QUEUE\nHead = %d Tail = %d\n", $time,rg_store_q_head,rg_store_q_tail);
	  
	  for(Integer i=0;i<`MEMQ_SIZE;i=i+1)
		 begin
			$fwrite(rg_store_dump_file, "%d %d %d %d\n",i, store_q[i][0].filled, store_q[i][0].str_addr, store_q[i][0].str_data);
		 end

	  $fwrite(rg_store_dump_file, "LOAD QUEUE\nHead = %d Tail = %d\n",rg_load_q_head,rg_load_q_tail);
	  
	  for(Integer i=0;i<`MEMQ_SIZE;i=i+1)
		 begin
			$fwrite(rg_store_dump_file, "%d %d %d\n",i, load_q[i].filled, load_q[i].ld_addr);
		 end
	  
	  $fwrite(rg_store_dump_file, "\n");
	  
   endrule
   
   
   rule rl_clear_mem_queues(wr_clear_mem_queues);
	  
	  $display("Time:%d\nClearing memory queues", $time);
	  
	  rg_load_q_head <= 0;
	  rg_load_q_tail <= 0;
	  rg_store_q_head <= 0;
	  rg_store_q_tail <= 0;
   
	  ff_load.clear();
	  ff_store_broadcast.clear();
	  ff_load_result.clear();
	  ff_load_broadcast.clear();
	  
	  for (Integer i=0; i<`MEMQ_SIZE; i=i+1)
		 begin
			load_q[i].filled <= False;
			store_q[i][0].filled <= False;
		 end
   
   endrule

   
   
   /* This rule allots the load, store queues during map stage.
	  Handles two instructions per cycle. */
   
   method Action allot_mem_q(Vector#(`FETCH_WIDTH, Mem_type) req);
   
	  wr_allot_mem_req <= req;

   endmethod
   
   /* Calculates effective address and update the respective buffers */ 
   method Action inputs(Bit#(`REG_WIDTH) base, Bit#(`REG_WIDTH) offset, 
	  Bit#(TLog#(`MEMQ_SIZE)) mem_q_index, Bit#(TLog#(`PRF_SIZE)) dest_op, 
	  Maybe#(Bit#(`REG_WIDTH)) str_data, Mem_size mem_size);

	  Bit#(`REG_WIDTH) lv_eff_addr = base + offset;
	  if(str_data matches tagged Valid .data) begin 
		 ff_store_broadcast.enq(Broadcast_type {
			valid: True,
			dest_tag: dest_op
			});
			$display("ff_store_broadcast enqueued  %d", $time);
			$display(fshow(Broadcast_type {
			valid: True,
			dest_tag: dest_op
			}));
	  end
	  else
		 begin
			ff_load.enq(Load_FIFO {
			   store_mask: load_q[mem_q_index].store_mask,
			   eff_addr: lv_eff_addr,
			   ld_size: mem_size,
			   dest_reg: dest_op,
			   load_q_index: mem_q_index
			   });
			$display("Time:%d\nff_load enqueued with",$time);
			$display(fshow(Load_FIFO {
			   store_mask: load_q[mem_q_index].store_mask,
			   eff_addr: lv_eff_addr,
			   ld_size: mem_size,
			   dest_reg: dest_op,
			   load_q_index: mem_q_index
			   }));
		 end

   
	  wr_eff_addr <= lv_eff_addr;
	  wr_mem_q_index <= mem_q_index;
	  wr_str_data <= str_data;
	  wr_mem_size <= mem_size;

   endmethod	  

   

   /* Return Load broadcast packet */
   
   method ActionValue#(Load_Broadcast_type) get_load_broadcast_packet();
	  
	  let lv_load_value = ff_load_result.first();
	  let lv_load_broadcast = ff_load_broadcast.first();
	  let lv_return = Load_Broadcast_type {
		 valid: lv_load_broadcast.valid,
		 dest_tag: lv_load_broadcast.dest_tag,
		 result: lv_load_value
		 };
	  ff_load_broadcast.deq();
	  ff_load_result.deq();
   
	  return lv_return;
	  
   endmethod
   
   
   /* Return store broadcast packet */
   method ActionValue#(Broadcast_type) get_store_broadcast_packet();
  	  let lv_store_broadcast = ff_store_broadcast.first();  
	  ff_store_broadcast.deq();
	  return lv_store_broadcast;   
   endmethod

   
   
	  
   
   /* Commits store instruction. Free the store queue, CAM check the 
	  load queue and update the status. */
   method Action commit_store();
	  
	  wr_commit_store <= True;
   
   endmethod
   

   /* Squash the pipe instructions if the load is wrongly speculated */
   method Action commit_load(Vector#(`FETCH_WIDTH, Bool) vc_is_load);
	  
	  wr_vc_is_load <= vc_is_load;

   endmethod

   method Vector#(`FETCH_WIDTH,Bool) if_load_aliased;

	  Vector#(`FETCH_WIDTH, Bool) lv_return = replicate(False);
	  
	  for(Integer i = 0; i < `FETCH_WIDTH; i = i+1)
         lv_return[i] = load_q_aliased[rg_load_q_head + fromInteger(i)]; 
	//  if(vc_is_load[0])
	//	 begin
	//		if(load_q_aliased[rg_load_q_head])
	//		   begin
	//			  lv_return[0] = True;				  
	//		   end
	//		else if(vc_is_load[1] && load_q_aliased[rg_load_q_head+1])
	//			  lv_return[1] = True;	
	//	 end
	//  else if(vc_is_load[1] && load_q_aliased[rg_load_q_head])
	//	 lv_return[1] = True;
	  
	  return lv_return;
	  
   endmethod
   


	  
   
   /* Clears the load and store queues in case of a squash */
   method Action clear_mem_queues();
	  wr_clear_mem_queues <= True;
   endmethod
   
   /* is the load queue full? */
   method Bool is_load_q_full();
      return wr_load_q_full;
   endmethod
   
   /* is the store queue full? */
   method Bool is_store_q_full();
      return wr_store_q_full;
   endmethod
   

   /* methods to send tail pointers of memory queues to main pipe */
   method Bit#(TLog#(`MEMQ_SIZE)) load_q_tail();
	  return rg_load_q_tail;
   endmethod
   
   
   method Bit#(TLog#(`MEMQ_SIZE)) store_q_tail();
	  return rg_store_q_tail;
   endmethod
   
   method ActionValue#(Load_request) get_dcache_read_req();
  	  let lv_load_request = ff_load_request_to_cache.first(); 
	  ff_load_request_to_cache.deq;
	  return lv_load_request;
   endmethod
   
   method Store_request get_dcache_store_req();
	  return wr_store_req;
   endmethod
   
   method Action put_load_data(Load_Broadcast_type load_data);
	  wr_load_data <= load_data;
   endmethod
      
   method Bit#(`ADDRESS_WIDTH) return_head_store_address();
	  return store_q[rg_store_q_head][0].str_addr;
   endmethod

   method Bit#(`ADDRESS_WIDTH) return_head_load_address();
	  return load_q[rg_load_q_head].ld_addr;
   endmethod 

   
endmodule

endpackage 
