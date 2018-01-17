/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name : bpu_bimodal
Author Name : Priyanka Gomatam, Neel Gala
Email id : priyanka.gomatam28@gmail.com, neelgala@gmail.com
Last updated on : 25th Dec 2013

Description:
This module implements a Bimodal Branch Predictor Unit. The Bimodal BPU uses a 2-bit prediction scheme, for there are 4 states that the branch instruction can take
    >Strongly not taken (00)
    >Weakly Not Taken (01)
    >Weakly Taken (10)
    >Strongly Taken (11)
A saturation counter is used for keeping track of the states. The state of each branch instruction is updated after it passes through the execution unit. The state is incremented or decremented if the branch is taken or not taken, respectively. 
Since this unit uses a 2-bit saturation scheme, the branch jump condition has to deviate twice before the prediction changes. 

In the case of an interrupt, the pipeline and the BPU are flushed and the PC is updated to service the generated interrupt. Same goes for any flush
generated from the external environment which causes the PC to be changed.

The hardware used to implement the prediction unit include the following: 
    * An array of registers are used to hold the following values of every branch instruction. The LSBs of the program counter are used to index them.
        > State of the instruction.
        > Valid bit of the instruction.
        > Target address of the instruction.
        > Tag value, which holds the remaining MSBs of the program counter.
    * A FIFO that contains the PC value and whether the jump has been taken or not taken. It basically contains the output of the BPU.
        This FIFO is dequed by the external environment ( possibly the decode stage) to acknowledge that the PC from the FIFO has been read.
   
*/

package bpu_bimodal;

`include "defined_parameters.bsv"
import FIFO :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import defined_types::*;
import RegFile::*;
import BRAM ::*;
import DReg ::*;

        
    interface Ifc_bpu_bimodal;
		`ifdef bpu
      method Action _training ((Bit#(`Addr_width)) pc, Bit#(`Addr_width) addr, Actual_jump branch_taken_or_not); //to train the bpu
		`endif
    	method ActionValue#(Predictor_output) send_output_();
			method Action deq_fifo;
      method Action _flush(Bit#(`Addr_width) new_pc_); // flushes the current excution and resets the PC.
			method Prediction_type pred_type;
    endinterface
    
  (*synthesize*)
  module mkbpu_bimodal (Ifc_bpu_bimodal);
    
    Reg#(Bit#(`Addr_width)) rg_currentpc_ <- mkReg('h00001000); //program counter register
		Reg#(Prediction_type) rg_current_prediction <-mkReg(Predicted_notaken);
    Wire#(Bool) wr_flush <- mkDWire(False);
    Wire#(Bit#(`Addr_width)) wr_new_pc <- mkDWire(0);
		Reg#(Bool) wr_fire_me[4] <-mkCReg(4,False);
    `ifdef bpu
			//arrays of BTB
    	BRAM_Configure cfg = defaultValue ;
    	cfg.latency=1;
    	cfg.outFIFODepth=2; // for pipelined cache

    	BRAM2Port#(Bit#(`BPU_index),Bit#(2)) rg_state_of_branch_instrn <- mkBRAM2Server(cfg);
    	BRAM2Port#(Bit#(`BPU_index),Bit#(`Addr_width)) rg_target_addr <- mkBRAM2Server(cfg);
    	BRAM2Port#(Bit#(`BPU_index),Bit#(1)) rg_valid_or_not <- mkBRAM2Server(cfg);
    	BRAM2Port#(Bit#(`BPU_index),Bit#(TSub#(`Addr_width, `BPU_index))) rg_tag <- mkBRAM2Server(cfg);
    	

    	Wire#(Actual_jump) wr_taken_or_not <- mkDWire(Notaken);//output of the exec unit, whether the branch has been taken or not
    	Wire#(Bit#(`Addr_width)) wr_target_addr <- mkDWire(0);//the target address of the branch instruction
    	Wire#(Bit#(`Addr_width)) wr_currentpc_ <- mkDWire(0);
    	Wire#(Bit#(TSub#(`Addr_width, `BPU_index))) wr_tag <- mkDWire(0);
    	Wire#(Bool) wr_fire <- mkDWire(False); 
			Reg#(Bit#(`BPU_index)) rg_counter <-mkReg(0);
			Reg#(Bool) rg_initialize <-mkReg(True);
    	Reg#(Bit#(`BPU_index)) rg_training_index <-mkReg(0);
    	Reg#(Bit#(TSub#(`Addr_width,`BPU_index))) rg_training_tag<-mkReg(0);
    	Reg#(Bit#(`Reg_width)) rg_training_addr<-mkReg(0);
    	Reg#(Actual_jump) rg_training_pred<-mkReg(Notaken);
    	Reg#(Bool) rg_training_fire <-mkDReg(False);
			Reg#(Bool) rg_fire_once<-mkDReg(False);


    	rule initialize_bp(rg_initialize);
				if(rg_counter==fromInteger(valueOf(TExp#(TSub#(`BPU_index,1)))))begin
					rg_initialize<=False;
					rg_fire_once<=True;
					wr_fire_me[0]<=True;
				end
				else
					rg_counter<=rg_counter+1;
				rg_state_of_branch_instrn.portA.request.put(BRAMRequest{write:True,address:rg_counter,datain:1,responseOnWrite:False});
				rg_target_addr.portA.request.put(BRAMRequest{write:True,address:rg_counter,datain:0,responseOnWrite:False});
				rg_valid_or_not.portA.request.put(BRAMRequest{write:True,address:rg_counter,datain:0,responseOnWrite:False});
				rg_tag.portA.request.put(BRAMRequest{write:True,address:rg_counter,datain:0,responseOnWrite:False});
    	endrule

/*
    	This rule will fire once for every training data entered. It updates the state of the branch instruction depending on whether it has been taken or not. 
    	The corresponding state of the instruction is indexed by its lower 4  bits.
*/
    	rule rl_training_request(wr_fire && !rg_initialize && !rg_training_fire);
//				$display($time,"\tBPU: Training: Sending request to BRAM for Address: :%h",wr_currentpc_);
    	  Bit#(`BPU_index) index = wr_currentpc_[`BPU_index-1:0];
    	  Bit#(TSub#(`Addr_width, `BPU_index)) tag = wr_currentpc_[31: `BPU_index];
    	  rg_state_of_branch_instrn.portA.request.put(BRAMRequest{write:False,address:index,datain:0,responseOnWrite:False});
    	  rg_tag.portA.request.put(BRAMRequest{write:False,address:index,datain:0,responseOnWrite:False});
    	  rg_training_index<=index;
    	  rg_training_tag<=tag;
    	  rg_training_addr<=wr_target_addr;
    	  rg_training_pred<=wr_taken_or_not;
    	  rg_training_fire<=True;
    	endrule

    	rule rl_training_response (rg_training_fire && !rg_initialize);
    	    rg_valid_or_not.portA.request.put(BRAMRequest{write:True,address:rg_training_index,datain:1,responseOnWrite:False});
    	    let stored_tag<-rg_tag.portA.response.get();
					let new_upd<-rg_state_of_branch_instrn.portA.response.get();
					if(stored_tag!=rg_training_tag)begin
    	      rg_tag.portA.request.put(BRAMRequest{write:True,address:rg_training_index,datain:rg_training_tag,responseOnWrite:False});
    	      rg_target_addr.portA.request.put(BRAMRequest{write:True,address:rg_training_index,datain:rg_training_addr,responseOnWrite:False});
						if(rg_training_pred==Taken)begin
//							$display($time,"\tBPU: Training PC:%h as New Taken in index: %h with tag: %h",{rg_training_tag,rg_training_index},rg_training_index,rg_training_tag);
    	        rg_state_of_branch_instrn.portA.request.put(BRAMRequest{write:True,address:rg_training_index,datain:1,responseOnWrite:False});
						end
						else begin
    	        rg_state_of_branch_instrn.portA.request.put(BRAMRequest{write:True,address:rg_training_index,datain:0,responseOnWrite:False});
//							$display($time,"\tBPU: Training PC:%h as New NotTaken in index: %h with tag: %h",{rg_training_tag,rg_training_index},rg_training_index,rg_training_tag);
						end
					end
					else begin
						if(rg_training_pred == Taken && new_upd < 3) begin
    	        rg_target_addr.portA.request.put(BRAMRequest{write:True,address:rg_training_index,datain:rg_training_addr,responseOnWrite:False});
    	        rg_state_of_branch_instrn.portA.request.put(BRAMRequest{write:True,address:rg_training_index,datain:new_upd+1,responseOnWrite:False});
//							$display($time,"\tBPU: Training PC:%h as old Taken in index: %h tag: %h",{rg_training_tag,rg_training_index},rg_training_index,rg_training_tag);
						end
						else if(rg_training_pred == Notaken && new_upd > 0) begin
    	        rg_state_of_branch_instrn.portA.request.put(BRAMRequest{write:True,address:rg_training_index,datain:new_upd-1,responseOnWrite:False});
//							$display($time,"\tBPU: Training PC:%h as old NotTaken in index: %h tag: %h",{rg_training_tag,rg_training_index},rg_training_index,rg_training_tag);
						end      
					end
    	endrule

/*This rule fires at every clock cycle, updating the PC Value.
  If the state is weakly/strongly taken, the PC is updated to the target address of the branch.
  If the state is weakly/strongly not taken, the PC is updated to hold the next instruction after the branch instruction.
*/
    rule rg_prediction_request(!wr_flush && !rg_initialize && rg_fire_once);
    	Bit#(`BPU_index) index = rg_currentpc_[`BPU_index-1:0];
    	Bit#(TSub#(`Addr_width, `BPU_index)) tag = rg_currentpc_[31: `BPU_index];
//			$display($time,"\tBPU: Sending request after flush Index: %h Tag: %h",index,tag);
      rg_target_addr.portB.request.put(BRAMRequest{write:False,address:rg_currentpc_[`BPU_index-1:0],datain:0,responseOnWrite:False});
      rg_valid_or_not.portB.request.put(BRAMRequest{write:False,address:rg_currentpc_[`BPU_index-1:0],datain:0,responseOnWrite:False});
      rg_tag.portB.request.put(BRAMRequest{write:False,address:rg_currentpc_[`BPU_index-1:0],datain:0,responseOnWrite:False});
      rg_state_of_branch_instrn.portB.request.put(BRAMRequest{write:False,address:rg_currentpc_[`BPU_index-1:0],datain:0,responseOnWrite:False});
    endrule

		rule rl_prediction_output(!wr_flush && !rg_fire_once && !rg_initialize && wr_fire_me[2]);
				wr_fire_me[2]<=False;
        let branch_address <- rg_target_addr.portB.response.get();
        let valid_or_not<-rg_valid_or_not.portB.response.get();
        let state<-rg_state_of_branch_instrn.portB.response.get();
        let tag <-rg_tag.portB.response.get();
        Bit#(`Addr_width) actual_increment_addr = rg_currentpc_+4;
    		Bit#(`BPU_index) index1 = rg_currentpc_[`BPU_index-1:0];
    		Bit#(TSub#(`Addr_width, `BPU_index)) tag1 = rg_currentpc_[31: `BPU_index];
        if(valid_or_not==1 && state >= 2 && tag == rg_currentpc_[31:`BPU_index]) begin
	    		$display($time,"\tBPU: Enquing PC Taken : %h Index: %h Tag: %h",rg_currentpc_,index1,tag1);
          rg_currentpc_ <= branch_address;        
					actual_increment_addr=branch_address;
					rg_current_prediction<=Predicted_taken;
        end
        else begin
	    		$display($time,"\tBPU: Enquing PC Not Taken : %h Index: %h Tag: %h",rg_currentpc_,index1,tag1);
          rg_currentpc_ <= rg_currentpc_+4;
					rg_current_prediction<=Predicted_notaken;
        end
    		Bit#(`BPU_index) index2 = actual_increment_addr[`BPU_index-1:0];
    		Bit#(TSub#(`Addr_width, `BPU_index)) tag2 = actual_increment_addr[31: `BPU_index];
//				$display($time,"\tBPU: Sending new Index: %h Tag: %h",index2,tag2);
      	rg_target_addr.portB.request.put(BRAMRequest{write:False,address:actual_increment_addr[`BPU_index-1:0],datain:0,responseOnWrite:False});
      	rg_valid_or_not.portB.request.put(BRAMRequest{write:False,address:actual_increment_addr[`BPU_index-1:0],datain:0,responseOnWrite:False});
      	rg_tag.portB.request.put(BRAMRequest{write:False,address:actual_increment_addr[`BPU_index-1:0],datain:0,responseOnWrite:False});
      	rg_state_of_branch_instrn.portB.request.put(BRAMRequest{write:False,address:actual_increment_addr[`BPU_index-1:0],datain:0,responseOnWrite:False});
    endrule

		rule flush_entire_pipe(wr_flush);
//			$display($time,"\tBPU: Flushing. Restarting with PC: %h",wr_new_pc);
//			fifo_pc_.clear();
      rg_currentpc_ <= wr_new_pc;
			rg_current_prediction<=Predicted_notaken;
			rg_fire_once<=True;
			rg_target_addr.portBClear();
			rg_valid_or_not.portBClear();
			rg_tag.portBClear();
			rg_state_of_branch_instrn.portBClear();
		endrule

/*This method is used to train the BPU.
  The method is called with the training info as arguments
*/

    method Action _training ((Bit #(`Addr_width)) _current_pc, Bit #(`Addr_width) _targ_addr, Actual_jump _branch_taken_or_not);
//				$display($time,"\tBPU: Training method called");
        wr_currentpc_ <= _current_pc;
        wr_target_addr <= _targ_addr;
        wr_taken_or_not <= _branch_taken_or_not;
        wr_fire <= True;
    endmethod


    method ActionValue#(Predictor_output) send_output_()if(!rg_initialize && !rg_fire_once);
			wr_fire_me[1]<=True;
			return Predictor_output{prog_counter_:rg_currentpc_,prediction_:rg_current_prediction};
    endmethod

	`endif

	`ifndef bpu
		rule flush_old_pc(wr_flush);
				rg_currentpc_<=wr_new_pc;
		endrule
		rule increment_pc(wr_fire_me[2] && !wr_flush);
			wr_fire_me[2]<=False;
			rg_currentpc_<=rg_currentpc_+4;
		endrule
    method ActionValue#(Predictor_output) send_output_();
			wr_fire_me[1]<=True;
			return Predictor_output{prog_counter_:rg_currentpc_,prediction_:rg_current_prediction};
    endmethod
	`endif
		method Action deq_fifo;
				wr_fire_me[0]<=True;
		endmethod

    method Action _flush(Bit#(`Addr_width) new_pc_);
				wr_new_pc<=new_pc_;
				wr_flush<=True;
    endmethod
		method Prediction_type pred_type;
			return rg_current_prediction;
		endmethod
  endmodule
endpackage
