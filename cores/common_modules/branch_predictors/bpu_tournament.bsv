/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name : bpu_tournament
Author Name : Priyanka Gomatam, Neel Gala
Email id : priyanka.gomatam28@gmail.com, neelgala@gmail.com
Last updated on : 2nd Jan 2014

Description:
This module implements a Tournament Branch Predictor Unit. It chooses one of the two branch predictors: bimodal or global. 

Both predictors use a 2-bit prediction scheme, for there are 4 states that the branch instruction can take
    >Strongly not taken (00)
    >Weakly Not Taken (01)
    >Weakly Taken (10)
    >Strongly Taken (11)

A saturation counter is used for keeping track of the states. The state of each branch instruction is updated after it passes through the execution unit. The state is incremented or decremented if the branch is taken or not taken, respectively. 
Since this unit uses a 2-bit saturation scheme, the branch jump condition has to deviate twice before the prediction changes. 

The difference between bimodal and global is that the bimodal predictor bases its prediction only on the local history of the instruction. The global predictor takes into account the global history pattern of the instructions, meaning that the prediction is based not only on the prediction history of the current branch instruction, but also on that of the preceding instructions.

A 2-bit selector is used to select between the two predictors. The default predictor chosen is the bimodal bpu. The selector can again take one of 4 states. The lower 2 states (00 and 01) take the bimodal predictor, and the upper 2 states (10 and 11) take the global predictor. The selector is incremented towards the global predictor or decremented towards the bimodal predictor, depending on the correctness of the predictor previously chosen. A selected predictor should go wrong twice for the selector to deviate and choose the other predictor. 

In the case of an interrupt, the pipeline and the BPU are flushed and the PC is updated to service the generated interrupt. Same goes for any flush
generated from the external environment which causes the PC to be changed.

The hardware used to implement the prediction unit include the following: 
    * An array of registers are used to hold the following values of every branch instruction. In the bimodal predictor, the LSBs of the program counter are used to index them. In the global predictor, the global history bits are used to index them.
        > State of the instruction.
        > Valid bit of the instruction.
        > Target address of the instruction.
        > Tag value, which holds the remaining MSBs of the program counter.
    * A FIFO that contains the PC value and whether the jump has been taken or not taken. It basically contains the output of the BPU.
        This FIFO is dequed by the external environment ( possibly the decode stage) to acknowledge that the PC from the FIFO has been read.
   
*/

package bpu_tournament;

`include "defined_parameters.bsv"
import FIFO :: *;
import SpecialFIFOs :: *;
import defined_types::*;


        
    interface Ifc_bpu_tournament;
      method Action _training ((Bit#(32)) pc, Bit#(32) addr, Actual_jump branch_taken_or_not); //to train the bpu
      method Predictor_output send_output_(); // output method
      method Action _deq_FIFO; // method to release the current contents of the FIFO
      method Action _flush(Bit#(32) new_pc_); // flushes the current excution and resets the PC.
      method Bit#(32) next_pc_();
 
    endinterface
    
  (*synthesize*)
  module mkbpu_tournament (Ifc_bpu_tournament);
    
    FIFO#(Predictor_output) fifo_pc_ <- mkLFIFO(); //Contains the output of the predictor : PC and prediction

    //arrays of BTB. The following declare the arrays used by the predictors.
    Reg#(Bit #(32)) rg_target_addr[`size_of_table]; // holds the target address of the branch instruction
    Reg#(Bit#(1)) rg_valid_or_not[`size_of_table]; //valid bit of the branch instruction
    Reg#(Bit #(TSub#(32, TAdd#(`bits_used_for_accessing,2)))) rg_tag[`size_of_table]; // the array that stores the upper bits of the PC other than the index.

    Reg#(Bit #(2)) rg_bimodal_state_of_branch_instrn[`size_of_table]; //stores the bimodal state of the branch instruction
    Reg#(Bit#(2)) rg_global_state_of_branch[`size_of_table]; // this array stores the global state of the PC.

    Reg#(Bit#(2)) rg_selector[`size_of_table]; // this array stores the chosen predictor for each PC.

    Reg#(Bit#(`bits_used_for_accessing)) rg_global_history_of_branch <- mkReg(0); // this is the global history shift register.
    `ifdef simulate
      Reg#(Bit#(32)) rg_currentpc_ <- mkReg('h200); //program counter register
    `else
      Reg#(Bit#(32)) rg_currentpc_ <- mkReg('h00); //program counter register
    `endif

    Wire#(Actual_jump) wr_taken_or_not <- mkDWire(Notaken); //output of the exec unit, whether the branch has been taken or not
    Wire#(Bit#(32)) wr_target_addr <- mkDWire(0); //the target address of the branch instruction for training
    Wire#(Bit #(32)) wr_currentpc_ <- mkDWire(0); // input pc for training 

    Wire#(Bool) wr_fire <- mkDWire(False); // this wire is responsible for firing the training rule
    Wire#(Bool) wr_flush <- mkDWire(False); // when true indicates that a flush has been issued from the upper top modules.


    // The following for loop initializes all the arrays declared earlier
    for(Integer i =0; i<`size_of_table; i=i+1) begin
        rg_bimodal_state_of_branch_instrn[i] <- mkReg(0);
        rg_global_state_of_branch[i] <- mkReg(0);
        rg_target_addr[i] <- mkReg(0);
        rg_valid_or_not[i] <- mkReg(0);
        rg_tag[i] <- mkReg(0);
	rg_selector[i] <- mkReg(0);
    end
 
/*
    This rule will fire once for every training data entered. It updates the state of the branch instruction depending on whether it has been taken or not. 
    The corresponding state of the instruction is indexed by its lower 4  bits in the bimodal predictor unit, and the global history in the global predictor unit.
*/
    rule rl_training (wr_fire == True);
        Bit#(`bits_used_for_accessing) index = wr_currentpc_[`bits_used_for_accessing+1:2];		// the index bits of the incoming PC.
        Bit#(TSub#(32, TAdd#(`bits_used_for_accessing,2))) tag = wr_currentpc_[31: `bits_used_for_accessing+2];	// the tag bits of the incoming PC.
        Bit#(`bits_used_for_accessing) global_index = rg_global_history_of_branch;			// the index for accessing the global register.
        if(rg_tag[index]!=tag)begin
          rg_target_addr[index] <= wr_target_addr; // update the address with branch address. If not taken then no need to update.
          rg_valid_or_not[index] <= 1;	// making the entry valid in the tables.
          rg_tag[index] <= tag;		// updating the tag entry in the table
          if(wr_taken_or_not==Taken)begin
            rg_bimodal_state_of_branch_instrn[index]<=2;
            rg_global_state_of_branch[global_index]<=2;
          end
          else begin
            rg_bimodal_state_of_branch_instrn[index]<=0;
            rg_global_state_of_branch[global_index]<=0;
          end
        end
	//updating the bimodal and global states        
        else if(wr_taken_or_not == Taken) begin	// if the PC was actually taken
            rg_valid_or_not[index] <= 1;	// making the entry valid in the tables.
            rg_target_addr[index] <= wr_target_addr; // update the address with branch address. If not taken then no need to update.
            if(rg_bimodal_state_of_branch_instrn[index] < 3)begin          // increment the bimodal counter.
                rg_bimodal_state_of_branch_instrn[index] <= rg_bimodal_state_of_branch_instrn[index] + 1;
            end
            if(rg_global_state_of_branch[global_index] < 3)begin 	   // increment the global counter.
            //    $display("Updating the global for address %d ",wr_currentpc_);
                rg_global_state_of_branch[global_index] <= rg_global_state_of_branch[global_index] + 1;
            end
            rg_global_history_of_branch <= (rg_global_history_of_branch << 1 )| 1;	// update the global history shift register.
         end
        else begin // if the branch is actually not taken
            if(rg_bimodal_state_of_branch_instrn[index] > 0)		// decrement the bimodal counter.
                rg_bimodal_state_of_branch_instrn[index] <= rg_bimodal_state_of_branch_instrn[index] - 1;

            if(rg_global_state_of_branch[global_index] > 0)		// decrement the global state counter.
                rg_global_state_of_branch[global_index] <= rg_global_state_of_branch[global_index] - 1;

            rg_global_history_of_branch <= rg_global_history_of_branch << 1; // update the global history shift register.
        end       

	//updating the selector counter. Increment the counter if global was right else decrement counter
      if(rg_tag[index]==tag)begin
          if(wr_taken_or_not == Taken) begin 
              if(rg_bimodal_state_of_branch_instrn[index] >= 2 && rg_global_state_of_branch[global_index] <=1 && rg_selector[index] > 0)
                  rg_selector[index] <= rg_selector[index] - 1;
              else if(rg_global_state_of_branch[global_index] >= 2 && rg_bimodal_state_of_branch_instrn[index] <= 1 && rg_selector[index] < 3)
                  rg_selector[index] <= rg_selector[index] + 1;
          end
          else begin
              if(rg_bimodal_state_of_branch_instrn[index] <= 1 && rg_global_state_of_branch[global_index] >= 2 && rg_selector[index] > 0)
                  rg_selector[index] <= rg_selector[index] - 1;
              else if(rg_global_state_of_branch[global_index] <= 1 && rg_bimodal_state_of_branch_instrn[index] >= 2 && rg_selector[index] < 3)
                  rg_selector[index] <= rg_selector[index] + 1;
          end 
      end
      else begin
        rg_selector[index]<=0;
      end

    endrule : rl_training
    
/*This rule fires at every clock cycle, updating the PC Value.
  If the state is weakly/strongly taken, the PC is updated to the target address of the branch.
  If the state is weakly/strongly not taken, the PC is updated to hold the next instruction after the branch instruction.
*/
    rule  rl_prediction(!wr_flush);
        
        Bit#(`bits_used_for_accessing) index = rg_currentpc_[`bits_used_for_accessing+1:2];		// index of the current PC
        Bit#(32) branch_address = rg_target_addr[index];						// branch address if jump is required
        Bit#(32) actual_increment_addr = rg_currentpc_+4;						// the address is jump is not required
        Bit#(`bits_used_for_accessing) global_index = rg_global_history_of_branch;			// global buffer index.
//        $display("Prediction: index:%d pc:%h selector:%d bimodal:%d global: %d rg_target_addr:%h",index,rg_currentpc_,rg_selector[index],rg_bimodal_state_of_branch_instrn[index],rg_global_state_of_branch[global_index],rg_target_addr[index]);
        if(rg_valid_or_not[index]==1 && rg_tag[index] == rg_currentpc_[31:`bits_used_for_accessing+2]) begin	// if the PC to be predicted is present in the BTB.
            if(rg_selector[index] <= 1) begin	// if bimodal is selected by the selector
                if( rg_bimodal_state_of_branch_instrn[index] >= 2 ) begin 
                    rg_currentpc_ <= branch_address;        
                    fifo_pc_.enq(Predictor_output { prog_counter_:rg_currentpc_, prediction_:Predicted_taken});
                end
                else begin
                    rg_currentpc_ <= actual_increment_addr;
                    fifo_pc_.enq(Predictor_output { prog_counter_:rg_currentpc_, prediction_:Predicted_notaken});
                end
            end
            else begin // global predcitor is selected for this PC
                if(rg_global_state_of_branch[global_index] >= 2) begin
                    rg_currentpc_<=branch_address;
                    fifo_pc_.enq(Predictor_output {prog_counter_:rg_currentpc_, prediction_:Predicted_taken});
                end
                else begin
                    rg_currentpc_ <= actual_increment_addr;
                    fifo_pc_.enq(Predictor_output { prog_counter_:rg_currentpc_, prediction_:Predicted_notaken});
                end
            end 
        end
        else begin
            rg_currentpc_<=actual_increment_addr;
            fifo_pc_.enq(Predictor_output { prog_counter_:rg_currentpc_, prediction_:Predicted_notaken});
        end
                    
    endrule : rl_prediction

/*
This method is used to train the BPU.
The method is called with the training info as arguments
*/

    method Action _training ((Bit #(32)) _current_pc, Bit #(32) _targ_addr, Actual_jump _branch_taken_or_not);
        wr_currentpc_ <= _current_pc;
        wr_target_addr <= _targ_addr;
        wr_taken_or_not <= _branch_taken_or_not;
        wr_fire <= True;
    endmethod

// The FIFO is dequed after the PC it contains has been read
    method Action _deq_FIFO();
        fifo_pc_.deq();
    endmethod

//Send the output of the predictor unit - PC and prediction
    method Predictor_output send_output_();
        return fifo_pc_.first();
    endmethod

//This method is invoked when there's an interrupt. The PC is updated to service the interrupt.
    method Action _flush(Bit#(32) new_pc_);
        wr_flush <= True;
        rg_currentpc_ <= new_pc_;
      	fifo_pc_.clear();
    endmethod


    method Bit#(32) next_pc_();
	return rg_currentpc_;
    endmethod

  endmodule
endpackage
