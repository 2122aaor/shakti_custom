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
import SpecialFIFOs :: *;
import defined_types::*;


        
    interface Ifc_bpu_bimodal;
      method Action _training ((Bit#(32)) pc, Bit#(32) addr, Actual_jump branch_taken_or_not); //to train the bpu
      method Predictor_output send_output_(); // output method
      method Action _deq_FIFO; // method to release the current contents of the FIFO
      method Action _flush(Bit#(32) new_pc_); // flushes the current excution and resets the PC.
      method Bit#(32) next_pc_();
 
    endinterface
    
  (*synthesize*)
  module mkbpu_bimodal (Ifc_bpu_bimodal);
    
    FIFO#(Predictor_output) fifo_pc_ <- mkPipelineFIFO();
    //arrays of BTB
    Reg#(Bit #(2)) rg_state_of_branch_instrn[`size_of_table]; //stores the state of the branch instruction
    Reg#(Bit #(32)) rg_target_addr[`size_of_table]; // holds the target address of the branch instruction
    Reg#(Bit#(1)) rg_valid_or_not[`size_of_table]; //valid bit of the branch instruction
    Reg#(Bit #(TSub#(32, `bits_used_for_accessing))) rg_tag[`size_of_table];
    
    Reg#(Bit #(32)) rg_currentpc_ <- mkReg(0); //program counter register

    Wire#(Actual_jump) wr_taken_or_not <- mkDWire(Notaken);//output of the exec unit, whether the branch has been taken or not
    Wire#(Bit#(32)) wr_target_addr <- mkDWire(0);//the target address of the branch instruction
    Wire#(Bit #(32)) wr_currentpc_ <- mkDWire(0);
    Wire#(Bit#(1)) wr_valid_or_not <- mkDWire(0);
    Wire#(Bit#(TSub#(32, `bits_used_for_accessing))) wr_tag <- mkDWire(0);
    Wire#(Bool) wr_fire <- mkDWire(False); 
    Wire#(Bool) wr_flush <- mkDWire(False);


    for(Integer i =0; i<`size_of_table; i=i+1) begin
        rg_state_of_branch_instrn[i] <- mkReg(0);
        rg_target_addr[i] <- mkReg(0);
        rg_valid_or_not[i] <- mkReg(0);
        rg_tag[i] <- mkReg(0);
    end

/*
    This rule will fire once for every training data entered. It updates the state of the branch instruction depending on whether it has been taken or not. 
    The corresponding state of the instruction is indexed by its lower 4  bits.
*/
    rule rl_training (wr_fire == True && !wr_flush);
        Bit#(`bits_used_for_accessing) index = wr_currentpc_[`bits_used_for_accessing-1:0];
        Bit#(TSub#(32, `bits_used_for_accessing)) tag = wr_currentpc_[31: `bits_used_for_accessing];
        rg_valid_or_not[index] <= wr_valid_or_not;
        rg_tag[index] <= tag;
        
        if(wr_taken_or_not == Taken && rg_state_of_branch_instrn[index] < 3) begin
          rg_target_addr[index] <= wr_target_addr;
          rg_state_of_branch_instrn[index] <= rg_state_of_branch_instrn[index] + 1;
        end
        else if(wr_taken_or_not == Notaken && rg_state_of_branch_instrn[index] > 0) begin
            rg_state_of_branch_instrn[index] <= rg_state_of_branch_instrn[index] - 1;
        end        
    endrule : rl_training

/*This rule fires at every clock cycle, updating the PC Value.
  If the state is weakly/strongly taken, the PC is updated to the target address of the branch.
  If the state is weakly/strongly not taken, the PC is updated to hold the next instruction after the branch instruction.
*/
    rule rl_prediction(!wr_flush);
        Bit#(`bits_used_for_accessing) index = rg_currentpc_[`bits_used_for_accessing-1:0];
        Bit#(32) branch_address = rg_target_addr[index];
        Bit#(32) actual_increment_addr = rg_currentpc_+4;

        if(rg_valid_or_not[index]==1 && rg_state_of_branch_instrn[index] >= 2 && rg_tag[index] == rg_currentpc_[31:`bits_used_for_accessing]) begin
	    $display("Enquing PC Taken : %d",rg_currentpc_);
            rg_currentpc_ <= rg_target_addr[index];        
            fifo_pc_.enq(Predictor_output { prog_counter_:rg_currentpc_, prediction_:Predicted_taken});
        end
        else begin
	    $display("Enquing PC Not Taken : %d",rg_currentpc_);
            rg_currentpc_ <= actual_increment_addr;
            fifo_pc_.enq(Predictor_output { prog_counter_:rg_currentpc_, prediction_:Predicted_notaken});
        end
    endrule : rl_prediction

/*This method is used to train the BPU.
  The method is called with the training info as arguments
*/

    method Action _training ((Bit #(32)) _current_pc, Bit #(32) _targ_addr, Actual_jump _branch_taken_or_not);
        wr_currentpc_ <= _current_pc;
        wr_target_addr <= _targ_addr;
        wr_taken_or_not <= _branch_taken_or_not;
        wr_valid_or_not <= 1;
        wr_fire <= True;
    endmethod
    
    method Action _deq_FIFO();
        fifo_pc_.deq();
    endmethod

    method Predictor_output send_output_();
        return fifo_pc_.first();
    endmethod

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
