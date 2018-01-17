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

package  bpu;

`include "defined_parameters.bsv"
import Vector::*;
import riscv_types::*;
`define TABLE_SIZE 16
`define INDEX_SIZE 4


interface Ifcbpu;
	method ActionValue#(Bpu_packet) _incoming_pc_1(Bit#(`REG_WIDTH) _pc);
	method ActionValue#(Bpu_packet) _incoming_pc_2(Bit#(`REG_WIDTH) _pc);
	method Action _training(Bit#(`REG_WIDTH) pc, Bit#(`REG_WIDTH) _squash_pc, Actual_jump _branch_taken_or_not);
	method Action _flush(Bit#(`REG_WIDTH) _new_pc);
endinterface

module mkBPU(Ifcbpu);

	//Contains the index of the global predictor	
	Reg#(Bit#(`INDEX_SIZE)) rg_global_idx <- mkReg(0);

	//Table which contains the selection bias towards bimodal/global for every entry
	Vector#(`TABLE_SIZE,Reg#(Bit#(2)))  rg_select	  <- replicateM(mkReg(2'b00));
	
	//Table to store tags
	Vector#(`TABLE_SIZE,Reg#(Maybe#(Bit#(TSub#(64,`INDEX_SIZE))))) 
		tb_tag <- replicateM(mkReg(tagged Invalid));

	//Table to store the bimodal state for the branch
	Vector#(`TABLE_SIZE,Reg#(Bit#(2))) 
		tb_bimodal_state  <- replicateM(mkReg(2'b00));	

	//Table to store the global state for the branch
	Vector#(`TABLE_SIZE,Reg#(Bit#(2))) 
		tb_global_state   <- replicateM(mkReg(2'b00));   

	//Table to store the next pc's value
	Vector#(`TABLE_SIZE,Reg#(Bit#(`REG_WIDTH))) 
		tb_branch_addr  <- replicateM(mkReg(0)); 	

	//Instantiating the bimodal, global and branch addr tables
	for(Integer i = 0; i<`TABLE_SIZE; i=i+1)  begin
		//tb_tag[i] <- mkReg(tagged Invalid);
		tb_bimodal_state[i] <- mkReg(2'b00);
		tb_global_state[i] <- mkReg(2'b00);
		tb_branch_addr[i] <- mkReg(0);
	end

	// wire for passing the training data's pc
	Wire#(Bit#(`REG_WIDTH)) wr_tr_pc <-mkWire();

	// wire for passing the training data's jump pc
	Wire#(Bit#(`REG_WIDTH)) wr_training_addr <- mkWire();

	// wire for passing the training data's decision (weather jump happened or not)
	Wire#(Actual_jump) wr_training_actual <- mkWire();

	// wire to control the training rule
	Wire#(Bool) wr_fire <- mkDWire(False); 

	// wire to flush
	Wire#(Bool) wr_flush <- mkDWire(False); 


	//This rule is to train the branch predictor tables. It fires only when a new training data  arives.
	rule rl_training(wr_fire);
	
		Bit#(`REG_WIDTH) lv_tr_pc = wr_tr_pc; 

		Bit#(`REG_WIDTH) lv_tr_addr = wr_training_addr;

		Actual_jump	 lv_tr_actual = wr_training_actual;
		
		Bit#(`INDEX_SIZE) lv_tr_idx = lv_tr_pc[`INDEX_SIZE-1:0];

		Bit#(`INDEX_SIZE) lv_tr_global_idx = rg_global_idx;

		Bit#(TSub#(`REG_WIDTH,`INDEX_SIZE)) 
			lv_tr_tag = lv_tr_pc[63:`INDEX_SIZE];
		
		tb_branch_addr[lv_tr_idx] <= lv_tr_addr;

		if(tb_tag[lv_tr_idx] != tagged Valid lv_tr_tag) begin
			tb_tag[lv_tr_idx] 		<= tagged Valid lv_tr_tag;
			if(lv_tr_actual == Taken) begin
				if(tb_global_state[lv_tr_global_idx] != 2'b11)
					tb_global_state[lv_tr_global_idx] <= tb_global_state[lv_tr_global_idx] + 1;
				if(tb_bimodal_state[lv_tr_idx] != 2'b11)
					tb_bimodal_state[lv_tr_idx] <= tb_bimodal_state[lv_tr_idx] + 1;

				if(tb_global_state[lv_tr_global_idx] >= 2'b10 && tb_bimodal_state[lv_tr_idx] <= 2'b01 
					&& rg_select[lv_tr_idx] != 2'b11)

					rg_select[lv_tr_idx] <= rg_select[lv_tr_idx] + 1;
				
				else if(tb_global_state[lv_tr_global_idx] <= 2'b10 && tb_bimodal_state[lv_tr_idx] >= 2'b01 
					&& rg_select[lv_tr_idx] != 2'b00)

					rg_select[lv_tr_idx] <= rg_select[lv_tr_idx] - 1;	
			end		
		end
			
		else begin
		$display("A hit in BTB");
		//Training the tables	
		if(lv_tr_actual == Taken) begin	
			if(tb_bimodal_state[lv_tr_idx] != 2'b11)

				tb_bimodal_state[lv_tr_idx] <= 
					tb_bimodal_state[lv_tr_idx] + 1;
			
		
			if(tb_global_state[lv_tr_global_idx] != 2'b11)

				tb_global_state[lv_tr_global_idx] <= 
					tb_global_state[lv_tr_global_idx] + 1;
			
			//To train the bimodal/global select signal.	

				if((tb_global_state[lv_tr_global_idx] <= 2'b01) 
					&& (tb_bimodal_state[lv_tr_idx] >= 2'b10))

					if(rg_select[lv_tr_idx]!= 2'b00) 
						rg_select[lv_tr_idx] <= 
							rg_select[lv_tr_idx]-1;
					
				else if((tb_global_state[lv_tr_global_idx] >= 2'b10) 
					&& (tb_bimodal_state[lv_tr_idx] <= 2'b01))

					if(rg_select[lv_tr_idx]!= 2'b11) 
						rg_select[lv_tr_idx] <= 
							rg_select[lv_tr_idx]+1;
					
	    end

		//In case of training data showing branch not taken
		else begin

			if(tb_bimodal_state[lv_tr_idx] != 2'b00)
				tb_bimodal_state[lv_tr_idx] <= 
					tb_bimodal_state[lv_tr_idx] - 1;
			
		
			if(tb_global_state[lv_tr_global_idx] != 2'b00)
				tb_global_state[lv_tr_global_idx] <= 
					tb_global_state[lv_tr_global_idx] - 1;
			
		
			//if(rg_select[lv_tr_idx] >= 2'b10) begin

				if((tb_global_state[lv_tr_global_idx] <= 2'b01) 
					&& (tb_bimodal_state[lv_tr_idx] >= 2'b10))
					
					if(rg_select[lv_tr_idx] != 2'b11) 
						rg_select[lv_tr_idx] <= 
							rg_select[lv_tr_idx]+1;
					
				else if((tb_global_state[lv_tr_global_idx] >= 2'b10) 
					&& (tb_bimodal_state[lv_tr_idx] <= 2'b01))
					
					if(rg_select[lv_tr_idx] != 2'b00) 
						rg_select[lv_tr_idx] <= 
							rg_select[lv_tr_idx]-1;
					
		end
	end
	endrule: rl_training

	//This method takes in the fetched data and predicts if jump must be taken or not and gives out the next pc's value.
	method ActionValue#(Bpu_packet) _incoming_pc_1(Bit#(64) pc);

		Bit#(`REG_WIDTH) lv_next_pc = 0;
		Prediction lv_prediction = Predict_not_taken;

		Bit#(`REG_WIDTH) lv_current_pc = pc;		
	
		Bit#(`INDEX_SIZE) idx = lv_current_pc[`INDEX_SIZE-1:0];
		Bit#(`INDEX_SIZE) global_idx = rg_global_idx;
		Bit#(64) lv_branch_not_taken_addr = lv_current_pc + 4;
		Bit#(64) lv_branch_taken_addr = tb_branch_addr[idx];

		
		if(tb_tag[idx] == tagged Valid lv_current_pc[63:`INDEX_SIZE]) begin

			if(rg_select[idx] <= 2'b01) begin

				if(tb_bimodal_state[idx] >= 2'b10) begin
					lv_prediction = Predict_taken;
					lv_next_pc    = lv_branch_taken_addr;
				end

				else begin
					lv_prediction = Predict_not_taken;
					lv_next_pc    = lv_branch_not_taken_addr;
				end
			end

			else begin
				if(tb_global_state[global_idx] >= 2'b10) begin
					lv_prediction = Predict_taken;
					lv_next_pc    = lv_branch_taken_addr;
				end

				else begin
					lv_prediction = Predict_not_taken;
					lv_next_pc    = lv_branch_not_taken_addr;
				end
			end
		end

		else begin
			lv_next_pc = lv_branch_not_taken_addr;
			lv_prediction = Predict_not_taken;	
		end

		$display("the next pc sent from bpu is %h", lv_next_pc, fshow(lv_prediction));				
		return Bpu_packet {
		   pc:lv_next_pc,
		   predict_taken_or_not:lv_prediction
		};
	
	endmethod

	method ActionValue#(Bpu_packet) _incoming_pc_2(Bit#(64) pc);

		Bit#(`REG_WIDTH) lv_next_pc = 0;
		Prediction lv_prediction = Predict_not_taken;

		Bit#(`REG_WIDTH) lv_current_pc = pc;		
	
		Bit#(`INDEX_SIZE) idx = lv_current_pc[`INDEX_SIZE-1:0];
		Bit#(`INDEX_SIZE) global_idx = rg_global_idx;
		Bit#(64) lv_branch_not_taken_addr = lv_current_pc + 4;
		Bit#(64) lv_branch_taken_addr = tb_branch_addr[idx];

		
		if(tb_tag[idx] == tagged Valid lv_current_pc[63:`INDEX_SIZE]) begin

			if(rg_select[idx] <= 2'b01) begin

				if(tb_bimodal_state[idx] >= 2'b10) begin
					lv_prediction = Predict_taken;
					lv_next_pc    = lv_branch_taken_addr;
				end

				else begin
					lv_prediction = Predict_not_taken;
					lv_next_pc    = lv_branch_not_taken_addr;
				end
			end

			else begin
				if(tb_global_state[global_idx] >= 2'b10) begin
					lv_prediction = Predict_taken;
					lv_next_pc    = lv_branch_taken_addr;
				end

				else begin
					lv_prediction = Predict_not_taken;
					lv_next_pc    = lv_branch_not_taken_addr;
				end
			end
		end

		else begin
			lv_next_pc = lv_branch_not_taken_addr;
			lv_prediction = Predict_not_taken;	
		end
	
		$display("the next pc sent from bpu is %h", lv_next_pc, fshow(lv_prediction));				
		return Bpu_packet {
		   pc:lv_next_pc,
		   predict_taken_or_not:lv_prediction
		};
	
	endmethod

	//This is the method to get the training data into the BPU.
	method Action _training(Bit#(`REG_WIDTH) pc
		, Bit#(`REG_WIDTH) jump_pc, Actual_jump branch_taken_or_not);

	   $display("training entered for pc with pc %h", pc, jump_pc);
	   wr_tr_pc <= pc;
	   wr_training_addr <= jump_pc;
	   wr_fire <= True;
	   wr_training_actual <= branch_taken_or_not;
	endmethod

	//when flush happens.
	method Action _flush(Bit#(`REG_WIDTH) new_pc);
		wr_flush <= True;
	endmethod
	
	

endmodule


endpackage 
