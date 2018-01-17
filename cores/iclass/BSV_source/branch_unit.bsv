package branch_unit;

import riscv_types::*;
import SpecialFIFOs::*;
import FIFO::*;
`include "defined_parameters.bsv"


interface Branch_unit;
	method Action inputs(Branch_func _op_code, Bit#(`REG_WIDTH) _operand1,
	       	Bit#(`REG_WIDTH) _operand2, Bit#(TLog#(`PRF_SIZE)) _dest,
	       	Bit#(`REG_WIDTH) _imm, Bit#(`REG_WIDTH) _squash_pc,
	       	Prediction _predict);
	
	method ActionValue#(Result_bypass_type) get_broadcast_packet();
   
    method ActionValue#(Bool) get_squash();

	method ActionValue#(Training_packet) _training_packet();
	
	method ActionValue#(Bit#(`REG_WIDTH)) next_pc();
endinterface

module	mk_branch_unit(Branch_unit);

	//FIFO for storing the branch's output broadcast packet
	Wire#(Result_bypass_type) wr_branch <-mkWire();
   
    Wire#(Bool) wr_squash <- mkWire();

	//FIFO for storing the training packet to be sent to BPU
	Wire#(Training_packet) wr_training  <- mkWire();

	//FIFO for storing te calculated next PC value
	Wire#(Bit#(`REG_WIDTH)) wr_next_pc  <-mkWire();

	//FIFO for storing the return value ie. PC + 1
	//Wire#(Bit#(`REG_WIDTH)) fifo_return  <-mkWire();
	
	//This method takes in the inputs and does the needfull computations	
	method Action inputs(Branch_func _op_code, Bit#(`REG_WIDTH) _operand1,
	    Bit#(`REG_WIDTH) _operand2, Bit#(TLog#(`PRF_SIZE)) _dest,
	    Bit#(`REG_WIDTH) _imm, Bit#(`REG_WIDTH) _pc,
	    Prediction _predict);

		Actual_jump lv_actual = Not_taken;
	   	Bool lv_squash = True;
	   	Bit#(TLog#(`PRF_SIZE)) lv_dest_addr = _dest;
	   	Bool lv_cb = False;
	   	Maybe#(Bit#(`REG_WIDTH)) lv_return = tagged Invalid;
	    Bit#(`REG_WIDTH) lv_effective = ((_imm<<1) + _pc);
	    Bit#(`REG_WIDTH) jump_pc = lv_effective;
		$display("Target PC in for branch %h", lv_effective);
   
	   	Branch_type lv_branch_type = UNCOND;

	   	case(_op_code)
		   
		JAL:
		   lv_squash = False;

		JALR: begin
			lv_effective = {(_operand1 + _imm)[`REG_WIDTH-1:2],2'b00};
		end
	
		//Conditional jumps from here. Computing if jump to be taken or not
		BNE: begin
			if (_operand1 != _operand2)
				lv_actual = Taken;
			lv_cb = True;	
		end

		BEQ: begin
			if (_operand1 == _operand2)
				lv_actual = Taken;
			lv_cb = True;	
		end
		  
		BGEU: begin
			if (_operand1 >= _operand2)
				lv_actual = Taken;
			lv_cb = True;	
		end
		  
		BLT: begin
			lv_cb = True;	
			if (_operand1[63] == _operand2[63] 
			 && _operand1<_operand2 && _operand1[63]==0)
				lv_actual=Taken;

			else if (_operand1[63]==_operand2[63] 
			 && _operand2>_operand1 && _operand1[63]==1)
					 lv_actual=Taken;
			
			else if (_operand1[63]!=_operand2[63] 
			 && _operand1[63]==1)

				lv_actual=Taken;
		end

		BGE: begin
			lv_cb = True;
			if(_operand1[63]==_operand2[63] 
			 && _operand1>=_operand2 && _operand1[63]==0)
				lv_actual=Taken;

			else if (_operand1[63]==_operand2[31] 
			 && _operand2<=_operand1 && _operand1[63]==1)
				lv_actual=Taken;
			
			else if (_operand1[63]!=_operand2[63] 
			 && _operand1[63]==0)
				 lv_actual=Taken;
		end

		BLTU: begin
			lv_cb = True;
			if(_operand1<_operand2) 
				lv_actual=Taken;
		end
		endcase
	   
		//Check if squashing needed or not
	   	if(lv_cb == True) begin
			lv_branch_type = COND;

			if(_predict == Predict_taken 
			 && lv_actual == Taken) begin 
				lv_squash = False;  
			end

			else if(_predict == Predict_not_taken 
			 && lv_actual == Taken) begin 
				lv_squash = True; 
			end

			else if(_predict == Predict_taken 
			 && lv_actual == Not_taken) begin 
				lv_squash = True; 
				lv_effective = _pc + 4; 
		 	end
			
			else begin 
				lv_squash = False; 
				lv_effective = _pc + 4; 
			end
			 
			 
			wr_training <= Training_packet {
				pc		: _pc,
				jump_pc		: jump_pc,
				taken_or_not	: lv_actual
			};
			 
			if(lv_squash == True)
				wr_next_pc <= lv_effective;
		end

		//Return value pushed on FIFO in case of uncond
		else begin
			//fifo_return.enq(_pc + 4);
			if(_op_code == JALR) wr_next_pc <= lv_effective;
		end

		//FIFO for branch broadcast is written
		wr_branch	<=	Result_bypass_type {
		  					dest_tag : lv_dest_addr,
		  					 _result : _pc + 4
							};
	    wr_squash <= lv_squash;
	endmethod

	//Method to get the broadcast from branch unit
	method ActionValue#(Result_bypass_type) get_broadcast_packet();	
		let lv_broadcast_branch = wr_branch;
	  	return lv_broadcast_branch;
   	endmethod
   
   
   method ActionValue#(Bool) get_squash();
	  return wr_squash; 
   endmethod
	
	//Method to get the BPU training packet from branch unit
	method ActionValue#(Training_packet) _training_packet();  
	  	let lv_training_packet = wr_training;
		$display(fshow(lv_training_packet));
	  	return lv_training_packet;
   	endmethod
   
	//Method to get the next PC in case of JALR,COND jumps
   	method ActionValue#(Bit#(`REG_WIDTH)) next_pc;
	  	let lv_pc = wr_next_pc;
	  	return lv_pc;	
   	endmethod
   
endmodule

endpackage
