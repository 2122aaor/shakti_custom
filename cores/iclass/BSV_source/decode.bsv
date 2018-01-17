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

package decode;

`include "defined_parameters.bsv"
import riscv_types::*;
import decoder::*;
import Vector:: *;
import FIFO::*;
import SpecialFIFOs::*;
import GetPut::*;

interface IfcPrf_decode;
//input methods
method ActionValue#(Decode_packet) decode_enq(Fetched_instruction fetch_0, Fetched_instruction_2 fetch_1);   
method Bool abandone_cache;
method Maybe#(Bit#(`REG_WIDTH))  send_pc();
endinterface : IfcPrf_decode

(*synthesize*)
module mkPrf_decode(IfcPrf_decode);

	Wire#(Maybe#(Bit#(`REG_WIDTH))) wr_next_pc <- mkDWire(tagged Invalid);
	Wire#(Bool) wr_change_pc <- mkDWire(False);

	
	method ActionValue#(Decode_packet) decode_enq(Fetched_instruction fetch_0, Fetched_instruction_2 fetch_1);   
		let lv_fetched_instr_1 = fetch_0;
		let lv_fetched_instr_2 = fetch_1;
		
		Decoded_info_type lv_instruction_decoded_1, lv_instruction_decoded_2;
			
		if(!isNoTrap(lv_fetched_instr_1.exception))
		   lv_instruction_decoded_1 = Decoded_info_type {
		  								//inst_type	:	NOP,
		  							inst_op		:	tagged ALU Arithmetic_op{ alu_type : ALU, alu_func : NOP},
										word_flag	: False,
										imm_valid	: False,
										rs1			:	0,
										rs1_valid	: True,
										rs2			:	0,
										rs2_valid	: True,
										rd			:	0,
										rd_valid	: True,
										imm			:	0,
		  							exception	: lv_fetched_instr_1.exception
		  						};
		
		else lv_instruction_decoded_1 = 	fn_decoder(lv_fetched_instr_1.instruction, 
		  						 lv_fetched_instr_1.program_counter);
		if(!isNoTrap(lv_fetched_instr_2.fetched_instruction.exception))
		  lv_instruction_decoded_2 = Decoded_info_type {
		  									//inst_type	:	NOP,
		  								inst_op		:	tagged ALU Arithmetic_op{ alu_type : ALU, alu_func : NOP},
											word_flag	: False,
											imm_valid	: False,
											rs1				:	0,
											rs1_valid	: True,
											rs2				:	0,
											rs2_valid	: True,
											rd				:	0,
											rd_valid	: True,
											imm				:	0,
		  								exception	: lv_fetched_instr_1.exception
		  									};
		else lv_instruction_decoded_2 = fn_decoder(lv_fetched_instr_2.fetched_instruction.instruction, 
		  								 lv_fetched_instr_2.fetched_instruction.program_counter);
		
		
		Vector#(`FETCH_WIDTH, Bool) lv_decode_packet_valid;
		
		lv_decode_packet_valid[0] = True;
		lv_decode_packet_valid[1] = lv_fetched_instr_2.valid;
		
		
				
		if(lv_instruction_decoded_1.inst_op matches tagged Branch .brnch &&& brnch.branch_func == JAL)
		   begin
		  	wr_next_pc <= tagged Valid (((lv_instruction_decoded_1.imm)<<1) + 
		  	   lv_fetched_instr_1.program_counter); 
		  	wr_change_pc <= True;
		  	lv_decode_packet_valid[1] = False;	
		   end
		
		else if(lv_instruction_decoded_2.inst_op matches tagged Branch .brnch &&& brnch.branch_func == JAL)
		   begin
		  	wr_next_pc <= tagged Valid (((lv_instruction_decoded_2.imm)<<1) + 
		  	   lv_fetched_instr_2.fetched_instruction.program_counter);
		  	wr_change_pc <= True;
		   end
		
		
		Vector#(`FETCH_WIDTH, Decoded_instruction) lv_decode_packet_instr;
		
		lv_decode_packet_instr[0] =  Decoded_instruction {
		   instruction_decoded: lv_instruction_decoded_1,
		   prediction: lv_fetched_instr_1.prediction,
		   program_counter: lv_fetched_instr_1.program_counter
		   };
		
		lv_decode_packet_instr[1] = Decoded_instruction {
		   instruction_decoded: lv_instruction_decoded_2,
		   prediction: lv_fetched_instr_2.fetched_instruction.prediction,
		   program_counter: lv_fetched_instr_2.fetched_instruction.program_counter
		   };
		
		$display("\n \t \t \t \t \t ********************* DECODE ************************\n");
		$display(fshow(Decode_packet { 
		 				valid : lv_decode_packet_valid,
		 				decode_packet : lv_decode_packet_instr
		 				}));
		$display("\n \t \t \t \t \t *****************************************************\n\n\n");

		return Decode_packet { 
						valid 			: lv_decode_packet_valid,
						decode_packet 	: lv_decode_packet_instr
	 					};

	endmethod

method Bool abandone_cache;
	return wr_change_pc;
endmethod

method Maybe#(Bit#(`REG_WIDTH))  send_pc();
    let next_pc = wr_next_pc;
    return next_pc;
endmethod

endmodule : mkPrf_decode
endpackage 

