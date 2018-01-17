/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

Author Names : Rahul Bodduna, N Sireesh
Email ID : rahul.bodduna@gmail.com
*/

package decoder;

import Vector:: *;
import riscv_types::*;
`include "defined_parameters.bsv"


(*noinline*)
function Decoded_info_type fn_decoder(Bit#(`INSTR_WIDTH) _instruction, Bit#(64) pc);
	//Instruction_type lv_inst_type = USER_INT;
  ALU_func lv_alu_func = NOP;
	Inst_op instruction_op = tagged ALU Arithmetic_op{ alu_type : ALU, alu_func : lv_alu_func};
  Bool lv_imm_valid = False;
  Bit#(`REG_WIDTH) lv_imm = 0;
  Bit#(TLog#(`REGFILE_SIZE)) lv_rs1 = 0;
  Bit#(TLog#(`REGFILE_SIZE)) lv_rs2 = 0;
  Bit#(5) lv_rd = _instruction[11:7];
  Branch_type lv_branch_type = COND;
  Branch_func lv_branch_op = BNE;
	ALU_func lv_alu_op = NOP; 
	Bool lv_rs1_valid = False;
	Bool lv_rs2_valid = False;
	Bool lv_rd_valid = True;
	Bool lv_word_flag = (_instruction[3]==1) ? True : False;
	TrapCause lv_exception = tagged No_trap;

	//Decoding the instruction, RS1, RS2, RD and imm/offset values
  case (_instruction[6:2]) matches
	//LUI : TODO
	5'b01101: begin
		lv_imm_valid = True;
		ALU_type lv_i_type = ALU;
		lv_alu_op = LUI;
		lv_imm = signExtend({_instruction[31:12],12'b0});
		instruction_op = tagged ALU Arithmetic_op{ alu_type : lv_i_type, alu_func : lv_alu_op};
	end

	//AUIPC : TODO
	5'b00101: begin
		ALU_type lv_i_type = ALU;
		lv_alu_op = AUIPC;
		lv_imm_valid = True;
		Bit#(20) lv_pre_imm = _instruction[31:12];
		lv_imm = signExtend({lv_pre_imm,12'b0});
		instruction_op = tagged ALU Arithmetic_op{ alu_type : lv_i_type, alu_func : lv_alu_op};
	end
	
	//JAL
	5'b11011: begin
		lv_imm_valid = True;
		lv_imm = signExtend({_instruction[31],_instruction[19:12],_instruction[20],_instruction[30:21]});
		lv_branch_type = UNCOND;
		lv_branch_op = JAL;
		instruction_op = tagged Branch Branch_op{ branch_type : lv_branch_type, branch_func : lv_branch_op};
	end

	//JALR
	5'b11001: begin
		lv_imm_valid = True;
		lv_imm = signExtend(_instruction[31:20]);
		lv_branch_type = UNCOND;
		lv_branch_op = JALR;
		instruction_op = tagged Branch Branch_op{ branch_type : lv_branch_type, branch_func : lv_branch_op};
	end

	//Conditional branch instructions
	5'b11000: begin
		lv_imm_valid = True;
		lv_imm = signExtend({_instruction[31],_instruction[7],_instruction[30:25],_instruction[11:8]});
		lv_branch_type = COND;
		lv_rd_valid = False;
		lv_rd = 0;
		case (_instruction[14:12])
		//BEQ
		3'b000:	begin
			lv_branch_op = BEQ;
		end
		//BNE
		3'b001:	begin
			lv_branch_op = BNE;
		end
		//BLT
		3'b100:	begin
			lv_branch_op = BLT;
		end
		//BGE
		3'b101:	begin
			lv_branch_op = BGE;
		end
		//BLTU
		3'b110:	begin
			lv_branch_op = BLTU;
		end
		//BGEU
		3'b111:	begin
			lv_branch_op = BGEU;
		end
		//Illegal Instruction
		default : begin
			lv_exception = tagged Exception IllegalInst;
		end
		endcase
		instruction_op = tagged Branch Branch_op{ branch_type : lv_branch_type, branch_func : lv_branch_op};
	end
	//Load instructions
	5'b00000: begin
		lv_imm_valid = True;
		lv_imm = signExtend(_instruction[31:20]);
		Mem_type lv_mem_type = LD;
		case (_instruction[14:12])
		//Illegal Instruction
		3'b111 : begin
		 	lv_mem_type = LD;
			lv_exception = tagged Exception IllegalInst;
		end
		endcase
		instruction_op = tagged Memory Mem_op{ mem_type : lv_mem_type, mem_size : _instruction[14:12]};
	end
	//Store instructions
	5'b01000: begin
		lv_imm_valid = True;
		lv_imm = signExtend({_instruction[31:25],_instruction[11:7]});
		lv_rd_valid = False;
		lv_rd = 0;
		Mem_type lv_mem_type = STR;
		case (_instruction[14:12])
		3'b100, 3'b101, 3'b110, 3'b111:	begin
			lv_exception = tagged Exception IllegalInst;
		end
		endcase
		instruction_op = tagged Memory Mem_op{ mem_type : lv_mem_type, mem_size : _instruction[14:12]};
	end
	//ALU instructions
	5'b0?1?0: begin
		if(_instruction[5] == 1 && _instruction[25] == 1) begin	
		ALU_type lv_i_type = MUL;
		lv_alu_op = MUL;

			case (_instruction[14:12])
			3'b000: begin
				lv_alu_op = MUL;
				lv_i_type = MUL;
			end
			3'b001: begin
				lv_alu_op = MULH;
				lv_i_type = MUL;
			end
			3'b010: begin
				lv_alu_op = MULHSU;
				lv_i_type = MUL;
			end
			3'b011: begin
				lv_alu_op = MULHU;
				lv_i_type = MUL;
			end
			3'b100: begin
				lv_alu_op = DIV;
				lv_i_type = DIV;
			end
			3'b101: begin
				lv_alu_op = DIVU;
				lv_i_type = DIV;
			end
			3'b110: begin
				lv_alu_op = REM;
				lv_i_type = DIV;
			end
			3'b111: begin
				lv_alu_op = REMU;
				lv_i_type = DIV;
			end
			endcase
		instruction_op = tagged ALU Arithmetic_op{ alu_type : lv_i_type, alu_func : lv_alu_op};
		end
		else begin
			//In case of I type, imm is decoded
			if(_instruction[5] == 1'b0) begin
				lv_imm_valid = True;
				lv_imm = signExtend(_instruction[31:20]);
			end
			ALU_type lv_i_type = ALU;
			lv_alu_op = ADD;
			case (_instruction[14:12])
			//ADDI,ADD,SUB
			3'b000:	begin
			if(_instruction[30] == 1 && _instruction[5] == 1)
				lv_alu_op = SUB;
			
			else lv_alu_op = ADD;
			end
			//SLT,SLTI
		 	3'b010:	begin
				lv_alu_op = SLT;
			end
			//SLTU,SLTUI
			3'b011:	begin
				lv_alu_op = SLTU;
			end
			//XOR,XORI
			3'b100:	begin
				lv_alu_op = XOR;
			end
			//OR,ORI
			3'b110:	begin
				lv_alu_op = OR;
			end
			//AND,ANDI
			3'b111:	begin
				lv_alu_op = AND;
			end
			//SLL,SLLI
			3'b001: begin
				lv_alu_op = SLL;
				if(_instruction[3] == 1'b1 && _instruction[25] == 1'b1) 
					lv_exception = tagged Exception IllegalInst;
			end
			//SRL,SRA,SRLI,SRAI
			3'b101: begin
				if(_instruction[3] == 1'b1 && _instruction[25] == 1'b1) 
					lv_exception = tagged Exception IllegalInst;
				case(_instruction[30])
				1'b0: begin
					lv_alu_op = SRL;
				end
				1'b1: begin
					lv_alu_op = SRA;
				end
				endcase
			end	
			endcase
		instruction_op = tagged ALU Arithmetic_op{ alu_type : lv_i_type, alu_func : lv_alu_op};
		end

	end
	5'b11100: begin
			//lv_inst_type = SYSTEM;
			lv_imm	= zeroExtend(_instruction[31:20]);  //For system instructions this is the CSR address 
			lv_rs1  = _instruction[19:15];							//This is either treated as register address or immediate value
			SystemInst lv_csr_op = CSRR; 

			case(_instruction[13:12])
			2'b00: begin
				if(_instruction[21]==0) begin
					lv_csr_op = _instruction[20]==1 ? EBreak : ECall;
				end
				else begin 
					case(_instruction[29:28])
						2'b00 : begin
							lv_csr_op = URet;
						end
						2'b01 : begin
							lv_csr_op = SRet;
						end
						2'b10 : begin
							lv_csr_op = HRet;
						end
						2'b11 : begin
							lv_csr_op = MRet;
						end
					endcase
				end
			end
			2'b01: begin
				lv_csr_op = CSRRW;
				if(_instruction[11:7]==0)
					lv_csr_op = CSRW;
			end
			2'b10: begin
				lv_csr_op = CSRRS;
				if(_instruction[19:15]==0)
					lv_csr_op = CSRR;
			end
			2'b11: begin
				lv_csr_op = CSRRC;
				if(_instruction[19:15]==0)
					lv_csr_op = CSRR;
			end
			endcase
			if(_instruction[14] == 1'b1) begin 	
				lv_imm_valid = True;
			end
			instruction_op = tagged System lv_csr_op;
	end
	default : lv_exception = tagged Exception IllegalInst;
	endcase
	//Decoding value of RS1
	if(!(lv_branch_op == JAL || lv_alu_op == LUI || lv_alu_op == AUIPC 
		|| (_instruction[6:0] == 7'b1110011 && _instruction[14:12] == 3'b000) 
		|| (_instruction[6:0] == 7'b0001111))) begin
		lv_rs1_valid = True;
		lv_rs1 = _instruction[19:15];
	end
   
	//Decoding values of RS2
   	if(_instruction[6:2] == 5'b11000 || _instruction[6:2] == 5'b01000 
		|| _instruction[6:2] == 5'b01100 || _instruction[6:2] == 5'b01110) begin
		lv_rs2_valid = True;
		lv_rs2 = _instruction[24:20];
	end	

	//Returning the decoded instruction
   	return (Decoded_info_type {
	  //inst_type: lv_inst_type,
		inst_op  : instruction_op,
	  word_flag: lv_word_flag,
	  imm_valid:lv_imm_valid,
	  rs1:lv_rs1,
	  rs1_valid: lv_rs1_valid,
	  rs2:lv_rs2,
	  rs2_valid: lv_rs2_valid,
	  rd:lv_rd,
	  rd_valid: lv_rd_valid,
	  imm:lv_imm,
	  exception: lv_exception 
	 });	
				
endfunction



endpackage 
                   
