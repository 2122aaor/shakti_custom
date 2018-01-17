/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name : riscv_decoder
Author Name : Neel Gala, Rishi Naidu
Email id : neelgala@gmail.com
Last updated on : 11th February, 2014

Description:

*/


package decoder;
`include "defined_parameters.bsv"
import defined_types::*;
import Assert::*;

// enum defining the different types of execution units present. NOP is not an execution unit. 
(*noinline*)
function Decoded_info_type fn_decoder (Bit#(32) _instruction,Prediction_type pred_type);

	Bool lv_is_imm = False;
	Register_type lv_rdtype=IntegerRF;
	Register_type lv_rs1type=IntegerRF;
	Register_type lv_rs2type=IntegerRF;
	Instruction_type lv_ins =NOP; // by default if the instruction is not a part of the ISA then a NOP will be issued.
	Bit#(5) lv_rs1,lv_rs2,lv_rs3,lv_rd; // declaring local variables.
  	Bit#(20) immediate_value=signExtend(_instruction[31:20]);
	Bit#(5) opcode = _instruction[6:2];
 	Bit#(7) funct7 = _instruction[31:25];
	Bit#(3) funct3 = _instruction[14:12];

	if(opcode==`CSR_op)begin
    	lv_ins=SYSTEM_INSTR;
  	end
	else if((opcode==`FENCE_op && funct3==`FENCE_f3) || (|(_instruction)==0))
		lv_ins=NOP;

  else if(opcode==`FMADD_op || opcode==`FNMSUB_op || opcode==`FNMADD_op || opcode==`FMSUB_op || opcode==`SPFLOAT_op 
					|| opcode== `MULDIV_op || opcode==`IMM_ARITH_op || opcode==`ARITH_op || opcode==`JAL_op || opcode == `JALR_op 	
					|| opcode==`AUIPC_op || opcode==`LUI_op || opcode==`BRANCH_op || opcode==`LOAD_op || opcode==`FLOAD_op 
					|| opcode==`STORE_op || opcode==`FSTORE_op || opcode==`ATOMIC_op)
    lv_ins=ALU;
	else 
		lv_ins=ILLEGAL;

  if(opcode==`LUI_op|| opcode==`AUIPC_op)
    immediate_value=_instruction[31:12];
  else if(opcode==`JAL_op)
    immediate_value={_instruction[31],_instruction[19:12],_instruction[20],_instruction[30:21]};
  else if (opcode==`BRANCH_op) // Branch instructions
    immediate_value=signExtend({_instruction[31],_instruction[7],_instruction[30:25],_instruction[11:8]});
  else if (opcode==`STORE_op || opcode==`FSTORE_op) // Store operations
    immediate_value=signExtend({_instruction[31:25],_instruction[11:7]});

  if(opcode==`JAL_op && immediate_value==0)
    lv_ins=ILLEGAL;
	// This condition will check if the Rd register has to be tagged during issue of instruction. It is 0 in case of Branch and Store instruction
  if(opcode==`STORE_op || opcode==`BRANCH_op ||opcode==`FSTORE_op) begin//Includes all branch and store instructions where rd register is not required 
		lv_rd=0;
  end 
  else begin
    lv_rd=_instruction[11:7];
  end

  `ifdef spfpu
    if(opcode==`FMADD_op || opcode==`FNMADD_op || opcode==`FMSUB_op || opcode==`FNMSUB_op ||opcode==`FSTORE_op || (opcode==`SPFLOAT_op && funct7[6:5]!='b01 && funct7[6:5]!='b11))
      lv_rs2type=FloatingRF;
		
		if(opcode==`FLOAD_op || opcode==`FMADD_op || opcode==`FMSUB_op || opcode==`FNMSUB_op || opcode==`FNMADD_op
			|| (opcode==`SPFLOAT_op &&(funct7[6:3]!='b1010 && funct7[6:3]!='b1100 && funct7[6:3]!='b1110))) // not floating for (FLE.S FLT.S FEQ.s) (FCVT.W.S, FCVT.WU.S) , FMV.X.S, FCLASS.S
      lv_rdtype=FloatingRF;

    if(opcode==`FMADD_op || opcode==`FNMADD_op || opcode==`FMSUB_op || opcode==`FNMSUB_op || opcode==`SPFLOAT_op && funct7[6:3]!='b1101 && funct7[6:3]!='b1111) // if floating and not FCVT.S.W, FCVT.S.WU, FMV.S.X
      lv_rs1type=FloatingRF;
  `endif


	if(opcode==`LUI_op|| opcode==`JAL_op || opcode==`AUIPC_op) 
		lv_rs1=0;
	else
		lv_rs1=_instruction[19:15];
	//In case of JAL, LUI, AUIPC, Load, Immediate Arithmetic, FCVT.*.*, FMV.*.* and FLW and CSR* operations rs2 is not required.
	if(opcode==`AUIPC_op || opcode==`LUI_op || opcode==`JAL_op || opcode==`JALR_op || opcode==`LOAD_op || 
					opcode==`FLOAD_op || opcode==`IMM_ARITH_op || opcode==`CSR_op || (opcode==`SPFLOAT_op && funct7[6:5]=='b11))
		lv_rs2=0;
	else
		lv_rs2=_instruction[24:20];

	if(_instruction[6:4]=='b100)
		lv_rs3=_instruction[31:27];
	else
		lv_rs3=0;
  
    if(opcode==`IMM_ARITH_op || opcode==`AUIPC_op || opcode == `JAL_op || opcode==`JALR_op || opcode==`LUI_op 
						|| opcode==`BRANCH_op || opcode==`LOAD_op || opcode==`FLOAD_op || opcode==`FSTORE_op || opcode==`STORE_op)
      lv_is_imm=True;

	return (Decoded_info_type{inst_type:lv_ins,
				rs1:lv_rs1,
				rs2:lv_rs2,
        `ifdef spfpu
  				rs3:lv_rs3,
        `endif
				rd:lv_rd,
				rd_type:lv_rdtype,
				rs1_type:lv_rs1type,
				rs2_type:lv_rs2type,
        opcode:opcode,
        funct3:funct3,
        funct7:funct7,
        immediate_value:immediate_value,
        is_imm:lv_is_imm,
        pred_type:pred_type});
endfunction
endpackage
