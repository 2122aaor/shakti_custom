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

This module implements the instruction decoder for the 64-bit RISC-V ISA (as updated on January 2014).
The decoder is based on the fact that there will be only 6 types of different execution units :
	> ALU
	> MEM (LOAD-STORE)
	> BRANCH
	> //Atomic

However the number of units of each type does not affect this decoder. A majority of the decoding usually happens in the excution stage to differentiate between 
the different closely related instructions. Hence there is not point of decoding the complete instruction here. Decoding here should be done
to ease out the Issue complexity. The full complexity of the decoder is not split into various pipes based on the amount of information necessary of the current and
immediate next stage.


Also since the instruction is being passed all the way to the execution units, we need not find the immediate data. We let the execution unit itself to 
pick the immediate data from the instruction bits. 

****************Changes in updated Riscv ISA*********************
Destination register position change
Atomic Instruction opcode change
J(jump only) Instruction removed
*/


package decoder;
`include "defined_parameters.bsv"
import defined_types::*;

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
	// The following if-else structure implements the type of execution unit the particular instruction will reach.
	// As of now only 6 execution units have been defined. For variable execution units the enum will have to be modifed
	// and so will the following if-else structure. The decoding conditions for the following have been derived the table-8.1
	// on page 43 of the RISC-V manual.
    
	// This condition will check if the Rd register has to be tagged during issue of instruction. It is 0 in case of Branch and Store instruction
    if(_instruction[6:2]=='b01000 || _instruction[6:2]=='b11000 || _instruction[6:2]=='b01001) begin//Includes all branch and store instructions where rd register is not required 
		lv_rd=0;
    end 
    else begin
      lv_rd=_instruction[11:7];
    end

	if(_instruction[6:2]=='b00001 || _instruction[6:4]=='b100 || (_instruction[6:4]=='b101 && _instruction[31:28]!='b1101 && _instruction[31:28]!='b1110)) // not floating for FCVT.W.S, FCVT.WU.S, FMV.X.S, FCLASS.S
		lv_rdtype=FloatingRF;
	if(_instruction[6:2]=='b01001 || _instruction[6:4]=='b100 || (_instruction[6:4]=='b101	&& _instruction[31:28]!='b1101 && _instruction[31:28]!='b1111)) // if floating and not FCVT.S.W, FCVT.S.WU, FMV.S.X
		lv_rs1type=FloatingRF;
	if(_instruction[6:2]=='b01001 || _instruction[6:4]=='b100 || (_instruction[6:4]=='b101	&& _instruction[31:30]!='b11)) // if floating and not FCVT.S.W, FCVT.S.WU, FMV.S.X
		lv_rs1type=FloatingRF;

  if(_instruction[6:2]=='b11100)begin
    lv_ins=PRIVILEGED;
  end
  else if(_instruction=='h6f)
    lv_ins=ILLEGAL;
  else if(_instruction[6:4]=='b100 || _instruction[6:4]=='b101)
    lv_ins=FPU;
	else if (_instruction[6:4]=='b110 || _instruction[6:4]==3'b001 || _instruction[6:4]=='b000 || _instruction[6:4]=='b010 || (_instruction[6:2]=='b01101) || (_instruction[6:4]==3'b011 && _instruction[25]==0)) // All the ALU operations integer and immediate
		lv_ins=ALU;
	else if (_instruction[6:4]==3'b011 && _instruction[25]==1) // All the MUL/DIV
		lv_ins=ALU;
	else if(_instruction[6:2]=='b11111)
		lv_ins=ILLEGAL;

	if(_instruction[6:2]=='b01101 || _instruction[6:2]=='b00101 || _instruction[6:2]=='b11011) // in case of JAL, LUI and AUIPC rs1 is not required.
		lv_rs1=0;
	else
		lv_rs1=_instruction[19:15];
	//In case of JAL, LUI, AUIPC, Load, Immediate Arithmetic, FCVT.*.*, FMV.*.* perations rs2 is not required.
	if(_instruction[6:2]=='b01101 || _instruction[6:4]=='b001 || _instruction[6:2]=='b11011 || _instruction[6:2]=='b00000 || (_instruction[6:4]=='b101 && _instruction[31:30]=='b11))
		lv_rs2=0;
	else
		lv_rs2=_instruction[24:20];

	if(_instruction[6:4]=='b100)
		lv_rs3=_instruction[31:27];
	else
		lv_rs3=0;
  
  if(_instruction[6:2]=='b01101 || _instruction[6:2]=='b00101) // LUI or AUIPC
    immediate_value=_instruction[31:12];
  else if(_instruction[6:2]=='b11011) // JAL
    immediate_value={_instruction[31],_instruction[19:12],_instruction[20],_instruction[30:21]};
  else if (_instruction[6:2]=='b11000) // Branch instructions
    immediate_value=signExtend({_instruction[31],_instruction[7],_instruction[30:25],_instruction[11:8]});
  else if (_instruction[6:2]=='b01000) // Store operations
    immediate_value=signExtend({_instruction[31:25],_instruction[11:7]});

    if(_instruction[6:2]=='b01101 || _instruction[6:2]=='b11011 || _instruction[6:4]=='b110 || _instruction[6:2]==0 || _instruction[6:4]=='b010 ||  _instruction[6:4]=='b001 )
      lv_is_imm=True;

	return (Decoded_info_type{inst_type:lv_ins,
				rs1:lv_rs1,
				rs2:lv_rs2,
				rs3:lv_rs3,
				rd:lv_rd,
				rd_type:lv_rdtype,
				rs1_type:lv_rs1type,
				rs2_type:lv_rs2type,
        opcode:_instruction[6:2],
        funct3:_instruction[14:12],
        funct7:_instruction[31:25],
        immediate_value:immediate_value,
        is_imm:lv_is_imm,
        pred_type:pred_type,
        priv_funct:_instruction[14:12],
        priv_addr:_instruction[31:20],
        priv_immediate:_instruction[14]});
endfunction
endpackage
