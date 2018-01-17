/*
Copyright (c) 2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name     : Scalar Unit for VPU
Author Name     : Sumanth Sridhar, Vinod G
e-mail Id       : sumanthsridhar.009@gmail.com, g.vinod1993@gmail.com
Last updated on : 30th June 2016
*/


package ScalarUnit;

import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import GetPut::*;
import ClientServer::*;
import RegFile::*;
import Vector::*;
// import RevertingVirtualReg::*;

import VectorAccelDefs::*;

`include "defined_parameters.bsv"

import alu64::*;
import ROB::*;
import SU_Functions::*;

interface ScalarUnitIfc;
	interface Put#(CMDQReq) 	putRoccInstr;
	interface Get#(Vcfg) 		rocc_readVCFG;
	interface Get#(VlenStruct)	rocc_readVLEN;
	interface Put#(Bool) 		rocc_resetVCFG;
	// interface Put#(Maybe#(Vlen)) rocc_writeVLEN;
	interface Get#(Maybe#(Bit#(64))) 	startWorkerPipe;
endinterface: ScalarUnitIfc


module mkScalarUnit (ScalarUnitIfc);

	/* 	va = vector address registers
		vs = vector shared registers
		the "SharedRegFile" is a simple RegFile, with vs0 hardwired to constant 0
		if the "USE_ROB" parameter is set, the vsRegFile will also contain an extra structure to store rob-address-pointers	*/
	RegFile#(Bit#(`VA_AddrWidth), Bit#(64))	
						vaRegFile	<- mkRegFileFull;

	`ifdef USE_ROB
		SharedRegFile	vsRegFile	<- mkSharedRegFileTagged;
	`else
		SharedRegFile	vsRegFile	<- mkSharedRegFile;
	`endif

	Wire#(DInstr32)		decodedInstr32		<- mkWire;
	Wire#(Instr32)		instr32				<- mkWire;
	Wire#(Bit#(64)) 	src1				<- mkWire;
	
	Reg#(Vcfg)		vcfg			<- mkReg(0);
	Reg#(Vlen)		vlen			<- mkReg(0);
	RWire#(Vcfg)	w_vcfg			<- mkRWire;
	RWire#(Vlen)	w_vlen			<- mkRWire;
	RWire#(Bit#(0)) resetVcfg		<- mkRWire;
	RWire#(Bit#(0)) setVL_response	<- mkRWire;		// using only the valid bit of RWire; no data

	Wire#(Maybe#(Bit#(64))) 	pcVal	<- mkWire;

	rule disp_ ;
		$display("vcfg: %d",vcfg,"; vlen: %d",vlen);
	endrule: disp_

	/* NOTE	A conflict occurs since VUNCFG is not queued into the VCMDQ
			So VUNCFG has 0-cycle latency before execution but VSETCFG has 
			a latency of 1-cycle; both may overlap
	*/
	(* descending_urgency = "reset_VCFG, exec_VSETCFG" *)
	rule reset_VCFG (isValid(resetVcfg.wget));
		w_vcfg.wset('0);
		$display("*exec_VUNCFG*");
	endrule: reset_VCFG

	rule exec_VSETCFG (decodedInstr32 == Vsetcfg);
		w_vcfg.wset( {src1[63:12], i32_funct7(instr32), i32_rs2(instr32)} );
		w_vlen.wset( '0 );
		$display("*exec_VSETCFG*");
	endrule: exec_VSETCFG

	rule exec_VSETVL (decodedInstr32 == Vsetvl);
		Bool selVL = (src1 < maxVecLt);
		w_vlen.wset( selVL? src1 : maxVecLt );
		setVL_response.wset('0);	// type=Bit#(0); so wset value doesn't matter
		$display("*exec_VSETVL*");
	endrule: exec_VSETVL

	rule exec_VMCS (decodedInstr32 == Vmcs);
		let rs2			= i32_rs2(instr32);
		let rd			= i32_rd (instr32);
		let vsRegSel	= { rs2[2:0], rd };	
		
		// NOTE VMCS instr takes {rs2[2:0],rd} but only 6-bits reqd for the current shared-regfile
		vsRegFile.upd_data(truncate(vsRegSel), src1);
		$display("*exec_VMCS*");
	endrule: exec_VMCS

	rule exec_VMCA (decodedInstr32 == Vmca);
		let vaRegSel	= i32_rd (instr32);
		vaRegFile.upd(vaRegSel, src1);
		$display("*exec_VMCA*");
	endrule: exec_VMCA

	rule exec_VF (decodedInstr32 == Vf);
		pcVal		<= Valid(src1 + zeroExtend( {i32_funct7(instr32),i32_rd(instr32) } ));
		$display("*exec_VF*");
	endrule: exec_VF

	rule update_VLEN_VCFG ;
		if (isValid(w_vcfg.wget))
			vcfg <= fromMaybe(?,w_vcfg.wget);
		if (isValid(w_vlen.wget))
			vlen <= fromMaybe(?,w_vlen.wget);
	endrule: update_VLEN_VCFG

	interface Get startWorkerPipe;
		method ActionValue#(Maybe#(Bit#(64))) get();
			return pcVal;
		endmethod
	endinterface: startWorkerPipe

	interface Put putRoccInstr;
		method Action put(CMDQReq vectorReq);
			decodedInstr32	<= vectorReq.instrName;
			instr32			<= vectorReq.ctrlInstruction;
			src1			<= vectorReq.value64;
		endmethod
	endinterface: putRoccInstr

	interface Get rocc_readVCFG;
		method ActionValue#(Vcfg) get();
			return (isValid(w_vcfg.wget)? validValue(w_vcfg.wget) : vcfg);
		endmethod
	endinterface: rocc_readVCFG

	interface Get rocc_readVLEN;
		method ActionValue#(VlenStruct) get();
			return ( VlenStruct {
				vlen			: (isValid(w_vlen.wget)? validValue(w_vlen.wget) : vlen),
				setVL_response	:  isValid(setVL_response.wget)
			});	// for setVL_response, just check if it has been wset and assign True/False
		endmethod
	endinterface: rocc_readVLEN

	interface Put rocc_resetVCFG;
		method Action put(Bool vcfgReset);
			if (vcfgReset) begin
				resetVcfg.wset('1);
			end
		endmethod
	endinterface: rocc_resetVCFG

/*	interface Put rocc_writeVLEN;
		method Action put(Maybe#(Vlen) vlen_in);
			if (isValid(vlen_in)) begin
				w_vlen.wset(fromMaybe('0,vlen_in));
				$display("update vlen = %h", fromMaybe('0,vlen_in));
			end
		endmethod
	endinterface: rocc_writeVLEN
*/	

endmodule: mkScalarUnit


///////////////////////////////////////////////////////////////////////////////////////////////////

/* TODO Move mkSUWorkerPipe into the main SU module after tests */

interface SUWorkerPipeIfc;
	interface Put#(Instr64) putInstr64;
	interface Put#(Maybe#(Bit#(64))) startWorkerPipe;
	interface Client#( DecodedDataQ3, RdToken_Data) fpuIfc;
	interface Client#( DecodedDataQ2, RdToken_Data) scalarMulDivIfc;
	interface Client#( DecodedDataQ2, RdToken_Data) smuIfc;
	interface Client#( DecodedDataQ3, RdToken_Data) masterSequencerIfc;
endinterface: SUWorkerPipeIfc

// FIXME handle VSTOP, VFENCE, VCJAL, VCJALR
(* synthesize *)
module mkSUWorkerPipe (SUWorkerPipeIfc);

	// todo remove these 2 instantiations after moving mkSUWorkerPipe to main SU module //
	RegFile#(Bit#(5), Bit#(64))	vaRegFile	<- mkRegFileFull;
	
	`ifdef USE_ROB
		SharedRegFile	vsRegFile	<- mkSharedRegFileTagged;
	`else
		SharedRegFile	vsRegFile	<- mkSharedRegFile;
	`endif
	//////////////////////////////////////////////////////////////////////////////////////
	
	`ifdef USE_ROB

		// mkROB (ROBIfc#(buffer_size, reg_addr_width, data_width))
		// Re-order buffer for values to be committed to the shared regfile
		ROBIfc#(`ROB_BUFFER_SIZE, `VS_AddrWidth, 64) rob	<- mkROB;
		
		// stores must be issued only in the commit stage
		FIFOF#(StoreBuffer) 			storeBuffer			<- mkGSizedFIFOF(True, True, `STORE_BUFFER_SIZE);
	`endif

	// mkPipelineFIFOF edited from SpecialFIFOs package
	// edited module can be found in SU_Functions package
	FIFOF#(Instr64)			f2d	<- mkPipelineFIFOF;				// fetch to decode
	FIFOF#(DecodedDataQ2)	d2e	<- mkPipelineFIFOF_UGEnq;		// decode to execute
	FIFOF#(RdToken_Data)	e2w	<- mkPipelineFIFOF_UGDeq;		// execute to write-back

	// FIFOFs with unguarded ".enq", guarded ".deq" [and ".first"]
	FIFOF#(DecodedDataQ3)	fpReqQ				<- mkGFIFOF(True, False);
	FIFOF#(DecodedDataQ2)	scalarMulDivReq		<- mkGFIFOF(True, False);
	FIFOF#(DecodedDataQ2)	toSMU				<- mkGFIFOF(True, False);
	FIFOF#(DecodedDataQ3)	toSequencer			<- mkGFIFOF(True, False);

	// FIFOFs with guarded ".enq", unguarded ".deq" [and ".first"]
	FIFOF#(RdToken_Data)	fpRespQ				<- mkGFIFOF1(False, True);
	FIFOF#(RdToken_Data)	scalarMulDivResp	<- mkGFIFOF1(False, True);
	FIFOF#(RdToken_Data)	fromSMU				<- mkGFIFOF1(False, True);
	FIFOF#(RdToken_Data)	fromVXU				<- mkGFIFOF1(False, True);


	Wire#(Bit#(64)) fetchResponse_instr64 <- mkWire;

	Wire#(Maybe#(Bit#(64))) pcVal	<- mkWire;
	Reg#(Maybe#(Bit#(64)))	vpc		<- mkRegU;

	///////////////////// FORWARDING PATHS /////////////////////
	
	// Structure to consolidate values from all forwarding paths
	Vector#(`FWD_FROM_TOTAL, Maybe#(RdToken_Data)) forwardingPaths ;

	`ifdef FWD_FROM_ScalarExec
		RWire#(RdToken_Data) fwdRd_exec <- mkRWire;
		forwardingPaths[0] = fwdRd_exec.wget;
	`else
		forwardingPaths[0] = Invalid;
	`endif

	`ifdef FWD_FROM_WriteBack
		RWire#(RdToken_Data) fwdRd_wb <- mkRWire;
		forwardingPaths[1] = fwdRd_wb.wget;
	`else
		forwardingPaths[1] = Invalid;
	`endif

	`ifdef FWD_FROM_MulDiv
		RWire#(RdToken_Data) fwdRd_MulDiv <- mkRWire;
		forwardingPaths[2] = fwdRd_MulDiv.wget;
	`else
		forwardingPaths[2] = Invalid;
	`endif

	`ifdef FWD_FROM_SMU
		RWire#(RdToken_Data) fwdRd_SMU <- mkRWire;
		forwardingPaths[3] = fwdRd_SMU.wget;
	`else
		forwardingPaths[3] = Invalid;
	`endif

	`ifdef FWD_FROM_FPU
		RWire#(RdToken_Data) fwdRd_FPU <- mkRWire;
		forwardingPaths[4] = fwdRd_FPU.wget;
	`else
		forwardingPaths[4] = Invalid;
	`endif

	`ifdef FWD_FROM_VectorUnit
		RWire#(RdToken_Data) fwdRd_VXU <- mkRWire;
		forwardingPaths[5] = fwdRd_VXU.wget;
	`else
		forwardingPaths[5] = Invalid;
	`endif

	////////////////////////////////////////////////////////////
	
	/*///////////// todo remove temp rules /////////////
	rule temp_rule_deq_scalarMulDivReq ;
		scalarMulDivReq.deq;
		$display("Deq scalarMulDivReq :",fshow(scalarMulDivReq.first));
	endrule

	rule temp_rule_deq_toSMU ;
		toSMU.deq;
		$display("Deq toSMU :",fshow(toSMU.first));
	endrule

	rule temp_rule_deq_fpReqQ ;
		fpReqQ.deq;
		$display("Deq fpReqQ :",fshow(fpReqQ.first));
	endrule

	rule temp_rule_deq_masterSeq ;
		toSequencer.deq;
		$display("Deq Master Seq :",fshow(toSequencer.first));
	endrule
	//////////////////////////////////////////////////*/


	/* FIXME
		Complete this pipeline stage
		Does the following:
			> When new VF arrives, updates PC to a valid PC given in the VF instruction
			> Calculates correct PC and updates it for consensual branches & jumps
			> Normal operation: Increments PC by 8 (64-bit addressing)
	*/
	rule pcGen (isValid(pcVal));
		vpc	<= pcVal;
	endrule: pcGen

	
	rule instrFetch /*(isValid(vpc))*/;
		// FIXME replace with appropriate instruction fetch methods
		$display("------------ FETCH ------------");
		Disp_instr64	 	disp_instr64	 = unpack(pack(fetchResponse_instr64));
		Disp_instr64_imm	disp_instr64_imm = unpack(pack(fetchResponse_instr64));
		$display(fshow(disp_instr64));
		$display(fshow(disp_instr64_imm));
		f2d.enq(fetchResponse_instr64);
	endrule: instrFetch

	
	/* NOTE	if critical path is here, could try splitting decode into 2 stages: decode & issue
			decode would access regfile and stall if necessary; issue would queue instructions	*/
	rule workerThreadDecode (f2d.notEmpty && (d2e.notFull || 
		scalarMulDivReq.notFull || toSMU.notFull || toSequencer.notFull || fpReqQ.notFull));
		
		$display("------------ DECODE ------------");

		// do 'f2d.deq' only after determining that there's no stall in the pipe
		let instr64	= f2d.first;

		let		imm32	= instr64[63:32];
		let		flags	= i64_flags(instr64);
		let		funct7	= i64_funct7(instr64);
		let		funct3	= i64_funct3(instr64);
		let		funct9	= i64_funct9(instr64);
		let		op		= i64_opcode(instr64);
		let		funct4	= i64_p(instr64);
		let		a_or_s	= (funct9[7]=='b1);		// indicates if vaRegFile or vsRegFile shd be used for Load,Store address

		Bool	s1		= (flags[2]=='b0);		// check if flags indicate shared regs
		Bool	s2		= (flags[1]=='b0);
		Bool	s3		= (flags[0]=='b0);
		
		Bit#(`VS_AddrWidth) rd	= truncate(i64_rd(instr64));
		Bit#(`VS_AddrWidth) rs1	= truncate(i64_rs1(instr64));
		Bit#(`VS_AddrWidth) rs2	= truncate(i64_rs2(instr64));
		Bit#(`VS_AddrWidth) rs3	= truncate(i64_rs3(instr64));
		Bit#(`VA_AddrWidth) as1	= truncate(i64_rs1(instr64));

		Bool isScalarArith		= (op==op_LUI) || (op==op_AUIPC) || (op==op_OPIMM) || (op==op_OPIMM32);
		Bool isIntArith			= ((op==op_OP) || (op==op_OP32)) && (funct9==9'd0);		// for other op_OP instructions, funct9 != 0
		Bool isIntMulDiv		= isIntArith   && (funct7==7'd1);

		Bool isScalarCompute	= ((flags==4'b0) && isIntArith && !isIntMulDiv) || isScalarArith; 
		Bool isScalarMulDiv		= (flags==4'b0) && isIntMulDiv;
		Bool isScalarLoad		= (flags==4'b0) && ( op==op_VLOAD );
		Bool isScalarStore		= (flags==4'b0) && ( op==op_VSTORE );
		Bool isScalarMem		= (isScalarLoad || isScalarStore);
		Bool isScalarFPOp		= (flags==4'b0) && ( op==op_MADD  || op==op_MSUB || op==op_NMSUB || op==op_NMADD || op==op_OP_FP);
		Bool isVecRedn			= isIntArith && (flags==4'b0100) && (funct7==7'b11) && (funct3==3'b110);
		
		Bool isVectorInstr		= !(isScalarCompute || isScalarMulDiv || isScalarMem || isScalarFPOp || isVecRedn);
		Bool isFMA				= (op==op_MADD  || op==op_MSUB || op==op_NMSUB || op==op_NMADD);

		Maybe#(Bit#(64)) operand_1_temp	= Invalid;
		Maybe#(Bit#(64)) operand_2_temp	= Invalid;
		Maybe#(Bit#(64)) operand_3_temp	= Invalid;
		Maybe#(Bit#(64)) operand_1		= Invalid;
		Maybe#(Bit#(64)) operand_2		= Invalid;
		Maybe#(Bit#(64)) operand_3		= Invalid;
		
		`ifdef USE_ROB
			// if valid value present in regfile, use that; else get addr of reserved line in ROB
			// rs_token holds the value of the ROB address; used to forward data from other fuctional units
			// ROB returns a 'Maybe#(Bit#(64))' value; 'Invalid' => stall pipeline
			if (vsRegFile.sub1(rs1) matches tagged RobAddr .robAddr) begin
				let rs1_token		= Valid (robAddr);
				operand_1_temp	 	= rob.forward_data(robAddr);
			end else if (vsRegFile.sub1(rs1) matches tagged Data .data) begin
				let rs1_token		= Invalid;
				operand_1_temp		= Valid (data);
			end

			if (vsRegFile.sub2(rs2) matches tagged RobAddr .robAddr) begin
				let rs2_token		= Valid (robAddr);
				operand_2_temp	 	= rob.forward_data(robAddr);
			end else if (vsRegFile.sub2(rs2) matches tagged Data .data) begin
				let rs2_token		= Invalid;
				operand_2_temp	 	= Valid (data);
			end

			if (vsRegFile.sub3(rs3) matches tagged RobAddr .robAddr) begin
				let rs3_token		= Valid (robAddr);
				operand_3_temp	 	= rob.forward_data(robAddr);
			end else if (vsRegFile.sub3(rs3) matches tagged Data .data) begin
				let rs3_token		= Invalid;
				operand_3_temp	 	= Valid (data);
			end
		`else
			//	if ROB not used, regFile returns a 'Maybe#(Bit#(64))' value; 'Invalid' => stall pipeline
			let rs1_token		= Valid(rs1);
			let rs2_token		= Valid(rs2);
			let rs3_token		= Valid(rs3);
			operand_1_temp 		= vsRegFile.sub1(rs1);
			operand_2_temp 		= vsRegFile.sub2(rs2);
			operand_3_temp 		= vsRegFile.sub3(rs3);
		`endif


		`ifdef FWD_TO_Decode
			// if there is a match on the forwarding datapaths for rs1/rs2/rs3, assign that value; else assign rob/regfile value
			operand_1 = isValid(fwd_data(rs1_token,forwardingPaths))? fwd_data(rs1_token,forwardingPaths) : operand_1_temp;
			operand_2 = isValid(fwd_data(rs2_token,forwardingPaths))? fwd_data(rs2_token,forwardingPaths) : operand_2_temp;
			operand_3 = isValid(fwd_data(rs3_token,forwardingPaths))? fwd_data(rs3_token,forwardingPaths) : operand_3_temp;
		`else 
			operand_1 = operand_1_temp;
			operand_2 = operand_2_temp;
			operand_3 = operand_3_temp;
		`endif

		Bool	operand_1_validity = s1 &&& operand_1 matches tagged Valid .x ? True : (!s1);
		Bool	operand_2_validity = s2 &&& operand_2 matches tagged Valid .x ? True : (!s2);
		Bool	operand_3_validity = s3 &&& operand_3 matches tagged Valid .x ? True : (!s3);

		// zero out rd-field to ensure that optimisation tools will delete this field before synth
		// but allow further stages to use instr64 as is, w/o introducing complex structs & logic
		let instr64_sans_rd	=  {instr64[63:24],8'b0,instr64[15:0]};
		
		if (!isVectorInstr) begin
			
			// if either operand is Invalid, STALL pipeline until data arrives, i.e., don't 'f2d.deq'
			if ( operand_1_validity && (operand_2_validity || isScalarLoad) && (!isFMA || operand_3_validity) ) begin
				
				// if all source regs and the dest reg are shared regs (ie, flags == 4'b0), then instr is decoded as a scalar instr
				if (isScalarCompute && isIntArith && ({i64_n(instr64),i64_p(instr64)} != 5'b0)) begin
					// todo Exception: Illegal Instruction
					$display("Illegal Instruction (IntArith): {n = %b; p = %b} != 0", i64_n(instr64),i64_p(instr64));
					f2d.deq;
				end else begin

					/*	if ROB is used, RdToken is the address of the reserved address line of the ROB (renamed register address)
						if not, RdToken = rd field of the instruction (since no reg renaming) */
					if (isScalarCompute && d2e.notFull) begin
						$display("Decoded: ScalarCompute");
						f2d.deq;

						`ifdef USE_ROB
							let 	x			<-	rob.reserve(rd, False);
							RdToken	rd_renamed	=	unpack(truncate(pack(x)));
							vsRegFile.upd_robAddr(rd, truncate(pack(x)));
						`else
							RdToken	rd_renamed	=	unpack(rd);
							vsRegFile.set_SB(rd);
						`endif

						let d	= DecodedDataQ2 {
							instr64		: instr64_sans_rd,
							rdAddr		: rd_renamed,
							operand1	: operand_1,
							operand2	: operand_2
						};

						$display("Enqued to d2e: \n\t\t", fshow(d));
						d2e.enq (d);

					end else if (isScalarMulDiv && scalarMulDivReq.notFull) begin
						$display("Decoded: ScalarMulDiv");
						f2d.deq;
						
						`ifdef USE_ROB
							let 	x			<-	rob.reserve(rd, False);
							RdToken	rd_renamed	=	unpack(truncate(pack(x)));
							vsRegFile.upd_robAddr(rd, truncate(pack(x)));
						`else
							RdToken	rd_renamed	=	unpack(rd);
							vsRegFile.set_SB(rd);
						`endif

						let d	= DecodedDataQ2 {
							instr64		: instr64_sans_rd,
							rdAddr		: rd_renamed,
							operand1	: operand_1,
							operand2	: operand_2
						};
						
						$display("Enqued to scalarMulDivReq: \n\t\t", fshow(d));
						scalarMulDivReq.enq (d);

					// FIXME LOAD should first check if a prev store is pending to same address; if so, it shd get value from corresponding STORE op
					end else if (isScalarLoad && toSMU.notFull ) begin
						$display("Decoded: ScalarMem (LOAD)");
						f2d.deq;

						`ifdef USE_ROB
							let 	x			<-	rob.reserve(rd, False);
							RdToken	rd_renamed	=	unpack(truncate(pack(x)));
							vsRegFile.upd_robAddr(rd, truncate(pack(x)));
						`else
							RdToken	rd_renamed	=	unpack(rd);
							vsRegFile.set_SB(rd);
						`endif

						let d	= DecodedDataQ2 {
							instr64		: instr64_sans_rd,
							rdAddr		: rd_renamed,
							operand1	: (a_or_s? Valid(vaRegFile.sub(as1)) : operand_1),
							operand2	: Invalid
						};
						
						$display("Enqued to toSMU: \n\t\t", fshow(d));
						toSMU.enq (d);

					`ifdef USE_ROB	// if ROB is used, stores must be issued only in the commit stage				
					end else if (isScalarStore && d2e.notFull && storeBuffer.notFull) begin
						$display("Decoded: ScalarMem (STORE)");
						f2d.deq;

						let 	x			<-	rob.reserve('0, True);			// since stores don't write to regfile, rd = ss0
						RdToken	rd_renamed	=	unpack(truncate(pack(x)));

						let d	= DecodedDataQ2 {
							instr64		: instr64_sans_rd,
							rdAddr		: rd_renamed,
							operand1	: Invalid,			// operand1 is address - stored in store buffer
							operand2	: operand_2			// operand2 is data to be stored @ given address - done in commit stage
						};

						let store_addr	= (a_or_s? vaRegFile.sub(as1) : validValue(operand_1));

						$display("Enqued to d2e: \n\t\t", fshow(d));
						$display("Enqued [address] to store buffer: ", fshow(store_addr));
						d2e.enq (d);
						storeBuffer.enq ( StoreBuffer { instr64: instr64_sans_rd, storeAddr: store_addr});

					`else	// if no ROB is used, all instructions are 'committed' when fetched; so stores can be issued right after decoding
					end else if (isScalarStore && toSMU.notFull) begin
						$display("Decoded: ScalarMem (STORE)");
						f2d.deq;

						RdToken	rd_renamed	=	unpack('0);
						// vsRegFile.set_SB(rd);

						let d	= DecodedDataQ2 {
							instr64		: instr64_sans_rd,
							rdAddr		: rd_renamed,
							operand1	: (a_or_s? Valid(vaRegFile.sub(as1)) : operand_1),
							operand2	: operand_2
						};
						
						$display("Enqued to toSMU: \n\t\t", fshow(d));
						toSMU.enq (d);
					
					`endif

					end else if (isScalarFPOp && fpReqQ.notFull) begin
						$display("Decoded: ScalarFPOp");
						f2d.deq;
						
						`ifdef USE_ROB
							let 	x			<-	rob.reserve(rd, False);
							RdToken	rd_renamed	=	unpack(truncate(pack(x)));
							vsRegFile.upd_robAddr(rd, truncate(pack(x)));
						`else
							RdToken	rd_renamed	=	unpack(rd);
							vsRegFile.set_SB(rd);
						`endif

						let d	= DecodedDataQ3 {
							instr64		: instr64_sans_rd,
							rdAddr		: rd_renamed,
							operand1	: operand_1,
							operand2	: operand_2,
							operand3	: operand_3
						};

						$display("Enqued to fpReqQ: \n\t\t", fshow(d));
						fpReqQ.enq (d);

					end else if (isVecRedn && toSequencer.notFull) begin
						$display("Decoded: VectorReduction");
						f2d.deq;
						
						`ifdef USE_ROB
							let 	x			<-	rob.reserve(rd, False);
							RdToken	rd_renamed	=	unpack(truncate(pack(x)));
							vsRegFile.upd_robAddr(rd, truncate(pack(x)));
						`else
							RdToken	rd_renamed	=	unpack(rd);
							vsRegFile.set_SB(rd);
						`endif

						let d	= DecodedDataQ3 {
							instr64		: instr64_sans_rd,
							rdAddr		: rd_renamed,
							operand1	: operand_1,
							operand2	: operand_2,
							operand3	: operand_3
						};

						$display("Enqued to toSequencer: \n\t\t", fshow(d));
						toSequencer.enq (d);
					end 
				end 
			end 
		end else begin
			if (  operand_1_validity && operand_2_validity && 
				(!isFMA || (isFMA && operand_3_validity)) && toSequencer.notFull) begin
				// all other vector instructions
				$display("Decoded: All Others (send to sequencer)");
				f2d.deq;

				let d	= DecodedDataQ3 {
					instr64		: instr64,
					rdAddr		: '0,
					operand1	: operand_1,
					operand2	: operand_2,
					operand3	: operand_3
				};

				$display("Enqued to toSequencer: \n\t\t", fshow(d));
				toSequencer.enq (d);
			end
		end

	endrule: workerThreadDecode
	
	
	/* 
	NOTE	VADDU (funct7 = b0100000) is not a valid instruction of the Hwacha ISA as of 25-08-2016
			Thus it is not handled by the following execution unit
			Proof:
				> Instruction missing from Yupsup's Hwacha thesis
				> Confirmed by Colin Schmidt in an email
	
	NOTE	No data forwarding paths added to this stage
	*/
	rule exec_Scalar_Compute (d2e.notEmpty);
		$display("---------- SCALAR EXEC ----------");
		
		d2e.deq;

		let d			= d2e.first;
		let vs_src1		= fromMaybe(?,d.operand1);
		let vs_src2		= fromMaybe(?,d.operand2);
		let d_funct7	= i64_funct7(d.instr64);
		let d_imm32		= d.instr64[63:32];
		let d_shamt		= d.instr64[37:32];			/* todo	shd an exception be raised if other bits of imm32 are not "0"?
															Check Pg. 38 on riscv-spec-v2.1.pdf			*/
		let d_isOP32	= i64_opcode(d.instr64)[8];
		let d_isImm		= ~i64_opcode(d.instr64)[10];
		let d_op		= i64_opcode(d.instr64);
		AluOp aluOp		= (d_isImm==1'b1)? unpack(i64_p(d.instr64)[2:0]) : unpack(i64_funct3(d.instr64));	/*	the funct4 field in immediate instructions (VI-type) is
																												the same as the p-field of regular instructions (VR-type) */
		Bit#(64) d_imm64	=	signExtend(d_imm32);

		$display("src1              = %h",vs_src1);
		$display("src2              = %h",vs_src2);
		$display("signExt(imm32)    = %h",d_imm64);
		$display(fshow(aluOp), ", alternate_op = %b, isImm = %b, isOP32 = %b", d_funct7[5], d_isImm, d_isOP32);

		Bit#(64) compute_result = '0;

		if (d_op==op_LUI) begin
			compute_result	= {d_imm32, 32'b0};
		end else if (d_op==op_AUIPC) begin
			compute_result	= {d_imm32, 32'b0} + fromMaybe(?,vpc);
		
		`ifdef USE_ROB
		end else if (d_op==op_VSTORE) begin
			compute_result	= vs_src2;
		`endif

		end else begin
			case (pack(aluOp))
				3'd0:	begin	// VADD
								//							in1		in2			  imm32			imm?	sub?			op32?
							compute_result	= fn_addsub	(vs_src1,	vs_src2,	d_imm32,	 d_isImm,	d_funct7[5],	d_isOP32);
						end
				
				3'd1:	begin	// VSLL
								//							in1		in2			  shamt			imm?	op32?
							compute_result	= fn_sll	(vs_src1,	vs_src2,	d_shamt,	 d_isImm,	d_isOP32);
						end
				
				3'd2:	begin	// VSLT
								//							in1		in2			  imm32			imm?
							compute_result	= fn_slt	(vs_src1,	vs_src2,	d_imm32,	 d_isImm);
						end
				
				3'd3:	begin	// VSLTU
								//							in1		in2			  imm32			imm?
							compute_result	= fn_sltu	(vs_src1,	vs_src2,	d_imm32,	 d_isImm) ;
						end
				
				3'd4:	begin	// VXOR
							if (d_isImm==1'b1) begin
								compute_result	= vs_src1 ^ signExtend(d_imm32);
							end else begin
								compute_result	= vs_src1 ^ vs_src2;
							end
						end
				
				3'd5:	begin	// VSRL
								//							in1		in2			  shamt			imm?	sra?			op32?
							compute_result	= fn_sra_srl(vs_src1,	vs_src2,	d_shamt,	 d_isImm,	d_funct7[5],	d_isOP32) ;
						end
				
				3'd6:	begin	// VOR
							if (d_isImm==1'b1) begin
								compute_result	= vs_src1 | signExtend(d_imm32) ;
							end else begin
								compute_result	= vs_src1 | vs_src2;
							end
						end
				
				3'd7:	begin	// VAND
							if (d_isImm==1'b1) begin
								compute_result	= vs_src1 & signExtend(d_imm32);
							end else begin
								compute_result	= vs_src1 & vs_src2;
							end
						end
			endcase
			fwdRd_exec.wset			( RdToken_Data { rdToken: d.rdAddr, data: compute_result} );
			e2w.enq					( RdToken_Data { rdToken: d.rdAddr, data: compute_result} );
			$display("Enq: ",fshow	( RdToken_Data { rdToken: d.rdAddr, data: compute_result} ));
		end

	endrule: exec_Scalar_Compute


	rule write_back (e2w.notEmpty || scalarMulDivResp.notEmpty || fromSMU.notEmpty || fpRespQ.notEmpty || fromVXU.notEmpty);
		$display("---------- WRITE BACK ----------");
		
		RdToken_Data	d;
		FU_Code			fu_code;

		`ifdef USE_ROB
			`ifdef Scalar_WriteBack_Priority
				fu_code	= 	arbitration_priority (e2w.notEmpty, scalarMulDivResp.notEmpty, 
										fromSMU.notEmpty, fpRespQ.notEmpty, fromVXU.notEmpty);
			`else
				RdToken exec	= (e2w.notEmpty)? e2w.first.rdToken : unpack('1);
				RdToken muldiv	= (scalarMulDivResp.notEmpty)? scalarMulDivResp.first.rdToken : unpack('1);
				RdToken mem		= (fromSMU.notEmpty)? fromSMU.first.rdToken : unpack('1);
				RdToken fpu		= (fpRespQ.notEmpty)? fpRespQ.first.rdToken : unpack('1);
				RdToken vecRedn	= (fromVXU.notEmpty)? fromVXU.first.rdToken : unpack('1);

				fu_code =	arbitration_oldestFirst (!rob.isHeadOverTail, exec, muldiv, mem, fpu, vecRedn);
			`endif
		`else
			// only priority arbitration supported when ROB not present
			fu_code	= 	arbitration_priority (e2w.notEmpty, scalarMulDivResp.notEmpty, 
									fromSMU.notEmpty, fpRespQ.notEmpty, fromVXU.notEmpty);
		`endif

		$display("Deq from: ", fshow(fu_code));

		if (fu_code == Exec && e2w.notEmpty) begin
			e2w.deq;
			d = e2w.first;
		end else if (fu_code == MulDiv && scalarMulDivResp.notEmpty) begin
			scalarMulDivResp.deq;
			d = scalarMulDivResp.first;
		end else if (fu_code == Mem && fromSMU.notEmpty) begin
			fromSMU.deq;
			d = fromSMU.first;
		end else if (fu_code == FPop && fpRespQ.notEmpty) begin
			fpRespQ.deq;
			d = fpRespQ.first;
		end else begin // if (fu_code == VecRedn && fromVXU.notEmpty) begin
			fromVXU.deq;
			d = fromVXU.first;
		end

		`ifdef USE_ROB
			let addr	= truncate(pack(d.rdToken));
			rob.store_data(addr, d.data);
			$display("store_data in rob: ",fshow(d));
		`else
			vsRegFile.upd_data(d.rdToken, d.data);
			$display("upd_data in vsRegFile: ", fshow(d));
		`endif

		fwdRd_wb.wset (d);

	endrule: write_back


	rule forwardingDataPaths ;
		
		if (scalarMulDivResp.notEmpty) begin
			fwdRd_MulDiv.wset (scalarMulDivResp.first);
		end
		
		if (fromSMU.notEmpty) begin
			fwdRd_SMU.wset (fromSMU.first);
		end
		
		if (fpRespQ.notEmpty) begin
			fwdRd_FPU.wset (fpRespQ.first);
		end
		
		if (fromVXU.notEmpty) begin
			fwdRd_VXU.wset (fromVXU.first);
		end

	endrule: forwardingDataPaths

	
	`ifdef USE_ROB

		(* descending_urgency = "rob_commit, write_back" *)
		rule rob_commit ( (!rob.isStore) || (rob.isStore && toSMU.notFull && storeBuffer.notEmpty) );
			$display("------------ COMMIT ------------");
			let commitVal	<- rob.commit();

			if (!commitVal.isStore) begin
				vsRegFile.upd_data(commitVal.reg_addr, commitVal.data);
			end else begin // if (commitVal.isStore && toSMU.notFull) begin
				let instr64_sans_rd	= storeBuffer.first.instr64;
				let store_addr		= storeBuffer.first.storeAddr;

				let d	= DecodedDataQ2 {
					instr64		: instr64_sans_rd,
					rdAddr		: '0,
					operand1	: Valid(store_addr),
					operand2	: Valid(commitVal.data)
				};

				storeBuffer.deq;
				$display("Enqued to toSMU: [STORE operation] \n\t\t", fshow(d));
				toSMU.enq (d);		// FIXME toSMU.enq can happen from 2 rules simultaneously; make sure this is allowed, or else have 2 Qs
			end
		endrule: rob_commit

	`endif

	
	interface Put putInstr64;
		method Action put(Instr64 instr64_in);
			fetchResponse_instr64 <= instr64_in;
		endmethod
	endinterface: putInstr64

	interface Put startWorkerPipe;
		method Action put(Maybe#(Bit#(64)) pcValNew);
			pcVal	<= pcValNew;
		endmethod
	endinterface: startWorkerPipe

	interface Client fpuIfc;
		interface Get request	= toGet(fpReqQ);
		interface Put response	= toPut(fpRespQ);
	endinterface: fpuIfc
	
	interface Client scalarMulDivIfc;
		interface Get request	= toGet(scalarMulDivReq);
		interface Put response	= toPut(scalarMulDivResp);
	endinterface: scalarMulDivIfc
	
	interface Client smuIfc;
		interface Get request	= toGet(toSMU);
		interface Put response	= toPut(fromSMU);
	endinterface: smuIfc
	
	interface Client masterSequencerIfc;
		interface Get request	= toGet(toSequencer);
		interface Put response	= toPut(fromVXU);
	endinterface: masterSequencerIfc


endmodule: mkSUWorkerPipe



endpackage: ScalarUnit

