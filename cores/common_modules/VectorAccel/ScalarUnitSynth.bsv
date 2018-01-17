package ScalarUnitSynth;

import FIFO::*;
import FIFOF::*;
import GetPut::*;
import RegFile::*;
// import RevertingVirtualReg::*;

import VectorAccelDefs::*;
import alu64::*;

///////////////////////////////////////////////////////////////////////////////////////////////////

interface SharedRegFile;
	method Action 	upd(Bit#(6) addr, Bit#(64) d);
	method Bit#(64)	sub(Bit#(6) addr);
endinterface: SharedRegFile

module mkSharedRegFile (SharedRegFile);

	// Create a register file which is indexed by 6 bits, and holds 64 bit data.
	RegFile#( Bit#(6), Bit#(64) ) sharedregfile <- mkRegFileFull;

	method Action upd (Bit#(6) addr, Bit#(64) d);
		// if (addr!=6'b0)
			sharedregfile.upd(addr, d);
	endmethod

	method Bit#(64) sub (Bit#(6) addr);
		Bit#(64) readVal;
		if (addr==6'b0)
			readVal = 64'b0;
		else
			readVal = sharedregfile.sub(addr);

		return readVal;
	endmethod

endmodule: mkSharedRegFile


///////////////////////////////////////////////////////////////////////////////////////////////////


interface ScalarUnitIfc;
	interface Put#(CMDQReq) 	putRoccInstr;
	interface Get#(Vcfg) 		rocc_readVCFG;
	interface Get#(VlenStruct)	rocc_readVLEN;
	interface Put#(Bool) 		rocc_resetVCFG;
	// interface Put#(Maybe#(Vlen)) rocc_writeVLEN;
	interface Get#(Maybe#(Bit#(64))) 	startWorkerPipe;
endinterface: ScalarUnitIfc


/* 	todo	for instructions where scoreboard (SB) bit is SET, pipeline must 
			interlock on the destination register until the result is sent
			back to the scalar unit
			Add support to allow this
	*/
module mkScalarUnit (ScalarUnitIfc);

	/* 	va = vector address registers
		vs = vector shared registers
		the "SharedRegFile" is a simple RegFile, with vs0 hardwired to constant 0	
		todo figure out where to instantiate */
	RegFile#(Bit#(5), Bit#(64))	vaRegFile	<- mkRegFileFull;
	SharedRegFile				vsRegFile	<- mkSharedRegFile;

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

	/* todo	A conflict occurs since VUNCFG is not queued into the VCMDQ
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
		setVL_response.wset('0);	// Bit#(0) type so wset value doesn't matter
		$display("*exec_VSETVL*");
	endrule: exec_VSETVL

	rule exec_VMCS (decodedInstr32 == Vmcs);
		let rs2			= i32_rs2(instr32);
		let rd			= i32_rd (instr32);
		let vsRegSel	= { rs2[0], rd };	/* NOTE VMCS instr actually takes {rs2[2:0],rd}
													But only 6-bits reqd for the current 
													shared-regfile */
		vsRegFile.upd(vsRegSel, src1);
		$display("*exec_VMCS*");
	endrule: exec_VMCS

	rule exec_VMCA (decodedInstr32 == Vmca);
		let vaRegSel	= i32_rd (instr32);
		vaRegFile.upd(vaRegSel, src1);
		$display("*exec_VMCA*");
	endrule: exec_VMCA

	rule exec_VF (decodedInstr32 == Vf);
		// todo figure out how VF is implemented
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
	interface Get#(Tuple2#(Bit#(8),Bit#(64))) get_result;
	interface Put#(Maybe#(Bit#(64)))	 	startWorkerPipe;
	// interface Client#( Instr64, resp_type ) fpReqResp;
	// interface Client#( Instr64, resp_type ) scalarMulDiv;
	// interface Client#( Instr64, resp_type ) smuIfc;
	// interface Client#( Instr64, resp_type ) masterSequencerIfc;
endinterface: SUWorkerPipeIfc

(* synthesize *)
module mkSUWorkerPipe (SUWorkerPipeIfc);

	// todo change fifo types if reqd
	// todo use mkSizedFIFO for improved performance
	FIFOF#(Instr64)						decode2execute	<- mkFIFOF;
	FIFOF#(Tuple2#(Bit#(8),Bit#(64)))	execResult		<- mkFIFOF;

	FIFO#(Instr64) 			fpReqQ			<- mkFIFO;
	FIFO#(Instr64)			scalarMulDivReq	<- mkFIFO;
	FIFO#(Instr64)			toSMU			<- mkFIFO;
	FIFO#(Instr64)			toSequencer		<- mkFIFO;
	Wire#(Bit#(64)) 		instr64 		<- mkWire;

	Wire#(Maybe#(Bit#(64))) pcVal	<- mkWire;
	Reg#(Maybe#(Bit#(64)))	pc		<- mkRegU;

	// todo remove these 2 instantiations after moving mkSUWorkerPipe to main SU module //
	RegFile#(Bit#(5), Bit#(64))	vaRegFile	<- mkRegFileFull;
	RegFile#(Bit#(6), Bit#(64))	vsRegFile	<- mkRegFileFull;
	//////////////////////////////////////////////////////////////////////////////////////

	Reg#(Bit#(64)) vs_src1				<- mkRegU;
	Reg#(Bit#(64)) vs_src2				<- mkRegU;

	Reg#(Bit#(1)) dummy <- mkRegU;

	
	rule tests_disp ;		
		Disp_i64 		disp_instr64	 = unpack(pack(instr64));
		Disp_i64_Imm 	disp_instr64_imm = unpack(pack(instr64));
		$display(fshow(disp_instr64));
		$display(fshow(disp_instr64_imm));
	endrule: tests_disp
	

	rule pcGen (isValid(pcVal));
		// todo find what more to be done here
		pc	<= pcVal;
	endrule: pcGen

	rule instrFetch (isValid(pc));
		// todo
		dummy <= '1;
	endrule: instrFetch

	rule workerThreadDecode ;
		$display("------------ DECODE ------------");

		// if all source regs and the dest reg are shared regs (ie, flags == 4'b0), then instr is decoded as a scalar instr
		let flags 	= i64_flags(instr64);
		let	funct7	= i64_funct7(instr64);
		let	funct3	= i64_funct3(instr64);
		let	funct9	= i64_funct9(instr64);
		let op 		= i64_opcode(instr64);

		Bool isScalarArith		= (op==op_LUI) || (op==op_AUIPC) || (op==op_OPIMM) || (op==op_OPIMM32);
		Bool isIntArith			= ((op==op_OP) || (op==op_OP32)) && (funct9==9'd0);		// for other op_OP instructions, funct9 != 0
		Bool isIntMulDiv		= isIntArith   && (funct7==7'd1);

		Bool isScalarCompute	= ((flags==4'b0) && isIntArith && !isIntMulDiv) || isScalarArith; 
		Bool isScalarMulDiv		= (flags==4'b0) && isIntMulDiv;
		Bool isScalarMem		= (flags==4'b0) && ( op==op_VLOAD || op==op_VSTORE );
		Bool isScalarFPOp		= (flags==4'b0) && ( op==op_MADD  || op==op_MSUB || op==op_NMSUB || op==op_NMADD || op==op_OP_FP);
		Bool isVecRedn			= isIntArith && (flags==4'b0100) && (funct7==7'b11) && (funct3==3'b110);

		let imm32	= instr64[63:32];
		let funct4	= i64_p(instr64);		
		let rd		= i64_rd(instr64);
		let rs1		= i64_rs1(instr64);
		let rs2		= i64_rs2(instr64);

		if (isScalarCompute) begin
			if (isIntArith && ({i64_n(instr64),i64_p(instr64)} != 5'b0)) begin
				// todo Exception: Illegal Instruction
				$display("Illegal Instruction (IntArith): {n = %b; p = %b} != 0", i64_n(instr64),i64_p(instr64));	
			end else begin
				decode2execute.enq (instr64);
				$display("Decoded: ScalarCompute");
			end

			vs_src1 <= vsRegFile.sub(truncate(rs1));
			vs_src2 <= vsRegFile.sub(truncate(rs2));
			
			// $display("Decoded: ScalarCompute");
		end else if (isScalarMulDiv) begin
			// todo set SB bit
			$display("Decoded: ScalarMulDiv");
			scalarMulDivReq.enq(instr64);
		end else if (isScalarMem) begin
			// todo set SB bit
			toSMU.enq(instr64);
			$display("Decoded: ScalarMem");
		end else if (isScalarFPOp) begin
			// todo set SB bit
			fpReqQ.enq(instr64);
			$display("Decoded: ScalarFPOp");
		end else if (isVecRedn) begin
			// todo set SB bit
			toSequencer.enq(instr64);
			$display("Decoded: VectorReduction");
		end else begin
			// all other vector instructions
			toSequencer.enq(instr64);
			$display("Decoded: All Others (send to sequencer)");
		end

	endrule: workerThreadDecode
	

	rule exec_Scalar_Compute;
		$display("---------- SCALAR EXEC ----------");
		
		decode2execute.deq;

		let d			= decode2execute.first;
		let d_funct7	= i64_funct7(d);
		let d_imm32		= d[63:32];
		let d_shamt		= d[37:32];			/* todo	shd an exception be raised if other bits of imm32 are not "0"?
													Check Pg. 38 on riscv-spec-v2.1.pdf			*/ 
		let d_isOP32	= i64_opcode(d)[8];
		let d_isImm		= ~i64_opcode(d)[10];
		let d_op		= i64_opcode(d);
		AluOp aluOp		= (d_isImm==1'b1)? unpack(i64_p(d)[2:0]) : unpack(i64_funct3(d));	/*	the funct4 field in immediate instructions (VI-type) is
																								the same as the p-field of regular instructions (VR-type) */
		Bit#(64) d_imm64	=	signExtend(d_imm32);

		$display("src1              = %h",vs_src1);
		$display("src2              = %h",vs_src2);
		$display("signExt(imm32)    = %h",d_imm64);
		$display(fshow(aluOp), ", alternate_op = %b, isImm = %b, isOP32 = %b", d_funct7[5], d_isImm, d_isOP32);

		// execDest	<= i64_rd(d);

		// todo check if VADDU (funct7 = b0100000) is a valid instruction
		if (d_op==op_LUI) begin
			execResult.enq(tuple2( i64_rd(d), {d_imm32, 32'b0} ));
		end else if (d_op==op_AUIPC) begin
			let x	= {d_imm32, 32'b0} + fromMaybe(?,pc);	// todo shd this be pc or pc+4 ?
			execResult.enq(tuple2( i64_rd(d), x));
		end else begin
			case (pack(aluOp))
				3'd0:	begin	// VADD
								//														in1			in2			  imm32			imm?	sub?			op32?
							execResult.enq			(	tuple2( i64_rd(d), fn_addsub	(vs_src1,	vs_src2,	d_imm32,	 d_isImm,	d_funct7[5],	d_isOP32) ) );
							$display("Enq: ",fshow	(	tuple2( i64_rd(d), fn_addsub	(vs_src1,	vs_src2,	d_imm32,	 d_isImm,	d_funct7[5],	d_isOP32))));
						end
				
				3'd1:	begin	// VSLL
								//														in1		in2			  shamt			imm?	op32?
							execResult.enq			(	tuple2( i64_rd(d), fn_sll	(vs_src1,	vs_src2,	d_shamt,	 d_isImm,	d_isOP32) ) );
							$display("Enq: ",fshow	(	tuple2( i64_rd(d), fn_sll	(vs_src1,	vs_src2,	d_shamt,	 d_isImm,	d_isOP32))));
						end
				
				3'd2:	begin	// VSLT
								//														in1		in2			  imm32			imm?
							execResult.enq			(	tuple2( i64_rd(d), fn_slt	(vs_src1,	vs_src2,	d_imm32,	 d_isImm) ) );
							$display("Enq: ",fshow	(	tuple2( i64_rd(d), fn_slt	(vs_src1,	vs_src2,	d_imm32,	 d_isImm) )));
						end
				
				3'd3:	begin	// VSLTU
								//														in1		in2			  imm32			imm?
							execResult.enq			(	tuple2( i64_rd(d), fn_sltu	(vs_src1,	vs_src2,	d_imm32,	 d_isImm) ) );
							$display("Enq: ",fshow	(	tuple2( i64_rd(d), fn_sltu	(vs_src1,	vs_src2,	d_imm32,	 d_isImm) )));
						end
				
				3'd4:	begin	// VXOR
							if (d_isImm==1'b1) begin			
								execResult.enq			(	tuple2( i64_rd(d), vs_src1 ^ signExtend(d_imm32) ) );
								$display("Enq: ",fshow	(	tuple2( i64_rd(d), vs_src1 ^ signExtend(d_imm32) )));
							end else begin			
								execResult.enq			(	tuple2( i64_rd(d), vs_src1 ^ vs_src2 ) );
								$display("Enq: ",fshow	(	tuple2( i64_rd(d), vs_src1 ^ vs_src2 )));
							end
						end
				
				3'd5:	begin	// VSRL
								//														in1		in2			  shamt			imm?	sra?			op32?
							execResult.enq			(	tuple2( i64_rd(d), fn_sra_srl(vs_src1,	vs_src2,	d_shamt,	 d_isImm,	d_funct7[5],	d_isOP32) ) );
							$display("Enq: ",fshow	(	tuple2( i64_rd(d), fn_sra_srl(vs_src1,	vs_src2,	d_shamt,	 d_isImm,	d_funct7[5],	d_isOP32) )));
						end
				
				3'd6:	begin	// VOR
							if (d_isImm==1'b1) begin			
								execResult.enq			(	tuple2( i64_rd(d), vs_src1 | signExtend(d_imm32) ) );
								$display("Enq: ",fshow	(	tuple2( i64_rd(d), vs_src1 | signExtend(d_imm32) )));
							end else begin			
								execResult.enq			(	tuple2( i64_rd(d), vs_src1 | vs_src2 ) );
								$display("Enq: ",fshow	(	tuple2( i64_rd(d), vs_src1 | vs_src2 )));
							end
						end
				
				3'd7:	begin	// VAND
							if (d_isImm==1'b1) begin			
								execResult.enq			(	tuple2( i64_rd(d), vs_src1 & signExtend(d_imm32) ) );
								$display("Enq: ",fshow	(	tuple2( i64_rd(d), vs_src1 & signExtend(d_imm32) )));
							end else begin			
								execResult.enq			(	tuple2( i64_rd(d), vs_src1 & vs_src2 ) );
								$display("Enq: ",fshow	(	tuple2( i64_rd(d), vs_src1 & vs_src2 )));
							end
						end
			endcase
		end

	endrule: exec_Scalar_Compute

	rule temp_rule_deq_masterSeq ;	// todo temp rule
		toSequencer.deq;
		$display("Deq Master Seq :",fshow(toSequencer.first));
	endrule: temp_rule_deq_masterSeq

/*	rule write_back ;
		$display("---------- WRITE BACK ----------");

		let d	= execResult.first;
		execResult.deq;
		vsRegFile.upd(truncate(tpl_1(d)), tpl_2(d));		// rd is a 8-bit field; regfile has 6-bit addr
		$display("execResult: {rd,result} = ",fshow(d));
	endrule: write_back
	*/

	interface Get get_result;
	    method ActionValue#(Tuple2#(Bit#(8),Bit#(64))) get();
	    	execResult.deq;
	    	return execResult.first;
	    endmethod
	endinterface: get_result

	interface Put putInstr64;
		method Action put(Instr64 instr64_in);
			instr64 <= instr64_in;
		endmethod
	endinterface: putInstr64

	interface Put startWorkerPipe;
	    method Action put(Maybe#(Bit#(64)) pcValNew);
	    	pcVal	<= pcValNew;
	    endmethod
	endinterface: startWorkerPipe

endmodule: mkSUWorkerPipe



endpackage: ScalarUnitSynth

