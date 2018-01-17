/*
Copyright (c) 2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name     : VXU lanes
Author Name     : Vinod G, Sumanth Sridhar
e-mail Id       : g.vinod1993@gmail.com, sumanthsridhar.009@gmail.com
Last updated on : 5th November 2016
*/

package lane;

import GetPut::*;
import ClientServer::*;
import RegFile::*;
import Vector::*;
import ConfigReg::*;
import FIFO::*;
import FIFOF::*;

import VectorAccelDefs::*;
import Bank::*;
import ALU64::*;

`include "vxu_defs.bsv"

typedef UInt#(TAdd#(1,TLog#(`B))) 			IntJ;		// just used for neater $display statements
typedef DataAndType#(Bit#(`BANK_WIDTH))		BankReadType;

function Bit#(`BANK_WIDTH) arithmeticRightShift (Bit#(`BANK_WIDTH) in, Bit#(TLog#(`BANK_WIDTH)) shiftBy);
	Int#(`BANK_WIDTH) shiftedValue	= (unpack(pack(in)) >> shiftBy);
	return (unpack(pack(shiftedValue)));
endfunction: arithmeticRightShift


function BankReadType replicateScalarOperands (DataAndType#(Bit#(`W)) sreg);
	Vector#(TDiv#(`BANK_WIDTH,`W), Bit#(`BANK_WIDTH)) temp;
	temp[0] = zeroExtend(sreg.data);
	for (Integer i=1; i<valueOf(TDiv#(`BANK_WIDTH,`W)); i=i+1) begin
		temp[i] = (temp[i-1] << `W) | zeroExtend(sreg.data);
	end
	return BankReadType { data: last(temp), dataType: sreg.dataType};
endfunction: replicateScalarOperands


function Bit#(`W) truncateToBitwidth (Bit#(`BANK_WIDTH) in, BitWidth bitWidth);
	Bit#(`W) temp;

	// TODO check if this should be zero or sign extended
	if (bitWidth == B)
		temp = signExtend(in[7:0]);
	else if (bitWidth == H)
		temp = signExtend(in[15:0]);
	else if (bitWidth == W)
		temp = signExtend(in[31:0]);
	else // if (bitWidth == D)
		temp = signExtend(in[63:0]);
	// else 
	// 	temp = signExtend(in[127:0]);

	return temp;
endfunction: truncateToBitwidth

// (*noinline*)
function Bit#(`W) extractElement (BankReadType in, Bit#(TLog#(TDiv#(`BANK_WIDTH,`w))) i);
	Bit#(`W) temp1;
	Bit#(`W) temp2;
	let data	= in.data;

	if (in.dataType == B) begin
		let shiftBy = {i, 3'd0};
		temp1 = truncate(data >> shiftBy);
		temp2 = signExtend(temp1[7:0]);
	end else if (in.dataType == H) begin
		let shiftBy = {i, 4'd0};
		temp1 = truncate(data >> shiftBy);
		temp2 = signExtend(temp1[15:0]);
	end else if (in.dataType == W) begin
		let shiftBy = {i, 5'd0};
		temp1 = truncate(data >> shiftBy);
		temp2 = signExtend(temp1[31:0]);
	end else begin // if (in.dataType == D) begin
		let shiftBy = {i, 6'd0};
		temp1 = truncate(data >> shiftBy);
		temp2 = signExtend(temp1[63:0]);
	end /* else begin // if (in.dataType == Q) begin
		let shiftBy = {i, 7'd0};
		temp1 = truncate(data >> shiftBy);
		temp2 = signExtend(temp1[127:0]);
	end */

	return temp2;
endfunction: extractElement

// (*noinline*)
function BankReadType insertElement (BankReadType vector, Bit#(`W) element, Bit#(TLog#(TDiv#(`BANK_WIDTH,`w))) i);
	Bit#(`BANK_WIDTH) resultVector;
	Bit#(`BANK_WIDTH) x	= zeroExtend(element);

	if (vector.dataType == B) begin
		let shiftBy	= {i, 3'd0};
		resultVector = (zeroExtend(x[7:0]) << shiftBy) | vector.data;		// TODO verify if such strong type assertion (B/H/W..) is warranted
	end else if (vector.dataType == H) begin
		let shiftBy	= {i, 4'd0};
		resultVector = (zeroExtend(x[15:0]) << shiftBy) | vector.data;
	end else if (vector.dataType == W) begin
		let shiftBy	= {i, 5'd0};
		resultVector = (zeroExtend(x[31:0]) << shiftBy) | vector.data;
	end else begin // if (vector.dataType == D) begin
		let shiftBy	= {i, 6'd0};
		resultVector = (zeroExtend(x[63:0]) << shiftBy) | vector.data;
	end /* else begin // if (vector.dataType == Q) begin
		let shiftBy	= {i, 7'd0};
		resultVector = (zeroExtend(x[127:0]) << shiftBy) | vector.data;
	end */

	return BankReadType {data: resultVector, dataType: vector.dataType};
endfunction: insertElement


interface LaneIfc;
	interface Put#(LaneSequencerPacket) putLaneSequencerPacket;
	interface Client#( XBar, Bit#(`BANK_WIDTH) ) useFuncUnits;
	interface Put#(Bit#(`BANK_PRED_WIDTH)) writeToPRF;
	interface Server#( Vector#(`B, Bool) , Vector#(`B, Bit#(`BANK_WIDTH)) ) readBRQs; 
	interface Server#( Vector#(`B, Bool) , Vector#(`B, Bit#(`BANK_PRED_WIDTH)) ) readBPQs;
endinterface: LaneIfc

module mkLane (LaneIfc)
	provisos (
		Max#(`VRF_WIDTH,`BANK_WIDTH,`VRF_WIDTH),
		Max#(`BANK_WIDTH,`W,`BANK_WIDTH),			// VRF_WIDTH >= BANK_WIDTH >= W

		Mul#(`HLEN,`W,`VRF_WIDTH),					// no of W-bit elements per line in VRF = bit-width of VRF
		Add#(`VRF_DEPTH,0,`BANK_DEPTH),				// no of vectors in VRF (depth of VRF) = no of vectors in each bank (depth of bank) 
		Mul#(`B,`L,bl),								// bn = B x L = total no of banks in the vector processor
		Mul#(`BANK_WIDTH,bl,`VRF_WIDTH),			// since each bank acts like a column of the total VRF
		
		Div#(`BANK_WIDTH,`W,elements_per_bank),
		Mul#(elements_per_bank,`W,`BANK_WIDTH),		// mod(bit-width of bank, 64) should be 0 as each bank entry can hold 'n' elements of W-bits
		Max#(`N_bankALUs,elements_per_bank,elements_per_bank),
		
		Div#(`BANK_WIDTH,8,bytes_per_bank),
		Mul#(bytes_per_bank,8,`BANK_WIDTH)			// mod(bit-width of bank, 8) should be 0 as each bank should be byte-addressable
	);

	Vector#( `B, VRFBankIfc )	bank	<- replicateM(mkVRFBank);
	Vector#( `B, RegFile#( Bit#(TLog#(`PRF_DEPTH)), Bit#(`BANK_PRED_WIDTH)) ) 
		predBank <- replicateM(mkRegFileFull);
	
	Vector#( `B, RWire#(BankReadType) ) 
		bankReadPort	<- replicateM(mkRWire);
	Vector#( `B, RWire#(Bit#(`BANK_PRED_WIDTH)) ) 
		predReadPort	<- replicateM(mkRWire);
	
	Vector#( `B, Reg# (BankReadType)  )
		bankLatch0		<- replicateM(mkReg(unpack('0)));
	Vector#( `B, Reg# (BankReadType)  )
		bankLatch1		<- replicateM(mkReg(unpack('0)));
	Vector#( `B, Reg# (BankReadType)  )
		bankLatch2		<- replicateM(mkReg(unpack('0)));
	Vector#( `B, Reg# (Bit#(`BANK_PRED_WIDTH))  )
		predicateLatch	<- replicateM(mkReg(0));
	
	Vector#( `B, Reg# (BankReadType)  )
		aluLatch		<- replicateM(mkReg(unpack('0)));	// NOTE can change to 'Wire' to reduce ALU latency to zero
	Vector#(TMul#(`B,`N_bankALUs), ALU64Ifc) 
		alu				<- replicateM(mkLaneALU);
	
	// FIXME check if sreg0, sreg1, sreg2 need to be Vectors; ie, if they are local to each bank/lane/what?
	Vector#( `B, Reg#( DataAndType#(Bit#(`W)) ) ) 
		sreg0			<- replicateM(mkReg(unpack('0)));
	Vector#( `B, Reg#( DataAndType#(Bit#(`W)) ) ) 
		sreg1			<- replicateM(mkReg(unpack('0)));
	Vector#( `B, Reg#( DataAndType#(Bit#(`W)) ) ) 
		sreg2			<- replicateM(mkReg(unpack('0)));

	
	Vector#( `B, Reg#(Maybe#(MicroOps)) ) 
		uopsReg	<- replicateM(mkConfigRegU);

	Vector#( `B, MicroOps) uops;
	for (Integer i=0; i<`B; i=i+1) begin
		uops[i] = validValue(uopsReg[i]) ;
	end


	RWire#(MicroOps) uops_new	<- mkRWire;
	Wire#( DataAndType#(Bit#(`W)) ) sreg0_new	<- mkWire;
	Wire#( DataAndType#(Bit#(`W)) ) sreg1_new	<- mkWire;
	Wire#( DataAndType#(Bit#(`W)) ) sreg2_new	<- mkWire;

	FIFO#(XBar)						toXBar			<- mkFIFO;
	FIFO#(Bit#(`BANK_WIDTH))		writeBuffer		<- mkFIFO;
	FIFO#(Bit#(`BANK_PRED_WIDTH))	predWriteBuffer	<- mkFIFO;

	// operand and pred read Qs for memory stores
	// SizedFIFOFs with guarded ".enq", unguarded ".deq" [and ".first"]
	// change fifo type to "BankReadType" if reqd
	Vector#( `B, FIFOF#(Bit#(`BANK_WIDTH)))
		brq		<- replicateM(mkGSizedFIFOF(False, True, `BRQ_DEPTH));
	Vector#( `B, FIFOF#(Bit#(`BANK_PRED_WIDTH)))
		bpq		<- replicateM(mkGSizedFIFOF(False, True, `BPQ_DEPTH));

	rule display ;
		$display("\n\n\n****************************************************\n");
		for (IntJ i=0; i<`B; i=i+1) begin
			$display("\n\n------------ BANK %d ------------",i);
			$display("\nuops: isValid? ", fshow(isValid(uopsReg[i])));
				$display("\tvrf_read: ",fshow(uops[i].vrf_read));
				$display("\tvrf_write: ",fshow(uops[i].vrf_write));
				$display("\tpred_read: ",fshow(uops[i].pred_read));
				$display("\tpred_write: ",fshow(uops[i].pred_write));
				$display("\topl: ",fshow(uops[i].opl));
				$display("\tpdl: ",fshow(uops[i].pdl));
				$display("\txbar: ",fshow(uops[i].xbar));
				$display("\tsreg: ",fshow(uops[i].sreg));
				$display("\tfop_alu: ",fshow(uops[i].fop_alu));
				// $display("\tfop_plu: ",fshow(uops[i].fop_plu));
				$display("\tfop_brq: ",fshow(uops[i].fop_brq));
				$display("\tfop_bpq: ",fshow(uops[i].fop_bpq));
				// $display("\tfop_vfu0_fma0: ",fshow(uops[i].fop_vfu0_fma0));
				// $display("\tfop_vfu0_imul: ",fshow(uops[i].fop_vfu0_imul));
				// $display("\tfop_vfu0_fconv: ",fshow(uops[i].fop_vfu0_fconv));
				// $display("\tfop_vfu1_fma1: ",fshow(uops[i].fop_vfu1_fma1));
				// $display("\tfop_vfu1_fcmp: ",fshow(uops[i].fop_vfu1_fcmp));
				// $display("\tfop_vfu2: ",fshow(uops[i].fop_vfu2));
				// $display("\tfop_vgu: ",fshow(uops[i].fop_vgu));
				$display("\n");
			$display("sreg0: ", fshow(sreg0[i]));
			$display("sreg1: ", fshow(sreg1[i]));
			$display("sreg2: ", fshow(sreg2[i]));
			$display("bankReadPort : ", fshow(bankReadPort[i].wget));
			$display("bankLatch0   : ", fshow(bankLatch0[i]));
			$display("bankLatch1   : ", fshow(bankLatch1[i]));
			$display("bankLatch2   : ", fshow(bankLatch2[i]));
			$display("predicateLatch: ", fshow(predicateLatch[i]));
			$display("\n");
		end
	endrule: display

	rule toBank0 ;
		uopsReg[0]	<= uops_new.wget;
		sreg0[0]	<= sreg0_new;
		sreg1[0]	<= sreg1_new;
		sreg2[0]	<= sreg2_new;
	endrule: toBank0

	rule systolicBankTraversal ;
		for (Integer i=1; i<`B; i=i+1) begin
			uopsReg[i]	<= uopsReg[i-1];
			sreg0[i]	<= sreg0[i-1];
			sreg1[i]	<= sreg1[i-1];
			sreg2[i]	<= sreg2[i-1];
		end
	endrule: systolicBankTraversal

	for (Integer i=0; i<`B; i=i+1) begin
		
		rule vrf_read (isValid(uopsReg[i]));
				let rawReadValue	= bank[i].sub(uops[i].vrf_read.addr);
				let shiftBy			= (zeroExtend(uops[i].vrf_read.byteOffset) * 'd8);
				rawReadValue.data	= arithmeticRightShift(rawReadValue.data, shiftBy);
				bankReadPort[i].wset(rawReadValue);
		endrule: vrf_read

		rule pred_read (isValid(uopsReg[i]));
				Bit#(`BANK_PRED_WIDTH) n_flag			= signExtend(uops[i].pred_read.n);
				Bit#(`BANK_PRED_WIDTH) rawReadValue		= predBank[i].sub(uops[i].pred_read.addr);
				Bit#(TLog#(`BANK_PRED_WIDTH)) shiftBy	= (zeroExtend(uops[i].pred_read.bitOffset));
				Bit#(`BANK_PRED_WIDTH) shiftedValue		= (rawReadValue >> shiftBy);
				predReadPort[i].wset(shiftedValue ^ n_flag);
		endrule: pred_read

		rule update_latches (isValid(uopsReg[i]));
				
				if (uops[i].opl == Zero && uops[i].sreg == False) begin
					bankLatch0[i] <= fromMaybe(?,bankReadPort[i].wget);
				end else if (uops[i].opl == Zero && uops[i].sreg == True) begin
					bankLatch0[i] <= replicateScalarOperands(sreg0[i]);
				end else begin
					bankLatch0[i] <= bankLatch0[i];
				end
				
				if (uops[i].opl == One && uops[i].sreg == False) begin
					bankLatch1[i] <= fromMaybe(?,bankReadPort[i].wget);
				end else if (uops[i].opl == One && uops[i].sreg == True) begin
					bankLatch1[i] <= replicateScalarOperands(sreg1[i]);
				end else begin
					bankLatch1[i] <= bankLatch1[i];
				end
				
				if (uops[i].opl == Two && uops[i].sreg == False) begin
					bankLatch2[i] <= fromMaybe(?,bankReadPort[i].wget);
				end else if (uops[i].opl == Two && uops[i].sreg == True) begin
					bankLatch2[i] <= replicateScalarOperands(sreg2[i]);
				end else begin
					bankLatch2[i] <= bankLatch2[i];
				end

				if (uops[i].pdl) begin
					predicateLatch[i] <= fromMaybe(?,predReadPort[i].wget);
				end else begin
					predicateLatch[i] <= predicateLatch[i];
				end

		endrule: update_latches

		rule enq_bankReadQ (isValid(uopsReg[i]) && uops[i].fop_brq);
			IntJ j = fromInteger(i);
			let enqVal	= fromMaybe(?,bankReadPort[i].wget);
			brq[i].enq(enqVal.data);
			$display("Enq to BRQ%d :", j, fshow(enqVal));
		endrule: enq_bankReadQ

		rule enq_predReadQ (isValid(uopsReg[i]) && uops[i].fop_bpq);
			IntJ j = fromInteger(i);
			let enqVal	= fromMaybe(?,predReadPort[i].wget);
			bpq[i].enq(enqVal);
			$display("Enq to BPQ%d :", j, fshow(enqVal));
		endrule: enq_predReadQ

		rule doALUOp (isValid(uopsReg[i]));
			
			let t_x	= bankLatch0[i].dataType;
			let t_y	= bankLatch1[i].dataType;
			BankReadType aluResult = unpack('0);
			aluResult.dataType	= unpack(max(pack(t_x),pack(t_y)));		// TODO verify this assertion
			
			for (Integer j=0; j<`N_bankALUs; j=j+1) begin
				Integer k	= (i*`N_bankALUs) + j;
				
				// since ALU64 is being used
				Bit#(64) x	= extractElement(bankLatch0[i], fromInteger(j));
				Bit#(64) y	= extractElement(bankLatch1[i], fromInteger(j));

				let funct3	= uops[i].fop_alu.funct3;
				let isOP32	= uops[i].fop_alu.isOP32;
				let altOp	= uops[i].fop_alu.doAlternateOperation;
				let isCmp	= uops[i].fop_alu.isVectorCompare;
				
				Bit#(64) z = alu[k].doALUOp(isCmp, funct3, x, y, pack(isOP32), pack(altOp));
				
				if (isCmp) begin
					aluResult.data = aluResult.data | (zeroExtend(z) << fromInteger(j));
					aluResult.dataType = Predicate;
				end else begin
					aluResult = insertElement(aluResult, z, fromInteger(j));
				end
				
			end

			aluLatch[i] <= aluResult;
		endrule: doALUOp
	
	end

	
	// defines a set of 'rules' that place bank operands on the common bus when the xbar uop is True
	// rules are added to this set as and when they are generated using 'rJoin' functions
	// this set will finally hold {do_xbar, do_xbar_1, do_xbar_2,..}
	Rules rules_do_xbar		= emptyRules; 
	Rules rules_vrf_write	= emptyRules; 
	Rules rules_pred_write	= emptyRules;

	// generates a rule to place operands on the common bus; 1 rule per bank
	// the generated rules are mutually exclusive since the scheduler will make sure that there is no structural hazard in using the bus
	for (Integer i=0; i<`B; i=i+1) begin 
		IntJ j = fromInteger(i);

		// define a rule 'r1' of type 'Rules' that holds 1 of the generated rules
		Rules r1 =
			rules
				// this is the actual rule
				rule do_xbar (isValid(uopsReg[i]) && isValid(uops[i].xbar)); 
					XBar t;

					// assuming FUs only operate on 1 element at a time
					t.op0 = truncateToBitwidth(bankLatch0[i].data,validValue(uops[i].xbar));
					t.op1 = truncateToBitwidth(bankLatch1[i].data,validValue(uops[i].xbar));
					t.op2 = truncateToBitwidth(bankLatch2[i].data,validValue(uops[i].xbar));

					// assuming predXBar is not independant of operandXBar
					t.pred = predicateLatch[i];
					
					toXBar.enq(t);
					$display("To XBar from Bank%d :", j, fshow(t));
				endrule: do_xbar
			endrules;

		/* NOTE	if the scheduler can ensure that the xbar uop is correctly generated & can 
		 		ensure no bus conflicts use rJoinMutuallyExclusive, else use rJoinDescendingUrgency  */
		rules_do_xbar	= rJoinMutuallyExclusive(r1,rules_do_xbar);
		// rules_do_xbar	= rJoinDescendingUrgency(r1,rules_do_xbar); 
		
		Rules r2 =
			rules
				rule vrf_write (isValid(uopsReg[i]) && isValid(uops[i].vrf_write));
					VRFWrite write_uop	= validValue(uops[i].vrf_write);
					Bit#(`BANK_WIDTH) writeData;
					case (write_uop.sourceSel)
						FromALU			:	begin
												let shiftBy	= (write_uop.byteOffset * 'd8);
												writeData	= (aluLatch[i].data << shiftBy);
											end
						FromWriteBuffer	:	begin
												writeData = writeBuffer.first;
												writeBuffer.deq;
											end
					endcase

					bank[i].upd(write_uop.addr,writeData,write_uop.pred);
					$display("Write data to Bank%d :", j, fshow(writeData));
				endrule: vrf_write
			endrules;				

		rules_vrf_write	= rJoinMutuallyExclusive(r2,rules_vrf_write);
		// rules_vrf_write	= rJoinDescendingUrgency(r2,rules_vrf_write); 
		
		Rules r3 =
			rules
				rule pred_write (isValid(uopsReg[i]) && isValid(uops[i].pred_write));
					PredWrite write_uop	= validValue(uops[i].pred_write);
					Bit#(`BANK_PRED_WIDTH) writeData = '0;
					case (write_uop.sourceSel)
						FromALU			:	begin
												// results of vector compare instructions from ALU
												let shiftBy	= write_uop.bitOffset;
												writeData	= truncate(aluLatch[i].data << shiftBy);
											end
						FromPLU			:	begin
												$display("// todo");
												// TODO
												// let shiftBy	= write_uop.bitOffset;
												// writeData	= (pluLatch[i].data << shiftBy);
											end
						FromWriteBuffer	:	begin
												writeData = predWriteBuffer.first;
												predWriteBuffer.deq;
											end
					endcase

					predBank[i].upd(write_uop.addr,writeData);
					$display("Write data to predBank%d :", j, fshow(writeData));
				endrule: pred_write
			endrules;				

		rules_vrf_write	= rJoinMutuallyExclusive(r3,rules_pred_write);
		// rules_vrf_write	= rJoinDescendingUrgency(r3,rules_pred_write); 
	end 

	// this function attaches/instantiates all the rules contained in the set
	addRules(rules_do_xbar); 
	addRules(rules_vrf_write); 
	addRules(rules_pred_write);

	
	interface Put putLaneSequencerPacket;
		method Action put(LaneSequencerPacket packetFromSeq);
			uops_new.wset(packetFromSeq.uops);
			sreg0_new <= packetFromSeq.sreg0;
			sreg1_new <= packetFromSeq.sreg1;
			sreg2_new <= packetFromSeq.sreg2;
		endmethod
	endinterface: putLaneSequencerPacket

	interface Client useFuncUnits;    
	    interface Get request	= toGet(toXBar);
    	interface Put response	= toPut(writeBuffer);
	endinterface: useFuncUnits

	interface Put writeToPRF	= toPut(predWriteBuffer);


	interface Server readBRQs;
		
		interface Put request;
			method Action put( Vector#( `B, Bool)  deqSignals);
				for (Integer i=0; i<`B; i=i+1) begin
					IntJ j = fromInteger(i);
					if (deqSignals[i] && brq[i].notEmpty) begin
						brq[i].deq;
						$display("Deq BRQ%d", j);
					end
				end
			endmethod
		endinterface: request
	
		interface Get response;
			method ActionValue#(Vector#( `B, Bit#(`BANK_WIDTH)) ) get();
				Vector#( `B, Bit#(`BANK_WIDTH)) brq_values;
				for (Integer i=0; i<`B; i=i+1) begin
					brq_values[i] = brq[i].first;
				end
				return brq_values;
			endmethod
		endinterface: response
	
	endinterface: readBRQs


	interface Server readBPQs;
		
		interface Put request;
			method Action put(Vector#( `B, Bool)  deqSignals);
				for (Integer i=0; i<`B; i=i+1) begin
					IntJ j = fromInteger(i);
					if (deqSignals[i] && bpq[i].notEmpty) begin
						bpq[i].deq;
						$display("Deq BPQ%d", j);
					end
				end
			endmethod
		endinterface: request
	
		interface Get response;
			method ActionValue#(Vector#( `B, Bit#(`BANK_PRED_WIDTH)) ) get();
				Vector#( `B, Bit#(`BANK_PRED_WIDTH)) bpq_values;
				for (Integer i=0; i<`B; i=i+1) begin
					bpq_values[i] = bpq[i].first;
				end
				return bpq_values;
			endmethod
		endinterface: response
	
	endinterface: readBPQs

endmodule: mkLane

endpackage: lane
