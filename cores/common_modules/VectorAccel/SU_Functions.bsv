/*
Copyright (c) 2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name     : Shared Register File
Author Name     : Sumanth Sridhar, Vinod G
e-mail Id       : sumanthsridhar.009@gmail.com, g.vinod1993@gmail.com
Last updated on : 30th June 2016
*/

package SU_Functions;

import RegFile::*;
import Vector::*;
import FIFOF::*;

import VectorAccelDefs::*;

`include "defined_parameters.bsv"

interface SharedRegFile;
	method Action 		upd_data	(Bit#(`VS_AddrWidth) addr, Bit#(64) d);
	
	`ifdef USE_ROB
		method Action 			upd_robAddr	(Bit#(`VS_AddrWidth) addr, Bit#(TLog#(`ROB_BUFFER_SIZE)) robAddr);
		method AddrOrData		sub1		(Bit#(`VS_AddrWidth) addr);
		method AddrOrData		sub2		(Bit#(`VS_AddrWidth) addr);
		method AddrOrData		sub3		(Bit#(`VS_AddrWidth) addr);
	`else
		method Action			set_SB		(Bit#(`VS_AddrWidth) addr);
		method Maybe#(Bit#(64))	sub1		(Bit#(`VS_AddrWidth) addr);
		method Maybe#(Bit#(64))	sub2		(Bit#(`VS_AddrWidth) addr);
		method Maybe#(Bit#(64))	sub3		(Bit#(`VS_AddrWidth) addr);
	`endif

endinterface: SharedRegFile


`ifdef USE_ROB
	
	(* synthesize *)
	module mkSharedRegFileTagged (SharedRegFile);

		RegFile#( Bit#(`VS_AddrWidth), Maybe#(Bit#(64)) ) 
			sharedregfile_data <- mkRegFileFull;
		
		RegFile#( Bit#(`VS_AddrWidth), Bit#(TLog#(`ROB_BUFFER_SIZE))) 
			sharedregfile_robAddr <- mkRegFileFull;

		// NOTE upd_data and upd_robAddr can occur in the same cycle but upd_robAddr must shadow upd_data
		method Action upd_data (Bit#(`VS_AddrWidth) addr, Bit#(64) d);
			// if (addr!=6'b0)
				sharedregfile_data.upd	(addr, Valid(d));
		endmethod

		// when a reg has a valid robAddr, this means that the data in the regfile is invalid as the correct data value is in the ROB
		method Action upd_robAddr (Bit#(`VS_AddrWidth) addr, Bit#(TLog#(`ROB_BUFFER_SIZE)) robAddr);
			// if (addr!=6'b0)
				sharedregfile_data.upd		(addr, Invalid);
				sharedregfile_robAddr.upd	(addr, robAddr);
		endmethod

		method AddrOrData sub1 (Bit#(`VS_AddrWidth) addr);
			AddrOrData readVal;
			if (addr==0)
				readVal = tagged Data 'b0;
			else begin
				if (isValid(sharedregfile_data.sub(addr))) begin
					readVal = tagged Data validValue(sharedregfile_data.sub(addr));
				end else begin
					readVal = tagged RobAddr sharedregfile_robAddr.sub(addr);
				end
			end

			return readVal;
		endmethod

		method AddrOrData sub2 (Bit#(`VS_AddrWidth) addr);
			AddrOrData readVal;
			if (addr==0)
				readVal = tagged Data 'b0;
			else begin
				if (isValid(sharedregfile_data.sub(addr))) begin
					readVal = tagged Data validValue(sharedregfile_data.sub(addr));
				end else begin
					readVal = tagged RobAddr sharedregfile_robAddr.sub(addr);
				end
			end

			return readVal;
		endmethod

		method AddrOrData sub3 (Bit#(`VS_AddrWidth) addr);
			AddrOrData readVal;
			if (addr==0)
				readVal = tagged Data 'b0;
			else begin
				if (isValid(sharedregfile_data.sub(addr))) begin
					readVal = tagged Data validValue(sharedregfile_data.sub(addr));
				end else begin
					readVal = tagged RobAddr sharedregfile_robAddr.sub(addr);
				end
			end

			return readVal;
		endmethod

	endmodule: mkSharedRegFileTagged

`else

	(* synthesize *)
	module mkSharedRegFile (SharedRegFile);

		RegFile#( Bit#(`VS_AddrWidth), Maybe#(Bit#(64)) ) 
			sharedregfile_data <- mkRegFileFull;

		method Action upd_data (Bit#(`VS_AddrWidth) addr, Bit#(64) d);
			// if (addr!=6'b0)
				sharedregfile_data.upd	(addr, Valid (d));
		endmethod

		// set ScoreBoard bit to indicate that the register value is invalid 
		// until the correct value is returned from whichever functional unit
		method Action set_SB (Bit#(`VS_AddrWidth) addr);
			sharedregfile_data.upd		(addr, Invalid);
		endmethod

		/*	the type 'AddrOrData' is used to maintain compatability with mkSharedRegFileTagged
			but in this module, it returns only 'tagged Data' as 'tagged robAddr' has no significance	*/
		method Maybe#(Bit#(64)) sub1 (Bit#(`VS_AddrWidth) addr);
			Maybe#(Bit#(64)) readVal;
			if (addr==0)
				readVal = Valid ('b0);
			else begin
				readVal = sharedregfile_data.sub(addr);
			end

			return readVal;
		endmethod

		method Maybe#(Bit#(64)) sub2 (Bit#(`VS_AddrWidth) addr);
			Maybe#(Bit#(64)) readVal;
			if (addr==0)
				readVal = Valid ('b0);
			else begin
				readVal = sharedregfile_data.sub(addr);
			end

			return readVal;
		endmethod

		method Maybe#(Bit#(64)) sub3 (Bit#(`VS_AddrWidth) addr);
			Maybe#(Bit#(64)) readVal;
			if (addr==0)
				readVal = Valid ('b0);
			else begin
				readVal = sharedregfile_data.sub(addr);
			end

			return readVal;
		endmethod

	endmodule: mkSharedRegFile

`endif

// copied from $BLUESPECDIR/BSVSource/Misc/SpecialFIFOs.bsv
module mkPipelineFIFOF_UGDeq (FIFOF#(a))
   provisos (Bits#(a,sa));

   // STATE ----------------

   Reg#(Maybe#(a)) rv[3] <- mkCReg(3, tagged Invalid);

   // INTERFACE ----------------

   Bool enq_ok = ! isValid(rv[1]);
   Bool deq_ok = isValid(rv[0]);

   method notFull = enq_ok;

   method Action enq(v) if (enq_ok);
      rv[1] <= tagged Valid v;
   endmethod

   method notEmpty = deq_ok;

   method Action deq() ;
      rv[0] <= tagged Invalid;
   endmethod

   method first() ;
      return fromMaybe(?,rv[0]);
   endmethod

   method Action clear();
      rv[2] <= tagged Invalid;
   endmethod

endmodule

// copied from $BLUESPECDIR/BSVSource/Misc/SpecialFIFOs.bsv
module mkPipelineFIFOF_UGEnq (FIFOF#(a))
   provisos (Bits#(a,sa));

   // STATE ----------------

   Reg#(Maybe#(a)) rv[3] <- mkCReg(3, tagged Invalid);

   // INTERFACE ----------------

   Bool enq_ok = ! isValid(rv[1]);
   Bool deq_ok = isValid(rv[0]);

   method notFull = enq_ok;

   method Action enq(v) ;
      rv[1] <= tagged Valid v;
   endmethod

   method notEmpty = deq_ok;

   method Action deq() if (deq_ok);
      rv[0] <= tagged Invalid;
   endmethod

   method first() if (deq_ok);
      return fromMaybe(?,rv[0]);
   endmethod

   method Action clear();
      rv[2] <= tagged Invalid;
   endmethod

endmodule

// typedef Bit#(TAdd#(TLog#(`ROB_BUFFER_SIZE),1))     RdToken;
`ifndef Scalar_WriteBack_Priority
(*noinline*)
function FU_Code arbitration_oldestFirst (Bool iwOvfl, RdToken fu0, RdToken fu1, 
	RdToken fu2, RdToken fu3, RdToken fu4) provisos (Log#(`ROB_BUFFER_SIZE,__n));
	
	let n	= valueOf(__n);

	let msb0	= (fu0[n] == 'b1);
	let msb1	= (fu1[n] == 'b1);
	let msb2	= (fu2[n] == 'b1);
	let msb3	= (fu3[n] == 'b1);
	let msb4	= (fu4[n] == 'b1);

	// if all msb are 0, or all are 1, then oldest token is min of all values
	// if some are zero, and some are 1, then oldest instruction is min of all values with msb 1
	Bool msbEq	= ((msb0 && msb1 && msb2 && msb3 && msb4) || (!(msb0 || msb1 || msb2 || msb3 || msb4)));

	RdToken a	= '0;
	RdToken b	= '0;
	RdToken c	= '0;
	RdToken d	= '0;
	RdToken e	= '0;
	
	// Bool iwOvfl indicates if issue window has overflown the no of bits assigned
	// so, 			newest instr token < oldest instr token < `ROB_BUFFER_SIZE
	// normally, 	oldest instr token < newest instr token < `ROB_BUFFER_SIZE
	if (msbEq || !iwOvfl) begin
		a	= fu0;
		b	= fu1;
		c	= fu2;
		d	= fu3;
		e	= fu4;
	end else begin
		a	= msb0? fu0 : '1;
		b	= msb1? fu1 : '1;
		c	= msb2? fu2 : '1;
		d	= msb3? fu3 : '1;
		e	= msb4? fu4 : '1;
	end

	let x	= ( min (a, min(min(b,c), min(d,e)) ) );
	
	if (x==a)
		return Exec;
	else if (x==b)
		return MulDiv;
	else if (x==c)
		return Mem;
	else if (x==d)
		return FPop;
	else if (x==e)
		return VecRedn;
	else
		return NONE;

endfunction: arbitration_oldestFirst
`endif

// TODO proper research and assignment of priorities to be done; currently arbitrary priority used
function FU_Code arbitration_priority (Bool fu0, Bool fu1, Bool fu2, Bool fu3, Bool fu4 );
	
	if (fu2)
		return Mem;
	else if (fu4)
		return VecRedn;
	else if (fu1)
		return MulDiv;
	else if (fu0)
		return Exec;
	else if (fu3)
		return FPop;
	else
		return NONE;
	

endfunction: arbitration_priority

function Maybe#(Bit#(64)) fwd_data 
	(Maybe#(RdToken) srcAddr, Vector#(`FWD_FROM_TOTAL, Maybe#(RdToken_Data)) forwardingPaths);

	`ifdef USE_ROB	
		/*	If oldest-first writeback arbitration is used, an extra bit will 
			be present in the token field which is used for arbitration
			But	in order to check addresses, the extra bit must be excluded; 
			hence all RdToken-type fields are truncated		*/
		Vector#(`FWD_FROM_TOTAL, Maybe#(Bit#(TLog#(`ROB_BUFFER_SIZE)))) 	addr;
		Maybe#(Bit#(TLog#(`ROB_BUFFER_SIZE))) srcAddr_2 = isValid(srcAddr)? Valid(truncate(pack(srcAddr))) : Invalid;
	`else
		Vector#(`FWD_FROM_TOTAL, Maybe#(Bit#(`VS_AddrWidth))) 				addr;
		Maybe#(RdToken)	srcAddr_2 = srcAddr;
	`endif

	for(Integer i=0;i<`FWD_FROM_TOTAL;i=i+1) begin
		let x	= validValue(forwardingPaths[i]).rdToken;
		addr[i] = isValid(forwardingPaths[i])? Valid(truncate(pack(x))) : Invalid;
	end

	let indx	= isValid(srcAddr_2)? findElem(srcAddr_2, addr) : Invalid;

	if (isValid(indx) && isValid(forwardingPaths[validValue(indx)]) )
		return Valid (validValue(forwardingPaths[validValue(indx)]).data);
	else
		return Invalid;
	
endfunction: fwd_data

endpackage: SU_Functions
