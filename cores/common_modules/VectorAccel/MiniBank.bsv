/*

Copyright (c) 2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name     : VRF Mini banks
Author Name     : Vinod G, Sumanth Sridhar
e-mail Id       : g.vinod1993@gmail.com, sumanthsridhar.009@gmail.com
Last updated on : 5th November 2016
*/

/*	
	Each line of a bank is physically partitioned into 'mBanks'
	mBanks/mini-Banks are smaller partitions of different, parallel, regions of the mixed-precision enabled VRF
	
	Why mBanks?
		> Depending on config, a single line of a Bank can hold multiple elements
		> Using the RegFile#() module, we can only write to a whole line of the Bank at a time, not individual elements
		  This is essential as writes to certain elements may be masked by predicate bits
		> mBanks allow independent write access to each element of the VRF
		> Simlutaneous writes into all elements of a line in a Bank is possible as all mBanks are connected in parallel
*/

package MiniBank;
export MiniBank::*;

import RegFile::*;
import Vector::*;

interface VRFBankSectionIfc#(numeric type dataWidth, numeric type bankWidth, numeric type bankDepth);
	
	method Action
		upd (Bit#(TLog#(bankDepth)) addr, Bit#(bankWidth) data, Bit#(TDiv#(bankWidth,dataWidth)) mBankEn);
	
	method Bit#(bankWidth) 
		sub (Bit#(TLog#(bankDepth)) addr);

endinterface: VRFBankSectionIfc

module mkVRFBankSection (VRFBankSectionIfc#(dataWidth, bankWidth, bankDepth)) 
	provisos (
		Max#(bankWidth,dataWidth,bankWidth),		// bankWidth >= dataWidth
		Div#(bankWidth,dataWidth,n_mBanks),
		Mul#(n_mBanks,dataWidth,bankWidth),			// bankWidth should be a multiple of dataWidth

		// compiler requested provisos
		Add#(1, a__, n_mBanks),
		Add#(dataWidth, b__, bankWidth)
	);

	Vector#( n_mBanks, RegFile#( Bit#(TLog#(bankDepth)), Bit#(dataWidth) )) 
		mBank			<- replicateM(mkRegFileFull);

	method Action upd (addr, data, mBankEn);
		for (Integer i=0; i<valueOf(n_mBanks); i=i+1) begin
			
			// extract relevant part of 'data' for each bank
			Bit#(dataWidth) d = data[ ((i+1)*valueOf(dataWidth))-1 : i*valueOf(dataWidth)];
			if (mBankEn[i]==1'b1) begin
				mBank[i].upd(addr, d);
			end
		end
	endmethod: upd

	method Bit#(bankWidth) sub (Bit#(TLog#(bankDepth)) addr);
		Vector#(n_mBanks, Bit#(bankWidth)) d;
		d[0] = {mBank[0].sub(addr), '0};
		
		// concatenate outputs from each mBank
		for (Integer i=1; i<valueOf(n_mBanks); i=i+1) begin
			d[i] = (d[i-1] >> valueOf(dataWidth)) | {mBank[i].sub(addr), '0};
		end
		
		return last(d);
	endmethod: sub

endmodule: mkVRFBankSection


module mkEmptySection (VRFBankSectionIfc#(dataWidth, bankWidth, bankDepth));

	method Action upd (addr, data, mBankEn);
		noAction ;
	endmethod: upd
	
	method Bit#(bankWidth) sub (Bit#(TLog#(bankDepth)) addr);
		return '0;
	endmethod: sub

endmodule: mkEmptySection


/////////////////////////////////////////// TESTBENCHES ///////////////////////////////////////////

module tb0 (Empty);

	VRFBankSectionIfc#(8,16,4)	bank	<- mkVRFBankSection;
	// VRFBankSectionIfc#(8,16,4)	bank	<- mkEmptySection;

	Bit#(10) s_start	= 'd0;
	Bit#(10) s_end		= 'd10;
	Reg#(Bit#(10)) s	<- mkReg(s_start);
	
	rule disp ;
		$display("\n\n\n***********************************  s = %d  ***********************************\n",s);
		$display("@0 :	%h",bank.sub('d0));
		$display("@1 :	%h",bank.sub('d1));
		$display("@2 :	%h",bank.sub('d2));
		$display("@3 :	%h",bank.sub('d3));
	endrule: disp
	
	rule count_s ;
		s <= s+1;
	endrule: count_s
	
	rule test0 (s>=(s_start+'d5) && s<s_end);
		Bit#(2)  x1	= truncate(s);
		Bit#(16) x2	= zeroExtend(s);
		Bit#(2)  x3	= truncate(s);
		bank.upd(x1, x2, x3);
		// bank.upd(truncate(s), truncate(s), truncate(s));
		$display("write %h @%h with pred = %b",x2,x1,x3);
	endrule: test0
	
	rule done (s==s_end);
		$display("\n\n\n");
		$finish;
	endrule: done

endmodule: tb0

endpackage: MiniBank
