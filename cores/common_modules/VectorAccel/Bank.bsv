/*

Copyright (c) 2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name     : VRF banks
Author Name     : Vinod G, Sumanth Sridhar
e-mail Id       : g.vinod1993@gmail.com, sumanthsridhar.009@gmail.com
Last updated on : 5th November 2016
*/



package Bank;

import MiniBank::*;
import VectorAccelDefs::*;

`include "vxu_defs.bsv"

typedef `N_Entries_128b		N128;
typedef `N_Entries_64b		N64;
typedef `N_Entries_32b		N32;
typedef `N_Entries_16b		N16;
typedef `N_Entries_8b		N8;

interface VRFBankIfc;
	
	method Action
		upd (Bit#(TLog#(`BANK_DEPTH)) addr, Bit#(`BANK_WIDTH) data, Bit#(`BANK_PRED_WIDTH) mBankEn);
	
	method DataAndType#(Bit#(`BANK_WIDTH))
		sub (Bit#(TLog#(`BANK_DEPTH)) addr);

endinterface: VRFBankIfc

(* synthesize *)
module mkVRFBank (VRFBankIfc) 
	provisos (
		Mul#(`B,`L,bl),								// bn = B x L = total no of banks in the vector processor
		Mul#(`BANK_PRED_WIDTH,bl,`PRF_WIDTH),		// since each bank acts like a column of the total VRF
		Mul#(`w,`PRF_WIDTH,`VRF_WIDTH),				// w = min precision; dictates max pred bits required
		Mul#(`w,`BANK_PRED_WIDTH,`BANK_WIDTH),		// w dictates how many pred bits are assigned per bank

		Add#(`N_Entries_8b,`N_Entries_16b,a__),
		Add#(`N_Entries_32b,`N_Entries_64b,b__),
		Add#(`N_Entries_128b,a__,c__),
		Add#(b__,c__,`BANK_DEPTH)					// sum of entries of diff precision = total entries
	);

	Bit#( TAdd#( 1, TLog#(`BANK_DEPTH))) endAddr128			= `N_Entries_128b;
	Bit#( TAdd#( 1, TLog#(`BANK_DEPTH))) endAddr64			= `N_Entries_64b + endAddr128;
	Bit#( TAdd#( 1, TLog#(`BANK_DEPTH))) endAddr32			= `N_Entries_32b + endAddr64;
	Bit#( TAdd#( 1, TLog#(`BANK_DEPTH))) endAddr16			= `N_Entries_16b + endAddr32;
	Bit#( TAdd#( 1, TLog#(`BANK_DEPTH))) endAddr8			= `N_Entries_8b  + endAddr16;

	`ifdef MVP_128b
		Bool mvp_128b	= True;
		VRFBankSectionIfc#(128,`BANK_WIDTH, N128)	miniBank128b	<- mkVRFBankSection;
	`else
		Bool mvp_128b	= False;
		VRFBankSectionIfc#(128,`BANK_WIDTH, 1)		miniBank128b	<- mkEmptySection;
	`endif
	
	`ifdef MVP_64b
		Bool mvp_64b	= True;
		VRFBankSectionIfc#( 64,`BANK_WIDTH, N64 )	miniBank64b		<- mkVRFBankSection;
	`else
		Bool mvp_64b	= False;
		VRFBankSectionIfc#( 64,`BANK_WIDTH, 1)		miniBank64b		<- mkEmptySection;
	`endif
		
	`ifdef MVP_32b
		Bool mvp_32b	= True;
		VRFBankSectionIfc#( 32,`BANK_WIDTH, N32 )	miniBank32b		<- mkVRFBankSection;
	`else
		Bool mvp_32b	= False;
		VRFBankSectionIfc#( 32,`BANK_WIDTH, 1)		miniBank32b		<- mkEmptySection;
	`endif
		
	`ifdef MVP_16b
		Bool mvp_16b	= True;
		VRFBankSectionIfc#( 16,`BANK_WIDTH, N16 )	miniBank16b		<- mkVRFBankSection;
	`else
		Bool mvp_16b	= False;
		VRFBankSectionIfc#( 16,`BANK_WIDTH, 1)		miniBank16b		<- mkEmptySection;
	`endif
	
	`ifdef MVP_8b
		Bool mvp_8b		= True;
		VRFBankSectionIfc#(  8,`BANK_WIDTH, N8  )	miniBank8b		<- mkVRFBankSection;
	`else
		Bool mvp_8b		= False;
		VRFBankSectionIfc#(  8,`BANK_WIDTH, 1)		miniBank8b		<- mkEmptySection;
	`endif

	method Action upd (addr, data, mBankEn);
		
		if (zeroExtend(addr) < endAddr128 && mvp_128b) begin
			miniBank128b.upd(truncate(addr), data, truncate(mBankEn));
			$display("write to miniBank128b");
		end else if (zeroExtend(addr) < endAddr64 && mvp_64b) begin
			miniBank64b.upd(truncate(addr), data, truncate(mBankEn));
			$display("write to miniBank64b");
		end else if (zeroExtend(addr) < endAddr32 && mvp_32b) begin
			miniBank32b.upd(truncate(addr), data, truncate(mBankEn));
			$display("write to miniBank32b");
		end else if (zeroExtend(addr) < endAddr16 && mvp_16b) begin
			miniBank16b.upd(truncate(addr), data, truncate(mBankEn));
			$display("write to miniBank16b");
		end else begin // if (zeroExtend(addr) < endAddr8 && mvp_8b) begin
			miniBank8b.upd(truncate(addr), data, zeroExtend(mBankEn));
			$display("write to miniBank8b");
		end 
		
	endmethod: upd

	// NOTE to enable the "display" statements below, change method to ActionValue type
	method DataAndType#(Bit#(`BANK_WIDTH)) sub (Bit#(TLog#(`BANK_DEPTH)) addr);
		DataAndType#(Bit#(`BANK_WIDTH)) readVal;

		if (zeroExtend(addr) < endAddr128 && mvp_128b) begin
			readVal.data = miniBank128b.sub(truncate(addr));
			readVal.dataType = Q;
		end else if (zeroExtend(addr) < endAddr64 && mvp_64b) begin
			readVal.data = miniBank64b.sub(truncate(addr));
			readVal.dataType = D;
		end else if (zeroExtend(addr) < endAddr32 && mvp_32b) begin
			readVal.data = miniBank32b.sub(truncate(addr));
			readVal.dataType = W;
		end else if (zeroExtend(addr) < endAddr16 && mvp_16b) begin
			readVal.data = miniBank16b.sub(truncate(addr));
			readVal.dataType = H;
		end else begin // if (zeroExtend(addr) < endAddr8 && mvp_8b) begin
			readVal.data = miniBank8b.sub(truncate(addr));
			readVal.dataType = B;
		end 

		return readVal;
	endmethod: sub

endmodule: mkVRFBank


/////////////////////////////////////////// TESTBENCHES ///////////////////////////////////////////

module tb0 (Empty);
	
	VRFBankIfc bank <- mkVRFBank;

	Bit#(16) s_start	= 'd0;
	Bit#(16) s_end		= 'd64;
	Reg#(Bit#(16)) s	<- mkReg(s_start);

	Bit#(16) t_start	= 'd1;
	Bit#(16) t_end		= 'd2;
	Reg#(Bit#(16)) t	<- mkReg(t_start);
	
	rule disp ;
		$display("\n---- s = %d",s);
		// $display("---- s = %b %d",s,s);
	endrule: disp
	
	rule count_s ;
		if (s==s_end)
			s <= s_start;
		else
			s <= s+1;			
	endrule: count_s

	rule count_t (s==s_end);
		t <= t+1;
	endrule: count_t
	
	rule test0 (s>=s_start && s<s_end && t=='d0);
		Bit#(TLog#(`BANK_DEPTH)) addr = truncate(s);
		DataAndType#(Bit#(`BANK_WIDTH)) x = bank.sub(addr);
		$display("@%d : ",addr,fshow(x));
	endrule: test0

	rule test1 (s>=s_start && s<s_end && t=='d1);
		Bit#(16) x = {zeroExtend(s)};
		Bit#(128) xx = {x,x,x,x,x,x,x,x};
		Bit#(TLog#(`BANK_DEPTH)) addr = truncate(s);
		Bit#(`BANK_PRED_WIDTH) pred = truncate(s);
		bank.upd(addr,xx,pred);
		$display("write @%d [pred = %b]: %h",addr,pred,xx);
	endrule: test1 
	
	rule test2 (s>=s_start && s<s_end && t=='d1);
		Bit#(TLog#(`BANK_DEPTH)) addr = truncate(s-1);
		DataAndType#(Bit#(`BANK_WIDTH)) x = bank.sub(addr);
		$display("@%d : ",addr,fshow(x));
	endrule: test2
	
	rule done (t==t_end);
		$display("\n\n\n");
		$finish;
	endrule: done

endmodule: tb0

endpackage: Bank
