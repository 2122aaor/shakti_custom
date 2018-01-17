/*
Copyright (c) 2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name     : Re-order Buffer for the Scalar Unit
Author Name     : Sumanth Sridhar, Vinod G
e-mail Id       : sumanthsridhar.009@gmail.com, g.vinod1993@gmail.com
Last updated on : 30th June 2016
*/

package ROB;

import Vector::*;

typedef struct {
	Bool isStore;
	Bit#(reg_addr_width) reg_addr;
	Bit#(data_width) data;
} ROBEntry#(numeric type reg_addr_width, numeric type data_width) deriving (Eq, Bits, FShow);


interface ROBIfc#(numeric type buffer_size, numeric type reg_addr_width, numeric type data_width);
	
	method ActionValue#(Bit#(TAdd#(TLog#(buffer_size),1)))
		reserve (Bit#(reg_addr_width) regAddr, Bool isStore);
	
	method Action store_data (
		Bit#(TLog#(buffer_size))	robAddr, 
		Bit#(data_width)			data
		);
	
	method Bool isStore ();

	method ActionValue#(ROBEntry#(reg_addr_width, data_width)) 
		commit ();

	method Maybe#(Bit#(data_width)) 
		forward_data (Bit#(TLog#(buffer_size)) robAddr);

	method Action flushROB ();

	method Bit#(TAdd#(TLog#(buffer_size),1)) getHead ();
	method Bit#(TAdd#(TLog#(buffer_size),1)) getTail ();
	method Bool isHeadOverTail ();		// return head >= tail

endinterface: ROBIfc

module mkROB (ROBIfc#(buffer_size, reg_addr_width, data_width))
	provisos ( 
		Log#(buffer_size, n)
	);

	// 2 vectors used in parallel as addr and data fields of an ROB entry need to be written to independently in the same cycle
	Vector#(buffer_size, Reg#(Maybe#(Bit#(reg_addr_width))) )	robBuffer_addr		<- replicateM (mkReg(Invalid));
	Vector#(buffer_size, Reg#(Maybe#(Bit#(  data_width  ))) )	robBuffer_data		<- replicateM (mkReg(Invalid));
	
	// needed since stores must be issued to the mem hierarchy only in the commit stage; used to tag an entry as a store data operand
	Vector#(buffer_size, Reg#(          Bool              ) )	robBuffer_isStore	<- replicateM (mkReg(False));

	// correct implementation of circular buffer pointers require 1 bit more than no of addr bits
	// extra bit indicates if "head"/"tail" is ahead
	Reg#(Bit#(TAdd#(n,1))) head <- mkReg(0);			// write address/pointer
	Reg#(Bit#(TAdd#(n,1))) tail <- mkReg(0);			// read address/pointer

	// the actual write & read pointers devoid of the extra bit
	Bit#(n) head_lsB = head[valueOf(TSub#(n,1)):0];
	Bit#(n) tail_lsB = tail[valueOf(TSub#(n,1)):0];

	Bool notEmpty 	= (head != tail);
	Bool notFull	= !( (head_lsB == tail_lsB) && (head[valueOf(n)] != tail[valueOf(n)]) );


	rule disp ;
		$display("Head: %d, Tail: %d", head_lsB, tail_lsB);
		$display("isEmpty = %b, isFull = %b", !notEmpty, !notFull);
		$display("---------------------------------");
		$display("ROB Contents: {rd, data}");
		for (Integer i=0; i<valueOf(buffer_size); i=i+1)
			$display("%d = [isStore = ",i,fshow(robBuffer_isStore[i]), "] ", fshow(robBuffer_addr[i]),", ",fshow(robBuffer_data[i]));
		$display("---------------------------------");
	endrule: disp



	/* NOTE	All three methods (reserve, store_data and commit) are "conflict_free"
			Make sure to assert that the 3 rules that call these 3 methods are "conflict_free"
			If an "Error" occurs during simulation, review logic used
	*/

	// reserves an entry in the ROB. If buffer is full, the parent rule is not allowed to fire
	method ActionValue#(Bit#(TAdd#(TLog#(buffer_size),1))) reserve (Bit#(reg_addr_width) regAddr, Bool isStore ) if (notFull);
		head <= head + 'b1;
		
		robBuffer_addr		[head_lsB] <= Valid(regAddr);
		robBuffer_isStore	[head_lsB] <= isStore;
		
		return head;	// NOTE	returns full head pointer, including extra bit
						// 		if only the ROBEntry address is reqd, truncate msb
	endmethod: reserve
	

	// updates the data field of corresponding ROB entry after checking if the ROB line has been reserved
	method Action store_data (Bit#(TLog#(buffer_size)) robAddr, Bit#(data_width) data) ;
		if (isValid(robBuffer_addr[robAddr])) begin
			$display("store {robAddr,data}: ",fshow(robAddr),", ", fshow(data));
			robBuffer_data[robAddr] <= Valid(data);
		end
	endmethod: store_data

	// meant to be used before doing a commit to check if the next instruction to be committed is a STORE
	// if it is, then you'd have to make sure that the STORE can be issued to the mem unit before calling "commit"
	method Bool isStore ();
		return robBuffer_isStore[tail_lsB];
	endmethod: isStore

	// returns the oldest entry in the ROB if it is VALID and releases the buffer-line
	method ActionValue#(ROBEntry#(reg_addr_width, data_width)) commit () 
		if (isValid(robBuffer_data[tail_lsB]));
		
		tail <= tail + 'b1;
		robBuffer_addr[tail_lsB] 	<= Invalid;
		robBuffer_data[tail_lsB] 	<= Invalid;
		robBuffer_isStore[tail_lsB]	<= False;
		
		return ( ROBEntry {
			isStore		: robBuffer_isStore[tail_lsB],
			reg_addr	: validValue(robBuffer_addr[tail_lsB]),
			data		: validValue(robBuffer_data[tail_lsB])  }
		);
	endmethod: commit

	// enables data forwarding from ROB to earlier stages of the pipeline for OoO execution
	// "forward_data" has lower priority than "store_data"
	method Maybe#(Bit#(data_width)) forward_data (Bit#(TLog#(buffer_size)) robAddr);
		if (isValid(robBuffer_addr[robAddr]) && isValid(robBuffer_data[robAddr]))
			return Valid ( validValue(robBuffer_data[robAddr]) );
		else
			return Invalid;
	endmethod: forward_data

	// NOTE parent calling rule of this method must have higher priority than parent rules of all other methods
	method Action flushROB ();
		$display("!!! FLUSHED ROB !!!");
		for(Integer i=0;i<valueOf(buffer_size);i=i+1) begin
			robBuffer_addr[i] 		<= Invalid;
			robBuffer_data[i] 		<= Invalid;
			robBuffer_isStore[i]	<= False;
		end
		head <= '0;
		tail <= '0;
	endmethod: flushROB

	method Bit#(TAdd#(n,1)) getHead ();
		return head;
	endmethod: getHead

	method Bit#(TAdd#(n,1)) getTail ();
		return tail;
	endmethod: getTail

	method Bool isHeadOverTail ();
		return (head[valueOf(n)] >= tail[valueOf(n)]);
	endmethod: isHeadOverTail

endmodule: mkROB

endpackage: ROB
