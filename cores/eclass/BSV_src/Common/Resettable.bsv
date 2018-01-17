/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Names : Abhinaya Agrawal
Email ID : agrawal.abhinaya@gmail.com
*/
package Resettable;

import RegFile :: *;

interface ResetWrapper#(type idx_t, type data_t);
	method data_t _read(idx_t idx);
	method Action _write(idx_t idx, data_t data);
	method Action _reset;
	method ActionValue#(Bool) _reset_complete;
endinterface

typeclass Resettable#(type a_t, type b_t, type c_t)
		dependencies(a_t determines (b_t, c_t));
	module mkResetWrapper#(a_t a, c_t init, Bit#(b_t) lo, Bit#(b_t) hi)(ResetWrapper#(Bit#(b_t), c_t));
endtypeclass

/*Instance for RegFile*/
instance Resettable#(RegFile#(Bit#(idx_sz), data_t), idx_sz, data_t)
			provisos(Bits#(data_t, data_sz), Alias#(Bit#(idx_sz), idx_t));
	module mkResetWrapper#(RegFile#(idx_t, data_t) rf, data_t init, idx_t lo, idx_t hi)(ResetWrapper#(idx_t, data_t));
		let reset_val = init;
		Reg#(Bool) rg_reset_init <- mkReg(False);
		Reg#(idx_t) rg_count <- mkReg(extend(lo));
		Reg#(Bool) rg_reset_complete <- mkReg(False);

		rule rl_reset(rg_reset_init == True && rg_count < extend(hi));
			rf.upd(rg_count, reset_val);
			rg_count <= rg_count + 1;
			$display($time, " REGFILE: Reset started. lo: %d, hi: %d", lo, hi);
		endrule
	
		rule rl_signal_reset_completion(rg_reset_init == True && rg_count == extend(hi));
			rf.upd(rg_count, reset_val);
			rg_count <= extend(lo);
			rg_reset_init <= False;
			rg_reset_complete <= True;
			$display($time, " REGFILE: Reset Done");
		endrule

		method data_t _read(idx_t idx) if(rg_reset_init == False) = rf.sub(idx);
		method Action _write(idx_t idx, data_t data) if(rg_reset_init == False) = rf.upd(idx, data);
		method Action _reset if(rg_reset_init == False); // The explicit method condition prevents the rule calling reset from firing more than once
			rg_reset_init <= True;
		endmethod
		method ActionValue#(Bool) _reset_complete if(rg_reset_complete == True);
			rg_reset_complete <= False;
			return rg_reset_complete;
		endmethod
	endmodule
endinstance

endpackage
