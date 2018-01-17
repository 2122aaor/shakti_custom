/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

Author Names : Rahul Bodduna, N Sireesh.
Email ID : rahul.bodduna@gmail.com
*/

package fetch;

`include "defined_parameters.bsv"
import FIFO::*;
import riscv_types::*;
import ConfigReg::*;
import SpecialFIFOs::*;
import GetPut::*;

interface IfcPrf_fetch;

method ActionValue#(Bit#(`REG_WIDTH)) fetch_enq(Bpu_packet lv_incoming_bpu_packet_1, Bpu_packet lv_incoming_bpu_packet_2);
method Action pc_frm_decode(Maybe#(Bit#(`REG_WIDTH)) program_counter);
method Action squash_pc(Bit#(`REG_WIDTH) program_counter);
method Bit#(`REG_WIDTH) return_pc();
method Bit#(`REG_WIDTH) return_pc_2();

endinterface : IfcPrf_fetch

(*synthesize*)
module mkPrf_fetch(IfcPrf_fetch);

   	Reg#(Bit#(`REG_WIDTH)) rg_program_counter <- mkReg('h80000000);
   	Wire#(Maybe#(Bit#(`REG_WIDTH)))  wr_next_pc <- mkDWire(tagged Invalid);

   	rule display_pc;
		$display("pc %h", rg_program_counter);	
	endrule

	method ActionValue#(Bit#(`REG_WIDTH)) fetch_enq(Bpu_packet lv_incoming_bpu_packet_1, Bpu_packet lv_incoming_bpu_packet_2);

		if(rg_program_counter[2] == 1 ) begin
			rg_program_counter <= lv_incoming_bpu_packet_1.pc;

		end

		else if(lv_incoming_bpu_packet_1.predict_taken_or_not == Predict_taken) begin
			
			rg_program_counter <= lv_incoming_bpu_packet_1.pc;
				
		end

		else begin
			rg_program_counter <= lv_incoming_bpu_packet_2.pc;
			$display("next_pc is %d", lv_incoming_bpu_packet_2.pc);
		end
		return rg_program_counter;
	endmethod

    method Action pc_frm_decode(Maybe#(Bit#(`REG_WIDTH)) program_counter);
		if(program_counter matches tagged Valid .pc) begin
			rg_program_counter <= pc;
			$display("Changing the pc to %h", pc);
		end
    endmethod

    method Action squash_pc(Bit#(`REG_WIDTH) program_counter);
	rg_program_counter <= program_counter;
	$display("new_pc_written @squash %d", program_counter);
    endmethod

    method Bit#(`REG_WIDTH) return_pc();
	return rg_program_counter;
    endmethod

    method Bit#(`REG_WIDTH) return_pc_2();
	return rg_program_counter+4;
    endmethod

endmodule : mkPrf_fetch
endpackage  
