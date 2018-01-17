/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Name : Neel Gala
Email ID : neelgala@gmail.com

Description : 

This module executes the memory operations. For the Load and Store operation we basically calculate the effective address.
Also find out the size of the data transfer required. The decoding for each ATOMIC operation is also done here.

Whether to signextend the loaded value or not is also figured out in this module.

*/
package  memory;

import defined_types::*;
`include "defined_parameters.bsv"

	(*noinline*)
	function Memory_output fn_memory (Bit#(32) _instruction, Bit#(`Reg_width) _operand1, Bit#(`Reg_width) _operand2);

        Bit#(12) immediate_value = 0;
		Bit#(2) lv_word_size =_instruction[13:12]; // indicates whether byte/halfword/word or double word needs to loaded/written from/to memory
		Bit#(1) lv_signextend =_instruction[14];
		Memory_type mem_type=LOAD;
		Bit#(32) lv_effective_addresss=0;
		Atomic_funct funct=SWAP;
		if(_instruction[6:2]=='b01000) begin
			mem_type=STORE;
            immediate_value = {_instruction[31:25],_instruction[11:7]};
			lv_effective_addresss=(_operand1+signExtend(immediate_value));
		end
/*		else if(_instruction[6:2]=='b01011)begin // Atomic operation
			mem_type=ATOMIC;
			lv_effective_addresss=_operand1;
			if(_instruction[31:27]=='b00001 )begin// AMOSWAP
				funct=SWAP;
			end
			else if(_instruction[31:27]=='b00000)begin// AMOADD
				funct=ADD;
			end
			else if(_instruction[31:27]=='b00100)begin// AMOXOR
				funct=XOR;
			end
			else if(_instruction[31:27]=='b01100)begin// AMOAND
				funct=AND;
			end
			else if(_instruction[31:27]=='b01000)begin// AMOOR
				funct=OR;
			end
			else if(_instruction[31:27]=='b11000)begin// AMOMINU
				funct=MINU;
			end
			else if(_instruction[31:27]=='b11100)begin// AMOMAXU
				funct=MAXU;
			end
			else if(_instruction[31:27]=='b10000)begin// AMOMIN
				funct=MIN;
			end
			else if(_instruction[31:27]=='b10100)begin// AMOMAX
				funct=MAX;
			end

		end
*/		else begin
			mem_type=LOAD;
            immediate_value = _instruction[31:20];
		 	lv_effective_addresss=(_operand1+signExtend(immediate_value));
		end

		let lv_data_out = _operand2;
		let lv_destination_address = _instruction[11:7];
		
		return Memory_output{destination:lv_destination_address,
							memory_address:lv_effective_addresss,
							memory_data:lv_data_out,
							word_size:lv_word_size,
							signextend:lv_signextend,
							amofunct:funct,
							mem_type:mem_type};
	endfunction
endpackage 
