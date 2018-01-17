/*Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*/
package Tb_integer_divider_riscv;
import divider::*;
import RegFile::*;
import types::*;
`include "defined_parameters.bsv"

(*synthesize*)
module mkTb_for_integer_divider_riscv(Empty);

	Reg#(Bit#(5)) rg_counter <- mkReg(0);	//Counter to get values from each line in Regfile
	Reg#(Bit#(32)) rg_clock <-mkReg(0);     //Clock register

	Ifc_divider divider <- mkdivider;


	Reg#(Bit#(32)) rg_instruction <-mkReg(32'b00011_00010_00001_0000001_111_0110011); //register to hold the 32 bit instruction

	rule rl_increment;
		
		rg_clock<=rg_clock+1;
		//$display("CLOCK=%d",rg_clock);

		if(rg_clock=='d70)
			$finish(0);
	endrule:rl_increment
   
	rule rl_give_input(rg_clock>'d4);
		
		ALU_func operation = REM;

		divider.input_operands('h7ae2378bc2f6dc23,'h0000000bc2f6dc23,operation,0,'d24);
		//		(_dividend, _divisor ,_Two_bits_for_DIVtype,_word_instruction_flag)
				//Set word_instruction_flag to 0 for non rv64 instructions

		rg_counter <= rg_counter +1; //rg_counter increment to read next line

	endrule:rl_give_input

	rule rl_get_output(divider._release == 1);

		
		$display("CLOCK=%d",rg_clock);
		$display("The output is %h", divider.result_); 
		$display("The destination address is %h", divider.destination_address_); 
		
		$finish(0);
		//divider._release();  //release the FIFO's after getting the output
	endrule
endmodule : mkTb_for_integer_divider_riscv
endpackage: Tb_integer_divider_riscv
