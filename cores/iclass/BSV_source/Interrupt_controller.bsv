/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Names : Rahul Bodduna
Email ID : rahul.bodduna@gmail.com
*/

package  Interrupt_controller;


import riscv_types:: *;
import FIFO:: *;
`include "defined_parameters.bsv"
import SpecialFIFOs:: *;
import ConfigReg:: *;

interface IfcInterrupt_controller;

method Bool if_commit_stall;
method Bit#(`REG_WIDTH) return_priv_jump_address();
method Action take_exception(Bit#(`REG_WIDTH) exception_code);
method Action take_address_exception(Bit#(`REG_WIDTH) address);
method ActionValue#(Maybe#(Bit#(`REG_WIDTH))) update_register_1(MACHINE_op inst_type, Bit#(`REG_WIDTH) csr_addr, Bool if_csr_valid, Bit#(`REG_WIDTH) value); 
method ActionValue#(Maybe#(Bit#(`REG_WIDTH))) update_register_2(MACHINE_op inst_type, Bit#(`REG_WIDTH) csr_addr, Bool if_csr_valid, Bit#(`REG_WIDTH) value); 

endinterface


(*conflict_free = "update_register_1, update_register_2"*)
(*synthesize*)
module mkInterrupt_controller(IfcInterrupt_controller);

// Machine information Registers
Reg#(Bit#(`REG_WIDTH)) rg_mcpuid <- mkConfigReg(0);
Reg#(Bit#(`REG_WIDTH)) rg_mimpid <- mkConfigReg(0);
Reg#(Bit#(`REG_WIDTH)) rg_mhartid <- mkConfigReg(0);

// machine Trap Setup
Reg#(Bit#(`REG_WIDTH)) rg_mstatus <- mkConfigReg({58'b0,2'b11,1'b1,2'b11,1'b1});
Reg#(Bit#(`REG_WIDTH)) rg_mtvec <- mkConfigReg(64'hfffffffffffffe00);
Reg#(Bit#(`REG_WIDTH)) rg_mtdeleg <- mkConfigReg(0);
Reg#(Bit#(`REG_WIDTH)) rg_mie <- mkConfigReg({56'b0,1'b1,3'b0,1'b1,3'b0});
Reg#(Bit#(`REG_WIDTH)) rg_mtimecmp <- mkConfigReg(64'hffffffffffffffff);

//Machine Timers and Counters
Reg#(Bit#(`REG_WIDTH)) rg_mtime <- mkConfigReg(0); 
Reg#(Bit#(`REG_WIDTH)) rg_mtimeh <- mkConfigReg(0);

//Machine Trap Handling
Reg#(Bit#(`REG_WIDTH)) rg_mscratch <- mkConfigReg(0);         //hold pointer to machine mode hart-local context space and swapped with a user register.
Reg#(Bit#(`REG_WIDTH)) rg_mepc <- mkConfigReg(0);
Reg#(Bit#(`REG_WIDTH)) rg_mcause <- mkConfigReg(0);
Reg#(Bit#(`REG_WIDTH)) rg_mbadaddr <- mkConfigReg(0);
Reg#(Bit#(`REG_WIDTH)) rg_mip <- mkConfigReg(0);

//Machine Protection and Translation
Reg#(Bit#(`REG_WIDTH)) rg_mbase <-   mkConfigReg(0);
Reg#(Bit#(`REG_WIDTH)) rg_mbound <-  mkConfigReg(0);
Reg#(Bit#(`REG_WIDTH)) rg_mibase <-  mkConfigReg(0);
Reg#(Bit#(`REG_WIDTH)) rg_mibound <- mkConfigReg(0);
Reg#(Bit#(`REG_WIDTH)) rg_mdbase <-  mkConfigReg(0);
Reg#(Bit#(`REG_WIDTH)) rg_mdbound <- mkConfigReg(0);

Reg#(Bool) rg_stall_commit[2] <- mkCReg(2,False);

rule rl_time_compare;
	rg_mtime <= rg_mtime + 1;
	if(rg_mtime == rg_mtimecmp)
		rg_stall_commit[0] <= True;	
		//raise exception for timer
endrule

method Bool if_commit_stall;
	return rg_stall_commit[1];
endmethod

method Bit#(`REG_WIDTH) return_priv_jump_address();
	return (rg_mtvec + 'h40*`Machine_mode);
endmethod

method Action take_exception(Bit#(`REG_WIDTH) exception_code);
	rg_mcause <=  exception_code;
endmethod

method Action take_address_exception(Bit#(`REG_WIDTH) address);
	rg_mbadaddr <= address;
endmethod

method ActionValue#(Maybe#(Bit#(`REG_WIDTH))) update_register_1(MACHINE_op inst_type, Bit#(`REG_WIDTH) csr_addr, Bool if_csr_valid, Bit#(`REG_WIDTH) value); 

let lv_value = ~value;
Maybe#(Bit#(`REG_WIDTH)) lv_result = tagged Invalid;
if(if_csr_valid) begin

	case(csr_addr[11:8])
		4'b0011 : begin	
					case(csr_addr[7:4])
					4'b0000: begin 
								case(csr_addr[3:0])
									4'b0000: begin
										case(inst_type) matches
											CSRRW : rg_mstatus <= value; 
											CSRRS : rg_mstatus <= rg_mstatus | value; 
											CSRRC : rg_mstatus <= rg_mstatus & lv_value; 
										endcase
										lv_result = tagged Valid rg_mstatus;
									end 
									4'b0001: begin
										case(inst_type) matches
											CSRRW : rg_mtvec <= value; 
											CSRRS : rg_mtvec <= rg_mtvec | value; 
											CSRRC : rg_mtvec <= rg_mtvec & lv_value; 
										endcase
										lv_result = tagged Valid rg_mtvec;
									end
									4'b0010: begin
										case(inst_type) matches
											CSRRW : rg_mtdeleg <= value; 
											CSRRS : rg_mtdeleg <= rg_mtdeleg | value; 
											CSRRC : rg_mtdeleg <= rg_mtdeleg & lv_value;
										endcase
										lv_result = tagged Valid rg_mtdeleg;
									end
									4'b0100: begin
										case(inst_type) matches
											CSRRW : rg_mie <= value; 
											CSRRS : rg_mie <= rg_mie | value; 
											CSRRC : rg_mie <= rg_mie & lv_value; 
										endcase
										lv_result = tagged Valid rg_mie;
									end
								endcase
								end
					4'b0010: begin
								case(inst_type) matches
									CSRRW : rg_mtimecmp <= value; 
									CSRRS : rg_mtimecmp <= rg_mtimecmp | value; 
									CSRRC : rg_mtimecmp <= rg_mtimecmp & lv_value; 
								endcase
								lv_result = tagged Valid rg_mtimecmp;
								end
					4'b0100: begin
								case(csr_addr[3:0])
									4'b0000: begin
										case(inst_type) matches
											CSRRW : rg_mscratch <= value; 
											CSRRS : rg_mscratch <= rg_mscratch | value; 
											CSRRC : rg_mscratch <= rg_mscratch & lv_value; 
										endcase
										lv_result = tagged Valid rg_mscratch;
									end 
									4'b0001: begin
										case(inst_type) matches
											CSRRW : rg_mepc <= value; 
											CSRRS : rg_mepc <= rg_mepc | value; 
											CSRRC : rg_mepc <= rg_mepc & lv_value; 
										endcase
										lv_result = tagged Valid rg_mepc;
									end
									4'b0100: begin
										case(inst_type) matches
											CSRRW : rg_mip <= value; 
											CSRRS : rg_mip <= rg_mip | value; 
											CSRRC : rg_mip <= rg_mip & lv_value; 
										endcase
										lv_result = tagged Valid rg_mip;
									end
								endcase
								end
					4'b1000: begin
								case(csr_addr[3:0])
									4'b0000: begin
										case(inst_type) matches
											CSRRW : rg_mbase <= value; 
											CSRRS : rg_mbase <= rg_mbase | value; 
											CSRRC : rg_mbase <= rg_mbase & lv_value; 
										endcase
										lv_result = tagged Valid rg_mbase;
									end 
									4'b0001: begin
										case(inst_type) matches
											CSRRW : rg_mbound <= value; 
											CSRRS : rg_mbound <= rg_mbound | value; 
											CSRRC : rg_mbound <= rg_mbound & lv_value; 
										endcase
										lv_result = tagged Valid rg_mbound;
									end
									4'b0010: begin
										case(inst_type) matches
											CSRRW : rg_mibase <= value; 
											CSRRS : rg_mibase <= rg_mibase | value; 
											CSRRC : rg_mibase <= rg_mibase & lv_value; 
										endcase
										lv_result = tagged Valid rg_mibase;
									end
									4'b0011: begin
										case(inst_type) matches
											CSRRW : rg_mibound <= value; 
											CSRRS : rg_mibound <= rg_mibound | value; 
											CSRRC : rg_mibound <= rg_mibound & lv_value; 
										endcase
										lv_result = tagged Valid rg_mibound;
									end
									4'b0100: begin
										case(inst_type) matches
											CSRRW : rg_mdbase <= value; 
											CSRRS : rg_mdbase <= rg_mdbase | value; 
											CSRRC : rg_mdbase <= rg_mdbase & lv_value; 
										endcase
										lv_result = tagged Valid rg_mdbase;
									end
									4'b0101: begin
										case(inst_type) matches
											CSRRW : rg_mdbound <= value; 
											CSRRS : rg_mdbound <= rg_mdbound | value; 
											CSRRC : rg_mdbound <= rg_mdbound & lv_value; 
										endcase
										lv_result = tagged Valid rg_mdbound;
									end
								endcase
								end
					endcase
					end
	endcase
end

return lv_result;
endmethod

method ActionValue#(Maybe#(Bit#(`REG_WIDTH))) update_register_2(MACHINE_op inst_type, Bit#(`REG_WIDTH) csr_addr, Bool if_csr_valid, Bit#(`REG_WIDTH) value); 

let lv_value = ~value;
Maybe#(Bit#(`REG_WIDTH)) lv_result = tagged Invalid;
if(if_csr_valid) begin
	case(csr_addr[11:8])
	
		4'b0011 : begin	
					case(csr_addr[7:4])
					4'b0000: begin 
								case(csr_addr[3:0])
									4'b0000: begin
										case(inst_type) matches
											CSRRW : rg_mstatus <= value; 
											CSRRS : rg_mstatus <= rg_mstatus | value; 
											CSRRC : rg_mstatus <= rg_mstatus & lv_value; 
										endcase
										lv_result = tagged Valid rg_mstatus;
									end 
									4'b0001: begin
										case(inst_type) matches
											CSRRW : rg_mtvec <= value; 
											CSRRS : rg_mtvec <= rg_mtvec | value; 
											CSRRC : rg_mtvec <= rg_mtvec & lv_value; 
										endcase
										lv_result = tagged Valid rg_mtvec;
									end
									4'b0010: begin
										case(inst_type) matches
											CSRRW : rg_mtdeleg <= value; 
											CSRRS : rg_mtdeleg <= rg_mtdeleg | value; 
											CSRRC : rg_mtdeleg <= rg_mtdeleg & lv_value; 
										endcase
										lv_result = tagged Valid rg_mtdeleg;
									end
									4'b0100: begin
										case(inst_type) matches
											CSRRW : rg_mie <= value; 
											CSRRS : rg_mie <= rg_mie | value; 
											CSRRC : rg_mie <= rg_mie & lv_value; 
										endcase
										lv_result = tagged Valid rg_mie;
									end
								endcase
								end
					4'b0010: begin
								case(inst_type) matches
									CSRRW : rg_mtimecmp <= value; 
									CSRRS : rg_mtimecmp <= rg_mtimecmp | value; 
									CSRRC : rg_mtimecmp <= rg_mtimecmp & lv_value; 
								endcase
								lv_result = tagged Valid rg_mtimecmp;
								end
					4'b0100: begin
								case(csr_addr[3:0])
									4'b0000: begin
										case(inst_type) matches
											CSRRW : rg_mscratch <= value; 
											CSRRS : rg_mscratch <= rg_mscratch | value; 
											CSRRC : rg_mscratch <= rg_mscratch & lv_value; 
										endcase
										lv_result = tagged Valid rg_mscratch;
									end 
									4'b0001: begin
										case(inst_type) matches
											CSRRW : rg_mepc <= value; 
											CSRRS : rg_mepc <= rg_mepc | value; 
											CSRRC : rg_mepc <= rg_mepc & lv_value; 
										endcase
										lv_result = tagged Valid rg_mepc;
									end
									4'b0100: begin
										case(inst_type) matches
											CSRRW : rg_mip <= value; 
											CSRRS : rg_mip <= rg_mip | value; 
											CSRRC : rg_mip <= rg_mip & lv_value; 
										endcase
										lv_result = tagged Valid rg_mip;
									end
								endcase
								end
					4'b1000: begin
								case(csr_addr[3:0])
									4'b0000: begin
										case(inst_type) matches
											CSRRW : rg_mbase <= value; 
											CSRRS : rg_mbase <= rg_mbase | value; 
											CSRRC : rg_mbase <= rg_mbase & lv_value; 
										endcase
										lv_result = tagged Valid rg_mbase;
									end 
									4'b0001: begin
										case(inst_type) matches
											CSRRW : rg_mbound <= value; 
											CSRRS : rg_mbound <= rg_mbound | value; 
											CSRRC : rg_mbound <= rg_mbound & lv_value; 
										endcase
										lv_result = tagged Valid rg_mbound;
									end
									4'b0010: begin
										case(inst_type) matches
											CSRRW : rg_mibase <= value; 
											CSRRS : rg_mibase <= rg_mibase | value; 
											CSRRC : rg_mibase <= rg_mibase & lv_value; 
										endcase
										lv_result = tagged Valid rg_mibase;
									end
									4'b0011: begin
										case(inst_type) matches
											CSRRW : rg_mibound <= value; 
											CSRRS : rg_mibound <= rg_mibound | value; 
											CSRRC : rg_mibound <= rg_mibound & lv_value; 
										endcase
										lv_result = tagged Valid rg_mibound;
									end
									4'b0100: begin
										case(inst_type) matches
											CSRRW : rg_mdbase <= value; 
											CSRRS : rg_mdbase <= rg_mdbase | value; 
											CSRRC : rg_mdbase <= rg_mdbase & lv_value; 
										endcase
										lv_result = tagged Valid rg_mdbase;
									end
									4'b0101: begin
										case(inst_type) matches
											CSRRW : rg_mdbound <= value; 
											CSRRS : rg_mdbound <= rg_mdbound | value; 
											CSRRC : rg_mdbound <= rg_mdbound & lv_value; 
										endcase
										lv_result = tagged Valid rg_mdbound;
									end
								endcase
								end
					endcase
					end
	endcase
end

return lv_result;
endmethod

endmodule

endpackage 
