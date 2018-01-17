/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Names : Neel Gala
Email ID : neelgala@gmail.com

Description : 
This is the 64-bit core of the c_class processor. It containes rules for each stage. The description of each stage 
is given in the respective rules.
*/
package riscv_AXI;

import defined_types::*;
import ConfigReg::*;
import Assert::*;
import SpecialFIFOs::*;
import FIFO::*;
import FIFOF::*;
import DReg::*;

import decoder::*;
import registerfile::*;
import set_associative_cache::*;
import execution_unit::*;
import memory_unit_AXI::*;
import defined_types::*;
import branch::*;
`include "defined_parameters.bsv"

	interface Ifc_riscv;
        // Methods for data fetch and write from/to the external evironment
    	method ActionValue#(Bool) _instruction_inputs(From_Memory#(`ICACHE_ADDR,`ICACHE_WORD_SIZE) mem_data);
    	method ActionValue#(To_Memory#(`ICACHE_ADDR)) instruction_outputs_();
    	method ActionValue#(Bool) dcache_response_from_memory_read(From_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE) mem_data);
    	method ActionValue#(Bool) dcache_response_from_memory_write(From_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE) mem_data);
    	method ActionValue#(To_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)) dcache_request_to_memory_read();
    	method ActionValue#(To_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)) dcache_request_to_memory_write();
    	method Action      sin(Bit#(1) in);
    	method Bit#(1)     sout();
			method Action mtip(Bit#(1) mtip1);
	endinterface
	
	(*synthesize*)
	module mkriscv(Ifc_riscv);
  
   	FIFOF#(ID_IE_type) ff_id_ie <-mkLFIFOF(); // instantiating ISB between decode and exec
		FIFOF#(IF_ID_type) ff_if_id <-mkLFIFOF(); // instantiating ISB between fetch and decode
		FIFOF#(IE_IMEM_type) ff_ie_imem <-mkLFIFOF(); // instantiating ISB between exec and memory
		FIFOF#(IMEM_IWB_type) ff_imem_iwb <-mkLFIFOF(); // instantiating ISB between memory and write-back
   	Reg#(Bool) wr_flush_decode_cache <-mkDReg(False);	// if true inidicates that the entire pipe needs to be flushed
   	Reg#(Bool) wr_flush_all <-mkDReg(False);	// if true inidicates that the entire pipe needs to be flushed
		Reg#(Bit#(`Addr_width)) wr_effective_address <-mkDReg(0); // captures the new pc when a branch is mispredicted.
		Reg#(Bit#(`Addr_width)) wr_effective_address1 <-mkDReg(0); // captures the new pc when a branch is mispredicted.
		Wire#(Bool) wr_stop_memory <-mkDWire(False);
		Wire#(Maybe#(Operand_forwading_type)) wr_forward_from_MEM <-mkDWire(tagged Invalid);// holds the forwarded data from the memory stage
		Wire#(Maybe#(Operand_forwading_type)) wr_forward_from_EXE <-mkDWire(tagged Invalid);// holds the forwarded data from the memory stage
   	Wire#(Maybe#(Trap_info)) wr_trap_data <-mkDWire(tagged Invalid);
   	Wire#(Maybe#(Bit#(32))) wr_write_back_pc <-mkDWire(tagged Invalid);
		Reg#(Bit#(32)) rg_programcounter[3] <-mkCReg(3,'h1000);
    Reg#(Bool) rg_ignore_cache_response[2] <-mkCReg(2,False);


    Ifc_execution_unit alu_unit <-mkexecution_unit(); 
    Ifc_registerfile register_file <-mkregisterfile();
    Ifc_memory_unit mem_unit <-mkmemory_unit();
    Ifc_icache icache <-mkicache();

		Wire#(Bool) wr_ff_if_id_notFull <-mkDWire(False);
		Reg#(Bool) rg_increment_pc[2] <-mkCReg(2,False);

    rule rl_flush_first_two_stages(wr_flush_decode_cache && !wr_flush_all);
			$display($time,"\tFlushing the fetch stage alone");
			ff_if_id.clear();
    	icache.flush();
			rg_programcounter[1]<=wr_effective_address;
    endrule

    rule rl_flush_all_stages(wr_flush_all);
      ff_if_id.clear();
      ff_id_ie.clear();
      ff_ie_imem.clear();
      icache.flush();
			rg_programcounter[1]<=wr_effective_address;
    endrule

		rule check_full;
			wr_ff_if_id_notFull<=ff_if_id.notFull;
		endrule

    rule rl_send_pc_fetch_request(!wr_flush_decode_cache && !wr_flush_all && wr_ff_if_id_notFull);
			icache.request_from_cpu(From_Cpu{address : rg_programcounter[2]});	
			$display($time,"\tFETCH: Address sent to I-Cache: %h",rg_programcounter[2]);
      rg_ignore_cache_response[1]<=False;
    endrule
    rule rg_receive_instruction(!wr_flush_decode_cache && !wr_flush_all);
      Maybe#(Exception_cause) exec=tagged Invalid;
      let resp_data<-icache.response_to_cpu;
			let prediction<-icache.prediction;
      if(resp_data.misaligned_error==1)
        exec=tagged Valid Inst_addr_misaligned;
      else if (resp_data.bus_error==1)
        exec=tagged Valid Inst_access_fault;
      if(!rg_ignore_cache_response[0])begin
        $display($time,"\t************* FETCH STAGE FIRING ************ PC: %h",resp_data.address);
        $display($time,"\tInstruction Fetched: %h \t PC: %h Prediction: ",resp_data.data_word,resp_data.address,fshow(prediction.prediction_)," NextPC: %h",prediction.prog_counter_);
        ff_if_id.enq(IF_ID_type{program_counter :resp_data.address,
                                  instruction	 	: resp_data.data_word,
                                  prediction		:	prediction.prediction_,
                                  exception:exec});
			  rg_programcounter[0]<=prediction.prog_counter_;
        rg_ignore_cache_response[0]<=prediction.jump;
      end
    endrule
    // This rule decodes the instruction and provides necessary info for the execution units.
    rule rl_decode_and_operand_fetch(!wr_flush_all && !wr_flush_decode_cache);
      let x = fn_decoder(ff_if_id.first().instruction,ff_if_id.first.prediction);
   		Maybe#(Exception_cause) exception=ff_if_id.first().exception;
			if(x.inst_type==NOP)begin
				ff_if_id.deq();	
			end
			else if(x.inst_type==ILLEGAL)begin
				$display($time,"\tDC_STAGE: ILLEGAL INSTRUCTION");
  		  exception=tagged Valid Illegal_inst;
				ff_id_ie.enq(ID_IE_type{op_fetch_data:tagged RESULT Arithout{aluresult:0,fflags:0},
				destination:0,	exception:exception, program_counter:ff_if_id.first.program_counter, rd_type:IntegerRF});
		    ff_if_id.deq();
			end
			else if(x.inst_type==SYSTEM_INSTR)begin
				ff_id_ie.enq(ID_IE_type{op_fetch_data:tagged CSR Arithout{aluresult:zeroExtend({x.opcode,x.funct3,x.immediate_value[11:0],x.rs1}),fflags:0},
				program_counter:ff_if_id.first.program_counter,	exception:exception, destination:x.rd, rd_type:x.rd_type});
		    ff_if_id.deq();
			end
			else begin
				let operands<- register_file._inputs_from_decode_stage(x.rs1,x.rs1_type,x.rs2,x.rs2_type  `ifdef spfpu ,x.rs3 `endif );
				if(operands matches tagged Valid .op_data)begin
		      ff_if_id.deq();
					$display($time,"\tGOT VALID OPERANDS");
  	    	if(x.opcode==`BRANCH_op || x.opcode==`JAL_op || x.opcode == `JALR_op) begin// conditional branch operations + JAL + JALR
  	    		$display($time,"\t Sending inputs to branch unit");
  	    		let branchresult= fn_branch(x.opcode,x.funct3,ff_if_id.first.program_counter,x.immediate_value,op_data.rs1,op_data.rs2,ff_if_id.first.prediction);
						if(branchresult.pred_result matches tagged Mispredicted .new_programcounter)begin
  	    			$display($time,"	Misprediction PC : %h New PC: %h",ff_if_id.first().program_counter,new_programcounter);
							wr_flush_decode_cache<=True; 
							wr_effective_address<=new_programcounter; // change the PC on flush
  	    		end
						else if(branchresult.pred_result matches tagged Correct_prediction .new_programcounter)begin
							$display($time," Correct Prediction PC: %h",ff_if_id.first.program_counter);
						end
						`ifdef bpu
       				icache._training(ff_if_id.first.program_counter, branchresult.training_data.branch_address, branchresult.training_data.actual);
						`endif
						ff_id_ie.enq(ID_IE_type{op_fetch_data:tagged RESULT Arithout{aluresult:branchresult.branchresult,fflags:0},
						destination:x.rd, exception:exception,	program_counter:ff_if_id.first.program_counter,	rd_type:x.rd_type});
  	    	end
					else 
						  ff_id_ie.enq(ID_IE_type{op_fetch_data:tagged ALU_INPUTS Alu_inputs{rs1:op_data.rs1, rs2: op_data.rs2, 
							`ifdef spfpu rs3: op_data.rs3,`endif
							opcode:x.opcode, funct3:x.funct3,funct7:x.funct7, immediate_value:x.immediate_value, is_imm:x.is_imm},
							program_counter:ff_if_id.first.program_counter,	exception:exception, destination:x.rd, rd_type:x.rd_type});
				end
				else begin
					$display($time,"\tWaiting for valid operands");
				end
			end
  	  $display($time,"\t********** DECODE STAGE FIRING ************")  ;
			$display($time,"\tDC_STAGE:	Instruction : %h PC : %h is ",ff_if_id.first().instruction,ff_if_id.first().program_counter," ",fshow(x.inst_type)," Rd_type: ",fshow(x.rd_type));
  	  $display($time,"\tRs1: %d",x.rs1," ",fshow(x.rs1_type)," Rs2: %d",x.rs2," ",fshow(x.rs2_type),`ifdef spfpu " Rs3: %d",x.rs3, `endif " Rd: %d",x.rd);
    endrule
		
		rule rl_forwarding_data_to_decode_from_exe;
			register_file._forwarding_from_execution(wr_forward_from_EXE); // forwarding from memory unit.
		endrule

    // This rule fetches the operands sends data to the relevant execution units.
    rule rl_execute(!wr_flush_all);
      let decoded_data=ff_id_ie.first().op_fetch_data;
			Maybe#(Exception_cause) exception=ff_id_ie.first.exception;
      if(decoded_data matches tagged ALU_INPUTS .inps)begin
      		$display($time,"\t********** EXECUTION STAGE FIRING ************ PC: :%h",ff_id_ie.first.program_counter)  ;
          $display($time,"\tExecution: ALU Operation");
          `ifdef spfpu
            $display($time,"\t Rs1: %h Rs2: %h Rs3: %h Immediate: %h",inps.rs1,inps.rs2,inps.rs3,inps.immediate_value);
          `elsif
            $display($time,"\t Rs1: %h Rs2: %h Immediate: %h",inps.rs1,inps.rs2,inps.immediate_value);
          `endif
          let {x,y} <- alu_unit.inputs(inps.opcode, inps.funct3, inps.funct7,
                                          inps.rs1, inps.rs2, `ifdef spfpu inps.rs3, `endif
                                          inps.immediate_value, ff_id_ie.first.program_counter,
                                          inps.is_imm);
					if(x matches tagged Busy)begin
			  		wr_forward_from_EXE <= tagged Valid Operand_forwading_type {	data_forward 	: ?,
															rd_forward   	:ff_id_ie.first.destination,
															rd_type       :ff_id_ie.first.rd_type,
															valid         : False};
					end
					else begin
          	ff_id_ie.deq();
						if(x matches tagged RESULT .res1)begin
							exception=exception matches tagged Invalid?y:exception;
			  			wr_forward_from_EXE <= tagged Valid Operand_forwading_type {	data_forward 	: res1.aluresult,
															rd_forward   	:ff_id_ie.first.destination,
															rd_type       :ff_id_ie.first.rd_type,
															valid         : True};
						end
						else if(x matches tagged MEMORY .res1 &&& res1.mem_type!=Store)begin
			  			wr_forward_from_EXE <= tagged Valid Operand_forwading_type {	data_forward 	: ?,
															rd_forward   	:ff_id_ie.first.destination,
															rd_type       :ff_id_ie.first.rd_type,
															valid         : False};
						end
            ff_ie_imem.enq(IE_IMEM_type{execresult:x, system_instruction:False,
																				program_counter:ff_id_ie.first.program_counter,
																				exception:exception,
																				destination:ff_id_ie.first.destination,
																				rd_type:ff_id_ie.first.rd_type});
          end
      end
      else if(decoded_data matches tagged RESULT .res) begin
      		$display($time,"\t********** EXECUTION STAGE FIRING ************ PC: :%h",ff_id_ie.first.program_counter)  ;
          $display($time,"\tExecution: BRANCH Instruction");
          ff_ie_imem.enq(IE_IMEM_type{execresult:tagged RESULT res, system_instruction:False,
																				program_counter:ff_id_ie.first.program_counter,
																				exception:exception,
																				destination:ff_id_ie.first.destination,
																				rd_type:ff_id_ie.first.rd_type});
          ff_id_ie.deq();
			  	wr_forward_from_EXE <= tagged Valid Operand_forwading_type {	data_forward 	: res.aluresult,
															rd_forward   	:ff_id_ie.first.destination,
															rd_type       :ff_id_ie.first.rd_type,
															valid         : True};
      end
      else if(decoded_data matches tagged CSR .res) begin
      		$display($time,"\t********** EXECUTION STAGE FIRING ************ PC: :%h",ff_id_ie.first.program_counter)  ;
          $display($time,"\tExecution: SYSTEM Instruction");
          ff_ie_imem.enq(IE_IMEM_type{execresult:tagged RESULT res, system_instruction:True,
																				program_counter:ff_id_ie.first.program_counter,
																				exception:exception,
																				destination:ff_id_ie.first.destination,
																				rd_type:ff_id_ie.first.rd_type});
          ff_id_ie.deq();
			  	wr_forward_from_EXE <= tagged Valid Operand_forwading_type {	data_forward 	: ?,
															rd_forward   	:ff_id_ie.first.destination,
															rd_type       :ff_id_ie.first.rd_type,
															valid         : False};
      end
    endrule

		rule rl_forwarding_data_to_decode;
			register_file._forwarding_from_memory(wr_forward_from_MEM); // forwarding from memory unit.
		endrule
    rule rl_memory_stage(!wr_flush_all && !wr_stop_memory);
      let info=ff_ie_imem.first();
      if(info.execresult matches tagged MEMORY .meminfo)begin 
      	$display($time,"\t*****************MEMORY STAGE*************************\t PC: %h",info.program_counter);
        let memresp <- mem_unit.communicate_with_core(meminfo);
        if(memresp matches tagged Valid .val)begin
          ff_ie_imem.deq;
					Arithout temp_var=?;
					temp_var.aluresult=val.destination_value;
					temp_var.fflags=0;
					let exception=info.exception matches tagged Invalid?val.exception:info.exception;
          ff_imem_iwb.enq(IMEM_IWB_type{badaddr:val.badaddr,commit_data:temp_var,
																				system_instruction:info.system_instruction,
																				program_counter:info.program_counter,
																				destination:info.destination,
																				rd_type:info.rd_type,
																				exception:exception});
				  wr_forward_from_MEM <= tagged Valid Operand_forwading_type {	data_forward 	:temp_var.aluresult,
																rd_forward   	:info.destination,
																rd_type       :info.rd_type,
																valid         : True};
        end
        else begin
				  wr_forward_from_MEM <= tagged Valid Operand_forwading_type {	data_forward 	: ?,
																rd_forward   	:info.destination,
																rd_type       :info.rd_type,
																valid         : False};
        end
      end
      else if(info.execresult matches tagged RESULT .res1)begin
      	$display($time,"\t*****************MEMORY STAGE BYPASSED********************\t PC: %h",info.program_counter);
        $display($time,"\t MEM_STAGE: Forwarding: Rd: %d %h",info.destination,res1.aluresult," ",fshow(info.rd_type));
				wr_forward_from_MEM <= tagged Valid Operand_forwading_type {data_forward 	: res1.aluresult,
																rd_forward   	:info.destination,
																rd_type       :info.rd_type,
																valid         :!info.system_instruction};
				ff_imem_iwb.enq(IMEM_IWB_type{badaddr:?,commit_data:res1,system_instruction:info.system_instruction,
																			program_counter:info.program_counter,
																			destination:info.destination,
																			rd_type:info.rd_type,
																			exception:info.exception});
          ff_ie_imem.deq;
      end
    endrule

		rule rl_write_back;
      let info=ff_imem_iwb.first().commit_data;
      $display($time,"\t*****************WRITE BACK STAGE*************************\t PC: %h",ff_imem_iwb.first.program_counter);
      ff_imem_iwb.deq(); // release the previous FIFO

      let csr_result<-register_file.csr_access(CSRInsn{rs1_data:0,
                                                      is_privilege:ff_imem_iwb.first.system_instruction,
                                                      exception:ff_imem_iwb.first.exception,
                                                      pc:ff_imem_iwb.first.program_counter,
                                                      badaddr:ff_imem_iwb.first.badaddr,
                                                      rd_data:info.aluresult,
                                                      fflags:info.fflags},
																											ff_imem_iwb.first.destination,ff_imem_iwb.first.rd_type);
      if(csr_result.redirect)begin
        $display("Flushing the PIPE. Jumping to Address: %h",csr_result.address);
		wr_stop_memory<=True;
        wr_flush_all<=True; 
        wr_effective_address1<=csr_result.address; // change the PC on flush
      end
			wr_write_back_pc<=tagged Valid ff_imem_iwb.first.program_counter;
		endrule


    rule rl_clock;
      $display("	\n\n");
      `ifdef simulate
				if(wr_write_back_pc matches tagged Valid .pc)
	        register_file._print_all_rf(pc);
      `endif
    endrule

		//////////////////////////// definition of methods /////////////////////////////////////////////////////////////////////////////////
    method ActionValue#(Bool) _instruction_inputs(From_Memory#(`ICACHE_ADDR,`ICACHE_WORD_SIZE) mem_data);
      let x <-icache.response_from_memory(mem_data);
			return x;
    endmethod

    method ActionValue#(To_Memory#(`ICACHE_ADDR)) instruction_outputs_=icache.request_to_memory;
    	method ActionValue#(Bool) dcache_response_from_memory_read(From_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE) mem_data) = mem_unit.response_from_memory_read(mem_data);
    	method ActionValue#(Bool) dcache_response_from_memory_write(From_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE) mem_data)= mem_unit.response_from_memory_write(mem_data);
    	method ActionValue#(To_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)) dcache_request_to_memory_read() = mem_unit.request_to_memory_read;
    	method ActionValue#(To_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)) dcache_request_to_memory_write()= mem_unit.request_to_memory_write;

    method    Action      sin(Bit#(1) in);
      register_file.sin(in);
    endmethod
    method    Bit#(1)     sout();
      return register_file.sout;
    endmethod
		method 	Action mtip(Bit#(1) mtip1);
			register_file.mtip(mtip1);
		endmethod
	endmodule
endpackage


