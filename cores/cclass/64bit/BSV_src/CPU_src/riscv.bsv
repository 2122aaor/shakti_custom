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
package riscv;

`include "defined_parameters.bsv"
import defined_types::*;
import ConfigReg::*;
import decoder::*;
import registerfile::*;
import execution_unit::*;
import memory_unit::*;
import defined_types::*;
import SpecialFIFOs::*;
import FIFO::*;
import FIFOF::*;
import DReg::*;
import All_types::*;
import set_associative_cache::*;

	interface Ifc_riscv;
        // Methods for data fetch and write from/to the external evironment
    method Action _instruction_inputs(Data_from_mem mem_data);
    method ActionValue#(To_Memory#(32,4,4)) instruction_outputs_();
    method Action _data_inputs(Data_from_mem mem_data);
    method Maybe#(Data_to_mem) data_outputs_();
    method Bool flush_from_cpu_();
    method    Action      sin(Bit#(1) in);
    method    Bit#(1)     sout();


	endinterface
	
	(*synthesize*)
	module mkriscv(Ifc_riscv);
  
    Reg#(Bit#(`Addr_width)) rg_pc <-mkReg('h200);
    FIFOF#(ID_IE_type) ff_id_ie <-mkLFIFOF(); // instantiating ISB between decode and exec
		FIFOF#(IF_ID_type) ff_if_id <-mkLFIFOF(); // instantiating ISB between fetch and decode
		FIFOF#(IE_IMEM_type) ff_ie_imem <-mkLFIFOF(); // instantiating ISB between exec and memory
		FIFOF#(IMEM_IWB_type) ff_imem_iwb <-mkLFIFOF(); // instantiating ISB between memory and write-back

    Wire#(Maybe#(Data_to_mem)) rg_data_to_instruction_memory <-mkDWire(tagged Invalid); // register to send data to the instruction cache
    Reg#(Maybe#(Data_from_mem)) rg_data_from_instruction_memory <-mkDReg(tagged Invalid); // register to receive data from the instruction cache
		
    Wire#(Bool) wr_flush_everything <-mkDWire(False);	// if true inidicates that the entire pipe needs to be flushed
		Wire#(Bit#(`Addr_width)) wr_effective_address <-mkDWire(0); // captures the new pc when a branch is mispredicted.
		Wire#(Operand_forwading_type) wr_forward_from_MEM <-mkDWire(Operand_forwading_type{data_forward:0,rd_forward:0,valid:False,rd_type:IntegerRF});// holds the forwarded data from the memory stage
    
    Ifc_execution_unit alu_unit <-mkexecution_unit(); // instantiating the RV64I exeuction unit
    Ifc_registerfile register_file <-mkregisterfile();
    Ifc_memory_unit mem_unit <-mkmemory_unit();
    //Ifc_set_associative_cache#(32,4,4,4,512) icache <- mkset_associative_cache("ICACHE");
    Ifc_icache icache <-mkicache();


    rule rl_flush_stuff(wr_flush_everything);
      rg_pc<=wr_effective_address;
      ff_if_id.clear();
    endrule

    rule rl_send_pc_fetch_request(!wr_flush_everything && ff_if_id.notFull);
      icache.request_from_cpu(From_Cpu{address : truncate(rg_pc)});	
    endrule
    rule rg_receive_instruction(!wr_flush_everything);
      $display($time,"\t************* FETCH STAGE FIRING ************ PC: %h",rg_pc);
      Bool found=False;
      Bit#(`Addr_width) address=0;
      Bit#(32) data =0;
      Exception exec=None;
      if(icache.response_to_cpu matches tagged Valid .resp_data &&& resp_data.address==truncate(rg_pc))begin
        data=resp_data.data_word;
        found=True;
        if(resp_data.misaligned_error==1)
          exec=Instruction_misaligned;
        else if (resp_data.bus_error==1)
          exec=Instruction_buserr;
      end
      if(icache.response_from_bus matches tagged Valid .resp_data &&& resp_data.address==truncate(rg_pc))begin
        data=resp_data.data_word;
        found=True;
        if(resp_data.misaligned_error==1)
          exec=Instruction_misaligned;
        else if (resp_data.bus_error==1)
          exec=Instruction_buserr;
      end
				// fill the next pipe fifo with the fetched instruction and the prediction type from the branch predictor
      if(found)begin
        $display($time,"\tInstruction Fetched: %h \t PC: %h ",data,rg_pc);
        ff_if_id.enq(IF_ID_type{ program_counter : rg_pc,
                                instruction	 : data,
                                prediction: Predicted_notaken,
                                exception:exec});
        rg_pc<=rg_pc+4;
      end
    endrule
    rule rl_get_instruction_from_cache(rg_data_from_instruction_memory matches tagged Valid .mem_data);
      $display("Seding data from riscv to icache");
      icache.response_from_memory(From_Memory{data_line:truncate(mem_data.read_data),
                                              bus_error:mem_data.bus_error,
                                              misaligned_error:mem_data.misaligned_error,
                                              address:truncate(mem_data.address)});
    endrule
    // this fetches instruction and error signal from the instruction cache

    // This rule decodes the instruction and provides necessary info for the execution units.
    rule rl_decode(!wr_flush_everything);
      let x = fn_decoder(ff_if_id.first().instruction,ff_if_id.first.prediction);
      ff_if_id.deq();
      Exception exception=ff_if_id.first().exception;
      if(exception==None && x.inst_type==ILLEGAL)
        exception=Illegal_instruction;
      ff_id_ie.enq(ID_IE_type{decoder_data:x,
                              program_counter:ff_if_id.first().program_counter,
                              exception:exception});
      $display($time,"\t********** DECODE STAGE FIRING ************")  ;
			$display($time,"\tDC_STAGE:	Instruction : %h PC : %h is ",ff_if_id.first().instruction,ff_if_id.first().program_counter," ",fshow(x.inst_type)," Rd_type: ",fshow(x.rd_type));
      $display($time,"\tRs1: %d Rs2: %d Rs3: %d Rd: %d",x.rs1,x.rs2,x.rs3,x.rd);
    endrule

    rule rl_operand_fetch;
      let decoded_data=ff_id_ie.first().decoder_data;
      register_file._inputs_from_decode_stage(decoded_data.rs1,
                                              decoded_data.rs1_type,
                                              decoded_data.rs2,
                                              decoded_data.rs2_type,
                                              decoded_data.rs3,
                                              decoded_data.rd,
                                              decoded_data.rd_type,
                                              True,
                                              (decoded_data.inst_type==PRIVILEGED),
                                              decoded_data.funct3,
                                              truncate(decoded_data.immediate_value));
    endrule

    // This rule fetches the operands sends data to the relevant execution units.
    rule rl_execute;
      $display($time,"\t********** EXECUTION STAGE FIRING ************ PC: :%h",ff_id_ie.first().program_counter)  ;
      if(ff_id_ie.first.exception==None)begin
        let decoded_data=ff_id_ie.first().decoder_data;
        if(decoded_data.inst_type == ALU)begin
          if(register_file.output_to_decode_stage matches tagged Valid .operands)begin
            let x <- alu_unit.inputs(decoded_data.opcode,
                                            decoded_data.funct3,
                                            decoded_data.funct7,
                                            operands.rs1,
                                            operands.rs2,
                                            decoded_data.immediate_value,
                                            ff_id_ie.first().program_counter,
                                            decoded_data.rd,
                                            decoded_data.pred_type,
                                            decoded_data.is_imm);
            if(x matches tagged Valid .exec_result)begin
              ff_id_ie.deq(); // release the previous FIFO
              if(exec_result.pred_result matches tagged Correct_prediction .y)begin
                ff_ie_imem.enq(IE_IMEM_type{exec_output:exec_result,
                                            exception:ff_id_ie.first.exception,
                                            rd_type:ff_id_ie.first.decoder_data.rd_type
                                            `ifdef simulate ,program_counter:ff_id_ie.first().program_counter `endif
                                          });
                $display($time,"\t Got result from the Arithmetic unit");
  //              if(exec_result.mem_type==STORE && exec_result.bypass) // special setting for BRANCH instructions
  //                bpu._training(ff_id_ie.first.pc, x.training_data.branch_address, x.training_data.actual);
              end
              else if(exec_result.pred_result matches tagged Mispredicted .z)begin
                $display($time,"	Misprediction PC : %h New PC: %h",ff_id_ie.first().program_counter,z);
                wr_flush_everything<=True; 
                wr_effective_address<=z; // change the PC on flush
                ff_ie_imem.enq(IE_IMEM_type{exec_output:exec_result,
                                           exception:ff_id_ie.first.exception
                                           `ifdef simulate ,program_counter:ff_id_ie.first().program_counter `endif
                                         });
              end
            end
          end
        end
        else if(decoded_data.inst_type==NOP)begin
          ff_id_ie.deq();
        end
        else if(decoded_data.inst_type==PRIVILEGED)begin
          if(register_file.output_to_decode_stage matches tagged Valid .operands)begin
            ff_ie_imem.enq(IE_IMEM_type{exec_output:Execution_output{aluresult:operands.rs1,bypass:True,destination:decoded_data.rd},
                                        exception:ff_id_ie.first.exception
                                        `ifdef simulate ,program_counter:ff_id_ie.first().program_counter `endif
                                         });
            ff_id_ie.deq();
          end
        end
      end
      else begin // Exception case
        ff_ie_imem.enq(IE_IMEM_type{exec_output:Execution_output{bypass:True,destination:0},
                                    exception:ff_id_ie.first.exception
                                   `ifdef simulate ,program_counter:ff_id_ie.first().program_counter `endif
                                  });
        $display($time,"\t Exception :",fshow(ff_id_ie.first().exception)," Raised");
        ff_id_ie.deq();
      end
    endrule

		rule rl_forwarding_data_to_decode;
			register_file._forwarding_from_memory(wr_forward_from_MEM); // forwarding from memory unit.
		endrule
    rule rl_memory_stage;
      `ifdef simulate
      $display($time,"\t*****************MEMORY STAGE*************************\t PC: %h",ff_ie_imem.first().program_counter);
      `endif
      let execdata=ff_ie_imem.first();
      if(execdata.exec_output.bypass)begin 
        $display($time,"\t MEM_STAGE: Bypassed ");
        ff_ie_imem.deq;
        ff_imem_iwb.enq(IMEM_IWB_type{memresp:MemoryUnitResponse{destination:execdata.exec_output.destination, destination_value:execdata.exec_output.aluresult,exception:execdata.exception},
                                      rd_type:execdata.rd_type
                                     `ifdef simulate ,program_counter:ff_ie_imem.first().program_counter `endif
                                    });
				wr_forward_from_MEM <= Operand_forwading_type {	data_forward 	: execdata.exec_output.aluresult,
																rd_forward   	: execdata.exec_output.destination,
																rd_type       :execdata.rd_type,
																valid         : True};
      end
      else begin
        let memresp <- mem_unit.communicate_with_core(execdata.exec_output);
        if(memresp matches tagged Valid .val)begin
          ff_ie_imem.deq;
          ff_imem_iwb.enq(IMEM_IWB_type{memresp:val,
                                        rd_type:execdata.rd_type
                                        `ifdef simulate ,program_counter:ff_ie_imem.first().program_counter `endif
                                      });
				  wr_forward_from_MEM <= Operand_forwading_type {	data_forward 	: val.destination_value,
																rd_forward   	:val.destination,
																rd_type       :execdata.rd_type,
																valid         : True};
        end
      end
    endrule

		rule rl_write_back;
            `ifdef simulate
              $display($time,"\t*****************WRITE BACK STAGE*************************\t PC: %h",ff_imem_iwb.first().program_counter);
              register_file._print_all_rf(ff_imem_iwb.first().program_counter,ff_imem_iwb.first().memresp.destination,ff_imem_iwb.first().memresp.destination_value,ff_imem_iwb.first.rd_type);
            `endif
            if(ff_imem_iwb.first().memresp.exception==None)begin
              $display($time,"\tWB_STAGE: Dest : %d %h",ff_imem_iwb.first().memresp.destination,ff_imem_iwb.first().memresp.destination_value);
              register_file._inputs_from_writeback_stage(ff_imem_iwb.first().memresp.destination,ff_imem_iwb.first().rd_type,ff_imem_iwb.first().memresp.destination_value,True);	
      			  ff_imem_iwb.deq(); // release the previous FIFO
            end
            else if(ff_imem_iwb.first().memresp.exception==Illegal_instruction)begin // TODO in case of other exceptions do the needful
                $display($time,"	Illegal reached Write-back");
                $finish(0);
            end
		endrule
		rule rl_clock;
			$display("	\n\n");
		endrule

		//////////////////////////// definition of methods /////////////////////////////////////////////////////////////////////////////////
    method Action _instruction_inputs(Data_from_mem mem_data);
      rg_data_from_instruction_memory<=tagged Valid mem_data;
    endmethod
    method ActionValue#(To_Memory#(32,4,4)) instruction_outputs_=icache.request_to_memory;
    method Action _data_inputs(Data_from_mem mem_data);
      mem_unit.input_from_memory(mem_data);
    endmethod
    method Maybe#(Data_to_mem) data_outputs_();
      return mem_unit.data_to_memory;
    endmethod
    method Bool flush_from_cpu_();
      return wr_flush_everything;
    endmethod
    
    method    Action      sin(Bit#(1) in);
      register_file.sin(in);
    endmethod
    method    Bit#(1)     sout();
      return register_file.sout;
    endmethod
	endmodule
endpackage


