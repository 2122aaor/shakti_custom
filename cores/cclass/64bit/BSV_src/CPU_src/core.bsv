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
last updated : Feb 20th 2015

Description : 
TODO provide proper logic for cacheable,bufferable_ and supervisor outpu signal to the bus.

This module is used to integrate the cache and processor and interface with the AHB-Lite bus. 
Requests from the both the cache for memory access are read and dcache is given priority over icache 
when simultaneous request occur. The design of the cache and the rules in this file ensure that 
neither of the caches starve and each is served atleast alternatively. 

*/
package core;

	import Assert::*;
	import TLM2::*;
	import DefaultValue :: *;
	import FIFOF::*;
	import SpecialFIFOs::*;
	import Utils::*;
	import GetPut::*;
	`include "TLM.defines"

  import riscv::*;
  import defined_types::*;
  import DReg::*;
  import All_types::*;
	`include "defined_parameters.bsv"
	typedef TLMResponse#(`TLM_PRM_RSP_STD) Rsp_tlm;
	typedef TLMRequest#(`TLM_PRM_REQ_STD) Req_tlm;
	typedef RequestDescriptor#(`TLM_PRM_REQ_STD) Req_Desc;

    interface Ifc_core_copy;
	  	interface TLMSendIFC#(Req_tlm,Rsp_tlm) intfc;
      method    Action      sin(Bit#(1) in);
      method    Bit#(1)     sout();
      method Bool flush;
      // Simulation only //
    endinterface

    typedef enum {Handling_Dcache,Handling_Icache,Idle} Controller_State deriving (Bits, Eq, FShow);

    (*synthesize*)
    (*preempts="check_request_to_memory_from_either_ports,read_request_from_icache"*)
    module mkcore_copy(Ifc_core_copy);

    Ifc_riscv core <-mkriscv();
	
		//TLM request FIFO.
		FIFOF#(Req_tlm) req_from_core <- mkBypassFIFOF;
		FIFOF#(Rsp_tlm) rsp_to_core <- mkBypassFIFOF;
    Wire#(bit) wr_instruction_data <-mkDWire(0);
    Wire#(Bool) wr_flush_everything<-mkDWire(False);

		Reg#(Bit#(4)) count <- mkReg (0);
		Reg#(Bit#(2)) rg_state <- mkReg (0);	
		Reg#(Bool) rg_need_to_drop_incoming_data <-mkReg(False);
    Reg#(Bit#(5)) rg_burst_length <-mkReg(1);
    Reg#(Bit#(`Addr_width)) rg_address<-mkReg(0);

    rule read_flush_signal_from_cpu;
        wr_flush_everything<=core.flush_from_cpu_();
    endrule
    
    rule flush_caches(wr_flush_everything);
				if(rg_state==2)
					rg_need_to_drop_incoming_data<=True;

    endrule

    
    rule check_request_to_memory_from_either_ports(rg_state==0 &&& !wr_flush_everything &&& core.data_outputs_ matches tagged Valid .info);
            $display($time,"	CORE: Sending Dcache request to Memory for Addr: %h",info.address);
						Req_Desc request_to_mem= ?;
            request_to_mem.addr = info.address;
            request_to_mem.burst_size =(info.word_size==2)?3:(info.word_size==3)?7:zeroExtend(info.word_size);
            request_to_mem.burst_mode =INCR;
						request_to_mem.burst_length=1;
            rg_burst_length<=1;
            rg_state<=1;
            wr_instruction_data <=1;
            if(info.read_write==1)begin //  write operation
							request_to_mem.data =info.write_data;
							request_to_mem.command =WRITE;
            end 
            else
						  request_to_mem.command =READ;
						
              request_to_mem.transaction_id =count;
						count <=count+1;
						Req_tlm reqf1 =tagged Descriptor request_to_mem;
						req_from_core.enq(reqf1);
            rg_address<=info.address;
    endrule

    rule read_request_from_icache(rg_state==0 && !wr_flush_everything);
          let info <-core.instruction_outputs_;
          $display($time,"	CORE: Sending Icache request to Memory. Address: %h ",info.address);
					Req_Desc request_to_mem= ?;
					request_to_mem.addr = zeroExtend(info.address);
					request_to_mem.burst_size =3; // 0-8bits, 1-16bits, 3-32bits, 7-64bits
					request_to_mem.burst_mode =INCR;
					request_to_mem.burst_length=unpack(info.burst_length);
          rg_burst_length<=unpack(info.burst_length);
          request_to_mem.command =READ;
					request_to_mem.transaction_id =count;
					count <=count+1;
					Req_tlm reqf1 =tagged Descriptor request_to_mem;
					req_from_core.enq(reqf1);
					rg_state<=2;
          wr_instruction_data<=0;
          rg_address<=zeroExtend(info.address);
    endrule
    


    rule send_response_from_memory_to_core(rg_state!=0 && !wr_flush_everything) ; 
			Rsp_tlm resp_to_core = rsp_to_core.first;
			rsp_to_core.deq;                
        //if(resp_to_core.transaction_id == count-1)begin
      if(rg_burst_length==1)
  			rg_state<=0;
      else
        rg_burst_length<=rg_burst_length-1;
			let bus_error_from_memory = (resp_to_core.status==ERROR) ? 1 : 0;
				if(rg_state==1)begin // dcache was being handled
						$display($time,"	Controller : Sending the data back to the DCACHE",$time);
						core._data_inputs(Data_from_mem{read_data:resp_to_core.data,bus_error:bus_error_from_memory,misaligned_error:0});
				end
				else if(rg_state==2)begin // if i-cache was being handled.
            rg_address<=rg_address+4;
						$display($time,"	Controller : Sending the data back to the ICACHE data:%h for address: %h",resp_to_core.data,rg_address,$time);
						core._instruction_inputs(Data_from_mem{read_data:resp_to_core.data,bus_error:bus_error_from_memory,misaligned_error:0, address:rg_address});
				end
    endrule
	
		interface TLMSendIFC  intfc = toSendIFC (req_from_core,rsp_to_core);
    method    Action      sin(Bit#(1) in);
      core.sin(in);
    endmethod
    method    Bit#(1)     sout();
      return core.sout;
    endmethod
     method Bool flush;
      return wr_flush_everything;
    endmethod
  endmodule
endpackage
