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

This module is used to integrate the cache and processor and interface with the AHB-Lite bus. 
Requests from the both the cache for memory access are read and dcache is given priority over icache 
when simultaneous request occur. The design of the cache and the rules in this file ensure that 
neither of the caches starve and each is served atleast alternatively. 

*/
package core_AHB;
	import Assert::*;
	import TLM2::*;
	import DefaultValue :: *;
	import FIFOF::*;
	import SpecialFIFOs::*;
	import Utils::*;
	import GetPut::*;
	import riscv_AHB::*;
	import defined_types::*;
	import DReg::*;
	`include "defined_parameters.bsv"
	`include "TLM.defines"

	typedef TLMResponse#(`TLM_PRM_RSP_STD) Rsp_tlm;
	typedef TLMRequest#(`TLM_PRM_REQ_STD) Req_tlm;
	typedef RequestDescriptor#(`TLM_PRM_REQ_STD) Req_Desc;
	typedef RequestData#(`TLM_PRM_REQ_STD) Req_Data;

	interface Ifc_core;
		interface TLMSendIFC#(Req_tlm,Rsp_tlm) intfc;
   	method    Action      sin(Bit#(1) in);
   	method    Bit#(1)     sout();
		method 	Action mtip(Bit#(1) mtip);
   	// Simulation only //
	endinterface

	typedef enum {Handling_Dcache,Handling_Icache,Idle} Controller_State deriving (Bits, Eq, FShow);

	function Bit#(`Addr_width) increment_addr(TLMBurstMode mode, Bit#(`Addr_width) addr, Bit#(TLog#(TDiv#(`Reg_width, 8))) size, Bit#(5) length);
		Bit#(`Addr_width) new_addr=addr;
			if(length==1 || mode==INCR)begin
				new_addr=addr+zeroExtend(size)+1;
			end
			else if(mode==WRAP)
				if(length==8)begin
					if(size==0)
						new_addr[2:0]=addr[2:0]+1;
					else if(size==1)
						new_addr[3:1]=addr[3:1]+1;
					else if(size==3)
						new_addr[4:2]=addr[4:2]+1;
				end
				else if(length==4)begin
					if(size==0)
						new_addr[1:0]=addr[1:0]+1;
					else if(size==1)
						new_addr[2:1]=addr[2:1]+1;
					else if(size==3)
						new_addr[3:2]=addr[3:2]+1;
				end
		return new_addr;
	endfunction
	

	(*synthesize*)
	(*preempts="check_request_to_memory_from_dcache,read_request_from_icache"*)
	module mkcore(Ifc_core);
  	Ifc_riscv core <-mkriscv();
	
		//TLM request FIFO.
		FIFOF#(Req_tlm) req_from_core <- mkBypassFIFOF;
		FIFOF#(Rsp_tlm) rsp_to_core <- mkBypassFIFOF;

		Reg#(Bit#(4)) count <- mkReg (0);
		Reg#(Controller_State) rg_state <- mkReg(Idle);	
		Reg#(Bool) rg_need_to_drop_incoming_data <-mkReg(False);
  	Reg#(Bit#(5)) rg_burst_length <-mkReg(1);
  	Reg#(Bit#(`Addr_width)) rg_address<-mkReg(0);
  	Reg#(Maybe#(Bit#(TMul#(8,TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE))))) rg_data_line <-mkReg(tagged Invalid);
		Reg#(Bit#(5)) rg_write_burst <-mkReg(1);
		Reg#(TLMBurstMode) rg_burst_mode <-mkReg(UNKNOWN);
		Reg#(Bit#(TLog#(TDiv#(`Reg_width, 8)))) rg_burst_size <-mkReg(0);
    
    rule check_request_to_memory_from_dcache(rg_state==Idle);
    	let info<-core.data_outputs_;
			Req_Desc request_to_mem= ?;
    	rg_address<=info.address;
    	request_to_mem.addr = info.address;
    	request_to_mem.burst_size =info.transfer_size==2?3:info.transfer_size; // 0-8bits, 1-16bits, 3-32bits, 7-64bits(info.word_size==2)?3:zeroExtend(info.word_size);
    	request_to_mem.burst_mode =WRAP;
			rg_burst_mode<=WRAP;
			request_to_mem.burst_length=unpack(info.burst_length);
    	rg_state<=Handling_Dcache;
    	rg_burst_length<=unpack(info.burst_length);
    	if(info.ld_st==Store)begin //  write operation
				request_to_mem.data =truncate(info.data_line);
				request_to_mem.command =WRITE;
			  rg_data_line<=tagged Valid (info.data_line>>`Reg_width);
    	  rg_write_burst<=unpack(info.burst_length);
    	end 
    	else begin
			  request_to_mem.command =READ;
    	end
    	request_to_mem.transaction_id =count;
			count <=count+1;
			Req_tlm reqf1 =tagged Descriptor request_to_mem;
			req_from_core.enq(reqf1);
    	$display($time,"	CORE: Sending DCACHE request to Memory for Addr: %h Data: %h Burst: %d Access type: ",info.address,info.data_line,info.burst_length,fshow(info.ld_st));
    endrule

    rule send_next_data_words(rg_data_line matches tagged Valid .info_data &&& rg_state==Handling_Dcache);
      Req_Data request_to_mem= ?;
      if(rg_write_burst==1)
        rg_data_line<=tagged Invalid;
      else begin
      	request_to_mem.data=truncate(info_data);
      	Req_tlm reqf1= tagged Data request_to_mem;
      	req_from_core.enq(reqf1);
        rg_data_line<=tagged Valid (info_data>>32);
				rg_write_burst<=rg_write_burst-1;
				$display($time,"\tCORE: Sending DATA packets for WRITE Burst: %d",rg_write_burst);
      end
    endrule

    rule read_request_from_icache(rg_state==Idle);
    	let info <-core.instruction_outputs_;
			Req_Desc request_to_mem= ?;
			request_to_mem.addr = zeroExtend(info.address);
			request_to_mem.burst_size =3; // 0-8bits, 1-16bits, 3-32bits, 7-64bits
			rg_burst_size<=3;
			request_to_mem.burst_mode =WRAP;
			rg_burst_mode<=WRAP;
			request_to_mem.burst_length=unpack(info.burst_length);
    	rg_burst_length<=unpack(info.burst_length);
    	request_to_mem.command =READ;
			request_to_mem.transaction_id =count;
			count <=count+1;
			Req_tlm reqf1 =tagged Descriptor request_to_mem;
			req_from_core.enq(reqf1);
			rg_state<=Handling_Icache;
    	rg_address<=zeroExtend(info.address);
    	$display($time,"	CORE: Sending ICACHE request to Memory. Address: %h burst_length: ",info.address,info.burst_length);
    endrule
    
    rule send_response_from_memory_to_data(rg_state==Handling_Dcache) ; 
			Rsp_tlm resp_to_core = rsp_to_core.first;
			rsp_to_core.deq;                
			let bus_error_from_memory = (resp_to_core.status==ERROR) ? 1 : 0;
      rg_address<=increment_addr(rg_burst_mode,rg_address,rg_burst_size,rg_burst_length);
			$display($time," CORE : Sending the data back to the DCACHE data:%h for address: %h current_burst: %d status: ",resp_to_core.data,rg_address,rg_burst_length,fshow(resp_to_core.status));
    	let x <-core._data_inputs(From_Memory_D{data_line:resp_to_core.data,bus_error:bus_error_from_memory,misaligned_error:0, address:rg_address});
			if(x || rg_burst_length==1)
				rg_state<=Idle;
    endrule

    rule send_response_from_memory_to_instruction(rg_state==Handling_Icache) ; 
			Rsp_tlm resp_to_core = rsp_to_core.first;
			rsp_to_core.deq;                
			let bus_error_from_memory = (resp_to_core.status==ERROR) ? 1 : 0;
      rg_address<=increment_addr(rg_burst_mode,rg_address,rg_burst_size,rg_burst_length);
			$display($time," CORE : Sending the data back to the ICACHE data:%h for address: %h current_burst: %d",resp_to_core.data,rg_address,rg_burst_length);
			let x<-core._instruction_inputs(From_Memory{data_line:resp_to_core.data,bus_error:bus_error_from_memory, address:rg_address});
			if(x || rg_burst_length==1)begin
				$display("GOTCHA");
  			rg_state<=Idle;
			end
    endrule
	
		interface TLMSendIFC  intfc = toSendIFC (req_from_core,rsp_to_core);
    method    Action      sin(Bit#(1) in);
      core.sin(in);
    endmethod
    method    Bit#(1)     sout();
      return core.sout;
    endmethod
		method 	Action mtip(Bit#(1) mtip1);
			core.mtip(mtip1);
		endmethod
  endmodule
endpackage
