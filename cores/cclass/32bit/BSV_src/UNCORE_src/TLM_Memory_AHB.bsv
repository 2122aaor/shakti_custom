
package TLM_Memory_AHB;
	import defined_types::*;
  import BRAMCore :: *;
	import TLM2::*;
	import DefaultValue :: *;
	import FIFOF::*;
	import SpecialFIFOs::*;
	import Utils::*;
	import GetPut::*;
  import defined_types::*;
  import DReg::*;
	import Assert::*;
	`include "defined_parameters.bsv"
	`include "TLM.defines"

	// these are data types of packets which are used
	typedef TLMResponse#(`TLM_PRM_RSP_STD) Resp_to_core_TLM;
	typedef TLMRequest#(`TLM_PRM_REQ_STD) Req_from_core_TLM;
	typedef RequestDescriptor#(`TLM_PRM_REQ_STD) Req_Desc;

	interface Ifc_BRAM#(numeric type base_address, numeric type mem_size);
	interface TLMRecvIFC#(Req_from_core_TLM,Resp_to_core_TLM) intfc_rcv;
  endinterface
  
 // (*synthesize*)
 typedef enum{Send_req,Get_resp} Mem_state deriving(Bits,Eq);

 function Bit#(TDiv#(`Reg_width,8)) get_writeen_bits(Bit#(2) transfer_size, Bit#(`Reg_width) address);
		if(transfer_size==0)// 8 bits write
			if(address[1:0]==0)
					return 'b0001;
			else if(address[1:0]==1)
					return 'b0010;
			else if(address[1:0]==2)
					return 'b0100;
			else
					return 'b1000;
		else if(transfer_size==1) // 16 bits
				if(address[1:0]==0)
						return 'b0011;
				else
						return 'b1100;
		else // 32-bits
			return 'b1111;
 endfunction
	
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

  module mkTLM_Memory#(parameter String mem_init_file, parameter String module_name)(Ifc_BRAM#(base_address,mem_size));

		BRAM_PORT_BE#(Bit#(TSub#(mem_size,2)),Bit#(`Reg_width),TDiv#(`Reg_width,8)) dmem <- mkBRAMCore1BELoad(valueOf(TExp#(TSub#(mem_size,2))),False,mem_init_file,False);

    Reg#(Bit#(`Reg_width)) rg_data <-mkReg(0);
    Reg#(Bit#(5)) rg_word_count<-mkReg(0);
	  Reg#(Bit#(4)) rg_id <-mkReg(0);
    FIFOF#(Req_from_core_TLM) req_from_core <- mkSizedBypassFIFOF(1);
    FIFOF#(Resp_to_core_TLM) rsp_to_core <- mkSizedBypassFIFOF(1);
		Reg#(Maybe#(RequestDescriptor#(`TLM_PRM_REQ_STD))) rg_req <-mkReg(tagged Invalid);
		Reg#(Mem_state) rg_state <-mkDReg(Send_req);

		rule keep_removing_requests_from_core;
			req_from_core.deq();
			if(req_from_core.first matches tagged Descriptor .descriptor)begin
				$display($time,"\t",module_name,"\tTLM Request Address: %h",descriptor.addr," Actual Address: %h",descriptor.addr-fromInteger(valueOf(base_address))," Max Size:%h",fromInteger(valueOf(TExp#(mem_size))));
				dynamicAssert (((descriptor.addr-fromInteger(valueOf(base_address))) < fromInteger(valueOf(TExp#(mem_size)))), ("Address generated out of range for Memory: "+module_name));
			end
		endrule

    rule get_request(rg_req matches tagged Invalid &&& req_from_core.first() matches tagged Descriptor .descriptor &&& rg_state==Send_req);

//			Bit#(`Addr_width) increment=descriptor.burst_size==0?1:descriptor.burst_size==1?2:4;
			Bit#(`Addr_width) increment=increment_addr(descriptor.burst_mode, descriptor.addr,descriptor.burst_size,pack(descriptor.burst_length)) ;//descriptor.burst_size==0?1:descriptor.burst_size==1?2:4;
			let new_desc=descriptor;
			Bit#(TSub#(mem_size,2)) index_address=(descriptor.addr-fromInteger(valueOf(base_address)))[valueOf(mem_size)-1:2];

			if(descriptor.command == WRITE)begin
				dmem.put(get_writeen_bits(descriptor.burst_size,descriptor.addr),index_address,descriptor.data);
				Resp_to_core_TLM resp_to_core = ?;
				resp_to_core.command= WRITE;
				resp_to_core.transaction_id = rg_id;
				resp_to_core.data=descriptor.data;
				resp_to_core.status = SUCCESS;
  			rsp_to_core.enq(resp_to_core);
			end
			else begin
				rg_state<=Get_resp;
				dmem.put(0,index_address,?);
				if(descriptor.burst_length>1)begin
					new_desc.addr=increment;
					rg_req<=tagged Valid new_desc;
					rg_word_count<=rg_word_count+1;
				end
			end

			$display($time,"\t",module_name," Request Type: ",fshow(descriptor.command)," Addr: %h data: %h",descriptor.addr,descriptor.data," BURST MODE: ",fshow(descriptor.burst_mode)," BURST SIZE: ",fshow(descriptor.burst_size)," BURST LENGTH ",fshow(descriptor.burst_length));
    endrule


		rule generate_burst_request(rg_req matches tagged Valid .descriptor);
			Bit#(TSub#(mem_size,2)) index_address=(descriptor.addr-fromInteger(valueOf(base_address)))[valueOf(mem_size)-1:2];
			//Bit#(`Addr_width) increment=descriptor.burst_size==0?1:descriptor.burst_size==1?2:4;
			Bit#(`Addr_width) increment=increment_addr(descriptor.burst_mode, descriptor.addr,descriptor.burst_size,pack(descriptor.burst_length)) ;//descriptor.burst_size==0?1:descriptor.burst_size==1?2:4;
			if(unpack(rg_word_count)==descriptor.burst_length)begin
				rg_word_count<=0;
				rg_req<=tagged Invalid;
			end
			else begin
				$display($time,"\t",module_name," Generating new addr: %h data: %h read_write: %d Word Count: %d ",descriptor.addr,descriptor.data,(descriptor.command==READ)?1'b0:1'b1,rg_word_count);
				rg_state<=Get_resp;
				dmem.put(0,index_address,0);
				let new_desc = descriptor;
				new_desc.addr=increment;
				rg_req<=tagged Valid new_desc;
				rg_word_count<=rg_word_count+1;
			end
		endrule

    rule read_request_from_main_mem(rg_state==Get_resp);
    	let data0 = dmem.read();
			Resp_to_core_TLM resp_to_core = ?;
			resp_to_core.command= READ;
			resp_to_core.transaction_id = rg_id;
			resp_to_core.data=data0;
			resp_to_core.status = SUCCESS;
  		rsp_to_core.enq(resp_to_core);
			$display($time,"\t",module_name,"\tSending response to Bus with data: %h",resp_to_core.data);
    endrule

    interface intfc_rcv = toRecvIFC (req_from_core,rsp_to_core); // toRecvIFC function is in Utils.bsv//it returns interface

  endmodule  
endpackage
