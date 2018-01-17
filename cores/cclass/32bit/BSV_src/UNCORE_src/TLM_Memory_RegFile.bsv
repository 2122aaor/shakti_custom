
package TLM_Memory_RegFile;
	import Connectable::*;
	import RegFile::*;
	import defined_types::*;
  import BRAM::*;
	import TLM2::*;
	import DefaultValue :: *;
	import FIFOF::*;
	import SpecialFIFOs::*;
	import Utils::*;
	import GetPut::*;
	`include "TLM.defines"
    import defined_types::*;
	`include "defined_parameters.bsv"

	// these are data types of packets which are used
	typedef TLMResponse#(`TLM_PRM_RSP_STD) Rsp_tlm;
	typedef TLMRequest#(`TLM_PRM_REQ_STD) Req_tlm;
	typedef RequestDescriptor#(`TLM_PRM_REQ_STD) Req_Desc;


    interface Ifc_BRAM;
	  interface TLMRecvIFC#(Req_tlm,Rsp_tlm) intfc_rcv;
	  //flush could also be kept in Data field of TLM packet if required
	  method Action flush_from_proc(Bool flush);
    endinterface
  
    (*synthesize*)
    module mkTLM_Memory(Ifc_BRAM);
      RegFile#(Bit#(TSub#(`Addr_space,2)), Bit#(`Reg_width)) dmem <-mkRegFileFullLoad("./code.mem");

      // Information contained in TLM request packet coming 
      // from C-class core,those are locally buffered in these registered 
      // FIFO buffering of incoming and outgoing TLM packets
      FIFOF#(Req_tlm) req_from_core <- mkSizedBypassFIFOF(1);
      FIFOF#(Rsp_tlm) rsp_to_core <- mkSizedBypassFIFOF(1);
      // FLush input from Core
      Wire#(Bool) procflush<-mkDWire(False);

  
      rule read_request_from_proc;
				if (req_from_core.first matches tagged Descriptor .d) begin
          req_from_core.deq();
          Bit#(`Reg_width) data0 = dmem.sub(d.addr[`Addr_space-1:2]);
          // Making A response TLM packet
          Rsp_tlm resp_to_core = ?;
          if(d.command==READ)begin // dmem read request.
            resp_to_core.command= READ;
            resp_to_core.transaction_id = d.transaction_id;
            if(d.burst_size=='d3)begin // word transfer
                resp_to_core.data = (zeroExtend(data0[31:0]));
            end
            else if (d.burst_size=='d1)begin // half_word
              if(d.addr[1:0] ==0)
                resp_to_core.data = (zeroExtend(data0[15:0]));
              else if(d.addr[1:0] ==2)
                resp_to_core.data = (zeroExtend(data0[31:16]));
            end
            else if (d.burst_size=='d0) begin// one byte
              if(d.addr[1:0] ==0)
                resp_to_core.data = (zeroExtend(data0[7:0]));
              else if(d.addr[1:0] ==1)
                resp_to_core.data = (zeroExtend(data0[15:8]));
              else if(d.addr[1:0] ==2)
                resp_to_core.data = (zeroExtend(data0[23:16]));
              else if(d.addr[1:0] ==3)
                resp_to_core.data = (zeroExtend(data0[31:24]));
            end
            resp_to_core.status = SUCCESS;
            // Enqueueing Response
            rsp_to_core.enq(resp_to_core);
            Bit#(22) x = d.addr[`Addr_space-1:2];
            $display($time,"	 Main Mem : Received single transaction request from D-cache READ for address : %h Size : %d sending data : %h data0:%h Line: %h",d.addr, d.burst_size,resp_to_core.data,data0,x);
          end
          else begin // dmem write request.
            Bit#(`Reg_width) new_data=0;
            resp_to_core.command= WRITE;
            resp_to_core.transaction_id = d.transaction_id; 
            resp_to_core.status = SUCCESS;
            if( d.burst_size=='d3)begin // word transfer
              if(d.addr[1:0] ==0)
                new_data = d.data[31:0];
								resp_to_core.status = SUCCESS;
            end
            else if ( d.burst_size=='d1)begin // half_word
              resp_to_core.status = SUCCESS;
               if(d.addr[1:0] ==0)
                  new_data={data0[31:16],d.data[15:0]}; 
               else if(d.addr[1:0] ==2)
                  new_data={d.data[15:0],data0[15:0]}; 
            end
            else if ( d.burst_size=='d0)begin // one byte
              resp_to_core.status = SUCCESS;
               if(d.addr[1:0] ==0)
                  new_data={data0[31:8],d.data[7:0]}; 
               else if(d.addr[1:0] ==1)
                  new_data={data0[31:16],d.data[7:0],data0[7:0]}; 
               else if(d.addr[1:0] ==2)
                  new_data={data0[31:24],d.data[7:0],data0[15:0]}; 
               else if(d.addr[1:0] ==3)
                  new_data={d.data[7:0],data0[23:0]}; 
            end
            else begin
              resp_to_core.status = ERROR;
            end
            rsp_to_core.enq(resp_to_core);
            // Enqueueing Response			
            dmem.upd(d.addr[`Addr_space-1:2],new_data);
            Bit#(22) x = d.addr[`Addr_space-1:2];
            $display($time," Main Mem : Received request from D-cache Write for address : %h Size : %d sending data : %h Line: %h",d.addr, d.burst_size,new_data,x);
          end
        end
      	endrule

				interface TLMRecvIFC  intfc_rcv = toRecvIFC (req_from_core,rsp_to_core); // toRecvIFC function is in Utils.bsv//it returns interface
				method Action flush_from_proc(Bool flush);// recieves any bus error generated during a write/read operation to memory
					procflush<=flush;
				endmethod

    endmodule  
	
endpackage
