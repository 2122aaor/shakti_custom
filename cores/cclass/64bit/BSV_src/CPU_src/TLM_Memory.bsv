
package TLM_Memory;
  import core::*;
	import Connectable::*;
	import RegFile::*;
	import defined_types::*;
  //import BRAMCore :: *;
  import BRAM::*;
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
      //BRAM instantiated as dmem
      //BRAM_DUAL_PORT#(Bit#(TSub#(`Addr_space,2)),Bit#(32)) dmem <- mkBRAMCore2Load(65536,False,"code.hex",False);
      BRAM_Configure cfg = defaultValue;
      cfg.allowWriteResponseBypass = True;
      cfg.loadFormat= tagged Hex "code.hex";
      cfg.latency=1;
      cfg.outFIFODepth=2;
      BRAM2Port#(Bit#(TSub#(`Addr_space,3)),Bit#(`Reg_width)) dmem <- mkBRAM2Server(cfg);
      Reg#(Bool) rg_state <-mkReg(False);

      // Information contained in TLM request packet coming 
      // from C-class core,those are locally buffered in these registered 
      Reg#(Bit#(`Addr_width)) rg_addr <-mkReg(0);
      Reg#(Bit#(1)) rg_rd_wr_to_memory <-mkReg(0);
      Reg#(Bit#(3)) rg_transfer_size <-mkReg(0);
      Reg#(Bit#(`Reg_width)) rg_data <-mkReg(0);
      
      Reg#(Bit#(4)) rg_count <-mkReg(0);
      // for checking ID of transaction
      Reg#(Bit#(4)) rg_id <-mkReg(0);
      Reg#(Bit#(32)) rg_clock<-mkReg(0);
      // FIFO buffering of incoming and outgoing TLM packets
      FIFOF#(Req_tlm) req_from_core <- mkSizedBypassFIFOF(1);
      FIFOF#(Rsp_tlm) rsp_to_core <- mkSizedBypassFIFOF(1);
      // FLush input from Core
      Wire#(Bool) procflush<-mkDWire(False);
      rule rl_clock;
        rg_clock<=rg_clock + 1;
      endrule

      rule get_request(!procflush);
				if (req_from_core.first matches tagged Descriptor .d) begin
					//After taking request we dequeue so that next request comes to FIRST in FIFO
					req_from_core.deq;
					//Expected ID for next Request Packet is updated
					rg_count <= rg_count + 1;
					// All parameters are copied to rg_???
					rg_addr <= d.addr;
					dmem.portA.request.put(BRAMRequest{write:False,address:d.addr[`Addr_space-1:3],datain:0,responseOnWrite:False});
					rg_data<=d.data;
					rg_rd_wr_to_memory <= (d.command == READ) ? 0 : 1;
					rg_transfer_size<=d.burst_size;
					rg_id<=d.transaction_id;
						
					$display($time,"	Got the request from the core addr: %h",d.addr);
				end
      endrule

  
      rule read_request_from_proc;
      	let data0 <- dmem.portA.response.get();
        // Making A response TLM packet
				Rsp_tlm resp_to_core = ?;
        if(rg_rd_wr_to_memory==0)begin // dmem read request.
					resp_to_core.command= READ;
					resp_to_core.transaction_id = rg_id;
            if(rg_transfer_size=='d7) // double word transfer
              resp_to_core.data = (data0);
            else if(rg_transfer_size=='d3)begin // word transfer
              if(rg_addr[2:0] ==0)
                resp_to_core.data = (zeroExtend(data0[31:0]));
              else if(rg_addr[2:0] ==4)
                resp_to_core.data = (zeroExtend(data0[63:32]));
            end
            else if (rg_transfer_size=='d1)begin // half_word
              if(rg_addr[2:0] ==0)
                resp_to_core.data = (zeroExtend(data0[15:0]));
              else if(rg_addr[2:0] ==2)
                resp_to_core.data = (zeroExtend(data0[31:16]));
              else if(rg_addr[2:0] ==4)
                resp_to_core.data = (zeroExtend(data0[47:32]));
              else if(rg_addr[2:0] ==6)
                resp_to_core.data = (zeroExtend(data0[63:48]));
            end
            else if (rg_transfer_size=='d0) begin// one byte
              if(rg_addr[2:0] ==0)
                resp_to_core.data = (zeroExtend(data0[7:0]));
              else if(rg_addr[2:0] ==1)
                resp_to_core.data = (zeroExtend(data0[15:8]));
              else if(rg_addr[2:0] ==2)
                resp_to_core.data = (zeroExtend(data0[23:16]));
              else if(rg_addr[2:0] ==3)
                resp_to_core.data = (zeroExtend(data0[31:24]));
              else if(rg_addr[2:0] ==4)
                resp_to_core.data = (zeroExtend(data0[39:32]));
              else if(rg_addr[2:0] ==5)
                resp_to_core.data = (zeroExtend(data0[47:40]));
              else if(rg_addr[2:0] ==6)
                resp_to_core.data = (zeroExtend(data0[55:48]));
              else if(rg_addr[2:0] ==7)
                resp_to_core.data = (zeroExtend(data0[63:56]));
            end
					resp_to_core.status = SUCCESS;
					// Enqueueing Response
					rsp_to_core.enq(resp_to_core);
					$display($time,"	 Main Mem : Received single transaction request from D-cache READ for address : %h Size : %d sending data : %h data0:%h",rg_addr, rg_transfer_size,resp_to_core.data,data0,$time);
        end
				else if(rg_rd_wr_to_memory==1)begin // dmem write request.
					Bit#(`Reg_width) new_data=0;
					resp_to_core.command= WRITE;
					resp_to_core.transaction_id = rg_id; 
					resp_to_core.status = SUCCESS;
            if( rg_transfer_size=='d3)begin // word transfer
              if(rg_addr[2:0] ==0)
                new_data = {data0[63:32],rg_data[31:0]};
              else if(rg_addr[2:0] ==4)
                new_data={rg_data[31:0],data0[31:0]};
              resp_to_core.status = SUCCESS;
            end
            else if ( rg_transfer_size=='d1)begin // half_word
              resp_to_core.status = SUCCESS;
               if(rg_addr[2:0] ==0)
                  new_data={data0[63:16],rg_data[15:0]}; 
               else if(rg_addr[2:0] ==2)
                  new_data={data0[63:32],rg_data[15:0],data0[15:0]}; 
               else if(rg_addr[2:0] ==4)
                  new_data={data0[63:48],rg_data[15:0],data0[31:0]}; 
               else if(rg_addr[2:0] ==6)
                  new_data={rg_data[15:0],data0[47:0]}; 
            end
            else if ( rg_transfer_size=='d0)begin // one byte
              resp_to_core.status = SUCCESS;
               if(rg_addr[2:0] ==0)
                  new_data={data0[63:8],rg_data[7:0]}; 
               else if(rg_addr[2:0] ==1)
                  new_data={data0[63:16],rg_data[7:0],data0[7:0]}; 
               else if(rg_addr[2:0] ==2)
                  new_data={data0[63:24],rg_data[7:0],data0[15:0]}; 
               else if(rg_addr[2:0] ==3)
                  new_data={data0[63:32],rg_data[7:0],data0[23:0]}; 
               else if(rg_addr[2:0] ==4)
                  new_data={data0[63:40],rg_data[7:0],data0[31:0]}; 
               else if(rg_addr[2:0] ==5)
                  new_data={data0[63:48],rg_data[7:0],data0[39:0]}; 
               else if(rg_addr[2:0] ==6)
                  new_data={data0[63:56],rg_data[7:0],data0[47:0]}; 
               else if(rg_addr[2:0] ==7)
                  new_data={rg_data[7:0],data0[55:0]}; 
            end
					else begin
						resp_to_core.status = ERROR;
					end
					rsp_to_core.enq(resp_to_core);
					// Enqueueing Response			
					dmem.portB.request.put(BRAMRequest{write:True,address:rg_addr[`Addr_space-1:3],datain:new_data,responseOnWrite:False});
					$display($time," Main Mem : Received request from D-cache Write for address : %h Size : %d sending data : %h",rg_addr, rg_transfer_size,new_data);
				end
      	endrule

				interface TLMRecvIFC  intfc_rcv = toRecvIFC (req_from_core,rsp_to_core); // toRecvIFC function is in Utils.bsv//it returns interface
				method Action flush_from_proc(Bool flush);// recieves any bus error generated during a write/read operation to memory
					procflush<=flush;
				endmethod

    endmodule  
/*	
		interface Intfc_top;
      method    Action      sin(Bit#(1) in);
      method    Bit#(1)     sout();
    endinterface
    //This top module directly connects core and memory w/o AHB
		module mkTb_core(Intfc_top);
			Wire#(Bool) wr_flush<-mkDWire(False);
			Ifc_core_copy proc <-mkcore_copy();
			Ifc_BRAM bram_mod <- mkTLM_Memory();
			mkConnection(proc.intfc,bram_mod.intfc_rcv);  //from Connectables
		
			//rule for connecting flush signal which can also be incorporated in TLM Data
			rule read_flush_signal_from_cpu;
				bram_mod.flush_from_proc(proc.flush);
			endrule
		
			method    Action      sin(Bit#(1) in);
				proc.sin(in);
			endmethod
			method    Bit#(1)     sout();
				return proc.sout;
			endmethod     
		endmodule 
    */
endpackage
