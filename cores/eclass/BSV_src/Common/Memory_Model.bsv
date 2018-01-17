/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Names : Abhinaya Agrawal
Email ID : agrawal.abhinaya@gmail.com
*/

package Memory_Model;

import RegFile       :: *;
import TLM2          :: *;
import Utils         :: *;
import FIFOF         :: *;
import SpecialFIFOs  :: *;
import DefaultValue  :: *;
import Vector        :: *;
import Req_Rsp       :: *;
import ISA_Defs      :: *;
import Sys_Configs   :: *;
import BRAMCore      :: *;
import TypeDefs      :: *;

interface Memory_IFC#(numeric type addr_sz);
   interface TLMRecvIFC #(Req_MEM, Rsp_MEM) bus_ifc;
endinterface

typedef Bit#(TDiv#(XLEN, 8)) DataByteEn;

function DataByteEn toDataByteEn(Bit#(2) size);
    return unpack(case (size)
            0:       4'b0001;
            1:       4'b0011;
            3:       4'b1111;
            // D is illegal
            default: 4'b0000;
        endcase);
endfunction

function DataByteEn toPermutedDataByteEn(Bit#(2) size, Bit#(2) addrLSB);
    return toDataByteEn(size) << addrLSB;
endfunction

module mkMemory_Model #(String mem_init_file, Addr mem_base_address)(Memory_IFC#(addr_sz))
   provisos(Alias#(Bit#(TSub#(addr_sz, 2)), internalAddr), Add#(addr_sz, __a, 34));

FIFOF #(Req_MEM) ff_reqs <- mkBypassFIFOF;
FIFOF #(Rsp_MEM) ff_rsps <- mkBypassFIFOF;

Reg#(FetchState) rg_mem_state[2] <- mkCReg(2, REQ);

BRAM_PORT_BE#(internalAddr, Data, TDiv#(SizeOf#(Data), 8)) memory <- mkBRAMCore1BELoad(valueof(TExp#(TSub#(addr_sz, 2))), False, mem_init_file, False);

rule rl_process_requests(rg_mem_state[1] == REQ);
   Req_MEM request = ff_reqs.first; //ff_reqs.deq;
   Addr offAddr = ?;
   internalAddr address = ?;

   if (request matches tagged Descriptor .r) begin
      //$display($time, " MEMORY: Request recieved for address: %h", r.addr);
      offAddr = r.addr - mem_base_address;
      offAddr = offAddr >> 2;
      address = truncate(offAddr);
      if (r.command == READ) begin
         memory.put(0, address, ?);
      end
      else if (r.command == WRITE) begin
         let writeen = toPermutedDataByteEn(r.burst_size, r.addr[1:0]);
         let alignment = {r.addr[1:0], 3'b0};
         let data = r.data << alignment;
         memory.put(writeen, address, data);
      end
   end
   rg_mem_state[1] <= RESP;
endrule

rule rl_receive_bram_response(rg_mem_state[0] == RESP);
   Data data = memory.read;
   let request = ff_reqs.first; ff_reqs.deq;
   Rsp_MEM rsp = defaultValue;
   Bit#(6) rsize = 0;
   if (request matches tagged Descriptor .r) begin
      let alignment = r.addr[1:0];
      rsp.transaction_id = r.transaction_id;
      rsp.command = r.command;
      rsp.status = SUCCESS;
      rsp.data = data;
      // The following code snippet can be used to return the requested part of the word instead of the entire word
      //rsize = (zeroExtend(r.burst_size) << 3) + 8;
      //if (r.command == READ) begin
      // if (alignment == 2'b01) data = data >> 8;
      // if (alignment == 2'b10) data = data >> 16;
      // if (alignment == 2'b11) data = data >> 24;
      // rsp.data = data[rsize-1:0];
      //end
      //$display($time, " MEMORY: Response Addr: %h, data: %h, burst_size: %h", r.addr, rsp.data, r.burst_size);
   end
   rg_mem_state[0] <= REQ;
   ff_rsps.enq (rsp);
endrule

interface bus_ifc = toRecvIFC (ff_reqs, ff_rsps);

endmodule

endpackage
