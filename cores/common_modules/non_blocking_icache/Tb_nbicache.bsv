/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Names : Neel Gala
Email ID : neelgala@gmail.com
*/
package Tb_nbicache;
import nbicache::*;
import All_types::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
(*synthesize*)
//(*conflict_free="send_request_from_cpu,abandon"*)
module mkTb_nbicache(Empty);
  Ifc_nbicache#(32,4,4,8,512,16) icache<-mknbicache("ICACHE");
  Reg#(Bit#(32)) rg_address <-mkReg(0);
//  Reg#(Bit#(512)) rg_test_data <-mkReg('hbbbbbbbbbbbbbbbbaaaaaaaaaaaaaaaaccccccccccccccccbabebabedeaddeadddddddddddddddddeeeeeeeeeeeeeeeeffffffffffffffff1111111111111111);
  Reg#(Bit#(256)) rg_test_data <-mkReg('hddddddddffffffffeeeeeeeebbbbbbbbaaaaaaaacccccccc1111111122222222);
  FIFO#(From_Memory#(32,4,8)) ff_memory_response <-mkFIFO;

  Reg#(Bit#(32)) rg_cnt <-mkReg(0);
  Wire#(Bool) clear <-mkDWire(False);

  rule send_request_from_cpu(!clear);
    icache.request_from_cpu(From_Cpu{address:rg_address,cache_enable:1'b1,transfer_size:'b10,prediction:Not_Taken});
 //   if(rg_address=='hc)
 //     rg_address<='h20;
 //   else if(rg_address=='h20)
 //     rg_address<='h4;
 //   else
      rg_address<=rg_address+4;
  endrule

  rule send_flush;
    let x<-$stime;
    x=x/10;
    if(x==15)
      clear<=True;
  endrule
/*
  rule abandon(clear);
      $display("CLEARING ALL REQUESTS");
      icache.abandon_cycle();
      ff_memory_response.clear();
      rg_address<=0;
  endrule
*/
  rule read_response_to_cpu;
    let x = icache.response_to_cpu();
    icache.response_deqResult();
    $display("Response to cpu is %h Prediction is :%s",x.data_word,fshow(x.prediction),$time);
  endrule

  rule trial;
    $display("\n\n");
  endrule

  rule read_request_to_memory(!clear && rg_cnt==0);
    let x<- icache.request_to_memory;
    $display("MEM: Recieved request for address: %h",x.address,$time);
    ff_memory_response.enq(From_Memory{bus_error:0,data_line:rg_test_data,address:x.address});
    rg_test_data<=rg_test_data+1;
    rg_cnt<=1;
  endrule

  rule send_response_from_memory(!clear && rg_cnt!=0);
    if(rg_cnt==3)begin
      icache.response_from_memory(ff_memory_response.first());
      ff_memory_response.deq();
      rg_cnt<=0;
    end
    else
      rg_cnt<=rg_cnt+1;
  endrule

  rule terminate;
    let x<-$stime;
    x=x/10;
    if(x==600)
      $finish(2);
  endrule

endmodule
endpackage
