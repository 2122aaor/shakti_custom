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
package Tb_icache;
  import icache::*;
  import ClientServer ::*;
  import GetPut       ::*;
  import Connectable  ::*;
  import FIFO ::*;
  (*synthesize*)
  module mkTb_icache(Empty);
    Ifc_icache#(4,8,4,512) icache <-mkicache("ICACHE");
    Reg#(Bit#(32)) rg_address <-mkReg(0);

    rule send_request_to_cache;
      let x<-icache.to_cpu.response.get();
      if(x.status==Idle)begin
        $display("cache is idle",$time);
        icache.to_cpu.request.put(From_Cpu{address:rg_address,transfer_size:0,cache_enable:1});
      end
      else if(x.status==Busy)begin
        $display("cache is busy",$time);
      end
      else if(x.status==Available)begin
        $display("cache is Available. Data : %d",x.data_word,$time);
        rg_address<=rg_address+1;
      end
    endrule

    rule terminate;
      let x<-$stime;
      x=x/10;
      $display("\n\nClock %d",x);
      if(x==520)
        $finish(2);
    endrule
  endmodule

endpackage
