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
