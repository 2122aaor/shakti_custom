package Tb_nbdcache;
import nbdcache::*;
import All_types_d::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
  `define way 4
  `define word_size 4
  `define block_size 8
  `define sets 512
  `define cbuff 16
  `define addr_width 32
(*synthesize*)
//(*conflict_free="send_request_from_cpu,abandon"*)
module mkTb_nbdcache(Empty);
  Ifc_nbdcache#(`addr_width,`way,`word_size,`block_size,`sets,`cbuff) dcache<-mknbdcache("DCACHE");
  Reg#(Bit#(32)) rg_address <-mkReg(0);
  Reg#(Bit#(TMul#(8,TMul#(`word_size,`block_size)))) rg_test_data <-mkReg(truncate(256'hddddddddffffffffeeeeeeeebbbbbbbbaaaaaaaacccccccc1111111122222222));
  FIFO#(From_Memory_d#(`addr_width,`word_size,`block_size)) ff_memory_response <-mkFIFO;
  Reg#(Access_type_d) rg_access_type <-mkReg(Load);
  Reg#(Bit#(TMul#(8,`word_size))) rg_data <-mkReg('h77777777);

  Reg#(Bit#(32)) rg_cnt <-mkReg(0);
  Wire#(Bool) clear <-mkDWire(False);

  rule send_request_from_cpu(!clear);
    let x<-$stime;
    dcache.request_from_cpu(From_Cpu_d{address:rg_address,cache_enable:1'b1,transfer_size:'b10, ld_st:rg_access_type, write_data:rg_data});

//    if(rg_address=='h8)
//      rg_address<='h0;
//    else
      rg_address<=rg_address+4;

    if(rg_address=='h4 && x/10<516)
      rg_access_type<=Store;
    else
      rg_access_type<=Load;
  endrule
//
//  rule send_flush;
//    let x<-$stime;
//    x=x/10;
//    if(x==15)
//      clear<=True;
//  endrule
//
//  rule abandon(clear);
//      $display("CLEARING ALL REQUESTS");
//      dcache.abandon_cycle();
//      ff_memory_response.clear();
//      rg_address<=0;
//      rg_cnt<=0;
//  endrule
//
  rule read_response_to_cpu;
    let x = dcache.response_to_cpu();
    dcache.response_deqResult();
    $display("Response to cpu is %h :",x.data_word,$time);
  endrule

  rule trial;
    let x<-$stime;
    if(x/10>512)
      $display("\n\n");
  endrule

  rule read_request_to_memory(!clear && rg_cnt==0);
    let x<- dcache.request_to_memory;
    $display("MEM: Recieved request for address: %h",x.address,$time);
    ff_memory_response.enq(From_Memory_d{bus_error:0,data_line:rg_test_data,address:x.address});
    rg_cnt<=1;
    rg_test_data<=rg_test_data+1;
  endrule

  rule send_response_from_memory(!clear && rg_cnt!=0);
    if(rg_cnt==7)begin
      dcache.response_from_memory(ff_memory_response.first());
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
