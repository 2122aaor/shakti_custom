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
package  ICache;
  import BRAMCore     ::*;
  import ClientServer ::*;
  import GetPut       ::*;
  import Connectable  ::*;
  import FIFO         ::*;
  import FIFOF        ::*;
  import SpecialFIFOs ::*;
  import BRAM         ::*;
  import Vector       ::*;
  import DReg         ::*;
  import ISA_Defs     ::*;
  import TypeDefs     :: *;


`define ICACHE_ADDR 32
`define ICACHE_WAYS 4
`define ICACHE_BLOCK_SIZE 8
`define ICACHE_WORD_SIZE 4
`define ICACHE_SETS 512

typedef enum {Idle,Stall,Handling_Request,Handling_Memory} Cache_State deriving (Bits,Eq);

  interface Ifc_set_associative_cache#(numeric type addr_width,numeric type ways, numeric type word_size, numeric type block_size, numeric type sets);
    method Action request_from_cpu(From_Cpu#(addr_width) req);
    method ActionValue#(To_Cpu#(addr_width,word_size)) response_to_cpu;
    method ActionValue#(To_Memory#(addr_width)) request_to_memory;
    method ActionValue#(Bool) response_from_memory(From_Memory#(addr_width,word_size) resp);
    method Action cache_enable(Bool enable_);
    method Action flush();
  endinterface

  module mkset_associative_cache#(parameter String name)(Ifc_set_associative_cache#(_addr_width,_ways,_word_size,_block_size,_sets))
   provisos(
      Log#(_word_size,log_word_size),
      Log#(_block_size,log_block_size),
      Log#(_sets,log_sets),
      Add#(intermediate2,log_sets,_addr_width),
      Add#(intermediate3,log_word_size,intermediate2),
      Add#(num_of_tag_bits,log_block_size,intermediate3),
      Add#(log_word_size,log_block_size,num_of_offset_bits),

      Add#(a__, TAdd#(num_of_offset_bits, 1), _addr_width),
      Add#(b__, TMul#(TMul#(8, _word_size), 1), TMul#(8, TMul#(_word_size, _block_size))),
    Add#(TMul#(8, _word_size), c__, TMul#(8, TMul#(_word_size, _block_size)))
);

    let v_ways=valueOf(_ways);
    let v_sets=valueOf(_sets);
    let v_num_of_tag_bits=valueOf(num_of_tag_bits);
    let v_num_of_offset_bits=valueOf(num_of_offset_bits);
    let v_word_size=valueOf(_word_size);
    let v_block_size=valueOf(_block_size);
    let v_addr_width=valueOf(_addr_width);
    let v_num_of_bytes=valueOf(TLog#(_word_size)); // number of bits to represent each byte in a word

   function ActionValue#(Integer) pseudo_lru_policy(Array#(Reg#(Bit#(TSub#(_ways,1)))) pseudo_lru_bits,Bit#(_addr_width) cpu_address, Integer matched_tag, Array#(Bit#(1)) valid_values)=
         actionvalue
      Integer replace_block=-1;                   // initialize to a non-existent block
      Bit#(TLog#(_sets)) set=cpu_address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits];
      Bit#(TSub#(_ways,1)) lru_bits = pseudo_lru_bits[set];          // read current lru bits.
      Integer block_to_replace=0;
      for(Integer i=v_ways-1;i>=0;i=i-1)           // find if all the blocks are valid or not. If not chose one to replace
          if(valid_values[i]==0)begin
            replace_block=i;
            block_to_replace=i;                      // this is the block which will be accessed. This is used to update the lru bits.
          end
      if(replace_block==-1)begin                  // if all blocks are valid.
        // Ways = n
        // number of bits in lru = n-1
        // index is from 0 to n-2
        Integer left=0;
        Integer right=0;
        Integer i=0;
        while(i<(v_ways-1))begin
          left=i+1;
          right=i+2;
          if(lru_bits[v_ways-2-i]==0)
            i=i+left;
          else
            i=i+right;
        end
         block_to_replace=fromInteger(i-v_ways+1);
      end
         if(matched_tag!=-1)
            block_to_replace=matched_tag;

      Integer m=v_ways-1+block_to_replace;
      Integer n=0;
      while(m>0)begin
          if(m%2==1)begin
              n=(m-1)/2;
              if(n<v_ways)
                  lru_bits[v_ways-2-n]=1;
          end
          else begin
              n=(m-2)/2;
              if(n<v_ways)
                  lru_bits[v_ways-2-n]=0;
          end
          m=n;
      end
      pseudo_lru_bits[set]._write(lru_bits); // update the LRU bits after the access is made
         return block_to_replace;
   endactionvalue;

    FIFOF#(To_Cpu#(_addr_width, _word_size)) ff_response_to_cpu <-mkSizedBypassFIFOF(v_block_size);
    FIFO#(From_Cpu#(_addr_width)) ff_request_from_cpu <-mkLFIFO(); // using FIFO of the same size as BRAM Output FIFO depth.
    FIFOF#(To_Memory#(_addr_width)) ff_request_to_memory <-mkSizedBypassFIFOF(1);
    FIFOF#(From_Memory#(_addr_width,_word_size)) ff_response_from_memory <-mkLFIFOF();
      Wire#(Bool) stall_request_from_cpu <-mkDWire(False);
      Wire#(Bool) wr_line_done<-mkDWire(False);

      Reg#(Cache_State) rg_state[3] <-mkCReg(3,Idle);

    Reg#(Bit#(TAdd#(1,TLog#(_sets)))) rg_index <-mkReg(0);
    Reg#(Bool) rg_initialize<-mkReg(True);
    Reg#(Bit#(TLog#(_ways))) rg_replace_block <-mkReg(0);
    Reg#(Bool) rg_enable <-mkReg(True);
    Wire#(Bool) wr_flush <-mkDWire(False);
    Reg#(Bool) rg_flush <-mkReg(False);

    Reg#(Bit#(TLog#(_block_size))) rg_word_count <-mkReg(0);
      Reg#(Bit#(TSub#(num_of_offset_bits,TLog#(_word_size)))) rg_offset <-mkReg(0);

      BRAM_DUAL_PORT#(Bit#(TLog#(_sets)),Bit#(num_of_tag_bits)) tag [v_ways];
      BRAM_DUAL_PORT#(Bit#(TLog#(_sets)),Bit#(1)) valid [v_ways];
      BRAM_DUAL_PORT#(Bit#(TLog#(_sets)),Bit#(TMul#(TMul#(8,_word_size),1))) data [v_ways][v_block_size];

    Reg#(Bit#(TSub#(_ways,1))) pseudo_lru [v_sets];
    for(Integer i=0;i<v_sets;i=i+1)
      pseudo_lru[i]<-mkReg(0);
    
    for(Integer i=0;i<v_ways;i=i+1) begin
      tag[i] <- mkBRAMCore2(v_sets,False);      
         for(Integer j=0;j<v_block_size;j=j+1)
         data[i][j] <-mkBRAMCore2(v_sets,False);
      valid[i]<-mkBRAMCore2(v_sets,False);
    end

    rule initialize_cache(rg_initialize);
      rg_index<=rg_index+1;
         for(Integer i=0;i<v_ways;i=i+1)
            valid[i].a.put(True,truncate(rg_index),0);
         if(rg_index==fromInteger(v_sets-1))
         rg_initialize<=False;
//    $display($time,"\t",name,"\tFlushing Cache");
    endrule
    rule cache_is_disabled(!rg_enable);
        ff_request_to_memory.enq(To_Memory {
                                     address:ff_request_from_cpu.first().address,
                                 burst_length:1,
                                 ld_st:Load});
    endrule

    rule read_from_bram(!rg_initialize && rg_enable && rg_state[0]==Handling_Request);
      ff_request_from_cpu.deq();
      Bit#(num_of_tag_bits) tag_values[v_ways]; // hold the tag values
      Bit#(1) valid_values [v_ways];      // hold the valid and dirty bits
      Bit#(TMul#(8,TMul#(_word_size,1))) data_values [v_ways][v_block_size]; // hold the cache lines.
      let cpu_addr=ff_request_from_cpu.first.address;
         for(Integer i=0;i<v_ways;i=i+1)begin
            valid_values[i]=valid[i].a.read();
            tag_values[i]=tag[i].a.read();
            for(Integer j=0;j<v_block_size;j=j+1)
               data_values[i][j]=data[i][j].a.read();
         end
         rg_offset<=cpu_addr[v_num_of_offset_bits-1:v_num_of_bytes];
         Integer matched_tag=-1;
      Bit#(_addr_width) address=cpu_addr;
         for(Integer i=0; i<v_ways; i=i+1)begin
            if(valid_values[i]==1'b1 && tag_values[i]==cpu_addr[v_addr_width-1:v_addr_width-v_num_of_tag_bits])
               matched_tag=i; // here this variable indicates which tags show a match.
         end

         Integer block_to_replace <- pseudo_lru_policy(pseudo_lru,cpu_addr,matched_tag,valid_values);


      if(cpu_addr[1:0]!=0)begin // miss-aligned error.
        ff_response_to_cpu.enq(To_Cpu {address:cpu_addr,data_word:0,bus_error:0,misaligned_error:1});
            rg_state[0]<=Idle;
      end
      else if(matched_tag!=-1)begin// on a hit return the required data to the completion buffer.
            Bit#(TSub#(num_of_offset_bits,TLog#(_word_size))) offset=cpu_addr[v_num_of_offset_bits-1:v_num_of_bytes];
        $display($time,"\t",name,"\tHit for address : %h Line: %h",cpu_addr,data_values[matched_tag][offset]);
        ff_response_to_cpu.enq(To_Cpu {address:cpu_addr,data_word:data_values[matched_tag][offset],bus_error:0,misaligned_error:0});
            rg_state[0]<=Idle;
      ///////////////////////////////////////////////////////////////////////////// Find new LRU bits /////////////////////////////////////////////////////////////////////////////////////////////
      end
      else begin
        $display($time,"\t",name,"\tMiss for address : %h",cpu_addr);
        ff_request_to_memory.enq(To_Memory {address:cpu_addr/* & signExtend(lower_zeros)*/,burst_length:fromInteger(v_block_size),ld_st:Load});
        rg_replace_block<=fromInteger(block_to_replace);
            rg_state[0]<=Handling_Memory;
      end
    endrule

    rule forward_from_memory_to_cpu(!wr_flush && rg_state[1]==Handling_Memory);
      let resp=ff_response_from_memory.first();
      if(resp.address[v_num_of_offset_bits-1:v_num_of_bytes]>=rg_offset && !rg_flush)begin
        $display($time,"\t",name,"\tEnquing in output FIFO with address: %h data: %h",resp.address,resp.data_line);
        ff_response_to_cpu.enq(To_Cpu{address:resp.address,data_word:resp.data_line,bus_error:resp.bus_error,misaligned_error:0});
      end
    endrule

    rule flush_response_cache(wr_flush);
      ff_response_to_cpu.clear();
      rg_flush<=True;
    endrule

    rule got_response_from_memory(!rg_initialize && rg_state[1]==Handling_Memory);
      let resp=ff_response_from_memory.first();
      ff_response_from_memory.deq();
      Bit#(TSub#(_addr_width,num_of_offset_bits)) input_tag=resp.address[v_addr_width-1:v_num_of_offset_bits];
      $display($time,"\t",name,"\tRecieved response from the memory. Address: :%h Tag : %h Data: %h ",resp.address,input_tag,resp.data_line);

      if(rg_enable)begin
            Bit#(TSub#(num_of_offset_bits,TLog#(_word_size))) offset=resp.address[v_num_of_offset_bits-1:v_num_of_bytes];
        data[rg_replace_block][offset].b.put(True,resp.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],resp.data_line);
            if(rg_word_count==fromInteger(v_block_size-2))
               wr_line_done<=True;
        if(rg_word_count==fromInteger(v_block_size-1))begin
          rg_word_count<=0;
          tag[rg_replace_block].b.put(True,resp.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],resp.address[v_addr_width-1:v_addr_width-v_num_of_tag_bits]);
          valid[rg_replace_block].b.put(True,resp.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],1);
               rg_state[1]<=Stall;
        end
        else begin
          rg_word_count<=rg_word_count+1;
        end
      end
    endrule

      rule stall_for_write_to_take_effect(rg_state[1]==Stall);
         rg_state[1]<=Idle;
      endrule

    method Action request_from_cpu (From_Cpu#(_addr_width) req)if(!rg_initialize && rg_state[2]==Idle && !ff_response_to_cpu.notEmpty && !wr_flush);
         if(rg_enable)
        for(Integer i=0;i<v_ways;i=i+1)begin // send address to the Block_rams
            tag[i].a.put(   False,req.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], ?);
                  for(Integer j=0;j<v_block_size;j=j+1)
               data[i][j].a.put( False,req.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], ?);
            valid[i].a.put(False,req.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], ?);
        end
      rg_flush<=False;
         rg_state[2]<=Handling_Request;
      ff_request_from_cpu.enq(req); // enqueue the request for next stage
      Bit#(num_of_tag_bits) tag1=req.address[v_addr_width-1:v_num_of_tag_bits];
      Bit#(TLog#(_sets)) set1=req.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits];
      Bit#(num_of_offset_bits) off1=req.address[v_num_of_offset_bits-1:0];
      $display("\n",$time,"\t",name,"\tBRAM: recieved request for Address :%h tag %d: Set : %d Offset :%d",req.address,tag1,set1,off1);
    endmethod

    method ActionValue#(To_Cpu#(_addr_width,_word_size)) response_to_cpu if(!rg_initialize && !wr_flush);
      ff_response_to_cpu.deq();
      return ff_response_to_cpu.first();
    endmethod
    
    method ActionValue#(To_Memory#(_addr_width)) request_to_memory if(!rg_initialize) ;
          ff_request_to_memory.deq;
      return ff_request_to_memory.first();
    endmethod

    method ActionValue#(Bool) response_from_memory(From_Memory#(_addr_width,_word_size) resp);
      ff_response_from_memory.enq(resp);
         return wr_line_done;
    endmethod
    
    method Action cache_enable(Bool enable_);
      rg_enable<=enable_;
    endmethod

    // This is a really important method. This method is fired when there is branch misprediction or
    // a trap has been taken. There are 4 scenarios which can happen:
    // 1. The flush occurs at the same time when a wrong is address is requested. This is taken care
    //    at the core level by not firing the request rule.
    // 2. The flush can occur when unwanted data has alredy been fetched from BRAM due to a "hit" and entered
    //    in to the ff_response_to_cpu FIFO.
    // 3. The flush can happen when the unwated access is just being sent to external memory for fetch. In this case the
    //    fetched data can be store in the cache but should not be used by the core.
    // 4. The flush can occurs during the line fetch duration. The rg_flush register ensures that
    //    nothing is being sent to the ff_response_to_cpu FIFO untill a new request is taken
    method Action flush();
      wr_flush<=True;
    endmethod

  endmodule
  
  interface Ifc_icache;
    method Action request_from_cpu(From_Cpu#(`ICACHE_ADDR) req);
    method ActionValue#(To_Cpu#(`ICACHE_ADDR,`ICACHE_WORD_SIZE)) response_to_cpu;
    method ActionValue#(To_Memory#(`ICACHE_ADDR)) request_to_memory;
    method ActionValue#(Bool) response_from_memory(From_Memory#(`ICACHE_ADDR,`ICACHE_WORD_SIZE) resp);
    method Action cache_enable(Bool enable_);
    method Action flush();
  endinterface

  (*synthesize*)
  module mkicache(Ifc_icache);
    
    Ifc_set_associative_cache#(`ICACHE_ADDR,`ICACHE_WAYS,`ICACHE_WORD_SIZE,`ICACHE_BLOCK_SIZE,`ICACHE_SETS) cache <-mkset_associative_cache("ICACHE");
    method Action request_from_cpu(From_Cpu#(32) req);
      cache.request_from_cpu(req);
    endmethod

    method ActionValue#(To_Cpu#(`ICACHE_ADDR,`ICACHE_WORD_SIZE)) response_to_cpu = cache.response_to_cpu;
    method ActionValue#(To_Memory#(`ICACHE_ADDR)) request_to_memory = cache.request_to_memory;
    method ActionValue#(Bool) response_from_memory(From_Memory#(`ICACHE_ADDR,`ICACHE_WORD_SIZE) resp);
      let x<-cache.response_from_memory(resp);
         return x;
    endmethod
    method Action cache_enable(Bool enable_);
      cache.cache_enable(enable_);
    endmethod
    method Action flush();
      cache.flush;
    endmethod

  endmodule
endpackage 
