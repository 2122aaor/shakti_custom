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
package  set_associative_cache;
  import All_types::*;
  import ClientServer ::*;
  import GetPut       ::*;
  import Connectable  ::*;
  import FIFO ::*;
  import FIFOF ::*;
  import SpecialFIFOs::*;
  import BRAM::*;
  import ConfigReg::*;
  //import LoadBuffer_i::*;
  //import CFFIFO::*;
  import Vector::*;
  import DReg::*;

  interface Ifc_set_associative_cache#(numeric type addr_width,numeric type ways, numeric type word_size, numeric type block_size, numeric type sets);
    method Action request_from_cpu(From_Cpu#(addr_width) req);
    method Maybe#(To_Cpu#(addr_width,word_size)) response_to_cpu;
    method ActionValue#(To_Memory#(addr_width,ways,block_size)) request_to_memory;
    method Action response_from_memory(From_Memory#(addr_width,word_size,block_size) resp);
    method Maybe#(To_Cpu#(addr_width,word_size)) response_from_bus;
    method Action cache_enable(Bool enable_);

    method Action clear_all();
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
  	Add#(a1,_word_size,8),							//to limit the word_size that user inputs to max. of 8 bytes (or doubleword)
//    Add#(b__, num_of_tag_bits, 32), // required due to zeroExtend at line 205,186,153,154,157
    Add#(a__, TMul#(_word_size, 8), TMul#(8, TMul#(_word_size, _block_size))),
    Add#(c__, 1, TSub#(_addr_width, TAdd#(TLog#(_block_size),TLog#(_word_size)))),
     Add#(b__, TMul#(8, _word_size), TMul#(8, TMul#(_word_size, _block_size))),
     Add#(28, d__, _addr_width)
);

    let v_ways=valueOf(_ways);
    let v_sets=valueOf(_sets);
    let v_num_of_tag_bits=valueOf(num_of_tag_bits);
    let v_num_of_offset_bits=valueOf(num_of_offset_bits);
    let v_word_size=valueOf(_word_size);
    let v_block_size=valueOf(_block_size);
    let v_addr_width=valueOf(_addr_width);
    let v_num_of_bytes=valueOf(TLog#(_word_size)); // number of bits to represent each byte in a word

  function Tuple2#(Bit#(TLog#(TMul#(8,TMul#(_word_size,_block_size)))),Bit#(TLog#(TMul#(8,TMul#(_word_size,_block_size))))) find_offset_index(Bit#(_addr_width) cpu_addr);
    let v_word_size=valueOf(_word_size);
    Bit#(TLog#(TMul#(8,TMul#(_word_size,_block_size)))) lower_offset=0;
    Bit#(TLog#(TMul#(8,TMul#(_word_size,_block_size)))) upper_offset=0;
    for(Integer i=0;i<v_block_size;i=i+1)begin
      if(fromInteger(i)==unpack(cpu_addr[v_num_of_offset_bits-1:v_num_of_bytes]))begin // the lower order bits used to access each byte.
        lower_offset= fromInteger(v_word_size)*8*fromInteger(i); // calculating the lower bit index value. For. eg if word is 32bit. possible values are 0,32,64,96...
      end
    end
    upper_offset=lower_offset+31;
    return tuple2(upper_offset,lower_offset);
  endfunction

    Wire#(Maybe#(To_Cpu#(_addr_width, _word_size))) wr_response_to_cpu <-mkDWire(tagged Invalid);
    FIFO#(From_Cpu#(_addr_width)) ff_request_from_cpu <-mkLFIFO(); // using FIFO of the same size as BRAM Output FIFO depth.
    FIFOF#(To_Memory#(_addr_width,_ways,_block_size)) ff_request_to_memory <-mkSizedBypassFIFOF(1);
    FIFOF#(From_Memory#(_addr_width,_word_size,_block_size)) ff_response_from_memory <-mkSizedBypassFIFOF(1);

    Reg#(Bit#(TAdd#(1,TLog#(_sets)))) rg_index <-mkReg(0);
    Reg#(Bool) rg_initialize<-mkReg(True);
    Wire#(Bool) stall_processor <-mkDWire(False);
    Reg#(Bit#(TLog#(_ways))) rg_replace_block <-mkReg(0);
    Reg#(Bool) rg_enable <-mkReg(True);
    Wire#(Maybe#(To_Cpu#(_addr_width,_word_size))) wr_response_from_bus <-mkDWire(tagged Invalid);

//    Wire#(Maybe#(From_Memory#(_addr_width,_word_size,_block_size))) wr_from_bus_to_cpu <-mkDWire(tagged Invalid);
    Reg#(Bit#(TMul#(8,TMul#(_word_size,_block_size)))) rg_data_line_frm_memory <-mkReg(0);
    Reg#(Bit#(TLog#(_block_size))) rg_word_count <-mkReg(0);
		
    BRAM_Configure cfg = defaultValue ;
    cfg.latency=1;
    cfg.outFIFODepth=2; // for pipelined cache
    BRAM2Port#(Bit#(TLog#(_sets)), Bit#(num_of_tag_bits)) tag [v_ways]; // declaring as many tag arrays as there are number of `Ways. Depth of each array is the number of sets.
    BRAM2Port#(Bit#(TLog#(_sets)), Bit#(1)) valid [v_ways];     // declaring as many alid bit arrays as there are number of `Ways. Depth of each array is the number of sets.
    BRAM2Port#(Bit#(TLog#(_sets)), Bit#(TMul#(8,TMul#(_word_size,_block_size)))) data [v_ways]; // decalring similar data arrays. each of width equal to block size.
	
    Reg#(Bit#(TSub#(_ways,1))) pseudo_lru [v_sets];
    for(Integer i=0;i<v_sets;i=i+1)
      pseudo_lru[i]<-mkReg(0);
    
    for(Integer i=0;i<v_ways;i=i+1) begin
      tag[i] <- mkBRAM2Server(cfg);		
      data[i] <- mkBRAM2Server(cfg);
      valid[i]<-mkBRAM2Server(cfg);
    end

    rule initialize_cache(rg_initialize);
    	rg_index<=rg_index+1;
      	for(Integer i=0;i<v_ways;i=i+1)
			valid[i].portA.request.put(BRAMRequest{write:True,address:truncate(rg_index), datain:0,responseOnWrite:False});
      	if(rg_index==fromInteger(v_sets-1))
        	rg_initialize<=False;
//		$display($time,"\tFlushing Cache");
    endrule
    rule cache_is_disabled(!rg_enable);
        ff_request_to_memory.enq(To_Memory {
                        				 address:ff_request_from_cpu.first().address,
                                 burst_length:1,
                                 ld_st:Load});
    endrule

    rule read_from_bram_and_load_buffer(!rg_initialize && rg_enable);
      Bit#(num_of_tag_bits) tag_values[v_ways];	// hold the tag values
      Bit#(1) valid_values [v_ways];		// hold the valid and dirty bits
      Bit#(TMul#(8,TMul#(_word_size,_block_size))) data_values [v_ways]; // hold the cache lines.
    	Bit#(TSub#(_addr_width,num_of_offset_bits)) input_tag=ff_request_from_cpu.first.address[v_addr_width-1:v_num_of_offset_bits];
			for(Integer i=0;i<v_ways;i=i+1)begin
				valid_values[i]<-valid[i].portA.response.get();
				tag_values[i]<-tag[i].portA.response.get();
				data_values[i]<-data[i].portA.response.get();
			end
      ///////////////////////////////////////////////// calculate the upper and lower offsets. ////////////////////////////////////////////////////////////
      let cpu_addr=ff_request_from_cpu.first.address;
      match {.upper_offset,.lower_offset}=find_offset_index(cpu_addr);
      ///////////////////////////////////////////////////////////////// find which tag to replace using PLRU ////////////////////////////////////////////////////////////////////////////////
		  $display($time,"\tUpper offset : %d Lower offset :%d for address %h",upper_offset,lower_offset,cpu_addr);
      Integer replace_block=-1;                   // initialize to a non-existent block
      Bit#(TLog#(_sets)) set=cpu_addr[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits];
      Bit#(TSub#(_ways,1)) lru_bits = pseudo_lru[set];          // read current lru bits.
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
        $display($time,"\t%s:performing PLRU",name);
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
      /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
			// declare intermediate variables which will hold the required tag, valid, and cache lines which have been indexed.
			// read the responses from the respective BRAMs.
			Integer matched_tag=-1;
      Bit#(_addr_width) address=ff_request_from_cpu.first.address;
			for(Integer i=0; i<v_ways; i=i+1)begin
				if(valid_values[i]==1'b1 && tag_values[i]==ff_request_from_cpu.first.address[v_addr_width-1:v_addr_width-v_num_of_tag_bits])
					matched_tag=i;	// here this variable indicates which tags show a match.
			end
      if(address[1:0]!=0)begin // miss-aligned error.
        wr_response_to_cpu<= tagged Valid (To_Cpu {address:ff_request_from_cpu.first().address,
            				                  data_word:0,
                                      bus_error:0,
                                      misaligned_error:1 });
        ff_request_from_cpu.deq();

      end
      else if(matched_tag!=-1)begin// on a hit return the required data to the completion buffer.
        $display($time,"\tHit for address : %h Line: %h",ff_request_from_cpu.first.address,data_values[matched_tag]);
        wr_response_to_cpu<= tagged Valid (To_Cpu {address:ff_request_from_cpu.first().address,
			       		                  data_word:data_values[matched_tag][upper_offset:lower_offset],
                               		bus_error:0,
                               		misaligned_error:0});
        ff_request_from_cpu.deq();

      ///////////////////////////////////////////////////////////////////////////// Find new LRU bits /////////////////////////////////////////////////////////////////////////////////////////////
      end
      else begin
        $display($time,"\tMiss for address : %h",ff_request_from_cpu.first.address);
  	    matched_tag = block_to_replace;
        ff_request_to_memory.enq(To_Memory {
                        				 address:{address[31:4],'b0},
                                 burst_length:fromInteger(v_block_size),
                                 ld_st:Load});
        rg_replace_block<=fromInteger(block_to_replace);
      end
      Integer m=v_ways-1+matched_tag;
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
      pseudo_lru[set]<=lru_bits; // update the LRU bits after the access is made
      $display($time,"\tChanged PLRU for set : %d with bits :%b",set,lru_bits);
    endrule


    rule got_response_from_memory(!rg_initialize);
      let resp=ff_response_from_memory.first();
      ff_response_from_memory.deq();
      wr_response_from_bus<= tagged Valid (To_Cpu{address:resp.address,data_word:resp.data_line,bus_error:resp.bus_error,misaligned_error:0});
      Bit#(TSub#(_addr_width,num_of_offset_bits)) input_tag=resp.address[v_addr_width-1:v_num_of_offset_bits];
      $display($time,"\tRecieved response from the memory. Address: :%h Tag : %h Data: %h ",resp.address,input_tag,resp.data_line);
      //let x<-ldbuff.remove(resp.address);
      if(rg_enable)begin
        if(rg_word_count==fromInteger(v_block_size-1))begin
          stall_processor<=True;
          let line=(rg_data_line_frm_memory>>(v_word_size*8))|{resp.data_line,'d0};
          $display($time,"\tWriting line :%h in cache",line);
          rg_data_line_frm_memory<=0;
          rg_word_count<=0;
          tag[rg_replace_block].portB.request.put(BRAMRequest{write:True,address:resp.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],datain:resp.address[v_addr_width-1:v_addr_width-v_num_of_tag_bits],responseOnWrite:False});
          valid[rg_replace_block].portB.request.put(BRAMRequest{write:True,address:resp.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],datain:1,responseOnWrite:False});
          data[rg_replace_block].portB.request.put(BRAMRequest{write:True,address:resp.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],datain:line,responseOnWrite:False});
          ff_request_from_cpu.deq();
        end
        else begin
          let line=rg_data_line_frm_memory;
          line=line>>(v_word_size*8);
          line=line|{resp.data_line,'d0};
          rg_data_line_frm_memory<=line;
          $display("line: %h",line);
          rg_word_count<=rg_word_count+1;
        end
      end
      else begin
          ff_request_from_cpu.deq();
      end
    endrule

    method Action request_from_cpu (From_Cpu#(_addr_width) req)if(!stall_processor && !rg_initialize);
      for(Integer i=0;i<v_ways;i=i+1)begin // send address to the Block_rams
          tag[i].portA.request.put(BRAMRequest{write:False,address:req.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:?,responseOnWrite:False});
          data[i].portA.request.put(BRAMRequest{write:False,address:req.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:?,responseOnWrite:False});
          valid[i].portA.request.put(BRAMRequest{write:False,address:req.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:?,responseOnWrite:False});
      end
      ff_request_from_cpu.enq(req); // enqueue the request for next stage
      Bit#(num_of_tag_bits) tag1=req.address[v_addr_width-1:v_num_of_tag_bits];
      Bit#(TLog#(_sets)) set1=req.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits];
      Bit#(num_of_offset_bits) off1=req.address[v_num_of_offset_bits-1:0];
      $display("\n",$time,"\tBRAM: recieved request for Address :%h tag %d: Set : %d Offset :%d",req.address,tag1,set1,off1);
    endmethod

    method Maybe#(To_Cpu#(_addr_width,_word_size)) response_to_cpu if(!rg_initialize);
      return wr_response_to_cpu();
    endmethod
    
    method Maybe#(To_Cpu#(_addr_width,_word_size)) response_from_bus if(!rg_initialize);
      return wr_response_from_bus();
    endmethod

    method ActionValue#(To_Memory#(_addr_width,_ways,_block_size)) request_to_memory if(!rg_initialize) ;
          ff_request_to_memory.deq;
      return ff_request_to_memory.first();
    endmethod

    method Action response_from_memory(From_Memory#(_addr_width,_word_size,_block_size) resp) if(!rg_initialize);
      ff_response_from_memory.enq(resp);
    endmethod
    
    method Action cache_enable(Bool enable_);
      rg_enable<=enable_;
    endmethod

    method Action clear_all();
	  $display($time,"\tSET-CACHE CLEARED");
      //ff_request_from_cpu.clear();
      //ff_request_to_memory.clear();
      //ff_response_from_memory.clear();
      //for(Integer i=0;i<v_ways;i=i+1)begin // send address to the Block_rams
      //    tag[i].portAClear;
      //    data[i].portAClear;
      //    valid[i].portAClear;
      //end
	    //stall_processor<=False;
    endmethod
//    method Maybe#(From_Memory#(_addr_width,_word_size,_block_size)) from_bus;
//      return wr_from_bus_to_cpu;
//    endmethod
  endmodule
  
  interface Ifc_icache;
    method Action request_from_cpu(From_Cpu#(32) req);
    method Maybe#(To_Cpu#(32,4)) response_to_cpu;
    method ActionValue#(To_Memory#(32,4,4)) request_to_memory;
    method Action response_from_memory(From_Memory#(32,4,4) resp);
    method Maybe#(To_Cpu#(32,4)) response_from_bus;
    method Action cache_enable(Bool enable_);

    method Action clear_all();
  endinterface

  (*synthesize*)
  module mkicache(Ifc_icache);
    
    Ifc_set_associative_cache#(32,4,4,4,512) cache <-mkset_associative_cache("ICACHE");
    method Action request_from_cpu(From_Cpu#(32) req);
      cache.request_from_cpu(req);
    endmethod

    method Maybe#(To_Cpu#(32,4)) response_to_cpu = cache.response_to_cpu;
    method ActionValue#(To_Memory#(32,4,4)) request_to_memory = cache.request_to_memory;
    method Action response_from_memory(From_Memory#(32,4,4) resp);
      cache.response_from_memory(resp);
    endmethod
    method Maybe#(To_Cpu#(32,4)) response_from_bus = cache.response_from_bus;
    method Action cache_enable(Bool enable_);
      cache.cache_enable(enable_);
    endmethod
    method Action clear_all();
      cache.clear_all();
    endmethod

  endmodule
endpackage 
