/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Names : Neel Gala
Email ID : neelgala@gmail.com

Description :
This module implements the non blocking instruction cache. The cache is a parameterized n-way set-associative cache. 
It contains a depth-paramterized completion buffer, load buffer and wait buffer. The completion buffer takes in requests
from the cpu and allocates a unique token to each request and holds a slot for each such request in the buffer. In return the 
completion buffer recieves out of order responses from the cache/memory and send them back to the cpu in-order.

The request from the CPU is sent to the cache and the completion buffer simulatneously. The cache is pipelined and implemented as
BRAMs. In case of a read request, the address is sent on the first cycle and the data is recieved on the second cycle. Once the data
is recieved from the BRAMs, a tag match is done and incase of a hit the respective word is sent back to the CPU. 

In case of BRAM miss, the entry is stored in the load buffer only if there is no existing entry in the load buffer for the same
  cache line miss. The load buffer holds the upper bits of the address which help identify the cacheline. Through a fully associative
  lookup on the load buffer for the cacheline address it is decided if a new entry needs to be added to the load buffer or not.

In case when the load buffer does hold a miss for the same cache line the currently requested miss is added to the wait bufer. The wait buffer
basically holds redundant misses on the same cache line. Once the lower level memory responds with the data the load buffer entry 
is served and all the entries in the wait buffer are also served. While serving an entry in the wait buffer the cpu requests are stalled untill
no further wait buffer entry can be served. 

The load buffer is implemented as a vector of concurrent registers. wait buffer mimics a searchable concurrent fifo.

*/

package nbdcache;
	import BRAM::*;
  import ClientServer ::*;
  import GetPut       ::*;
  import Connectable  ::*;
  import FIFO ::*;
  import FIFOF ::*;
  import SpecialFIFOs::*;
  import Vector::*;
  import ConfigReg::*;
  import CBuff::*;
  import set_associative_dcache::*;
  import All_types_d::*;
  import riscv_types::*;

  // parameterized interface arguments are in order : address width, number of ways, number of bytes in a word, number of words in a cache line, number of sets, completion buffer size
	interface Ifc_nbdcache#(numeric type addr_width, numeric type ways, numeric type word_size, numeric type block_size, numeric type sets,numeric type completion_buffer_size,numeric type prf_size);
    method Action request_from_cpu(From_Cpu_d#(addr_width,word_size,prf_size) req); // recieve request from the CPU.
    method To_Cpu_d#(addr_width,word_size,prf_size) response_to_cpu;  // send response from the completion buffer to cpu.
    method Action response_deqResult();         // remove entry in the completion buffer from the cpu.
    
    method ActionValue#(To_Memory_d#(addr_width,word_size,block_size)) request_to_memory; // send request to memory
    method Action response_from_memory(From_Memory_d#(addr_width,word_size,block_size) resp); // recieve response from the memory.
    method ActionValue#(WriteBack_structure_d#(addr_width,word_size,block_size)) write_back_data; // send request to memory

    method Action abandon_cycle();
  endinterface

  module mknbdcache#(parameter String name)(Ifc_nbdcache#(_addr_width,_ways,_word_size,_block_size,_sets,_cbuff_size,_prf_size))
    provisos(
      Add#(TSub#(32,TAdd#(TLog#(_sets),TLog#(TMul#(_block_size,_word_size)))),0,_num_of_tag_bits),// finding the number of bits required to represent the tag
      Log#(TMul#(_block_size,_word_size),_num_of_offset_bits),// finding the number of bits requires to represent every byte in the line
      Add#(TAdd#(_num_of_tag_bits,_num_of_offset_bits),TLog#(_sets),32),// confirming if above calculations fit in specified Address width

        // following required by the bluespec compiler.
      Add#(a__, TLog#(_sets), _addr_width),
      Add#(b__, TLog#(_word_size), a__),
      Add#(c__, TLog#(_block_size), b__),
      Add#(d__, _word_size, 8),
      Add#(e__, TMul#(_word_size, 8), TMul#(8, TMul#(_word_size, _block_size))),
      Add#(g__, 16, TMul#(8, TMul#(_word_size, _block_size))),
      Add#(h__, 64, TMul#(8, TMul#(_word_size, _block_size))),
      Add#(i__, 32, TMul#(8, TMul#(_word_size, _block_size))),
      Add#(j__, 8, TMul#(8, TMul#(_word_size, _block_size))),
	  Add#(f__, TAdd#(c__, TLog#(_sets)), _addr_width),
//      Add#(g__, 32, TMul#(8, TMul#(_word_size, _block_size))),
//      Add#(f__, c__, 32),
      Add#(k__, 1, TSub#(_addr_width, TAdd#(TLog#(_block_size),TLog#(_word_size))))
    );

    CBuff#(To_Cpu_d#(_addr_width,_word_size,_prf_size),_cbuff_size) completionbuffer <-mkMyCBuff();       // declarin the completion buffer. refer to CBuff.bsv for more details on its working.
    Ifc_set_associative_dcache#(_addr_width,_ways,_word_size,_block_size,_sets,_cbuff_size,_prf_size) bram_cache <-mkset_associative_dcache(name);  // declaring pipelined set associative cache.
    // recieve response from the cache for a previously generated request and out into the completion buffer
    rule complete_request_from_cpu; 
      let resp<-bram_cache.response_to_cpu;
      $display("%s: Completing request in cbuff for token : %d ",name,resp.token,$time);
      completionbuffer.put(resp.token,resp.response);
    endrule

    // a new request from the cpu is only taken if there is an empty slot in the completion buffer. 
    // a token of the empty slot is taken and the request is sent to the cache and also saved in the completion buffer.
    // this reserves a slot in the completion buffer for the request made.
    method Action request_from_cpu(From_Cpu_d#(_addr_width,_word_size,_prf_size) req); 
      if(req.ld_st==Load)begin
        let token <-completionbuffer.getToken(); // this reserves the slot in the completion buffer.
        bram_cache.request_from_cpu(Cpu_req_with_token_d{request:req,token:token}); // request sent to the cache with the token number.
        $display(" %s: Got token : %d for input Load address : %h ", name,token,req.address,$time);
      end
      else begin
        bram_cache.request_from_cpu(Cpu_req_with_token_d{request:req,token:0}); // request sent to the cache with the token number.
        $display(" %s: Got token : %d for input Store address : %h ", name,0,req.address,$time);
      end
    endmethod

    method response_to_cpu = completionbuffer.getResult(); // when the head of the completion buffer is valid send data to cpu.

      // once the completion buffer head has been read, discard the entry and make it free.
    method Action response_deqResult();
      completionbuffer.deqResult();
    endmethod

    // cache request for memory access is simply bypassed.
    method request_to_memory=bram_cache.request_to_memory();

    method write_back_data=bram_cache.write_back_data();

      // the response from the memory is simply bypassed to the BRAM/cache.
    method Action response_from_memory(From_Memory_d#(_addr_width,_word_size,_block_size) resp);
      bram_cache.response_from_memory(resp);
    endmethod

    method Action abandon_cycle();
      completionbuffer.clear_all();
      bram_cache.clear_all();
    endmethod

  endmodule
endpackage
