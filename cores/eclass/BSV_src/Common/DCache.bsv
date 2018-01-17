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

Description: This module implements a parameterized blocking data cache. 
In case of Load-hit the data-cache simply sends a "word" (which could be 32 or 64 as defined during instantiation).
The core has to then further process this word and sign or zeroextend it incase it requires a byte or a half word.
In case of Load-miss the data cache fetches the entire line from the Main-Memory and then sends the respective word
to the core for further processing. Thus whenever the cache is enabled it will communicate with the external memory
only in line reads/write and nothing less than a line.
In case of Store-hit the data-cache updates the required bits (byte,half word or word) with the data provided by the
core. This is 2 cycle process since BRAMs are being used. The core is given an acknowledgement only in the second cycle.
In case of Store-miss the required line is fetched from the external memory in bursts of words. The specific which
requires a store is updated and the entire line is written into the cache.

During any miss if the line that needs to replaced is dirty then it is written to the external memory after the read/write
  operation of the new line is over.
*/
package  DCache;
  import ClientServer ::*;
  import GetPut       ::*;
  import Connectable  ::*;
  import FIFO ::*;
  import FIFOF ::*;
  import SpecialFIFOs::*;
  import BRAMCore::*;
  import ConfigReg::*;
  import Vector::*;
  import DReg::*;
  import TypeDefs :: *;

`define DCACHE_ADDR 32
`define DCACHE_WAYS 4
`define DCACHE_BLOCK_SIZE 8
`define DCACHE_WORD_SIZE 4
`define DCACHE_SETS 64

typedef enum {Idle,Stall,Handling_Request,Handling_Memory} Cache_State deriving (Bits,Eq);

  interface Ifc_set_associative_dcache#(numeric type addr_width,numeric type ways, numeric type word_size, numeric type block_size, numeric type sets);
    method Action request_from_cpu(From_Cpu_D#(addr_width,word_size) req);
    method Maybe#(To_Cpu_D#(addr_width,word_size)) response_to_cpu;
    method ActionValue#(To_Memory_D#(addr_width,word_size,block_size)) request_to_memory;
    method ActionValue#(Bool) response_from_memory(From_Memory_D#(addr_width,word_size,block_size) resp);
    method Action clear_all();
  endinterface

  module mkset_associative_dcache#(parameter String name)(Ifc_set_associative_dcache#(_addr_width,_ways,_word_size,_block_size,_sets))
   provisos(
      Log#(_word_size,log_word_size),
      Log#(_block_size,log_block_size),
      Log#(_sets,log_sets),
      Add#(intermediate2,log_sets,_addr_width),
      Add#(intermediate3,log_word_size,intermediate2),
      Add#(num_of_tag_bits,log_block_size,intermediate3),
      Add#(log_word_size,log_block_size,num_of_offset_bits),

      Add#(a__, TMul#(_word_size, 8), TMul#(_block_size, TMul#(8, _word_size))),
      Add#(b__, 8, TMul#(_word_size, 8)),
     Add#(c__, 16, TMul#(_word_size, 8)),
      Add#(d__, TMul#(8, _word_size), TMul#(TMul#(8, _word_size), _block_size)),
      Add#(num_of_tag_bits, e__, _addr_width)
   );

    let v_ways=valueOf(_ways);
    let v_sets=valueOf(_sets);
    let v_num_of_tag_bits=valueOf(num_of_tag_bits);
    let v_num_of_offset_bits=valueOf(num_of_offset_bits);
    let v_word_size=valueOf(_word_size);
    let v_block_size=valueOf(_block_size);
    let v_addr_width=valueOf(_addr_width);
    let v_num_of_bytes=valueOf(TLog#(_word_size)); // number of bits to represent each byte in a word


  function Bit#(TMul#(_word_size,8)) update_word (Bit#(TMul#(_word_size,8)) read_word, Bit#(TMul#(8,_word_size)) write_data, Bit#(2) transfer_size, Bit#(_addr_width) address);
      Bit#(TLog#(TMul#(8,_word_size))) byte_offset = address[v_num_of_bytes-1:0];
      if(transfer_size==0)begin
         if(byte_offset==0)
            read_word[7:0]=write_data[7:0];
         else if(byte_offset==1)
            read_word[15:8]=write_data[7:0];
         else if(byte_offset==2)
            read_word[23:16]=write_data[7:0];
         else 
            read_word[31:24]=write_data[7:0];
      end
      else if(transfer_size==1)begin
         if(byte_offset==0)
            read_word[15:0]=write_data[15:0];
         else
            read_word[31:16]=write_data[15:0];
      end
      else
         read_word=write_data;
      return read_word;
   endfunction

   function Bit#(TMul#(TMul#(8,_word_size),_block_size)) concat_words (Array#(Bit#(TMul#(8,_word_size))) data_values);
      Bit#(TMul#(TMul#(8,_word_size),_block_size))  data_line='1;
      for(Integer i=v_block_size-1;i>=0;i=i-1)
         data_line={truncate(data_line),data_values[i]};
      return data_line;
   endfunction

   `ifdef atomic
      function Bit#(TMul#(_word_size,8)) atomic_operation(Bit#(TMul#(_word_size,8)) loaded_value, Bit#(TMul#(_word_size,8)) rs2, Bit#(5) atomic_op);
         case (atomic_op)
            `AMOSWAP_f5:return rs2;
            `AMOADD_f5:return (loaded_value+rs2);
            `AMOXOR_f5:return (loaded_value^rs2);
            `AMOAND_f5:return (loaded_value&rs2);
            `AMOOR_f5: return (loaded_value|rs2);
            `AMOMINU_f5:return min(loaded_value,rs2);
            `AMOMAXU_f5:return max(loaded_value,rs2);
            default:return loaded_value;
         endcase  
      endfunction
   `endif

    Wire#(Maybe#(To_Cpu_D#(_addr_width, _word_size))) wr_response_to_cpu <-mkDWire(tagged Invalid);
    Wire#(Bool) wr_line_done<-mkDWire(False);
    FIFOF#(From_Cpu_D#(_addr_width,_word_size)) ff_request_from_cpu <-mkFIFOF1(); // using FIFO of the same size as BRAM Output FIFO depth.
    FIFOF#(To_Memory_D#(_addr_width,_word_size,_block_size)) ff_request_to_memory <-mkSizedBypassFIFOF(1);
    FIFOF#(From_Memory_D#(_addr_width,_word_size,_block_size)) ff_response_from_memory <-mkLFIFOF();

    Reg#(Bit#(TAdd#(1,TLog#(_sets)))) rg_index <-mkReg(0);
    Reg#(Bool) rg_initialize<-mkReg(True);
    Reg#(Bit#(TLog#(_ways))) rg_replace_block <-mkReg(0);
    Reg#(Bool) rg_enable <-mkReg(True);
    Reg#(Bit#(TLog#(_block_size))) rg_word_count <-mkReg(0);
    Reg#(Bool) rg_dirty_line_write_back <-mkReg(False);
    Reg#(Bit#(TMul#(8,TMul#(_word_size,_block_size)))) rg_dirty_line_data <-mkReg(0);
    Reg#(Bit#(_addr_width)) rg_dirty_line_addr <-mkReg(0);
    Reg#(Bool) rg_line_write_stall <-mkReg(False);
      Reg#(Bit#(TSub#(num_of_offset_bits,TLog#(_word_size)))) rg_offset <-mkReg(0);
    
      Reg#(Cache_State) rg_state[3] <-mkCReg(3,Idle);

      BRAM_DUAL_PORT#(Bit#(TLog#(_sets)),Bit#(num_of_tag_bits)) tag [v_ways];
      BRAM_DUAL_PORT#(Bit#(TLog#(_sets)),Bit#(1)) valid [v_ways];
      BRAM_DUAL_PORT#(Bit#(TLog#(_sets)),Bit#(TMul#(TMul#(8,_word_size),1))) data [v_ways][v_block_size];
      BRAM_DUAL_PORT#(Bit#(TLog#(_sets)),Bit#(1)) dirty [v_ways];

    Reg#(Bit#(TSub#(_ways,1))) pseudo_lru [v_sets]; // This holds the bits for pseduo-lru replacement policy within each set.
    for(Integer i=0;i<v_sets;i=i+1)
      pseudo_lru[i]<-mkReg(0);
    
    for(Integer i=0;i<v_ways;i=i+1) begin
      tag[i] <- mkBRAMCore2(v_sets,False);      
         for(Integer j=0;j<v_block_size;j=j+1)
         data[i][j] <-mkBRAMCore2(v_sets,False);
      valid[i]<-mkBRAMCore2(v_sets,False);
      dirty[i]<-mkBRAMCore2(v_sets,False);
    end

    // This rule initializes the cache so that each line is invalid and not dirty. This task will take
    // "set" number cycles to complete on each system reset.
    rule initialize_cache(rg_initialize);
      rg_index<=rg_index+1;
         for(Integer i=0;i<v_ways;i=i+1)begin
             valid[i].a.put(True,truncate(rg_index),0);
             dirty[i].a.put(True,truncate(rg_index),0);
         end
            if(rg_index==fromInteger(v_sets-1))begin
               //$display($time,"\t",name,"\tDCACHE PARAMETERS: tag bits: %0d set bits: %0d offset bits: %0d address bits: %0d",v_num_of_tag_bits,valueOf(log_sets),v_num_of_offset_bits,valueOf(_addr_width));
         rg_initialize<=False;
            end
        //$display($time,"\t",name,"\tFlushing Cache Index: %d",rg_index);
      //$display("Set bits: %d Block Bits: %d Word Bits: %d Tag Bits: %d",valueOf(log_sets),valueOf(log_block_size),valueOf(log_word_size),valueOf(num_of_tag_bits));
    endrule

    // When the cache is disabled the request is directly sent to the
    // external memory.
    rule cache_is_disabled(!rg_enable && rg_state[0]==Handling_Request);
        ff_request_to_memory.enq(To_Memory_D {
                                     address:ff_request_from_cpu.first().address,
                                 transfer_size:ff_request_from_cpu.first.transfer_size,
                                 data_line:zeroExtend(ff_request_from_cpu.first.data),
                                 burst_length:1,
                                 ld_st:ff_request_from_cpu.first.load_store});
            rg_state[0]<=Handling_Memory;
            //$display($time,"\t",name,"\tCache is disabled. Sending Request to Memory Addres: %h Data: %h, Access:",ff_request_from_cpu.first.address,ff_request_from_cpu.first.data,fshow(ff_request_from_cpu.first.load_store));
    endrule

    // This rule is fired when a read/write request has been taken from the core and when cache is enabled.
    // If cache is disabled then no request is put in the BRAMs and thus this will not fire. 
    // The explicit use of rg_enable in the condition is to indicate the compiler that this rule
    // and the previous one are mutually exclusive.
    rule read_from_bram(!rg_initialize && rg_enable && rg_state[0]==Handling_Request);
      let transfersize =ff_request_from_cpu.first.transfer_size;
      Bit#(num_of_tag_bits) tag_values[v_ways]; // hold the tag values
      Bit#(1) valid_values [v_ways];      // holds the valid bits
      Bit#(1) dirty_values [v_ways];      // holds the dirty bits
      Bit#(TMul#(8,TMul#(_word_size,1))) data_values [v_ways][v_block_size]; // holds the cache lines.
      Bit#(TSub#(_addr_width,num_of_offset_bits)) input_tag=ff_request_from_cpu.first.address[v_addr_width-1:v_num_of_offset_bits]; // tag of the address from the core.
      Bit#(v_num_of_offset_bits) zero_offset_bits=0;
      // get responses form the respective BRAMs.
         for(Integer i=0;i<v_ways;i=i+1)begin
            valid_values[i]=valid[i].a.read();
            tag_values[i]=tag[i].a.read();
            for(Integer j=0;j<v_block_size;j=j+1)
               data_values[i][j]=data[i][j].a.read();
            dirty_values[i]=dirty[i].a.read();
         end
      ///////////////////////////////////////////////// calculate the upper and lower offsets. ////////////////////////////////////////////////////////////
      let cpu_addr=ff_request_from_cpu.first.address;
//      let byte_offset=find_byte_offset(cpu_addr); // find the offset of the first byte being accessed in the word.
      ///////////////////////////////////////////////////////////////// find which tag to replace using PLRU ////////////////////////////////////////////////////////////////////////////////
         Bit#(TSub#(num_of_offset_bits,TLog#(_word_size))) offset=ff_request_from_cpu.first.address[v_num_of_offset_bits-1:v_num_of_bytes];
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
        //$display($time,"\t",name,"\t%s:performing PLRU",name);
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
         Integer matched_tag=-1;
      Bit#(_addr_width) address=ff_request_from_cpu.first.address;
      // this for loop checks if any of the lines have a tag that matches with that of the core address.
         for(Integer i=0; i<v_ways; i=i+1)begin
            if(valid_values[i]==1'b1 && tag_values[i]==ff_request_from_cpu.first.address[v_addr_width-1:v_addr_width-v_num_of_tag_bits])
               matched_tag=i; // here this variable indicates which tags show a match.
         end
      // This condition checks if the access by the CPU is misaligned or not.
      if((transfersize=='b10 && address[1:0]!=0)||(transfersize=='b01 && address[0]!=0))begin // miss-aligned error.
        $display($time,"\t",name,"\t Misaligned Error due to Address: %h",cpu_addr);
        wr_response_to_cpu<= tagged Valid (To_Cpu_D {address:ff_request_from_cpu.first().address,
                                          data_word:0,
                                      bus_error:0,
                                      load_store:ff_request_from_cpu.first.load_store,
                                      misaligned_error:1 });
            rg_state[0]<=Idle;
      end
      else if(matched_tag!=-1)begin// on a hit return the required data to the completion buffer.
        if(ff_request_from_cpu.first().load_store==Load)begin // if it is a Load-hit simply written the required word to the core.
              $display($time,"\t",name,"\tLoad Hit for address : %h Line: %h",ff_request_from_cpu.first.address,matched_tag);
              wr_response_to_cpu<= tagged Valid (To_Cpu_D {address:ff_request_from_cpu.first().address,
                                        data_word:data_values[matched_tag][offset],
                                        bus_error:0,
                                        load_store:ff_request_from_cpu.first.load_store,
                                        misaligned_error:0});
                     rg_state[0]<=Idle;
              ff_request_from_cpu.deq();
        end
        else if(ff_request_from_cpu.first().load_store==Store) begin // STORE operation had a hit in the cache.
          $display($time,"\t",name,"\t Store Hit for address : %h Line: %h",ff_request_from_cpu.first.address,matched_tag);
          Bit#(TMul#(_word_size,8)) accessed_word1=data_values[matched_tag][offset]; // read the word from the line.
//          let accessed_word<-update_word(byte_offset,accessed_word1,ff_request_from_cpu.first.data,transfersize); // update the word
               let accessed_word=update_word(accessed_word1,ff_request_from_cpu.first.data,transfersize,ff_request_from_cpu.first.address);
               rg_state[0]<=Stall;
               $display($time,"\t",name,"\t Changed word : %h to word: %h",accessed_word1,accessed_word);
               tag[matched_tag].b.put(True,ff_request_from_cpu.first.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],ff_request_from_cpu.first.address[v_addr_width-1:v_addr_width-v_num_of_tag_bits]);
               valid[matched_tag].b.put(True,ff_request_from_cpu.first.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],1);
               dirty[matched_tag].b.put(True,ff_request_from_cpu.first.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],1);
               data[matched_tag][offset].b.put (True,ff_request_from_cpu.first.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],accessed_word);
            wr_response_to_cpu<= tagged Valid (To_Cpu_D {address:ff_request_from_cpu.first().address, data_word:accessed_word,
                                    bus_error:0, load_store:ff_request_from_cpu.first.load_store, misaligned_error:0});
          ff_request_from_cpu.deq();
        end
            `ifdef atomic
         else if(ff_request_from_cpu.first().load_store==Atomic) begin // Atomic operation had a hit in the cache.
           $display($time,"\t",name,"\t Atomic Hit for address : %h Line: %h",ff_request_from_cpu.first.address,matched_tag);
           Bit#(TMul#(_word_size,8)) accessed_word1=data_values[matched_tag][offset]; // read the word from the line.
           let accessed_word=atomic_operation(accessed_word1,ff_request_from_cpu.first.data,ff_request_from_cpu.first.atomic_op); // update the word
                  tag[matched_tag].b.put(True,ff_request_from_cpu.first.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],ff_request_from_cpu.first.address[v_addr_width-1:v_addr_width-v_num_of_tag_bits]);
                  valid[matched_tag].b.put(True,ff_request_from_cpu.first.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],1);
                  dirty[matched_tag].b.put(True,ff_request_from_cpu.first.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],1);
                  data[matched_tag][offset].b.put (True,ff_request_from_cpu.first.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],accessed_word);
               wr_response_to_cpu<= tagged Valid (To_Cpu_D {address:ff_request_from_cpu.first().address, data_word:accessed_word1,
                                    bus_error:0, load_store:ff_request_from_cpu.first.load_store, misaligned_error:0});

                  rg_state[0]<=Stall;
          ff_request_from_cpu.deq();
         end
            `endif
      ///////////////////////////////////////////////////////////////////////////// Find new LRU bits /////////////////////////////////////////////////////////////////////////////////////////////
      end
      else begin// If the operation is miss in the cache
        $display($time,"\t",name,"\tMiss for address : %h",ff_request_from_cpu.first.address);
         matched_tag = block_to_replace;
            rg_offset<=address[v_num_of_offset_bits-1:v_num_of_bytes];
        ff_request_to_memory.enq(To_Memory_D { // send the request to memory to 
                                     address:address,
                                 data_line:?,
                                 burst_length:fromInteger(v_block_size),
                                 transfer_size:fromInteger(valueOf(log_word_size)),
                                 ld_st:Load});
        rg_replace_block<=fromInteger(block_to_replace); // store the index of the line that needs to be re-written with the new line.
            rg_state[0]<=Handling_Memory;
            Bit#(_addr_width) old_address={tag_values[fromInteger(block_to_replace)],cpu_addr[v_addr_width-v_num_of_tag_bits-1:0]};
            $display($time,"\t",name,"\t Replacing line: %d of address: %h",block_to_replace,old_address);
        if(dirty_values[fromInteger(block_to_replace)]==1)begin // if the replaced block is dirty tag a state to ensure it is written back to external memory.
          $display($time, "\t line : %d is dirty",block_to_replace);
          rg_dirty_line_write_back<=True;
          rg_dirty_line_data<=concat_words(data_values[fromInteger(block_to_replace)]);
          // Following is the address of the line that needs to be replaced.
          rg_dirty_line_addr<={tag_values[fromInteger(block_to_replace)],cpu_addr[v_addr_width-v_num_of_tag_bits-1:0]};
        end
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
      $display($time,"\t",name,"\tChanged PLRU for set : %d with bits :%b",set,lru_bits);
    endrule

    // This rule ensures that when the memory has responded with acknowledgment that the dirty line store is
    // over and the ff_request_from_cpu FIFO is empty then dequeues the ff_response_from_memory FIFO and also
    // disables the rg_line_write_stall flag.
   rule empty_response_from_memory_after_dirty_line_write( rg_line_write_stall && !rg_initialize);
      $display($time,"\t",name,"\tEmptying the response FIFO");
      ff_response_from_memory.deq();
         if(rg_word_count==fromInteger(v_block_size-1))begin
            rg_word_count<=0;
        rg_line_write_stall<=False;
         end
         else
            rg_word_count<=rg_word_count+1;
         if(rg_word_count==fromInteger(v_block_size-2))
               wr_line_done<=True;
    endrule

    
    // This rule will fire when the external memory has responded with some data for the cache. 
    // All line reads will come as bursts  to this rule. So if a line has 4 words then this rule will fire 4 times.
    // Each time it fires the line variable if updated. At the end of 4 words the line will be written in the BRAM if the cache is
    // enabled. Also as the words keep coming they are checked if it is required by the core or not. 
    rule got_response_from_memory(!rg_initialize && rg_state[1]==Handling_Memory && !rg_line_write_stall);
      let resp=ff_response_from_memory.first();
      ff_response_from_memory.deq();

      Bit#(TSub#(_addr_width,num_of_offset_bits)) input_tag=resp.address[v_addr_width-1:v_num_of_offset_bits];
      $display($time,"\t",name,"\tRecieved response from the memory. Address: :%h Tag : %h Data: %h ",resp.address,input_tag,resp.data_line);

      let word=resp.data_line;
      if(rg_enable)begin // only if the cache is enabled
            if(resp.address[v_num_of_offset_bits-1:v_num_of_bytes]==rg_offset)begin // if the word from the memory is begin accessed by the core as well.
               $display($time,"\t",name,"\tRg_word: %d Address Word: %d",rg_word_count,ff_request_from_cpu.first.address[3:2]);
               wr_response_to_cpu<= tagged Valid (To_Cpu_D{address:resp.address,data_word:resp.data_line,bus_error:resp.bus_error,misaligned_error:0,load_store:ff_request_from_cpu.first.load_store});
            end
            if(rg_offset==resp.address[v_num_of_offset_bits-1:v_num_of_bytes]) // does the word need to be updated by the core data.
               if(ff_request_from_cpu.first.load_store==Store)begin 
            $display($time, "\tupdating word for store");
            //let w<-update_word(byte_offset,word,ff_request_from_cpu.first.data,ff_request_from_cpu.first.transfer_size);
            let w=update_word(word,ff_request_from_cpu.first.data,ff_request_from_cpu.first.transfer_size,resp.address);
            word=w;
               end
               `ifdef atomic
                  else if(ff_request_from_cpu.first.load_store==Atomic)begin
                     word=atomic_operation(word,ff_request_from_cpu.first.data,ff_request_from_cpu.first.atomic_op);
               end
               `endif
            $display($time, "\tUpdated word is :%h",word);
            if(rg_word_count==fromInteger(v_block_size-2))
               wr_line_done<=True;
            Bit#(TSub#(num_of_offset_bits,TLog#(_word_size))) offset=resp.address[v_num_of_offset_bits-1:v_num_of_bytes];
         data[rg_replace_block][offset].b.put(True,resp.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],word);
         if(rg_word_count==fromInteger(v_block_size-1))begin // when all the words for the line have been recieved.
           ff_request_from_cpu.deq(); // dequeue the FIFO indicating the end of a instruction.
               rg_state[1]<=Stall;
           rg_word_count<=0; // reset the register
           $display($time,"\t",name,"\tWriting Word :%h in cache",word);
           // update the tag with the latest address field
           tag[rg_replace_block].b.put(True,resp.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],resp.address[v_addr_width-1:v_addr_width-v_num_of_tag_bits]);
           // make the entry in the cache valid
           valid[rg_replace_block].b.put(True,resp.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],1);
           if(ff_request_from_cpu.first.load_store==Store)begin // if line is modified due to write mark it dirty.
             dirty[rg_replace_block].b.put(True,resp.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],1);
           end
           else begin // else leave it as non-dirty.
             dirty[rg_replace_block].b.put(True,resp.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],0);
           end
           // if the dirty line needs to be written back to main memory.
           if(rg_dirty_line_write_back)begin
             $display($time,"\t",name,"\t Sending Write request to Memory for dirty line Addr: %h Data: %h",{rg_dirty_line_addr},rg_dirty_line_data);
             rg_dirty_line_write_back<=False;
             Bit#(v_addr_width) line_addr=rg_dirty_line_addr[v_addr_width-1:v_num_of_offset_bits]<<v_num_of_offset_bits;
             ff_request_to_memory.enq(To_Memory_D {
                                      address:line_addr,
                                      data_line:rg_dirty_line_data,
                                      transfer_size:v_word_size==8?0:v_word_size==16?1:2, // TODO: make this generic.
                                      burst_length:fromInteger(v_block_size),
                                      ld_st:Store});
             rg_line_write_stall<=True;
           end
         end
        else begin
          rg_word_count<=rg_word_count+1;
        end
      end
      else begin
        ff_request_from_cpu.deq;
            rg_state[1]<=Idle;
            wr_response_to_cpu<= tagged Valid (To_Cpu_D{address:resp.address,data_word:resp.data_line,bus_error:resp.bus_error,misaligned_error:0,load_store:ff_request_from_cpu.first.load_store});
      end
    endrule

      rule stall_for_write_to_take_effect(rg_state[0]==Stall);
         rg_state[0]<=Idle;
      endrule
    
    method Action request_from_cpu (From_Cpu_D#(_addr_width,_word_size) req)if(!rg_initialize && rg_state[2]==Idle);
         Bool enble=True;
      if(req.address>='h40000000 && req.address<='h4000000f)
            enble=False;
         if(enble)begin
        for(Integer i=0;i<v_ways;i=i+1)begin // send address to the Block_rams
            tag[i].a.put(False,req.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],?);
                  for(Integer j=0;j<v_block_size;j=j+1)
               data[i][j].a.put(False,req.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],?);
            valid[i].a.put(False,req.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],?);
            dirty[i].a.put(False,req.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],?);
        end
         end

         rg_state[2]<=Handling_Request;
      rg_enable<=enble;
      ff_request_from_cpu.enq(req); // enqueue the request for next stage

      `ifdef simulate
      Bit#(num_of_tag_bits) tag1=req.address[v_addr_width-1:v_addr_width-v_num_of_tag_bits];
      Bit#(TLog#(_sets)) set1=req.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits];
      Bit#(num_of_offset_bits) off1=req.address[v_num_of_offset_bits-1:0];
      $display("\n",$time,"\t",name,"\tBRAM: recieved request for Address :%h Data: %h tag %h: Set : %d Offset :%h ",req.address,req.data,tag1,set1,off1,fshow(req.load_store));
      `endif
    endmethod

    method Maybe#(To_Cpu_D#(_addr_width,_word_size)) response_to_cpu if(!rg_initialize);
      return wr_response_to_cpu();
    endmethod

    method ActionValue#(To_Memory_D#(_addr_width,_word_size,_block_size)) request_to_memory if(!rg_initialize) ;
          ff_request_to_memory.deq;
      return ff_request_to_memory.first();
    endmethod

    method ActionValue#(Bool) response_from_memory(From_Memory_D#(_addr_width,_word_size,_block_size) resp) if(!rg_initialize);
      ff_response_from_memory.enq(resp);
         return wr_line_done;
    endmethod
    method Action clear_all();
     //$display($time,"\t",name,"\tSET-CACHE CLEARED");
    endmethod
  endmodule
  
  interface Ifc_dcache;
    method Action request_from_cpu(From_Cpu_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE) req);
    method ActionValue#(Maybe#(To_Cpu_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE))) response_to_cpu;
    method ActionValue#(To_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)) request_to_memory;
    method ActionValue#(Bool) response_from_memory(From_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE) resp);
    method Action clear_all();
  endinterface

  (*synthesize*)
  module mkdcache(Ifc_dcache);
    
    Ifc_set_associative_dcache#(`DCACHE_ADDR,`DCACHE_WAYS,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE,`DCACHE_SETS) cache <-mkset_associative_dcache("DCACHE");
    method Action request_from_cpu(From_Cpu_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE) req);
      cache.request_from_cpu(req);
    endmethod
    method ActionValue#(Maybe#(To_Cpu_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE))) response_to_cpu;
      noAction;
      return cache.response_to_cpu;
    endmethod
    method ActionValue#(To_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)) request_to_memory = cache.request_to_memory;
    method ActionValue#(Bool) response_from_memory(From_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE) resp);
      let x <-cache.response_from_memory(resp);
         return x;
    endmethod
    method Action clear_all();
      cache.clear_all();
    endmethod
  endmodule
endpackage 
