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
package set_associative_dcache;
  import All_types_d::*;
  import riscv_types::*;
  import ClientServer ::*;
  import GetPut       ::*;
  import Connectable  ::*;
  import FIFO ::*;
  import FIFOF ::*;
  import SpecialFIFOs::*;
	import BRAM::*;
  import ConfigReg::*;
  import LoadBuffer_d::*;
  import CFFIFO::*;
  import Vector::*;

  interface Ifc_set_associative_dcache#(numeric type addr_width,numeric type ways, numeric type word_size, numeric type block_size, numeric type sets, numeric type _cbuff_size,numeric type prf_size);
    method Action request_from_cpu(Cpu_req_with_token_d#(addr_width,word_size,_cbuff_size,prf_size) req);
    method ActionValue#(Cpu_resp_with_token_d#(addr_width,_cbuff_size,word_size,prf_size)) response_to_cpu;
    method ActionValue#(To_Memory_d#(addr_width,word_size,block_size)) request_to_memory;
    method Action response_from_memory(From_Memory_d#(addr_width,word_size,block_size) resp);
    method ActionValue#(WriteBack_structure_d#(addr_width,word_size,block_size)) write_back_data();
    method Action clear_all();
  endinterface

  module mkset_associative_dcache#(parameter String name)(Ifc_set_associative_dcache#(_addr_width,_ways,_word_size,_block_size,_sets,_cbuff_size,_prf_size))
  	provisos(
		Log#(_word_size,log_word_size),
		Log#(_block_size,log_block_size),
		Log#(_sets,log_sets),
		Add#(intermediate2,log_sets,_addr_width),
		Add#(intermediate3,log_word_size,intermediate2),
		Add#(num_of_tag_bits,log_block_size,intermediate3),
		Add#(log_word_size,log_block_size,num_of_offset_bits),
  	Add#(a1,_word_size,8),							//to limit the word_size that user inputs to max. of 8 bytes (or doubleword)
    Add#(c__, 16, TMul#(8, TMul#(_word_size, _block_size))),
    Add#(d__, 64, TMul#(8, TMul#(_word_size, _block_size))),
    Add#(e__, 32, TMul#(8, TMul#(_word_size, _block_size))),
    Add#(f__, 8, TMul#(8, TMul#(_word_size, _block_size))),
    Add#(a__, TMul#(_word_size, 8), TMul#(8, TMul#(_word_size, _block_size))),
    Add#(b__, 1, TSub#(_addr_width, TAdd#(TLog#(_block_size),TLog#(_word_size)))),
	Add#(g__, TAdd#(num_of_tag_bits, TLog#(_sets)), _addr_width)
    
);
    let v_log_sets=valueOf(log_sets);
    let v_ways=valueOf(_ways);
    let v_sets=valueOf(_sets);
    let v_num_of_tag_bits=valueOf(num_of_tag_bits);
    let v_num_of_offset_bits=valueOf(num_of_offset_bits);
    let v_word_size=valueOf(_word_size);
    let v_block_size=valueOf(_block_size);
    let v_addr_width=valueOf(_addr_width);
    let v_num_of_bytes=valueOf(TLog#(_word_size)); // number of bits to represent each byte in a word

  function Bool isfound(WaitBuff_d#(_addr_width,_word_size,_block_size,_cbuff_size,_ways,_prf_size) element, Bit#(_addr_width) key);
      Bit#(TSub#(_addr_width,TAdd#(TLog#(_block_size),TLog#(_word_size)))) _address_mask = signExtend(1'b1);
      Bit#(_addr_width) x=zeroExtend(_address_mask)<<v_num_of_offset_bits;
    if((element.address&x)==(key&x))
      return True;
    else
      return False;
  endfunction

  function Tuple2#(Bit#(TLog#(TMul#(8,TMul#(_word_size,_block_size)))),Bit#(TLog#(TMul#(8,TMul#(_word_size,_block_size))))) find_offset_index(Bit#(2) transfer_size, Bit#(_addr_width) cpu_addr);
    let v_word_size=valueOf(_word_size);
    Bit#(TLog#(TMul#(8,TMul#(_word_size,_block_size)))) lower_offset=0;
    Bit#(TLog#(TMul#(8,TMul#(_word_size,_block_size)))) upper_offset=0;
    for(Integer i=0;i<v_block_size;i=i+1)begin
      if(fromInteger(i)==unpack(cpu_addr[v_num_of_offset_bits-1:v_num_of_bytes]))begin // the lower order bits used to access each byte.
        lower_offset= fromInteger(v_word_size)*8*fromInteger(i); // calculating the lower bit index value. For. eg if word is 32bit. possible values are 0,32,64,96...
      end
    end
    lower_offset=lower_offset+(cpu_addr[v_num_of_bytes-1:0]*8); // exact byte offset to start the transaction from.
      if(transfer_size=='b00) // one byte (8 bits)
          upper_offset=lower_offset+7;
      else if (transfer_size=='b01) // 2 bytes (16 bits)
          upper_offset=lower_offset+15;
      else if(transfer_size=='b10) // 4 bytes (32 bits)
          upper_offset=lower_offset+31;
      else if(transfer_size=='b11) // 8 bytes (64 bits)
          upper_offset=lower_offset+63;
    return tuple2(upper_offset,lower_offset);
  endfunction

  function Bit#(TMul#(8,TMul#(_word_size,_block_size))) update_data_line (Bit#(TLog#(TMul#(8,TMul#(_word_size,_block_size)))) lower_offset, Bit#(TLog#(TMul#(8,TMul#(_word_size,_block_size)))) upper_offset, Bit#(2) transfer_size, Bit#(TMul#(8,TMul#(_word_size,_block_size))) data, Bit#(TMul#(8,_word_size)) write_data);
      Bit#(lower_offset) y = data[lower_offset-1:0];
      let x= data[fromInteger(8*v_word_size*v_block_size-1):upper_offset+1];
      Bit#(8) temp1= write_data[upper_offset-lower_offset:0];
      Bit#(16) temp2=write_data[upper_offset-lower_offset:0];
      Bit#(32) temp3=write_data[upper_offset-lower_offset:0];
      Bit#(64) temp4=write_data[upper_offset-lower_offset:0];
      Bit#(TMul#(8,TMul#(_word_size,_block_size))) new_data=0; // stupid workarounds for bluespec
      if(transfer_size==0)
          new_data={x,temp1}; // stupid workarounds for bluespec
      else if(transfer_size==1)
          new_data={x,temp2}; // stupid workarounds for bluespec
      else if(transfer_size==2)
          new_data={x,temp3}; // stupid workarounds for bluespec
      else if(transfer_size==3)
          new_data={x,temp4}; // stupid workarounds for bluespec
      new_data=new_data << lower_offset;
      new_data=new_data|y; // finally updated data line.
      return new_data;
  endfunction

    FIFO#(Hit_structure_d#(_addr_width,_word_size,_block_size,_cbuff_size,_ways,_prf_size)) ff_response_to_cpu <-mkBypassFIFO();
    FIFO#(Cpu_req_with_token_d#(_addr_width,_word_size,_cbuff_size,_prf_size)) ff_request_from_cpu <-mkSizedFIFO(2); // using FIFO of the same size as BRAM Output FIFO depth.
    FIFO#(To_Memory_d#(_addr_width,_word_size,_block_size)) ff_request_to_memory <-mkBypassFIFO();
    FIFO#(WriteBack_structure_d#(_addr_width,_word_size,_block_size)) ff_write_back_queue <-mkBypassFIFO();
    FIFO#(From_Memory_d#(_addr_width,_word_size,_block_size)) ff_response_from_memory <-mkBypassFIFO();
    Reg#(Bit#(3)) rg_burst_mode <-mkReg(v_block_size==4?'b011:v_block_size==8?'b101:v_block_size==16?'b111:0);
    Ifc_LoadBuff_d#(_addr_width,8,_word_size,_block_size,_cbuff_size,_ways,_prf_size) ldbuff <-mkLoadBuff_d();
//    Ifc_StoreBuff#(8,_word_size,_block_size,_cbuff_size,_ways) stbuff <-mkStoreBuff();
    SFIFO#(8,WaitBuff_d#(_addr_width,_word_size,_block_size,_cbuff_size,_ways,_prf_size),Bit#(_addr_width)) waitbuff <-mkCFSFIFO(isfound);
    Reg#(Maybe#(Recent_access_d#(_addr_width,_word_size,_block_size))) recently_updated_line <-mkReg(tagged Invalid);

    Reg#(Bit#(TAdd#(1,TLog#(_sets)))) rg_index <-mkReg(0);
    Reg#(Bool) rg_initialize<-mkReg(True);
    Reg#(Bool) stall_processor <-mkReg(False);
		
    BRAM_Configure cfg = defaultValue ;
    cfg.latency=1;
    cfg.outFIFODepth=2; // for pipelined cache
    cfg.allowWriteResponseBypass=True;
		BRAM2Port#(Bit#(TLog#(_sets)), Bit#(num_of_tag_bits)) tag [v_ways]; // declaring as many tag arrays as there are number of `Ways. Depth of each array is the number of sets.
		BRAM2Port#(Bit#(TLog#(_sets)), Bit#(2)) valid_dirty [v_ways];     // declaring as many alid bit arrays as there are number of `Ways. Depth of each array is the number of sets.
		BRAM2Port#(Bit#(TLog#(_sets)), Bit#(TMul#(8,TMul#(_word_size,_block_size)))) data [v_ways]; // decalring similar data arrays. each of width equal to block size.

		Reg#(Bit#(TSub#(_ways,1))) pseudo_lru [v_sets];
		for(Integer i=0;i<v_sets;i=i+1)
			pseudo_lru[i]<-mkReg(0);

		for(Integer i=0;i<v_ways;i=i+1)begin
			tag[i] <- mkBRAM2Server(cfg);		
			data[i] <- mkBRAM2Server(cfg);
			valid_dirty[i]<-mkBRAM2Server(cfg);
		end

    rule initialize_cache(rg_initialize);
      rg_index<=rg_index+1;
      for(Integer i=0;i<v_ways;i=i+1)
        valid_dirty[i].portB.request.put(BRAMRequest{write:True,address:truncate(rg_index), datain:0,responseOnWrite:False});
      if(rg_index==fromInteger(v_sets-1))
        rg_initialize<=False;
      $display(" %s: Flushing Cache", name,$time);
    endrule

    rule read_from_bram_and_load_buffer(!stall_processor && !rg_initialize);
			Bit#(num_of_tag_bits) tag_values[v_ways];	// hold the tag values
			Bit#(2) valid_dirty_values [v_ways];		// hold the valid and dirty bits
		  Bit#(TMul#(8,TMul#(_word_size,_block_size))) data_values [v_ways]; // hold the cache lines.
      Bit#(TSub#(_addr_width,num_of_offset_bits)) input_tag=ff_request_from_cpu.first.request.address[v_addr_width-1:v_num_of_offset_bits];
      Bit#(_addr_width) line_address=zeroExtend(input_tag)<<v_num_of_offset_bits;
      Bit#(_addr_width) address=ff_request_from_cpu.first.request.address;
			for(Integer i=0;i<v_ways;i=i+1)begin
				valid_dirty_values[i]<-valid_dirty[i].portA.response.get();
				tag_values[i]<-tag[i].portA.response.get();
				data_values[i]<-data[i].portA.response.get();
			end
      ///////////////////////////////////////////////// calculate the upper and lower offsets. ////////////////////////////////////////////////////////////
      let cpu_addr=ff_request_from_cpu.first.request.address;
      let transfer_size=ff_request_from_cpu.first.request.transfer_size;
      match {.upper_offset,.lower_offset}=find_offset_index(transfer_size,cpu_addr);
      $display(" %s: Upper Offset :%d, Lower Offset:%d", name, upper_offset,lower_offset,$time);
      ///////////////////////////////////////////////////////////////// find which tag to replace using PLRU ////////////////////////////////////////////////////////////////////////////////
      Integer replace_block=-1;                   // initialize to a non-existent block
      Bit#(TLog#(_sets)) set=cpu_addr[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits];
      Bit#(TSub#(_ways,1)) lru_bits = pseudo_lru[set];          // read current lru bits.
      Integer block_to_replace=0;
      for(Integer i=v_ways-1;i>=0;i=i-1)           // find if all the blocks are valid or not. If not chose one to replace
          if(valid_dirty_values[i][1]==0)begin
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
        $display(" %s:PLRU block to replace chosen : %d",name,block_to_replace,$time);
      end
      // Check if there is a hit in the bram. if hit then matched_tag != -1. If it is -1 then block_to_replace will indicate the block which is dirty and needs replacement
			Integer matched_tag=-1;
      ff_request_from_cpu.deq();
			for(Integer i=0; i<v_ways; i=i+1)begin
				if(valid_dirty_values[i][1]==1'b1 && tag_values[i]==ff_request_from_cpu.first.request.address[v_addr_width-1:v_addr_width-v_num_of_tag_bits]) // tag match
					matched_tag=i;	// here this variable indicates which tags show a match.
			end

      if((transfer_size==1 && address[0]==1) || (transfer_size==2 && address[1:0]!=0))begin // miss-aligned error.
          ff_response_to_cpu.enq(Hit_structure_d{data_line:0,
                                              transfer_size:transfer_size,
											  u_signed : ff_request_from_cpu.first.request.u_signed,
                                              token:ff_request_from_cpu.first.token,
                                              write_data:ff_request_from_cpu.first.request.write_data,
                                              upper_offset:upper_offset,
                                              lower_offset:lower_offset,
                                              replace_block:fromInteger(matched_tag),
                                              address:ff_request_from_cpu.first.request.address,
											  destination_tag:ff_request_from_cpu.first.request.destination_tag,
                                              ld_st:ff_request_from_cpu.first.request.ld_st});

      end
      else if(matched_tag!=-1)begin// on a hit return the required data to the completion buffer.
        $display(" %s: Hit for address : %h", name,ff_request_from_cpu.first.request.address);
          ff_response_to_cpu.enq(Hit_structure_d{data_line:data_values[matched_tag],
                                              transfer_size:transfer_size,
											  u_signed : ff_request_from_cpu.first.request.u_signed,
                                              token:ff_request_from_cpu.first.token,
                                              write_data:ff_request_from_cpu.first.request.write_data,
                                              upper_offset:upper_offset,
                                              lower_offset:lower_offset,
                                              replace_block:fromInteger(matched_tag),
                                              address:ff_request_from_cpu.first.request.address,
											  destination_tag:ff_request_from_cpu.first.request.destination_tag,
                                              ld_st:ff_request_from_cpu.first.request.ld_st});
      ///////////////////////////////////////////////////////////////////////////// Find new LRU bits /////////////////////////////////////////////////////////////////////////////////////////////
        Bit#(TSub#(_ways,1)) lru_bits_new = pseudo_lru[set]; // TODO verify if this works correctly
        Integer m=v_ways-1+matched_tag;
        Integer n=0;
        while(m>0)begin
            if(m%2==1)begin
                n=(m-1)/2;
                if(n<v_ways)
                    lru_bits_new[v_ways-2-n]=1;
            end
            else begin
                n=(m-2)/2;
                if(n<v_ways)
                    lru_bits_new[v_ways-2-n]=0;
            end
            m=n;
        end
        pseudo_lru[set]<=lru_bits_new; // update the LRU bits after the access is made
        $display(" %s: Changed PLRU for set : %d with bits :%b", name,set,lru_bits_new,$time);
        //recently_updated_line<=tagged Invalid;
      //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      end
      else begin // miss has occured
        $display(" %s: Miss for address : %h ", name,ff_request_from_cpu.first.request.address,fshow(ff_request_from_cpu.first.request.ld_st),$time);
        if(replace_block==-1)begin
		  Bit#(_addr_width) replace_line_address = zeroExtend({tag_values[block_to_replace],set}) << v_num_of_offset_bits;
          ff_write_back_queue.enq(WriteBack_structure_d{address:replace_line_address,
                                                      data_line:data_values[block_to_replace],
                                                      transfer_size:transfer_size,
                                                      burst_mode:rg_burst_mode});
          $display(" %s: Enquing into the WRITE BACK QUEUE", name,$time);
        end
        Bool hit =False;
        if(recently_updated_line matches tagged Valid .z)
          if(z.address[v_addr_width-1:v_num_of_offset_bits]==input_tag)
            hit=True;

        if(ldbuff.search(ff_request_from_cpu.first.request.address))begin // if the line has already been accessed earlier then enqueue the request into the waitbuffer.
          waitbuff.enq(WaitBuff_d{ld_st:ff_request_from_cpu.first.request.ld_st,
								u_signed : ff_request_from_cpu.first.request.u_signed,
                                address:ff_request_from_cpu.first.request.address,
                                transfer_size:ff_request_from_cpu.first.request.transfer_size,
                                write_data:ff_request_from_cpu.first.request.write_data,
                                upper_offset:upper_offset,
                                lower_offset:lower_offset,
                                token:ff_request_from_cpu.first.token,
								destination_tag:ff_request_from_cpu.first.request.destination_tag,
                                replace_block:fromInteger(block_to_replace)});
          $display(" %s: sending request to wait buffer for token : %d with tag :%h", name,ff_request_from_cpu.first.token,ff_request_from_cpu.first.request.address,$time);
        end
        else if(ff_request_from_cpu.first.request.ld_st==Load && hit==True)begin // load miss and hit in recently updated buffer.
          ff_response_to_cpu.enq(Hit_structure_d{data_line:validValue(recently_updated_line).data_line,// TODO
											  destination_tag:ff_request_from_cpu.first.request.destination_tag,
											  u_signed : ff_request_from_cpu.first.request.u_signed,
                                              address:ff_request_from_cpu.first.request.address,
                                              transfer_size:transfer_size,
                                              token:ff_request_from_cpu.first.token,
                                              write_data:ff_request_from_cpu.first.request.write_data,
                                              upper_offset:upper_offset,
                                              lower_offset:lower_offset,
                                              replace_block:fromInteger(block_to_replace),
                                              ld_st:ff_request_from_cpu.first.request.ld_st});
          
        end
        else begin // load or store miss
          $display(" %s: Inserting Address %h in LdBuff for token : %d", name,ff_request_from_cpu.first.request.address,ff_request_from_cpu.first.token,$time);
          ldbuff.insert(LoadBufferData_d{request:To_Memory_d{address:line_address,transfer_size:transfer_size,burst_mode:rg_burst_mode,ld_st:ff_request_from_cpu.first.request.ld_st,write_data:ff_request_from_cpu.first.request.write_data},
                                     metadata:Metadata_d{upper_offset:upper_offset,
														lower_offset:lower_offset,
														token:ff_request_from_cpu.first.token,
											  			u_signed : ff_request_from_cpu.first.request.u_signed,
														destination_tag:ff_request_from_cpu.first.request.destination_tag,
														replace_block:fromInteger(block_to_replace)}
										},FillReq);
        end
        matched_tag=block_to_replace;
      end
    endrule

    rule load_buffer_rule(!rg_initialize);
      match {.status,.info} = ldbuff.usearch();
      if(status==FillReq)begin
        ff_request_to_memory.enq(To_Memory_d{address:info.request.address,
                                          transfer_size:fromInteger(valueOf(TLog#(_word_size))),
                                          burst_mode:fromInteger(valueOf(TLog#(_block_size))),
                                          write_data:?,
                                          ld_st:Load});
        ldbuff.update(info.request.address,FillResp);
      end
    endrule

    rule write_into_bram_on_store_hit(ff_response_to_cpu.first.ld_st==Store && !rg_initialize);
      let x=ff_response_to_cpu.first();
      ff_response_to_cpu.deq();
      Bit#(2) valid_dirty_bits=2'b11;
      Bit#(TMul#(8,TMul#(_word_size,_block_size))) y= update_data_line(x.lower_offset,x.upper_offset,x.transfer_size,x.data_line,x.write_data);
      data[x.replace_block].portB.request.put(BRAMRequest{write:True,address:x.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:y,responseOnWrite:False});
      valid_dirty[x.replace_block].portB.request.put(BRAMRequest{write:True,address:x.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:valid_dirty_bits,responseOnWrite:False});
      recently_updated_line<=tagged Valid (Recent_access_d{data_line:y,address:x.address});
    endrule
    
    rule process_wait_buffer(stall_processor && !rg_initialize);
      let resp=ff_response_from_memory.first();
      Bit#(TSub#(_addr_width,num_of_offset_bits)) input_tag=resp.address[v_addr_width-1:v_num_of_offset_bits];
      let x=waitbuff.first();
      waitbuff.deq;
      if(x.address[v_addr_width-1:v_num_of_offset_bits]==input_tag)begin
        $display(" %s: Wait buffer search made a hit", name,$time);
        ff_response_to_cpu.enq(Hit_structure_d{data_line:validValue(recently_updated_line).data_line,
											  u_signed : x.u_signed,
                                              address:x.address,
											  destination_tag:x.destination_tag,
                                              transfer_size:x.transfer_size,
                                              token:x.token,
                                              write_data:x.write_data,
                                              upper_offset:x.upper_offset,
                                              lower_offset:x.lower_offset,
                                              replace_block:x.replace_block,
                                              ld_st:x.ld_st});
      end
      else
        waitbuff.enq(x);
      match {.y,.z}= waitbuff.search(resp.address);
      if(!y)begin
        ff_response_from_memory.deq();
        stall_processor<=False;
      end
    endrule

    rule got_response_from_memory(!stall_processor && !rg_initialize);
      let resp=ff_response_from_memory.first();
      Bit#(TSub#(_addr_width,num_of_offset_bits)) input_tag=resp.address[v_addr_width-1:v_num_of_offset_bits];
      let x<-ldbuff.remove(resp.address);
      tag[x.metadata.replace_block].portB.request.put(BRAMRequest{write:True,address:x.request.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],datain:x.request.address[v_addr_width-1:v_addr_width-v_num_of_tag_bits],responseOnWrite:False});
      data[x.metadata.replace_block].portB.request.put(BRAMRequest{write:True,address:x.request.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:resp.data_line,responseOnWrite:False});
      valid_dirty[x.metadata.replace_block].portB.request.put(BRAMRequest{write:True,address:x.request.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:2'b10,responseOnWrite:False});
      recently_updated_line<=tagged Valid (Recent_access_d{data_line:resp.data_line,address:resp.address});
      ff_response_to_cpu.enq(Hit_structure_d{data_line:resp.data_line,
                                              transfer_size:x.request.transfer_size,
											  destination_tag:x.metadata.destination_tag,
                                              address:x.request.address,
											  u_signed : x.metadata.u_signed,
                                              token:x.metadata.token,
                                              write_data:x.request.write_data,
                                              upper_offset:x.metadata.upper_offset,
                                              lower_offset:x.metadata.lower_offset,
                                              replace_block:x.metadata.replace_block,
                                              ld_st:x.request.ld_st});
      $display(" %s: Recieved response from the memory. Address: :%h Tag : %h Destination :%h", name,resp.address,resp.address,x.metadata.destination_tag,$time);
      match {.y,.z}= waitbuff.search(resp.address);
      if(y)begin
        $display(" %s: Stalling processor",name);
        stall_processor<=True;     
      end
      else 
        ff_response_from_memory.deq();
    endrule

    method Action request_from_cpu (Cpu_req_with_token_d#(_addr_width,_word_size,_cbuff_size,_prf_size) req)if(!stall_processor && !rg_initialize);
      for(Integer i=0;i<v_ways;i=i+1)begin // send address to the Block_rams
          tag[i].portA.request.put(BRAMRequest{write:False,address:req.request.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:?,responseOnWrite:False});
          data[i].portA.request.put(BRAMRequest{write:False,address:req.request.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:?,responseOnWrite:False});
          valid_dirty[i].portA.request.put(BRAMRequest{write:False,address:req.request.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:?,responseOnWrite:False});
      end
      ff_request_from_cpu.enq(req); // enqueue the request for next stage
      Bit#(num_of_tag_bits) tag1=req.request.address[v_addr_width-1:v_num_of_tag_bits];
      Bit#(TLog#(_sets)) set1=req.request.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits];
      Bit#(num_of_offset_bits) off1=req.request.address[v_num_of_offset_bits-1:0];
      $display(" %s: BRAM: recieved request for token : %d Address :%h tag %d: Set : %d Offset :%d Access type :", name,req.token,req.request.address,tag1,set1,off1,fshow(req.request.ld_st),$time);
    endmethod

    method ActionValue#(Cpu_resp_with_token_d#(_addr_width,_cbuff_size,_word_size,_prf_size)) response_to_cpu if(ff_response_to_cpu.first.ld_st!=Store && !rg_initialize);
      ff_response_to_cpu.deq();
      let x = ff_response_to_cpu.first();
      bit mis_aligned_error=0;
      if((x.transfer_size==1 && x.address[0]==1) || (x.transfer_size==2 && x.address[1:0]!=0))
        mis_aligned_error=1;
      return Cpu_resp_with_token_d{response:To_Cpu_d{data_word:x.data_line[x.upper_offset:x.lower_offset],
										u_signed : x.u_signed,
										transfer_size : x.transfer_size,
                                        bus_error:0,
                                        mis_aligned_error:mis_aligned_error,
                                        destination_tag:x.destination_tag},
                                token:x.token,
                                ld_st:x.ld_st};
    endmethod

    method ActionValue#(To_Memory_d#(_addr_width,_word_size,_block_size)) request_to_memory if(!rg_initialize);
      ff_request_to_memory.deq();
      return ff_request_to_memory.first();
    endmethod

    method Action response_from_memory(From_Memory_d#(_addr_width,_word_size,_block_size) resp)if(!rg_initialize);
      ff_response_from_memory.enq(resp);
    endmethod
    method ActionValue#(WriteBack_structure_d#(_addr_width,_word_size,_block_size)) write_back_data()if(!rg_initialize);
      ff_write_back_queue.deq();
      return ff_write_back_queue.first();
    endmethod
    method Action clear_all()if(!rg_initialize);
      ff_response_to_cpu.clear();
      ff_request_from_cpu.clear();
      ff_request_to_memory.clear();
      ff_response_from_memory.clear();
      ldbuff.clear_all();
      waitbuff.clear_all();
      for(Integer i=0;i<v_ways;i=i+1)begin // send address to the Block_rams
          tag[i].portAClear;
          data[i].portAClear;
          valid_dirty[i].portAClear;
          tag[i].portBClear;
          data[i].portBClear;
          valid_dirty[i].portBClear;
      end
      stall_processor<=False;
      recently_updated_line<=tagged Invalid;
    endmethod
  endmodule
endpackage
