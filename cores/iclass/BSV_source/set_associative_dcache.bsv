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

  module mkset_associative_dcache(Ifc_set_associative_dcache#(_addr_width,_ways,_word_size,_block_size,_sets,_cbuff_size,_prf_size))
  	provisos(
		Log#(_word_size,log_word_size),
		Log#(_block_size,log_block_size),
		Log#(_sets,log_sets),
		Add#(intermediate2,log_sets,_addr_width),
		Add#(intermediate3,log_word_size,intermediate2),
		Add#(num_of_tag_bits,log_block_size,intermediate3),
		Add#(log_word_size,log_block_size,num_of_offset_bits),
  	Add#(a1,_word_size,8),							//to limit the word_size that user inputs to max. of 8 bytes (or doubleword)
    Add#(c__, 16, TMul#(8, _word_size)),
    Add#(d__, 64, TMul#(8, _word_size)),
    Add#(e__, 32, TMul#(8, _word_size)),
    Add#(f__, 8, TMul#(8, _word_size)),
    Add#(a__, TMul#(8, _word_size), TMul#(8, TMul#(_word_size, _block_size))),
    Add#(b__, 1, TSub#(_addr_width, TAdd#(TLog#(_block_size),TLog#(_word_size)))),
	Add#(g__, TAdd#(num_of_tag_bits, TLog#(_sets)), _addr_width),
	Add#(h__, 1, TSub#(_addr_width, TLog#(_word_size)))
    
);
    let v_log_sets=valueOf(log_sets);
    let v_ways=valueOf(_ways);
    let v_sets=valueOf(_sets);
    let v_num_of_tag_bits=valueOf(num_of_tag_bits);
    let v_num_of_offset_bits=valueOf(num_of_offset_bits);
    let v_word_size=valueOf(_word_size);
    let v_block_size=valueOf(_block_size);
    let v_block_bits=valueOf(log_block_size);
    let v_addr_width=valueOf(_addr_width);
    let v_num_of_bytes=valueOf(TLog#(_word_size)); // number of bits to represent each byte in a word
    let vl_word_size=valueOf(_word_size);
    let vl_block_size=valueOf(_block_size);
    let vl_addr_width=valueOf(_addr_width);
    let vl_num_of_bytes=valueOf(TLog#(_word_size)); // number of bits to represent each byte in a word


  function Tuple2#(Bool,Bool) isfound(WaitBuff_d#(_addr_width,_word_size,_block_size,_cbuff_size,_ways,_prf_size) element, Bit#(_addr_width) key);
      Bit#(TSub#(_addr_width,TAdd#(TLog#(_block_size),TLog#(_word_size)))) _address_mask = signExtend(1'b1);
      Bit#(_addr_width) x=zeroExtend(_address_mask)<<v_num_of_bytes;
	  Bool isStore = False;
	  if(element.ld_st == Store)
		isStore = True;
    if((element.address&x)==(key&x))
      return tuple2(isStore,True);
    else
      return tuple2(isStore,False);
  endfunction

	function Bit#(TMul#(8, TMul#(_word_size, _block_size))) concate_vec(Array#(Bit#(TMul#(8, _word_size))) data_words);
		Bit#(TMul#(8, TMul#(_word_size, _block_size))) data_line = 0;
		for(Integer i = 0; i < v_block_size; i = i+1) 
			data_line[(i+1)*8*v_word_size-1:(i)*8*v_word_size] = data_words[i];
		return data_line;
	endfunction
				

  function Tuple2#(Bit#(TLog#(TMul#(8,_word_size))),Bit#(TLog#(TMul#(8,_word_size)))) find_offset_index(Bit#(2) transfer_size, Bit#(_addr_width) cpu_addr);
    let v_word_size=valueOf(_word_size);
    Bit#(TLog#(TMul#(8,_word_size))) lower_offset=0;
    Bit#(TLog#(TMul#(8,_word_size))) upper_offset=0;
    //for(Integer i=0;i<v_block_size;i=i+1)begin
    //  if(fromInteger(i)==unpack(cpu_addr[v_num_of_offset_bits-1:v_num_of_bytes]))begin // the lower order bits used to access each byte.
    //    lower_offset= fromInteger(v_word_size)*8*fromInteger(i); // calculating the lower bit index value. For. eg if word is 32bit. possible values are 0,32,64,96...
    //  end
    //end
    //lower_offset=lower_offset+(cpu_addr[v_num_of_bytes-1:0]*8); // exact byte offset to start the transaction from.
    lower_offset=cpu_addr[v_num_of_bytes-1:0]*8; // exact byte offset to start the transaction from.
      if(transfer_size=='b00) // one byte (8 bits)
          upper_offset=lower_offset+7;
      else if (transfer_size=='b01) // 2 bytes (16 bits)
          upper_offset=lower_offset+15;
      else if(transfer_size=='b10) // 4 bytes (32 bits)
          upper_offset=lower_offset+31;
      else if(transfer_size=='b11) // 8 bytes (64 bits)
          upper_offset=lower_offset+63;
    return tuple2(upper_offset,lower_offset); endfunction

//  function Bit#(TMul#(8,TMul#(_word_size,_block_size))) update_data_line (Bit#(TLog#(TMul#(8,TMul#(_word_size,_block_size)))) lower_offset, Bit#(TLog#(TMul#(8,TMul#(_word_size,_block_size)))) upper_offset, Bit#(2) transfer_size, Bit#(TMul#(8,TMul#(_word_size,_block_size))) data, Bit#(TMul#(8,_word_size)) write_data);
//      Bit#(lower_offset) y = data[lower_offset-1:0];
//      let x= data[fromInteger(8*v_word_size*v_block_size-1):upper_offset+1];
//      Bit#(8) temp1= write_data[upper_offset-lower_offset:0];
//      Bit#(16) temp2=write_data[upper_offset-lower_offset:0];
//      Bit#(32) temp3=write_data[upper_offset-lower_offset:0];
//      Bit#(64) temp4=write_data[upper_offset-lower_offset:0];
//      Bit#(TMul#(8,TMul#(_word_size,_block_size))) new_data=0; // stupid workarounds for bluespec
//      if(transfer_size==0)
//          new_data={x,temp1}; // stupid workarounds for bluespec
//      else if(transfer_size==1)
//          new_data={x,temp2}; // stupid workarounds for bluespec
//      else if(transfer_size==2)
//          new_data={x,temp3}; // stupid workarounds for bluespec
//      else if(transfer_size==3)
//          new_data={x,temp4}; // stupid workarounds for bluespec
//      new_data=new_data << lower_offset;
//      new_data=new_data|y; // finally updated data line.
//      return new_data;
//  endfunction

  function Bit#(TMul#(8,_word_size)) update_data_word (Bit#(TLog#(_word_size)) byte_offset,/*Bit#(TLog#(TMul#(8,_word_size))) lower_offset, Bit#(TLog#(TMul#(8,_word_size))) upper_offset,*/ Bit#(2) transfer_size, Bit#(TMul#(8,_word_size)) data, Bit#(TMul#(8,_word_size)) write_data);

      //Bit#(lower_offset) y = data[lower_offset-1:0];
      //Bit#(TMul#(8,_word_size)) x= data;
      //if(transfer_size==0)
		if(transfer_size==0)begin
			case(byte_offset)
				0 : data[7:0]=write_data[7:0];
				1 : data[15:8]=write_data[7:0];
				2 : data[23:16]=write_data[7:0];
				3 : data[31:24]=write_data[7:0];
				4 : data[39:32]=write_data[7:0];
				5 : data[47:40]=write_data[7:0];
				6 : data[55:48]=write_data[7:0];
				7 : data[63:56]=write_data[7:0];
			endcase
		end
		else if(transfer_size==1)begin
			case(byte_offset[2:1])
				0 : data[15:0]=write_data[15:0];
				1 : data[31:16]=write_data[15:0];
				2 : data[47:32]=write_data[15:0];
				3 : data[63:48]=write_data[15:0];
			endcase
		end
		else if(transfer_size==2)begin
			case(byte_offset[2])
				0 : data[31:0]=write_data[31:0];
				0 : data[63:32]=write_data[31:0];
			endcase
		end
		else if(transfer_size==3)begin
			data=write_data;
		end
      //else if(transfer_size==1)
      //    x[upper_offset:lower_offset]=write_data[8*transfer_size:0]; // stupid workarounds for bluespec
      //else if(transfer_size==2)
      //    x[upper_offset:lower_offset]=write_data[8*transfer_size:0]; // stupid workarounds for bluespec
      //else if(transfer_size==3)
      //    x[upper_offset:lower_offset]=write_data[8*transfer_size:0]; // stupid workarounds for bluespec
		return data;
  endfunction

    FIFO#(Hit_structure_d#(_addr_width,_word_size,_block_size,_cbuff_size,_ways,_prf_size)) ff_response_to_cpu <-mkBypassFIFO();
    FIFO#(Cpu_req_with_token_d#(_addr_width,_word_size,_cbuff_size,_prf_size)) ff_request_from_cpu <-mkSizedFIFO(2); // using FIFO of the same size as BRAM Output FIFO depth.
    FIFO#(To_Memory_d#(_addr_width,_word_size,_block_size)) ff_request_to_memory <-mkBypassFIFO();
    FIFO#(WriteBack_structure_d#(_addr_width,_word_size,_block_size)) ff_write_back_queue <-mkBypassFIFO();
    FIFO#(From_Memory_d#(_addr_width,_word_size,_block_size)) ff_response_from_memory <-mkBypassFIFO();
    Reg#(Bit#(3)) rg_burst_mode <-mkReg(v_block_size==4?'b011:v_block_size==8?'b101:v_block_size==16?'b111:0);
    Ifc_LoadBuff_d#(_addr_width,8,_word_size,_block_size,_cbuff_size,_ways,_prf_size) ldbuff <-mkLoadBuff_d();
//    Ifc_StoreBuff#(8,_word_size,_block_size,_cbuff_size,_ways) stbuff <-mkStoreBuff();
    SFIFO#(8,WaitBuff_d#(_addr_width,_word_size,_block_size,_cbuff_size,_ways,_prf_size),Bit#(_addr_width)) waitbuff <-mkCFSFIFO(isfound, "DCACHE");
    Vector#(_block_size, Reg#(Maybe#(Recent_access_d#(_addr_width,_word_size,_block_size)))) recently_updated_line <-replicateM(mkReg(tagged Invalid));
	Reg#(Bit#(TLog#(_ways))) rg_replace_block <- mkReg(0);

	Reg#(Bit#(TAdd#(1,TLog#(_block_size)))) rg_word_count <- mkReg(0);
    Reg#(Bit#(TAdd#(1,TLog#(_sets)))) rg_index <-mkReg(0);
    Reg#(Bool) rg_initialize<-mkReg(True);
    Reg#(Bool) stall_processor <-mkReg(False);
    Reg#(Bit#(2)) rg_valid_dirty_bits <-mkReg(0);
		
    BRAM_Configure cfg = defaultValue ;
    cfg.latency=1;
    cfg.outFIFODepth=2; // for pipelined cache
    cfg.allowWriteResponseBypass=True;
		BRAM2Port#(Bit#(TLog#(_sets)), Bit#(num_of_tag_bits)) tag [v_ways]; // declaring as many tag arrays as there are number of `Ways. Depth of each array is the number of sets.
		BRAM2Port#(Bit#(TLog#(_sets)), Bit#(2)) valid_dirty [v_ways];     // declaring as many alid bit arrays as there are number of `Ways. Depth of each array is the number of sets.
		BRAM2Port#(Bit#(TLog#(_sets)), Bit#(TMul#(8,_word_size))) data [v_ways][v_block_size]; // decalring similar data arrays. each of width equal to block size.

		Reg#(Bit#(TSub#(_ways,1))) pseudo_lru [v_sets];
		for(Integer i=0;i<v_sets;i=i+1)
			pseudo_lru[i]<-mkReg(0);

		for(Integer i=0;i<v_ways;i=i+1)begin
			tag[i] <- mkBRAM2Server(cfg);		
			for(Integer j=0;j<v_block_size;j=j+1)
				data[i][j] <- mkBRAM2Server(cfg);
			valid_dirty[i]<-mkBRAM2Server(cfg);
		end

    rule initialize_cache(rg_initialize);
      rg_index<=rg_index+1;
      for(Integer i=0;i<v_ways;i=i+1)
        valid_dirty[i].portB.request.put(BRAMRequest{write:True,address:truncate(rg_index), datain:0,responseOnWrite:False});
      if(rg_index==fromInteger(v_sets-1))
        rg_initialize<=False;
      $display("DCACHE : Flushing Cache",$time);
    endrule

    rule read_from_bram_and_load_buffer(!stall_processor && !rg_initialize);
		Bit#(num_of_tag_bits) tag_values[v_ways];	// hold the tag values
		Bit#(2) valid_dirty_values [v_ways];		// hold the valid and dirty bits
		Bit#(TMul#(8,_word_size)) data_values [v_ways][v_block_size]; // hold the cache lines.
		let cpu_addr=ff_request_from_cpu.first.request.address;
		Bit#(TLog#(_block_size)) block_offset = cpu_addr[v_num_of_offset_bits-1:v_num_of_bytes];
		Bit#(TSub#(_addr_width,log_word_size)) input_tag=ff_request_from_cpu.first.request.address[v_addr_width-1:v_num_of_bytes];
		Bit#(_addr_width) byte_address=ff_request_from_cpu.first.request.address[v_addr_width-1:v_num_of_offset_bits]<<v_num_of_offset_bits;
		Bit#(_addr_width) address=ff_request_from_cpu.first.request.address;
		for(Integer i=0;i<v_ways;i=i+1)begin
			valid_dirty_values[i]<-valid_dirty[i].portA.response.get();
			tag_values[i]<-tag[i].portA.response.get();
			for(Integer j=0;j<v_block_size;j=j+1)
				data_values[i][j]<-data[i][j].portA.response.get();
		end
      ///////////////////////////////////////////////// calculate the upper and lower offsets. ////////////////////////////////////////////////////////////
      let transfer_size=ff_request_from_cpu.first.request.transfer_size;
      match {.upper_offset,.lower_offset}=find_offset_index(transfer_size,cpu_addr);
      $display("DCACHE : Upper Offset :%d, Lower Offset:%d",upper_offset,lower_offset,$time);
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
        $display("DCACHE : PLRU block to replace chosen : %d",block_to_replace,$time);
      end
      // Check if there is a hit in the bram. if hit then matched_tag != -1. If it is -1 then block_to_replace will indicate the block which is dirty and needs replacement
			Integer matched_tag=-1;
      ff_request_from_cpu.deq();
			for(Integer i=0; i<v_ways; i=i+1)begin
				if(valid_dirty_values[i][1]==1'b1 && tag_values[i]==ff_request_from_cpu.first.request.address[v_addr_width-1:v_addr_width-v_num_of_tag_bits]) // tag match
					matched_tag=i;	// here this variable indicates which tags show a match.
			end

      //if((lower_offset[2:0]!=0) || (transfer_size==1 && lower_offset[3]==1) || (transfer_size==2 && lower_offset[4:3]!=0) || (transfer_size==3 && lower_offset[5:3]!=0)) begin
      //    ff_response_to_cpu.enq(Hit_structure_d{data_word:0,
      //                                        transfer_size:transfer_size,
	  //  									  u_signed : ff_request_from_cpu.first.request.u_signed,
      //                                        token:ff_request_from_cpu.first.token,
      //                                        write_data:ff_request_from_cpu.first.request.write_data,
      //                                        upper_offset:upper_offset,
      //                                        lower_offset:lower_offset,
      //                                        replace_block:fromInteger(matched_tag),
      //                                        address:ff_request_from_cpu.first.request.address,
	  //  									  destination_tag:ff_request_from_cpu.first.request.destination_tag,
      //                                        ld_st:ff_request_from_cpu.first.request.ld_st});

      //end
      if(matched_tag!=-1)begin// on a hit return the required data to the completion buffer.
        $display("DCACHE : Hit for address : %h", ff_request_from_cpu.first.request.address);
          ff_response_to_cpu.enq(Hit_structure_d{data_word:data_values[matched_tag][block_offset],
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
        $display("DCACHE : Changed PLRU for set : %d with bits :%b", set,lru_bits_new,$time);
        //recently_updated_line<=tagged Invalid;
      //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      end
      else begin // miss has occured
        $display("DCACHE : Miss for address : %h ", ff_request_from_cpu.first.request.address,fshow(ff_request_from_cpu.first.request.ld_st),$time);
        if(replace_block==-1)begin
		  Bit#(_addr_width) replace_line_address = zeroExtend({tag_values[block_to_replace],set}) << v_num_of_offset_bits;
          ff_write_back_queue.enq(WriteBack_structure_d{address:replace_line_address,
                                                      data_line:concate_vec(data_values[block_to_replace]),
                                                      transfer_size:transfer_size,
                                                      burst_mode:rg_burst_mode});
          $display("DCACHE : Enquing into the WRITE BACK QUEUE", $time);
        end
        Bool hit =False;
        if(recently_updated_line[block_offset] matches tagged Valid .z)
          if(z.address[v_addr_width-1:v_num_of_bytes]==input_tag)
            hit=True;

        if(hit==True)begin // load miss and hit in recently updated buffer.
          ff_response_to_cpu.enq(Hit_structure_d{data_word:validValue(recently_updated_line[block_offset]).data_word,// TODO
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
        else if(ldbuff.search(ff_request_from_cpu.first.request.address))begin // if the line has already been accessed earlier then enqueue the request into the waitbuffer.
          waitbuff.enq0(WaitBuff_d{ld_st:ff_request_from_cpu.first.request.ld_st,
								u_signed : ff_request_from_cpu.first.request.u_signed,
                                address:ff_request_from_cpu.first.request.address,
                                transfer_size:ff_request_from_cpu.first.request.transfer_size,
                                write_data:ff_request_from_cpu.first.request.write_data,
                                upper_offset:upper_offset,
                                lower_offset:lower_offset,
                                token:ff_request_from_cpu.first.token,
								destination_tag:ff_request_from_cpu.first.request.destination_tag,
                                replace_block:fromInteger(block_to_replace)});
          $display("DCACHE : sending request to wait buffer for token : %d with tag :%h", ff_request_from_cpu.first.token,ff_request_from_cpu.first.request.address,$time);
        end
        else begin // load or store miss
          $display("DCACHE : Inserting Address %h in LdBuff for token : %d", ff_request_from_cpu.first.request.address,ff_request_from_cpu.first.token,$time);
          ldbuff.insert(LoadBufferData_d{request:To_Memory_d{address:cpu_addr,transfer_size:transfer_size,burst_mode:rg_burst_mode,ld_st:ff_request_from_cpu.first.request.ld_st,write_data:ff_request_from_cpu.first.request.write_data},

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
		$display("DCACHE : enqueued into requesting memory with address %h", info.request.address);
      end
    endrule

    rule write_into_bram_on_store_hit(ff_response_to_cpu.first.ld_st==Store && !rg_initialize);
      let x=ff_response_to_cpu.first();
      ff_response_to_cpu.deq();
	  Bit#(TLog#(_block_size)) block_offset = x.address[v_num_of_offset_bits-1:v_num_of_bytes];
	  Bit#(TLog#(_word_size)) byte_offset = x.address[v_num_of_bytes-1:0];
      Bit#(2) valid_dirty_bits=2'b11;
      Bit#(TMul#(8,_word_size)) y= update_data_word(/*x.lower_offset,x.upper_offset*/byte_offset,x.transfer_size,x.data_word,x.write_data);
	  $display("DCACHE : stored into BRAM lower_offset %d upper_offset %d transfer_size %d, with data %h overwritten by %h",x.lower_offset,x.upper_offset,x.transfer_size, x.data_word, x.write_data);
	  $display("COMMIT:data sent to the function at address %h and output data is %h", x.address, y);
      data[x.replace_block][block_offset].portB.request.put(BRAMRequest{write:True,address:x.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:y,responseOnWrite:False});
      //valid_dirty[x.replace_block].portB.request.put(BRAMRequest{write:True,address:x.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:valid_dirty_bits,responseOnWrite:False});
      recently_updated_line[block_offset]<=tagged Valid (Recent_access_d{data_word:y,address:x.address});
    endrule
    
    rule process_wait_buffer(stall_processor && !rg_initialize);
    	let resp=ff_response_from_memory.first();
    	Bit#(TSub#(_addr_width,log_word_size)) input_tag=resp.address[v_addr_width-1:v_num_of_bytes];
		Bit#(TLog#(_block_size)) block_offset = resp.address[v_num_of_offset_bits-1:v_num_of_bytes];
    	let x=waitbuff.first();
    	waitbuff.deq;
    	if(x.address[v_addr_width-1:v_num_of_bytes]==input_tag)begin
    	  $display("DCACHE :  Wait buffer search made a hit for address %h", resp.address, $time);
    	  ff_response_to_cpu.enq(Hit_structure_d{data_word:validValue(recently_updated_line[block_offset]).data_word,
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
    		waitbuff.enq1(x);
    	match {.y,.z}= waitbuff.search(resp.address);
    	if(!y)begin
    		ff_response_from_memory.deq();
    		stall_processor<=False;
    	end
    endrule

	rule got_response_from_memory(!stall_processor && !rg_initialize);
		let resp=ff_response_from_memory.first();
		Bit#(TSub#(_addr_width,log_word_size)) input_tag=resp.address[v_addr_width-1:v_num_of_bytes];
		Bit#(TLog#(_block_size)) block_offset = resp.address[v_num_of_offset_bits-1:v_num_of_bytes];
		Bit#(TLog#(_ways)) replace_block;
		replace_block = rg_replace_block;
		match {.u, .v} <-ldbuff.remove(resp.address);
		if(u) begin
			$display("DCACHE : Hit in the load buffer", resp.address);
			replace_block = v.metadata.replace_block;
			rg_replace_block <= v.metadata.replace_block;
			ff_response_to_cpu.enq(Hit_structure_d{data_word	:	resp.data_word,
		                                        transfer_size	:v.request.transfer_size,
		  									  	destination_tag	:v.metadata.destination_tag,
		                                        address			:v.request.address,
		  									  	u_signed 		: v.metadata.u_signed,
		                                        token			:v.metadata.token,
		                                        write_data		:v.request.write_data,
		                                        upper_offset	:v.metadata.upper_offset,
		                                        lower_offset	:v.metadata.lower_offset,
		                                        replace_block	:v.metadata.replace_block,
		                                        ld_st			:v.request.ld_st});
    		ff_response_from_memory.deq();
			if(v.request.ld_st== Load) begin
				$display("DCACHE : Its a load so writing into BRAM at address %h at set %h", resp.address, v.metadata.replace_block);
				data[v.metadata.replace_block][block_offset].portB.request.put(BRAMRequest{
					  										write	:	True,
															address :	resp.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],
															datain	:	resp.data_word,responseOnWrite:False});
      			recently_updated_line[block_offset] <= tagged Valid (Recent_access_d{address:resp.address,data_word:resp.data_word});
				rg_valid_dirty_bits <= 2'b10;
			end
			else
				rg_valid_dirty_bits <= 2'b11;
		end
		else begin
    		match {.x,.y,.z}= waitbuff.search(resp.address);
    		//if(waitbuff.search(zeroExtend(input_tag)))begin
			if(!x) begin
				$display("DCACHE : Its a load so writing into BRAM at address %h at set %h", resp.address, v.metadata.replace_block);
				data[replace_block][block_offset].portB.request.put(BRAMRequest{
				  										write	:	True,
														address :	resp.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],
														datain	:	resp.data_word,responseOnWrite:False});
      			recently_updated_line[block_offset] <= tagged Valid (Recent_access_d{address:resp.address,data_word:resp.data_word});
			end
    		if(y)begin
    			$display("DCACHE : Stalling processor");
    			stall_processor<=True;     
    		end
    		else begin 
				$display("DCACHE : Dequeing the memory FIFO");
    			ff_response_from_memory.deq();
			end
		end
        if(rg_word_count==fromInteger(v_block_size-1))begin
			$display("DCACHE : writing into brams for address %h in the set %h", resp.address, replace_block);
      		tag[replace_block].portB.request.put(BRAMRequest{write:True,address:resp.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],datain:resp.address[v_addr_width-1:v_addr_width-v_num_of_tag_bits],responseOnWrite:False});
      		valid_dirty[replace_block].portB.request.put(BRAMRequest{write:True,address:resp.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],datain:rg_valid_dirty_bits,responseOnWrite:False});
          	rg_word_count<=0;
        end
        else begin
          rg_word_count<=rg_word_count+1;
        end
		//tag[x.metadata.replace_block].portB.request.put(BRAMRequest{write:True,address:x.request.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits],datain:x.request.address[v_addr_width-1:v_addr_width-v_num_of_tag_bits],responseOnWrite:False});
		//data[x.metadata.replace_block].portB.request.put(BRAMRequest{write:True,address:x.request.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:resp.data_line,responseOnWrite:False});
		//valid_dirty[x.metadata.replace_block].portB.request.put(BRAMRequest{write:True,address:x.request.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:2'b10,responseOnWrite:False});
		//recently_updated_line<=tagged Valid (Recent_access_d{data_word:resp.data_word,address:resp.address});
		//ff_response_to_cpu.enq(Hit_structure_d{data_line:resp.data_line,
		//                                        transfer_size:x.request.transfer_size,
		//  									  destination_tag:x.metadata.destination_tag,
		//                                        address:x.request.address,
		//  									  u_signed : x.metadata.u_signed,
		//                                        token:x.metadata.token,
		//                                        write_data:x.request.write_data,
		//                                        upper_offset:x.metadata.upper_offset,
		//                                        lower_offset:x.metadata.lower_offset,
		//                                        replace_block:x.metadata.replace_block,
		//                                        ld_st:x.request.ld_st});
		$display("DCACHE : Recieved response from the memory. Address: :%h Tag : %h Destination :%h",resp.address,resp.address,v.metadata.destination_tag,$time);
	endrule

	method Action request_from_cpu (Cpu_req_with_token_d#(_addr_width,_word_size,_cbuff_size,_prf_size) req)if(!stall_processor && !rg_initialize);
		for(Integer i=0;i<v_ways;i=i+1)begin // send address to the Block_rams
		    tag[i].portA.request.put(BRAMRequest{write:False,address:req.request.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:?,responseOnWrite:False});
			for(Integer j=0;j<v_block_size;j=j+1)  // send address to the Block_rams
			    data[i][j].portA.request.put(BRAMRequest{write:False,address:req.request.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:?,responseOnWrite:False});
		    valid_dirty[i].portA.request.put(BRAMRequest{write:False,address:req.request.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits], datain:?,responseOnWrite:False});
		end
		ff_request_from_cpu.enq(req); // enqueue the request for next stage
		Bit#(num_of_tag_bits) tag1=req.request.address[v_addr_width-1:v_addr_width-v_num_of_tag_bits];
		Bit#(TLog#(_sets)) set1=req.request.address[v_addr_width-v_num_of_tag_bits-1:v_num_of_offset_bits];
		Bit#(num_of_offset_bits) off1=req.request.address[v_num_of_offset_bits-1:0];
		$display("DCACHE :  BRAM: recieved request for token : %d Address :%h tag %d: Set : %d Offset :%d Access type :", req.token,req.request.address,tag1,set1,off1,fshow(req.request.ld_st),$time);
	endmethod
	
	method ActionValue#(Cpu_resp_with_token_d#(_addr_width,_cbuff_size,_word_size,_prf_size)) response_to_cpu if(ff_response_to_cpu.first.ld_st!=Store && !rg_initialize);
		ff_response_to_cpu.deq();
		let x = ff_response_to_cpu.first();
		bit mis_aligned_error=0;
		$display("DCACHE : transfer_size is %d, address is %d", x.transfer_size, x.address);
		if((x.lower_offset[2:0]!=0) || (x.transfer_size==1 && x.lower_offset[3]==1) || (x.transfer_size==2 && x.lower_offset[4:3]!=0) || (x.transfer_size==3 && x.lower_offset[5:3]!=0)) begin
		  $display("DCACHE : Misaligned error");
		  mis_aligned_error=1;
		end
		return Cpu_resp_with_token_d{response:To_Cpu_d{data_word:x.data_word[x.upper_offset:x.lower_offset],
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
		//waitbuff.clear_all();
		for(Integer i=0;i<v_ways;i=i+1)begin // send address to the Block_rams
			tag[i].portAClear;
			for(Integer j=0;j<v_block_size;j=j+1) // send address to the Block_rams
		   		data[i][j].portAClear;
			valid_dirty[i].portAClear;
			tag[i].portBClear;
			for(Integer j=0;j<v_block_size;j=j+1) // send address to the Block_rams
				data[i][j].portBClear;
			valid_dirty[i].portBClear;
		end
		stall_processor<=False;
		//for(Integer j=0;j<v_block_size;j=j+1) // send address to the Block_rams
		//	recently_updated_line[j]<=tagged Invalid;
	endmethod
  endmodule
endpackage
