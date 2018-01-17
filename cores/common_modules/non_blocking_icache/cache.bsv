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

Description : 
This is a completely parameterized n-way-set-associative cache. It will work for a VIPT or PIPT based architecture. 
VIPT caches are under development.
The parameters are set in the file defined_parameters.bsv 

*/

package cache;

	import BRAM::*;
	import defined_types::*;
  import DReg::*;
	`include "defined_parameters.bsv"
	import Assert::*;

	typedef enum {Initialize,Handle_request, Handle_response, Handle_block_replace, Handle_memory} State deriving (Bits,Eq,FShow); // defines the current state of the cache
	typedef enum {Idle,Busy,Available} Cache_output deriving(Bits,Eq,FShow); // defines the status of the output of the cache to the processor.

	interface Ifc_cache;
			/* Input methods */
        //  gets input from the cpu. wbwt-1:writeback 0:writethru wawna-0:write-noallocate 1:writeallocate
    method Action _flush_from_cpu(Bool flush);
		method Action _data_from_cpu(Bit#(`Addr_width) address, Bit#(TMul#(8,`Word_size)) datain, Bit#(1) rd_wr, Bit#(2) wbwt_wawna, Bit#(2) transfer_size, Bit#(1) cache_enable);
		method Action _data_from_memory(Bit#(TMul#(8,TMul#(`Word_size,`Block_size))) datain);   // gets input from the memory
		method Action _bus_error_from_memory(bit buserror);                                     // recieves any bus error generated during a write/read operation to memory
			/* Output Methods */
		method Bit#(TMul#(`Word_size,8)) data_to_cpu_;                                          // output to the cpu carrying the data
		method Cache_output cache_output;                                                       // status of the output to the cpu
		method Maybe#(Bit#(`Addr_width)) address_to_memory_;                                    // address to the memory.
		method Bit#(TMul#(8,TMul#(`Word_size,`Block_size))) data_to_memory_;                    // data to be written to memory
		method Bit#(1) rd_wr_to_memory_;                                                        // read of write operation to the memory.
    method Bit#(1) bus_error_to_cpu_();                                                     // indicates that there was a bus error while perfoming an operation on the memory. 
    method Bit#(2) transfer_size_to_memory_();                                              // the size of read/write requested by the processor. 0- byte. 1-halfword(16 bits) 2- word (32bits) 3-doubleword (64bits)
    method Bit#(3) burst_mode_to_memory();                                                  // follows the AHB lit protocol.
    method Bool mis_aligned_access();                                                         // indicates there has been a misaligned to the cache.

	endinterface

	(*synthesize*)
	module mkcache#(parameter String name)(Ifc_cache); // module definition
        
    Wire#(Bool) wr_flush_everything <-mkDWire(False);
		Wire#(Maybe#(Bit#(`Addr_width))) wr_address_from_cpu <-mkDWire(tagged Invalid); 		// carries the input address from the cpu.
		Wire#(Bit#(TMul#(8,`Word_size))) wr_data_from_cpu <-mkDWire(0);				// carries the input data to be written in memory from the cpu.
		Wire#(Bit#(1)) wr_rd_wr_cpu<-mkDWire(0); 									// read write wire from the cpu. input from cpu
    Wire#(Bit#(2)) wr_transfer_size <-mkDWire(0);                               // size of request made by the cup. input from cpu.
		Wire#(Bit#(TMul#(`Word_size,8))) wr_data_to_cpu <-mkDWire(0); 				// contains the final word to the cpu. output to cpu
		Wire#(Cache_output) wr_cache_output <-mkDWire(Idle); 						// can be idle, Busy, Available. output to cpu
    Wire#(Bit#(1)) wr_writeback <-mkDWire(0); 									// 0 means writethrough mode. 1 means writeback mode. input from cpu
    Wire#(Bit#(1)) wr_writeallocate<-mkDWire(0); 								// 0 meands write-noallocate. 1 meands writeallocate. input from cpu
    Wire#(Bit#(2)) wr_transfer_size_to_mem <-mkDWire(0);                        // sending the transfer size to the memory. 0-byte, 1-haflword, 2-word. output to memory
    Wire#(Bit#(3)) wr_burst_mode_to_memory<-mkDWire(0);                         // sending burst mode to memory - AHB lite encoding. output to memory
		Wire#(Maybe#(Bit#(32))) wr_address_to_memory <- mkDWire(tagged Invalid);	// address to the memory. output to memory
		Wire#(Bit#(TMul#(8,TMul#(`Word_size,`Block_size)))) wr_data_to_memory <- mkDWire(0); // the complete data to be written. output to memory
		Wire#(Bit#(1)) wr_rd_wr_memory <-mkDWire(0);								// 0 is read 1 is write. output to memory
		Wire#(Maybe#(Bit#(TMul#(8,TMul#(`Word_size,`Block_size))))) wr_data_from_memory <-mkDWire(tagged Invalid); // carries the data coming from the memory.
    Wire#(Bit#(1)) wr_bus_error<-mkDWire(0);                                    // 0 means no error. 1 means error. input wire from memory
    Wire#(Bit#(1)) wr_error_to_cpu<-mkDWire(0);                                 // 0 means no error. 1 means error. output wire to cpu
    Wire#(Bit#(1)) wr_cache_enable <-mkDWire(0);                                // 0 means cache is disabled 1 means enabled.input wire from cpu
    Wire#(Bool) wr_mis_aligned <-mkDWire(False);                               // this wire when set to TRUE indicates if there has been a miss-aligned access to the cache.
        
    Reg#(Bit#(1)) rg_cache_enable <-mkReg(0);                                   // 0 means cache is disabled 1 means enabled.register
		Reg#(Bit#(32)) rg_address_from_cpu <-mkReg(0); 								// carries the input address from the cpu.
		Reg#(Bit#(TMul#(8,`Word_size))) rg_data_from_cpu <-mkReg(0);				// carries the input data to be written in memory from the cpu.
		Reg#(Bit#(32)) rg_prev_block_address<-mkReg(0);								// address of the block to replaced.
		Reg#(State) rg_state <-mkReg(Initialize);	                                // holds the information about which state the cache is in currently.
		Reg#(Bit#(TLog#(`Num_of_sets))) rg_counter <-mkReg(0);                       // counter to count the number of entries in the BRAM.
		Reg#(Bit#(TLog#(`Ways))) rg_block_to_replace <-mkReg(0); 					// contains the block to be replaced in case of miss or the block to be updated in case of hit.
		Reg#(Bit#(TLog#(`Num_of_sets))) rg_set<-mkReg(0);							// the set that is being accessed.
		Reg#(Bit#(TLog#(TMul#(8,TMul#(`Word_size,`Block_size))))) rg_lower_offset<-mkReg(0);	// the lower offset index of the word in the data line to be updated
		Reg#(Bit#(TLog#(TMul#(8,TMul#(`Word_size,`Block_size))))) rg_upper_offset<-mkReg(0);	// the upper offset index of the word in the data line to be updated
    Reg#(Bit#(1)) rg_rd_wr <-mkReg(0); 											// 0 : read 1 : write
    Reg#(Bit#(1)) rg_writeback <-mkReg(0); 										// 0 is writethru 1 is writeback
    Reg#(Bit#(1)) rg_writeallocate<-mkReg(0); 									// 0 is write noallocate 1 is write-allocate
    Reg#(Bit#(2)) rg_transfer_size<-mkReg(0);                                   // the size of data request made by the cpu. 0-byte. 1-halfowrd. 2-word.
    Reg#(Bit#(3)) rg_burst_mode<-mkReg(0);                                      // AHB lite protocol encoding for burst mode.
    Reg#(Bit#(1)) rg_hit <-mkReg(0) ; 											// 0 miss occurred. 1 hit occurred.
    Reg#(Bit#(1)) rg_isblock_dirty <-mkReg(0) ; 								// 0 block is not dirty. 1 block is dirty hence need to be replaced.
    Reg#(Bit#(TMul#(8,TMul#(`Word_size,`Block_size)))) rg_prev_block_data<-mkReg(0);
		
		// Declaring the required Tag memory array. Tag has 2 extra MSB bits representing the valid bit at the MSB and dirty bit at MSB-1 bit. 
		BRAM_Configure cfg = defaultValue ;
		//BRAM1Port#(Bit#(TLog#(`Num_of_sets)), Bit#(TSub#(`Addr_width,TAdd#(TLog#(`Num_of_sets),TLog#(TMul#(`Block_size,`Word_size)))))) tag [`Ways];
		BRAM1Port#(Bit#(TLog#(`Num_of_sets)), Bit#(`Num_of_tag_bits)) tag [`Ways]; // declaring as many tag arrays as there are number of `Ways. Depth of each array is the number of sets.
		BRAM1Port#(Bit#(TLog#(`Num_of_sets)), Bit#(2)) valid_dirtybit [`Ways];     // declaring as many alid bit arrays as there are number of `Ways. Depth of each array is the number of sets.
		BRAM1Port#(Bit#(TLog#(`Num_of_sets)), Bit#(TMul#(8,TMul#(`Word_size,`Block_size)))) data [`Ways]; // decalring similar data arrays. each of width equal to block size.
    Reg#(Bit#(`Addr_width)) rg_pre_cache_addr <-mkReg(32'hFFFFFFFF);
    Reg#(Bit#(TMul#(8,TMul#(`Word_size,`Block_size)))) rg_pre_cache_data <-mkReg(0);

		// initialize the pseduo lru bits for each set. number of bits is the number of way's -1. Algorithm in documents.
		Reg#(Bit#(TSub#(`Ways,1))) pseudo_lru [`Num_of_sets];
		for(Integer i=0;i<`Num_of_sets;i=i+1)
			pseudo_lru[i]<-mkReg(0);

		for(Integer i=0;i<`Ways;i=i+1)begin
			tag[i] <- mkBRAM1Server(cfg);		
			data[i] <- mkBRAM1Server(cfg);
			valid_dirtybit[i]<-mkBRAM1Server(cfg);
		end

//		rule put_space;
//			$display("\n\n\n");
//		endrule
		rule show_state;
			$display("%s State=",name,fshow(rg_state),$time);
		endrule

        rule flush_and_go_to_initial_state(wr_flush_everything);
            wr_address_to_memory<=tagged Invalid;
            wr_cache_output<=Idle;
            wr_data_to_cpu<=0;
            rg_state<=Handle_request;    
		      	for(Integer i=0;i<`Ways;i=i+1)begin
      				valid_dirtybit[i].portAClear(); // invalidate all the valid bits and 
              tag[i].portAClear();
              data[i].portAClear();
            end
            rg_hit<=0;
            rg_pre_cache_addr<=32'hFFFFFFFF;
            rg_pre_cache_data<=0;
            rg_isblock_dirty<=0;
            rg_address_from_cpu<=0;
            rg_data_from_cpu<=0;

        endrule

        /*
		 This rule is fired on system reset and flushes all the entries in the cache to 0. Untill the flush is not over the output of the cache 
         to the cpu is made BUSY. Flushing will take cycles = Number of sets. Flushing will only invalidate the valid bits. If required the 
         data and tag entries can be initialized here by uncommenting the lines.
        */ 
		rule rl_reset_cycle(rg_state==Initialize && !wr_flush_everything);
				wr_cache_output<=Busy;											// make the cache output to the cpu busy.
				if({1'b0,rg_counter}!=fromInteger(`Num_of_sets-1))				// increment the counter to initialize all the memory elements.
                    rg_counter<=rg_counter+1;									// count till the end of all sets.
				else
					rg_state<=Handle_request;                                   // once over go to handle request from the processor.
				$display("%s flushing entry number - %d",name,rg_counter,$time);
				for(Integer i=0;i<`Ways;i=i+1)begin
					valid_dirtybit[i].portA.request.put(BRAMRequest{write:True,address:rg_counter,datain:0,responseOnWrite:False}); // invalidate all the valid bits and 
				end
        // finds the amount of burst mode needed based on the number of words in the line. It uses the AHB-lite bus encoding for burst mode. 
        if(`Block_size==4)        // Number of words in each block is 4. words here refers to the basic transaction performed by the cpu. usually 32-bit or 64bit.
            rg_burst_mode<='b011; // INCR 4
        else if (`Block_size==8)
            rg_burst_mode<='b101; // INCR 8
        else if (`Block_size==16)
            rg_burst_mode<='b111; // INCR 16
		endrule

		/*
			This rule handles the request made by the CPU. Each request from the cpu contains the address, data to be written, read or write. transfer size, write-back or writethru
            write-allocate or no allocate and whether cache needs to be enabled or diabled. 

            if the cache is disabled, address and data is directly sent to the memory along with the read/write signal and transfer size. Burst mode is made 0 since the cpu will only
            perform a single transaction. hit flag is also made 0 here. This mode will be mainly used when the processor needs to read data from the external bus.

            if cache is enabled, request is sent to the BRAMs for fetching the valid_dirty bits, tags and the respective data lines. Simultaneously the indexes of the byte/halfword/word to
            be affected in the dataline are also calculated and stored in the registers rg_upper_offset and rg_lower_offset respectively.
			The state is transfered to Handle_response. Important data such as address, writeback mode, allocate mode etc, are catptured in registers.

		*/
		rule rl_handle_request(rg_state==Handle_request && !wr_flush_everything);
			$display("%s:Handle Request",name,$time);
			if(wr_address_from_cpu matches tagged Valid .cpu_addr)begin // send request to cache to get the tags and respective data lines.
        if(wr_cache_enable==1 && ((wr_transfer_size==1 && cpu_addr[0]==1) || (wr_transfer_size==2 && cpu_addr[1:0]!=0))) begin // miss aligned address. in case of LH offset==3 and LW offset!=0
            wr_cache_output<=Available;
            wr_data_to_cpu<=0;
            wr_mis_aligned<=True;
        end
        else if(wr_cache_enable==0) begin            // if the cache is diabled simply send the address and data to memory and update other register accordingly.
            rg_cache_enable<=0;
            rg_state<=Handle_memory;
            rg_address_from_cpu<=cpu_addr;
            wr_address_to_memory<=tagged Valid cpu_addr;
            wr_rd_wr_memory<=wr_rd_wr_cpu;
            rg_rd_wr<=wr_rd_wr_cpu;
            rg_hit<=0;                      // shows a miss occurred in cache.
            rg_data_from_cpu<=wr_data_from_cpu;
            wr_transfer_size_to_mem<=wr_transfer_size;
            wr_data_to_memory<=zeroExtend(wr_data_from_cpu);
            wr_burst_mode_to_memory<=0;     // single transaction at the memory.
        end
        else begin
            Integer num_of_bytes=valueOf(TLog#(`Word_size)); // number of bits to represent each byte in a word
            $display("%s:PRECACHE ADDR :%h CPUADDR : %h",name,rg_pre_cache_addr,cpu_addr,$time);
            //////////////////////////////////////// Find which word of the cache line is the cpu requesting ///////////////////////////////////////////
            Bit#(TLog#(TMul#(8,TMul#(`Word_size,`Block_size)))) lower_offset=0;
            Bit#(TLog#(TMul#(8,TMul#(`Word_size,`Block_size)))) upper_offset=0;
            for(Integer i=0;i<`Block_size;i=i+1)begin
                if(fromInteger(i)==unpack(cpu_addr[`Num_of_offset_bits-1:num_of_bytes]))begin // the lower order bits used to access each byte.
                    lower_offset= fromInteger(`Word_size)*8*fromInteger(i); // calculating the lower bit index value. For. eg if word is 32bit. possible values are 0,32,64,96...
      			upper_offset= (fromInteger(`Word_size)*8)*(fromInteger(i)+1)-1; // calculating the the upper bit index value. if work is 32 bit. possible values are 31,63,95...
                end
            end
            lower_offset=lower_offset+(cpu_addr[num_of_bytes-1:0]*8); // exact byte offset to start the transaction from.
            if(wr_transfer_size=='b00) // one byte (8 bits)
                upper_offset=lower_offset+7;
            else if (wr_transfer_size=='b01) // 2 bytes (16 bits)
                upper_offset=lower_offset+15;
            else if(wr_transfer_size=='b10) // 4 bytes (32 bits)
                upper_offset=lower_offset+31;
            else if(wr_transfer_size=='b11) // 8 bytes (64 bits)
                upper_offset=lower_offset+63;
      
            rg_lower_offset<=lower_offset;
            rg_upper_offset<=upper_offset;
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            if(rg_pre_cache_addr[31:4]==cpu_addr[31:4])begin
               wr_cache_output<=Available;
               wr_data_to_cpu<=rg_pre_cache_data[upper_offset:lower_offset];
            end
            else begin
                rg_pre_cache_addr<=cpu_addr;
                rg_cache_enable<=1;
                rg_address_from_cpu<=cpu_addr;
                rg_data_from_cpu<=wr_data_from_cpu;
                rg_set<=cpu_addr[`Addr_width-`Num_of_tag_bits-1:`Num_of_offset_bits];
                for(Integer i=0;i<`Ways;i=i+1)begin
                    tag[i].portA.request.put(BRAMRequest{write:False,address:cpu_addr[`Addr_width-`Num_of_tag_bits-1:`Num_of_offset_bits], datain:?,responseOnWrite:False});
                    data[i].portA.request.put(BRAMRequest{write:False,address:cpu_addr[`Addr_width-`Num_of_tag_bits-1:`Num_of_offset_bits], datain:?,responseOnWrite:False});
                    valid_dirtybit[i].portA.request.put(BRAMRequest{write:False,address:cpu_addr[`Addr_width-`Num_of_tag_bits-1:`Num_of_offset_bits], datain:?,responseOnWrite:False});
                end
                rg_rd_wr<=wr_rd_wr_cpu;
                rg_writeback<=wr_writeback;
                rg_writeallocate<=wr_writeallocate;
                rg_state<=Handle_response;
                rg_transfer_size<=wr_transfer_size;
                if(wr_rd_wr_cpu==0)begin
                    $display("%s:Handling Request : Read cycle Initiating for Address : %32h.",name,cpu_addr,$time);
                end
                else begin
                    $display("%s:Handling Request : Write cycle Initiating for Address : %32h.",name,cpu_addr,$time);
                end
            end
        end
			end
		endrule

        /*
        This rule recieves the data,tag and valid bits from the BRAM arrays and decides whether there was a hit or miss.
        firs the tags are checked if they are valid and they match the tags from the CPU. If a valid match is found it is a hit else a miss.

        On a read hit the byte/halfword/word from the dataline is selected using the rg_lower_offset and rg_upper_offset registers
        and sent to the cpu. The LRU bits are updated based on the pseudo LRU policy. 

        On a write hit however, the dataline is read from the cache modified. It is written back to the cache only if write-back mode is active. In this case
        the dirty bit is also made 1. In write-through however, the modified data is first written sent to the memory and the modified line is held in a register
        and is then updated in the cache only when the memory finishes the write and return without an error. The psedo LRU bits are updated based on the accessed
        tag.

        On a miss however, we check if an invalid tag (tag with valid bit 0) exists. If so then this tag is to be replaced with the fetched data. If all tags
        are valid then the current lru bits are used to decide which tag is to be replaced. The LRU bits are then updated. 
        */

		rule rl_read_response_from_BRAM(rg_state==Handle_response && !wr_flush_everything);
			$display("%s:Handling Response Cycle",name,$time);	
			Bit#(`Num_of_tag_bits) tag_values[`Ways];	// hold the tag values
			Bit#(2) valid_dirtybitvalues [`Ways];		// hold the valid and dirty bits
		  Bit#(TMul#(8,TMul#(`Word_size,`Block_size))) data_values [`Ways]; // hold the cache lines.

			// declare intermediate variables which will hold the required tag, valid, dirty, and cache lines which have been indexed.
			// read the responses from the respective BRAMs.
			for(Integer i=0;i<`Ways;i=i+1)begin
				valid_dirtybitvalues[i]<-valid_dirtybit[i].portA.response.get();
				tag_values[i]<-tag[i].portA.response.get();
				data_values[i]<-data[i].portA.response.get();
			end

			// check which tag is matching. " variable matched tag will tell which one of the tags has matched
			// TODO for VIPT change the matching of the tag form cpu to TLB provided data.
			$display("%s:Tags from the cpu : %32h",name,rg_address_from_cpu,$time);
			Integer matched_tag=-1;
			for(Integer i=0; i<`Ways; i=i+1)begin
				if(valid_dirtybitvalues[i][1]==1'b1 && tag_values[i]==rg_address_from_cpu[`Addr_width-1:`Addr_width-`Num_of_tag_bits])
					matched_tag=i;	// here this variable indicates which tags show a match.
			end
            $display("%s:Upper offset : Lower Offset %d : %d",name,rg_upper_offset,rg_lower_offset,$time);
			Bit#(TSub#(`Ways,1)) lru_bits_new = pseudo_lru[rg_set];
			/////////////////////////////////////////////////////////////////// Miss has occurred in the cache //////////////////////////////////////////////////////////////////////
			if(matched_tag==-1)begin // miss occurred go to memory stage
                rg_hit<=0;
                
				wr_cache_output<=Busy;  // make cache busy
				$display("%s:No tags match. Sending request to Memory",name,$time);
				rg_state<=Handle_memory; // change state
                if(rg_rd_wr==0) begin// read operation will always be in terms of word transfers
                    wr_transfer_size_to_mem<=fromInteger(valueOf(TLog#(`Word_size)));
                    wr_burst_mode_to_memory<=rg_burst_mode;
                    Bit#(`Num_of_offset_bits) lower_bits=0;
    				wr_address_to_memory<=tagged Valid ({rg_address_from_cpu[`Addr_width-1:`Num_of_offset_bits],lower_bits}); // send address to memory
                    rg_address_from_cpu<={rg_address_from_cpu[`Addr_width-1:`Num_of_offset_bits],lower_bits};
                end
                else begin
                    wr_transfer_size_to_mem<=rg_transfer_size;
                    wr_burst_mode_to_memory<=0;
    				wr_address_to_memory<=tagged Valid (rg_address_from_cpu); // send address to memory
                end
				wr_data_to_memory<=zeroExtend(rg_data_from_cpu); // read operation so no memory to be sent
				wr_rd_wr_memory<=rg_rd_wr; // read operation
				
				///////////////////////////////////////////////////////////////// find which tag to replace using PLRU ////////////////////////////////////////////////////////////////////////////////
				Integer replace_block=-1;                   // initialize to a non-existent block
				let addr=rg_address_from_cpu[`Addr_width-`Num_of_tag_bits-1:`Num_of_offset_bits];
				let lru_bits = pseudo_lru[rg_set];          // read current lru bits.
				$display("%s:LRU bits : %b",name,pseudo_lru[rg_set]);
				for(Integer i=`Ways-1;i>=0;i=i-1)           // find if all the blocks are valid or not. If not chose one to replace
                    if(valid_dirtybitvalues[i][1]==0)begin
						replace_block=i;
                        matched_tag=i;                      // this is the block which will be accessed. This is used to update the lru bits.
                    end

				if(replace_block==-1)begin                  // if all blocks are valid.
					// Ways = n
					// number of bits in lru = n-1
					// index is from 0 to n-2
					Integer left=0;
					Integer right=0;
					Integer i=0;
					$display("%s:performing PLRU",name,$time);
					while(i<(`Ways-1))begin
						left=i+1;
						right=i+2;
						if(lru_bits[`Ways-2-i]==0)
							i=i+left;
						else
							i=i+right;
					end
					$display("%s:Block to be replaced",name,fromInteger(i-`Ways+1),$time);
					rg_block_to_replace<=fromInteger(i-`Ways+1);
                    matched_tag=fromInteger(i-`Ways+1);         // this variable now indicates which tag is to be replaced.
					rg_prev_block_address<={tag_values[matched_tag],rg_address_from_cpu[`Addr_width-`Num_of_tag_bits-1:0]}; // data of the valid block being replaced.
					rg_prev_block_data<=data_values[matched_tag];
				end
				else begin // if there is one invalid block replace that.
					$display("%s:Replacing Invalid Block",name,fromInteger(replace_block),$time);
					rg_block_to_replace <= fromInteger(replace_block);
				end
			end
				////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
			else begin // hit occured in the current set
                rg_hit<=1;
                if(rg_rd_wr==0)begin // read operation . read the dataline and send to cpu
    				$display("%s:Tag hit occurred on Block %d Sending data to CPU",name,matched_tag,$time);
                    $display("%s:Valid bits : %b",name,valid_dirtybitvalues[matched_tag],$time);
	    			wr_data_to_cpu <= (data_values[matched_tag][rg_upper_offset:rg_lower_offset]);
                    rg_pre_cache_data<=data_values[matched_tag];
		    		rg_state<=Handle_request;   // go back to hnadling the next request.
			    	wr_cache_output<=Available; // indicate the CPU that the data is valid.
                end
                else begin // write operation
                   /* TODO  The following chunk of code is a bad work-around against the bluespec type check. All we wish to do is update the specified byte/halfword/word in the dataline.
                   Due to heavy parameterization the  bluespec compiler doesn't compile a straighforward code. Hence the following huge Mess. This will need a good cleanup soon.
                   */
                    let x =data_values[matched_tag][fromInteger(`Word_size*8*`Block_size-1):rg_upper_offset+1]; // bits in line above the words to be changed.
                    Bit#(rg_lower_offset) y =data_values[matched_tag][rg_lower_offset-1:0]; // bits in line lower to the word to be changed.
                    Bit#(8) temp1=rg_data_from_cpu[rg_upper_offset-rg_lower_offset:0];
                    Bit#(16) temp2=rg_data_from_cpu[rg_upper_offset-rg_lower_offset:0];
                    Bit#(32) temp3=rg_data_from_cpu[rg_upper_offset-rg_lower_offset:0];
                    Bit#(64) temp4=rg_data_from_cpu[rg_upper_offset-rg_lower_offset:0];
                    $display("%s:TY : %h",name,temp1,temp2,temp3);
                    Bit#(TMul#(8,TMul#(`Word_size,`Block_size))) new_data=0; // stupid workarounds for bluespec
                    if(rg_transfer_size==0)
                        new_data={x,temp1}; // stupid workarounds for bluespec
                    else if(rg_transfer_size==1)
                        new_data={x,temp2}; // stupid workarounds for bluespec
                    else if(rg_transfer_size==2)
                        new_data={x,temp3}; // stupid workarounds for bluespec
                    else if(rg_transfer_size==3)
                        new_data={x,temp4}; // stupid workarounds for bluespec
                    $display("%s:N1: %h",name,new_data);
                    new_data=new_data << rg_lower_offset;
                    $display("%s:N2: %h",name,new_data);
                    new_data=new_data|y; // finally updated data line.

                    rg_pre_cache_data<=new_data;
                    if(rg_writeback==1) begin//write_back_operation. update the cache and go back to handling new requests from the cpu.
                        $display("%s:Tag hit occured for Write operation on Block : %d. Updated line is : %h",name,matched_tag,new_data,$time);
				                data[matched_tag].portA.request.put(BRAMRequest{write:True, address:rg_set, datain:new_data, responseOnWrite:False}); // update the data line of the corresponding set
        				        valid_dirtybit[matched_tag].portA.request.put(BRAMRequest{write:True, address:rg_set,datain:2'b11, responseOnWrite:False}); // make the valid bit 1 and dirty bit 1
                        wr_data_to_cpu <= (data_values[matched_tag][rg_upper_offset:rg_lower_offset]);
    		            		rg_state<=Handle_request;
	    		    	        wr_cache_output<=Available;
                    end
                    else begin // write-through selected. We will first writedata into the memory and then update the cache.
                        wr_cache_output<=Busy;  // make cache busy
                        $display("%s:No tags match. Sending request to Memory",name,$time);
                        rg_prev_block_data<=new_data;
                        rg_state<=Handle_memory; // change state
                        wr_transfer_size_to_mem<=rg_transfer_size;
                        wr_burst_mode_to_memory<=0; // single write operation. No burst mode. 
                        wr_address_to_memory<=tagged Valid rg_address_from_cpu; // send address to memory
                        wr_data_to_memory<=zeroExtend(rg_data_from_cpu); // write_data_to_be_sent
                        wr_rd_wr_memory<=rg_rd_wr; // write operation
                        rg_block_to_replace<=fromInteger(matched_tag);  
				
                    end
                end
			end
            ///////////////////////////////////////////////////////////////////////////// Find new LRU bits /////////////////////////////////////////////////////////////////////////////////////////////
            Integer m=`Ways-1+matched_tag;
            Integer n=0;
            rg_isblock_dirty<=valid_dirtybitvalues[matched_tag][0]; // indicates if the block is dirty and needs to be written to the memory.
            while(m>0)begin
                if(m%2==1)begin
                    n=(m-1)/2;
                    if(n<`Ways)
                        lru_bits_new[`Ways-2-n]=1;
                end
                else begin
                    n=(m-2)/2;
                    if(n<`Ways)
                        lru_bits_new[`Ways-2-n]=0;
                end
                m=n;
            end
            $display("%s:New LRU bits:%b",name,lru_bits_new);
            pseudo_lru[rg_set]<=lru_bits_new; // update the LRU bits after the access is made
            //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		endrule

        /*
            This rule handles the memory response. if a bus error has occurred while performing the operation on the bus/memory then the error bit is raised to the cpu and control
            goes back to handling the next cpu request. if the cache is disabled, no action is performed on the cache and the goes back to handling requests. 
        */

		rule rl_handle_memory_Read(rg_state==Handle_memory && !wr_flush_everything);
			$display("%s:Handling Memory",name,$time);
			if(wr_data_from_memory matches tagged Valid .dataline)begin // if memory has responded with valiud data
                if(wr_bus_error==1) begin// error has occurred in the bus
                    wr_cache_output<=Available;
                    wr_error_to_cpu<=1; // send error bit the cpu.
                    rg_state<=Handle_request; // go back to handling requests.
                end
                else if(rg_cache_enable==0) begin // cache is disabled. 
                    wr_cache_output<=Available; // tell cpu that operation is over
                    wr_data_to_cpu<=dataline[rg_upper_offset:rg_lower_offset]; // send some data to the cpu.
                    rg_pre_cache_data<=dataline;
                    rg_state<=Handle_request; // handle next request.
                end
                else begin
                    if(rg_rd_wr==0 || (rg_hit==0 && rg_writeback==1 && rg_rd_wr==1)) begin //read operation or a miss occurred in the write-back mode. 
                        $display("%s:Memory responded with valid data : %h",name,dataline,$time);
                        let tag_value=rg_address_from_cpu[`Addr_width-1:`Addr_width-`Num_of_tag_bits]; // new tag value to be writte in cache
                        $display("%s:Write Tag value To set : %d and into block : %d",name,rg_set,rg_block_to_replace,$time);
                        Bit#(2) valid_dirty=2'b10; // make the valid bit 1 in case of read.
                        Bit#(TMul#(8,TMul#(`Word_size,`Block_size))) data1=dataline;

                        if(rg_hit==0 && rg_writeback==1 && rg_rd_wr==1)begin	// miss has occurred in writeback mode. 
                           
                            /* TODO  The following chunk of code is a bad work-around against the bluespec type check. All we wish to do is update the specified byte/halfword/word in the dataline.
                           Due to heavy parameterization the  bluespec compiler doesn't compile a straighforward code. Hence the following huge Mess. This will need a good cleanup soon.
                           */
                            
                            let x =dataline[fromInteger(`Word_size*8*`Block_size-1):rg_upper_offset+1]; // bits in line above the words to be changed.
                            Bit#(rg_lower_offset) y =dataline[rg_lower_offset-1:0]; // bits in line lower to the word to be changed.
                            Bit#(8) temp1=rg_data_from_cpu[rg_upper_offset-rg_lower_offset:0];
                            Bit#(16) temp2=rg_data_from_cpu[rg_upper_offset-rg_lower_offset:0];
                            Bit#(32) temp3=rg_data_from_cpu[rg_upper_offset-rg_lower_offset:0];
                            Bit#(64) temp4=rg_data_from_cpu[rg_upper_offset-rg_lower_offset:0];
                            Bit#(TMul#(8,TMul#(`Word_size,`Block_size))) new_data=0; // stupid workarounds for bluespec
                            if(rg_transfer_size==0)
                                new_data={x,temp1}; // stupid workarounds for bluespec
                            else if(rg_transfer_size==1)
                                new_data={x,temp2}; // stupid workarounds for bluespec
                            else if(rg_transfer_size==2)
                                new_data={x,temp3}; // stupid workarounds for bluespec
                            else if(rg_transfer_size==3)
                                new_data={x,temp4}; // stupid workarounds for bluespec
                            new_data=new_data << rg_lower_offset;
                            new_data=new_data|y;
                            data1=new_data;
                            rg_pre_cache_data<=data1;

                            valid_dirty=2'b11;				// make the line dirty in case of write-back mode
                            if(rg_isblock_dirty==1)begin // if read miss or write-back miss the previous dirty block needs to be written to memory.
                                rg_state<=Handle_block_replace; // replace the previous block
                                wr_cache_output<=Busy;
                                wr_rd_wr_memory<=1;
                                wr_burst_mode_to_memory<=rg_burst_mode;
                                wr_transfer_size_to_mem<=fromInteger(valueOf(TLog#(`Word_size)));
                                Bit#(`Num_of_offset_bits) lower_bits=0;
                				wr_address_to_memory<=tagged Valid ({rg_prev_block_address[`Addr_width-1:`Num_of_offset_bits],lower_bits}); // send address to memory such that the complete word is to be written.
//                                wr_address_to_memory<= tagged Valid rg_prev_block_address;
                                wr_data_to_memory<=rg_prev_block_data;
                            end
                            else begin
                                rg_state<=Handle_request;
                                wr_cache_output<=Available;
                            end
                        end
                        else begin
                            rg_state<=Handle_request;
                            wr_cache_output<=Available;
                            rg_pre_cache_data<=dataline;
                        end
                        
                        tag[rg_block_to_replace].portA.request.put(BRAMRequest{write:True, address:rg_set, datain:tag_value, responseOnWrite:False}); // replace the tag
                        valid_dirtybit[rg_block_to_replace].portA.request.put(BRAMRequest{write:True, address:rg_set,datain:valid_dirty, responseOnWrite:False}); // update the valid and dirty bits.
                        data[rg_block_to_replace].portA.request.put(BRAMRequest{write:True, address:rg_set, datain:data1, responseOnWrite:False}); // update the datal line of the corresponding data
                        wr_data_to_cpu<= (dataline[rg_upper_offset:rg_lower_offset]);
                        $display("%s:Sending data to Cpu",name,$time);
                    end
                    else begin // write-through operation 
                        if(rg_writeallocate==1)begin // write allocate mode is one. 
                            $display("%s:Write allocate mode selected. Not updating cache",name,$time);
                            if(rg_hit==0) begin // TODO : read the updated line from the memory and write into cache
                                $display("%s: Writethrough write-allocate mode write over in the cache. Now starting reading of the line",name);
                                rg_rd_wr<=0;
                                rg_hit<=0;
                                wr_rd_wr_memory<=0;
                                wr_transfer_size_to_mem<=fromInteger(valueOf(TLog#(`Word_size))); // transfer size is the word size in each block
                                wr_burst_mode_to_memory<=rg_burst_mode; // burst mode activated
                                Bit#(`Num_of_offset_bits) lower_bits=0; // lower byte offset are made zero.
                                wr_address_to_memory<=tagged Valid ({rg_address_from_cpu[`Addr_width-1:`Num_of_offset_bits],lower_bits}); // send address to memory
                            end
                            else if(rg_hit==1) begin // write allocate mode update cache on a hit.
                                wr_data_to_cpu <= zeroExtend(rg_prev_block_data[31:0]);
                                rg_state<=Handle_request;
                                wr_cache_output<=Available;
                                $display(":%s:Updating block after memory is written in Write-Allocate mode : %d. Updated line is : %h",name,rg_block_to_replace,rg_prev_block_data,$time);
                                rg_pre_cache_addr<=rg_address_from_cpu;
                                rg_pre_cache_data<=rg_prev_block_data;
                                let tag_value=rg_address_from_cpu[`Addr_width-1:`Addr_width-`Num_of_tag_bits]; // new tag value to be writte in cache
                                data[rg_block_to_replace].portA.request.put(BRAMRequest{write:True, address:rg_set, datain:rg_prev_block_data, responseOnWrite:False}); // update the data line of the corresponding set
                                valid_dirtybit[rg_block_to_replace].portA.request.put(BRAMRequest{write:True, address:rg_set,datain:2'b11, responseOnWrite:False}); // make the valid bit 1 and dirty bit 1
                                tag[rg_block_to_replace].portA.request.put(BRAMRequest{write:True, address:rg_set, datain:tag_value, responseOnWrite:False}); // replace the tag
                            end
                        end
                        else begin // no allocate mode. no update on cache required.
                            rg_pre_cache_addr<=32'hFFFFFFFF;
                            wr_cache_output<=Available; // tell cpu that operation is over
                            wr_data_to_cpu<=dataline[rg_upper_offset:rg_lower_offset]; // send some data to the cpu.
                            rg_state<=Handle_request; // handle next request.
                        end
                    end
                end
            end
            else begin // memory has not responded with data.
                $display("%s:Waiting for memory to respond with valid data",name,$time);
                if(rg_hit==0 && rg_writeback==1 && rg_rd_wr==1 && rg_cache_enable==1)begin	// miss has occurred in writeback mode. We need to read the data first and then change the word in the cache.
                        wr_rd_wr_memory<=0; // hence we send a read signal.
                end
                else begin
                        wr_rd_wr_memory<=rg_rd_wr;
                end
                wr_data_to_memory<=zeroExtend(rg_data_from_cpu); // in case of write we need to write the data from the cpu only.
                wr_cache_output<=Busy;                           // tell the cpu cache is busy 
                if((rg_rd_wr==0 && rg_cache_enable==1)||(rg_hit==0 && rg_writeback==1 && rg_rd_wr==1 && rg_cache_enable==1)) begin // read operation will always be in terms of word transfers
                    wr_transfer_size_to_mem<=fromInteger(valueOf(TLog#(`Word_size))); // transfer size is the word size in each block
                    wr_burst_mode_to_memory<=rg_burst_mode; // burst mode activated
                    Bit#(`Num_of_offset_bits) lower_bits=0; // lower byte offset are made zero.
    				wr_address_to_memory<=tagged Valid ({rg_address_from_cpu[`Addr_width-1:`Num_of_offset_bits],lower_bits}); // send address to memory
                end
                else begin
                    wr_transfer_size_to_mem<=rg_transfer_size;
                    wr_burst_mode_to_memory<=0; // single burst mode.
                    wr_address_to_memory<= tagged Valid rg_address_from_cpu;
                end

            end

		endrule

        /*
            The following rule handles replacing a bloc/dataline to the memory. It will send the complete data line to the output and generate appropriate
            burst modes and transfer-size signals.
        */

		rule rl_handle_replacement_of_block(rg_state==Handle_block_replace && !wr_flush_everything);
			if(wr_data_from_memory matches tagged Valid .dataline)begin // if memory has responded with valiud data
                if(wr_bus_error==1) begin// error has occurred in the bus
                    wr_cache_output<=Available;
                    wr_error_to_cpu<=1;
                    rg_state<=Handle_request;
                end
                else begin // end of memory operation
                    wr_data_to_cpu <= zeroExtend(rg_prev_block_data[31:0]);
                    rg_state<=Handle_request;
                    wr_cache_output<=Idle;
                end
			end
			else begin
				$display("%s:Waiting for memory to respond with valid data",name,$time);
				wr_rd_wr_memory<=1; // always write operation
        Bit#(`Num_of_offset_bits) lower_bits=0; // send the address without the lower ofset bits.
 				wr_address_to_memory<=tagged Valid ({rg_prev_block_address[`Addr_width-1:`Num_of_offset_bits],lower_bits}); // send address to memory
				wr_data_to_memory<=rg_prev_block_data;
				wr_cache_output<=Busy;
                wr_transfer_size_to_mem<=fromInteger(valueOf(TLog#(`Word_size)));
                wr_burst_mode_to_memory<=rg_burst_mode;
			end

		endrule
                            ////////////////////////// method definitions ///////////////////////////
        method Action _flush_from_cpu(Bool flush);
            wr_flush_everything<=flush;
        endmethod
		method Action _data_from_cpu(Bit#(32) address, Bit#(TMul#(8,`Word_size)) datain, Bit#(1) rd_wr, Bit#(2) wbwt_wawna, Bit#(2) transfer_size, Bit#(1) cache_enable);
            wr_transfer_size<=transfer_size;
			wr_address_from_cpu<=tagged Valid address;
			wr_data_from_cpu<=datain;
			wr_rd_wr_cpu<=rd_wr;
            wr_writeback<=wbwt_wawna[1];
            wr_writeallocate<=wbwt_wawna[0];
            wr_cache_enable<=cache_enable;
		endmethod
		method Action _data_from_memory(Bit#(TMul#(8,TMul#(`Word_size,`Block_size))) datain);
			wr_data_from_memory<=tagged Valid datain;
		endmethod
		method Action _bus_error_from_memory(bit buserror);
			wr_bus_error<=buserror;
		endmethod
			/* Output Methods */
		method Bit#(TMul#(`Word_size,8)) data_to_cpu_;
			return wr_data_to_cpu;
		endmethod
		method Maybe#(Bit#(32)) address_to_memory_;
			return wr_address_to_memory;
		endmethod
		method Bit#(TMul#(8,TMul#(`Word_size,`Block_size))) data_to_memory_; // data to be written to memory
			return wr_data_to_memory;
		endmethod
		method Bit#(1) rd_wr_to_memory_;
			return wr_rd_wr_memory;
		endmethod
		method Cache_output cache_output;
			return wr_cache_output;
		endmethod
        method Bit#(1) bus_error_to_cpu_();
            return wr_error_to_cpu;
        endmethod
        method Bit#(2) transfer_size_to_memory_();
            return wr_transfer_size_to_mem;
        endmethod
        method Bit#(3) burst_mode_to_memory();
            return wr_burst_mode_to_memory;
        endmethod
        method Bool mis_aligned_access();                                                         // indicates there has been a misaligned to the cache.
            return wr_mis_aligned;
        endmethod
	endmodule
endpackage

