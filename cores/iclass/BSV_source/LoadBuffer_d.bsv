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
package LoadBuffer_d;
	import All_types_d::*;
	import Vector::*;
	
	interface Ifc_LoadBuff_d#(numeric type addr_width,numeric type size,numeric type word_size, numeric type block_size,numeric type cbuff_size,numeric type ways,numeric type prf_size);
		method Action insert(LoadBufferData_d#(addr_width,word_size,block_size,cbuff_size,ways,prf_size) req, LdStatus_d status);
		method Bool search (Bit#(addr_width) addr);
		method ActionValue#(Tuple2#(Bool, LoadBufferData_d#(addr_width,word_size,block_size,cbuff_size,ways,prf_size))) remove(Bit#(addr_width) addr);
		method Action clear_all();
		method Action update (Bit#(addr_width) addr, LdStatus_d stat);
		method Tuple2#(LdStatus_d,LoadBufferData_d#(addr_width,word_size,block_size,cbuff_size,ways,prf_size)) usearch ();
	endinterface
	
	module mkLoadBuff_d(Ifc_LoadBuff_d#(addr_width,size,word_size,block_size,cbuff_size,ways,prf_size))
	provisos(Add#(a__, 1, TSub#(addr_width, TLog#(word_size))));
		let vsize=valueOf(size);
		let v_num_of_offset_bits=valueOf(TAdd#(TLog#(word_size),TLog#(block_size)));
		let v_addr_width=valueOf(addr_width);
		let v_num_of_bytes=valueOf(TLog#(word_size));
		
		Vector#(size,Array#(Reg#(LdStatus_d))) ld_status <-replicateM(mkCReg(4,Invld));
		Reg#(LoadBufferData_d#(addr_width,word_size,block_size,cbuff_size,ways,prf_size)) buff[vsize][3];
		Reg#(Bit#(TAdd#(TLog#(size),1))) cnt[4] <-mkCReg(4,0);
		Bit#(TAdd#(TLog#(size),1)) sz = fromInteger(vsize);
		for(Integer i=0;i<vsize;i=i+1)begin
			buff[i]<-mkCReg(3,?);
		end
		
		
		method Action insert(LoadBufferData_d#(addr_width,word_size,block_size,cbuff_size,ways,prf_size) req, LdStatus_d status)if(cnt[1]!=sz);
			Bit#(TLog#(size)) idx=0;
			for(Integer i=0;i<vsize;i=i+1)
				if(ld_status[i][1]==Invld)
					idx=fromInteger(i);
			$display("DCACHE: LDBUFF: Inserting tag %h in index : %d",req.request.address,idx,$time);
			ld_status[idx][1]<=status;
			buff[idx][1]<=req;
			cnt[1]<=cnt[1]+1;
		endmethod
		
		method Bool search(Bit#(addr_width) addr);
			Bool s = False;
			Bit#(TSub#(addr_width,TLog#(word_size))) _address_mask = signExtend(1'b1);
			Bit#(addr_width) x=zeroExtend(_address_mask)<<v_num_of_offset_bits;
			for(Integer i=0;i<vsize;i=i+1)begin
				if(ld_status[i][1]!=Invld && (buff[i][1].request.address&x)==(addr&x))
					s=True;
			end
			return s;
		endmethod
		
		method ActionValue#(Tuple2#(Bool, LoadBufferData_d#(addr_width,word_size,block_size,cbuff_size,ways,prf_size))) remove(Bit#(addr_width) addr);
			Bit#(TLog#(size)) idx=0;
	  		Bool search_hit = False;
			Bit#(TSub#(addr_width,TLog#(word_size))) _address_mask = signExtend(1'b1);
			Bit#(addr_width) x=zeroExtend(_address_mask)<<v_num_of_bytes;
			$display("DCACHE: LDBUFF: Input search = %h Masked Addr :%h",addr,x,$time);
			for(Integer i=0;i<vsize;i=i+1) begin
				if((buff[i][0].request.address&x)==(addr&x) && ld_status[i][0]==FillResp) begin
					idx=fromInteger(i);
		  			search_hit = True;
				end
			end
			if(search_hit) begin
				ld_status[idx][0]<=Invld;
				cnt[0]<=cnt[0]-1;
			end
			$display("LDBUFF: removing tag : %h at index : %d",addr,idx,$time);
			return tuple2(search_hit,buff[idx][0]);
		endmethod
		
		method Action clear_all();
		cnt[3]<=0;
		for(Integer i=0;i<vsize;i=i+1)
			ld_status[i][3]<=Invld;
		endmethod
		
		method Action update (Bit#(addr_width) addr, LdStatus_d stat);
			for (Integer i=0;i<vsize;i=i+1)begin
				if(ld_status[i][2]!=Invld && buff[i][2].request.address==addr)
					ld_status[i][2]<=stat;
			end
		endmethod
		
		method Tuple2#(LdStatus_d,LoadBufferData_d#(addr_width,word_size,block_size,cbuff_size,ways,prf_size)) usearch ();
			LdStatus_d u= Invld;
			LoadBufferData_d#(addr_width,word_size,block_size,cbuff_size,ways,prf_size) x=?;
			for(Integer i=0;i<vsize;i=i+1)begin
			  	if(ld_status[i][2]==WrBack || ld_status[i][2]==FillReq) begin
			    	x=buff[i][2];
			    	u=ld_status[i][2];
			  	end
			end
			return tuple2(u,x);
		endmethod
	endmodule
	
	//interface Ifc_StoreBuff_d#(numeric type addr_width,numeric type size,numeric type word_size, numeric type block_size,numeric type cbuff_size,numeric type ways,numeric type prf_size);
	//  method Action insert(LoadBufferData_d#(addr_width,word_size,block_size,cbuff_size,ways,prf_size) req, LdStatus_d status);
	//  method Tuple2#(LoadBufferData_d#(addr_width,word_size,block_size,cbuff_size,ways,prf_size),Bool) search (Bit#(addr_width) addr);
	//  method ActionValue#(Tuple2#(Bool, LoadBufferData_d#(addr_width,word_size,block_size,cbuff_size,ways,prf_size))) remove(Bit#(addr_width) addr);
	//  method Action clear_all();
	//endinterface
/*
  module mkStoreBuff_d(Ifc_StoreBuff_d#(addr_width,size,word_size,block_size,cbuff_size,ways));
    let vsize=valueOf(size);

    Vector#(size,Array#(Reg#(LdStatus_d))) ld_status <-replicateM(mkCReg(3,Invld));
    Reg#(LoadBufferData_d#(addr_width,word_size,block_size,cbuff_size,ways)) buff[vsize][3];
    Reg#(Bit#(TAdd#(TLog#(size),1))) cnt[3] <-mkCReg(3,0);
    Bit#(TAdd#(TLog#(size),1)) sz = fromInteger(vsize);
    for(Integer i=0;i<vsize;i=i+1)begin
      buff[i]<-mkCReg(3,?);
    end


    method Action insert(LoadBufferData_d#(addr_width,sword_size,block_size,cbuff_size,ways) req, LdStatus_d status)if(cnt[1]!=sz);
      Bit#(TLog#(size)) idx=0;
      for(Integer i=0;i<vsize;i=i+1)
        if(ld_status[i][1]==Invld)
          idx=fromInteger(i);
      $display("LDBUFF: Inserting tag %h in index : %d",req.request.address,idx,$time);
      ld_status[idx][1]<=status;
      buff[idx][1]<=req;
      cnt[1]<=cnt[1]+1;
    endmethod

    method Tuple2#(LoadBufferData_d#(addr_width,word_size,block_size,cbuff_size,ways),Bool) search (Bit#(addr_width) addr);
      Bool s = False;
      LoadBufferData_d#(addr_width,word_size,block_size,cbuff_size,ways) x=buff[0][1];
      for(Integer i=0;i<vsize;i=i+1)
        if(ld_status[i][1]!=Invld && buff[i][1].request.address==addr)begin
          s=True;
          x=buff[i][1];
        end
      return tuple2(x,s);
    endmethod

    method ActionValue#(LoadBufferData_d#(addr_width,word_size,block_size,cbuff_size,ways)) remove(Bit#(addr_width) addr)if(cnt[0]!=0);
      Bit#(TLog#(size)) idx=0;
      for(Integer i=0;i<vsize;i=i+1)
        if(buff[i][0].request.address==addr)
          idx=fromInteger(i);
      ld_status[idx][0]<=Invld;
      cnt[0]<=cnt[0]-1;
      $display("LDBUFF: removing tag : %h at index : %d",addr,idx,$time);
      return buff[idx][0];
    endmethod

    method Action clear_all();
      cnt[2]<=0;
      for(Integer i=0;i<vsize;i=i+1)
        ld_status[i][2]<=Invld;
    endmethod
  endmodule
*/
endpackage
