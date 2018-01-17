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
package  CFFIFO;
  import FIFO::*;
  import Vector::*;

  interface SFIFO#(numeric type n, type data_type, type search_type);
    method Action enq(data_type x);
    method Action deq;
    method data_type first;
    method Tuple2#(Bool,Bit#(TAdd#(1,TLog#(TAdd#(n,1))))) search(search_type s);
    method Action clear_all();
  endinterface
  module mkCFSFIFO#(function Bool find(data_type x, search_type y))(SFIFO#(size,data_type,search_type))
    provisos(Bits#(data_type, data_size), Add#(size, 1, size1), Log#(size1, sz), Add#(sz, 1, sz1));
    
    Integer ni = valueOf(size); 
    Bit#(sz1) nb = fromInteger(ni); 
    Bit#(sz1) n2 = 2*nb;
    Vector#(size, Reg#(data_type)) data <- replicateM(mkRegU);
    Reg#(Bit#(sz1)) enqP[3] <- mkCReg(3,0);
    Reg#(Bit#(sz1)) deqP[3] <- mkCReg(3,0);
    Reg#(Bool) enqEn[3] <- mkCReg(3,True);
    Reg#(Bool) deqEn[3] <- mkCReg(3,False);
    Reg#(data_type) temp[2] <- mkCReg(2,?);
    Reg#(Maybe#(Bit#(sz1))) tempEnqP[3] <- mkCReg(3,Invalid);
    Reg#(Maybe#(Bit#(sz1))) tempDeqP[3] <- mkCReg(3,Invalid);
    Bit#(sz1) cnt0 = enqP[0] >= deqP[0]?  enqP[0] - deqP[0]: (enqP[0]%nb + nb) - deqP[0]%nb;
    Bit#(sz1) cnt1 = enqP[1] >= deqP[1]?  enqP[1] - deqP[1]: (enqP[1]%nb + nb) - deqP[1]%nb;

    rule canonicalize;
      if(!enqEn[1] && cnt1 != nb) enqEn[1] <= True;
      if(!deqEn[1] && cnt1 != 0) deqEn[1] <= True;
      if(isValid(tempEnqP[1])) begin
        data[validValue(tempEnqP[1])] <= temp[1];
        tempEnqP[1] <= Invalid;
      end
      if(isValid(tempDeqP[1])) begin
        deqP[1] <= validValue(tempDeqP[1]); tempDeqP[1]<=Invalid;
      end
    endrule

    method Action enq(data_type x) if(enqEn[0]);
      temp[0] <= x; tempEnqP[0] <= Valid (enqP[0]%nb);
      enqP[0] <= (enqP[0] + 1)%n2; enqEn[0] <= False;
    endmethod

    method Action deq if(deqEn[0]);
      tempDeqP[0] <= Valid ((deqP[0] + 1)%n2);
      deqEn[0] <= False;
      $display("WAITBUFF: Dequeing pointer :%d",(deqP[0]+1)%n2);
    endmethod

    method data_type first if(deqEn[0] );
      return data[deqP[0]%nb];
    endmethod

    method Tuple2#(Bool,Bit#(TAdd#(1,TLog#(TAdd#(size,1))))) search(search_type s);
      Bool ret = False;
      Bit#(sz1) d=0;
      for(Bit#(sz1) i = 0; i < nb; i = i + 1)
      begin
        let ptr = (deqP[0] + i)%nb;
        if(find(data[ptr], s) && i < cnt0 && ptr!=((deqP[0] + 1)%n2))begin
          ret = True;
          d=ptr;
        end
      end
      return tuple2(ret,d);
    endmethod
    method Action clear_all();
      enqP[2]<=0;
      deqP[2]<=0;
      deqEn[2]<=False;
      enqEn[2]<=True;
      tempEnqP[2]<=Invalid;
      tempDeqP[2]<=Invalid;
    endmethod
  endmodule
/*
  module mkPipelineSFifo#(function Bool isFound(t v, st k))(SFIFO#(n, t, st)) 
    provisos(Bits#(t, tSz), Add#(n, 1, n1), Log#(n1, sz), Add#(sz, 1, sz1), Bits#(st, stz));
    Integer ni = valueOf(n);
    Bit#(sz1) nb = fromInteger(ni);
    Bit#(sz1) n2 = 2*nb;
    Vector#(n, Reg#(t)) data <- replicateM(mkRegU);
    Reg#(Bit#(sz1)) enqP[2] <- mkCReg(2,0);
    Reg#(Bit#(sz1)) deqP[2] <- mkCReg(2,0);
    Bit#(sz1) cnt0 = enqP[0] >= deqP[0]? enqP[0] - deqP[0]:(enqP[0]%nb + nb) - deqP[0]%nb;
    Bit#(sz1) cnt1 = enqP[0] >= deqP[1]? enqP[0] - deqP[1]:(enqP[0]%nb + nb) - deqP[1]%nb;

    method Action enq(t x) if(cnt1 < nb);
      enqP[0] <= (enqP[0] + 1)%n2; data[enqP[0]%nb] <= x;
    endmethod
    method Action deq if(cnt0 != 0);
      deqP[0] <= (deqP[0] + 1)%n2;
    endmethod
    method t first if(cnt0 != 0);
      return data[deqP[0]%nb];
    endmethod
    method Bool search(st s);
      Bool ret = False;
      for(Bit#(sz1) i = 0; i < nb; i = i + 1)
      begin
        let ptr = (deqP[0] + i)%nb;
        if(isFound(data[ptr], s) && i < cnt1)
          ret = True;
      end
      return ret;
    endmethod
endmodule
*/
/*
  interface Ifc_CFFIFO#(numeric type size, type data_type);
    method Action enq(data_type x);
    method Action deq ;
    method data_type first ;
  endinterface
  module mkCFFIFO(Ifc_CFFIFO#(size,data_type))
    provisos(Bits#(data_type, data_size), Add#(size, 1, size1), Log#(size1, sz), Add#(sz, 1, sz1));

    Integer ni = valueOf(size); 
    Bit#(sz1) nb = fromInteger(ni); 
    Bit#(sz1) n2 = 2*nb;
    Vector#(size, Reg#(data_type)) data <- replicateM(mkRegU);

    Reg#(Bit#(sz1)) enqP[2] <- mkCReg(2,0);
    Reg#(Bit#(sz1)) deqP[2] <- mkCReg(2,0);
    Reg#(Bool) enqEn[2] <- mkCReg(2,True);
    Reg#(Bool) deqEn[2] <- mkCReg(2,False);
    Reg#(data_type) newData[2] <- mkCReg(2,?);
    Reg#(Maybe#(Bit#(sz1))) oldEnqP[2] <- mkCReg(2,Invalid);

    rule canonicalize;
      Bit#(sz1) cnt = enqP[1] >= deqP[1]? enqP[1] - deqP[1]: (enqP[1]%nb + nb) - deqP[1]%nb;
      if(!enqEn[1] && cnt != nb) 
        enqEn[1] <= True;
      if(!deqEn[1] && cnt != 0) 
        deqEn[1] <= True;
      if(isValid(oldEnqP[1])) begin
        data[validValue(oldEnqP[1])] <= newData[1];
        oldEnqP[1] <= Invalid;
      end
    endrule
    method Action enq(data_type x) if(enqEn[0]);
      newData[0] <= x;
      oldEnqP[0] <= Valid (enqP[0]%nb);
      enqP[0] <= (enqP[0] + 1)%n2;
      enqEn[0] <= False;
    endmethod

    method Action deq if(deqEn[0]);
      deqP[0] <= (deqP[0] + 1)%n2; deqEn[0] <= False;
    endmethod

    method data_type first if(deqEn[0]);
      return newData[deqP[0]%nb];
    endmethod
  endmodule

  interface SFIFO#(numeric type n, type data_type, type search_type);
    method Action enq(data_type x);
    method Action deq;
    method data_type first;
    method Bool search (search_type search_value);
  endinterface
*/
endpackage 
