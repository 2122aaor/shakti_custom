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

Description : This module implements the completion buffer.

the buffer has two pointers - iidx and ridx. iidx is like a tail pointer to which new entries are allocated and ridx is the head pointer
from which old entries a removed. A counter is mainted which counts the number of reserved entries in the buffer. the buffer is said
to be full when this counter value = size of the buffer. The buffer is empty is the counter value is 0. These conditions are used
to ensure proper firing of the rules/methods in higher level modules instantiatin this module.

*/
 package  CBuff; 

   // parameterized buffer with arguments : type of data to be stored in each entry, depth of buffer.
  interface CBuff#(type data_type, numeric type size);
    method ActionValue#(Bit#(TAdd#(TLog#(size),1))) getToken(); // this method returns a token and reserves a slot.
    method Action put(Bit#(TAdd#(TLog#(size),1)) tok, data_type d); // this method updates a previously reserved slot with valid data.
    method data_type getResult(); // this method return the valid data at the head of the buffer.
    method Action deqResult(); // this increments the head of the buffer.
    method Action clear_all(); // clear all the entries in the buffer.
  endinterface

  module mkMyCBuff(CBuff#(data_type,size))
   provisos(Bits#(Maybe#(data_type), a__));

    Reg#(Bit#(TAdd#(TLog#(size),1))) iidx <- mkReg(0); // pointer to issue a new entry. like a tail pointer.
    Reg#(Bit#(TAdd#(TLog#(size),1))) ridx <- mkReg(0); // pointer to return a valid entry in order.
    Reg#(Bit#(TAdd#(TLog#(size),1))) cnt[3] <- mkCReg(3,0); // contains the number of valid entries.
    Integer vsize = valueOf(size);
    Bit#(TAdd#(TLog#(size),1)) sz = fromInteger(vsize);
    
    Reg#(Maybe#(data_type)) cb[vsize][3] ; // actuall buffer holding the data
    for(Integer i=0;i<vsize;i=i+1)
        cb[i]  <- mkCReg(3,Invalid);
    
    // method reserves a slot by incrementing the iidx pointer and returning iidx as the token.
    // this method can only be called if the number of already reserved slots is less than the size.
    method ActionValue#(Bit#(TAdd#(TLog#(size),1))) getToken()if(cnt[0]!=sz);  
      cb[iidx][0]<=tagged Invalid; // make the head invalid.
      iidx<=iidx==sz-1?0:iidx+1; // increment issue pointer to point to a new head.
      cnt[0]<=cnt[0]+1; // increment reserved slot count.
      return iidx; // return token
    endmethod

    // this method validates previously reserved slot in the buffer with valid data.
    method Action put(Bit#(TAdd#(TLog#(size),1)) idx, data_type d); 
      cb[idx][1]<= tagged Valid d;
    endmethod
  
    // this method returns the data in the head of the buffer pointed by ridx only if it has
    // valid data in the entry and there is atleast one reserved slot in the buffer.
    method data_type getResult()if(cnt[1]!=0 &&& cb[ridx][2] matches tagged Valid .x );
      return x;
    endmethod

    // this method removes the head entry and increments the ridx pointer and also 
    // reduces the count of the number of reserved slots by 1
    method Action deqResult()if(cnt[1]!=0); 
      cb[ridx][2]<= tagged Invalid;
      ridx<=ridx==sz-1?0:ridx+1;
      cnt[1]<=cnt[1]-1;
    endmethod
    
    method Action clear_all();
      ridx<=0;
      iidx<=0;
      cnt[2]<=0;
    endmethod
  endmodule
endpackage 
