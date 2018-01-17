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
package  All_types;
import riscv_types::*;
//import TLM3 ::*;
	typedef enum {Idle,Busy,Available} Cache_output deriving(Bits,Eq,FShow); // defines the status of the output of the cache to the processor.
  typedef enum {Invld,FillReq,FillResp} LdStatus deriving(Bits,Eq,FShow);
  typedef enum {Load,Store} Access_type deriving (Bits,Eq,FShow);

  typedef struct{
    Bit#(TMul#(8,_word_size)) data_word;
    Bit#(addr_width) address;
  }Recent_access#(numeric type addr_width, numeric type _word_size, numeric type _block_size) deriving(Eq,Bits);

  typedef struct{
    Bit#(addr_width) address;
    Bit#(2) transfer_size;
    Bit#(1) cache_enable;
    Prediction prediction1;
    Prediction prediction2;
  }From_Cpu#(numeric type addr_width) deriving(Bits,Eq);
  
  typedef struct{
    Bit#(TMul#(word_size,8)) data_word;
    Bit#(1) bus_error;
    Bit#(1) mis_aligned_error;
    Bit#(addr_width) pc;
    Prediction prediction1;
    Prediction prediction2;
  }To_Cpu#(numeric type addr_width,numeric type word_size) deriving(Bits,Eq);

  typedef struct{
    From_Cpu#(addr_width) request;
    Bit#(TAdd#(TLog#(cbuff_size),1)) token;
  }Cpu_req_with_token#(numeric type addr_width,numeric type cbuff_size) deriving(Bits,Eq);
  
  typedef struct{
    To_Cpu#(addr_width,word_size) response;
    Bit#(TAdd#(TLog#(cbuff_size),1)) token;
  }Cpu_resp_with_token#(numeric type addr_width,numeric type cbuff_size,numeric type word_size) deriving(Bits,Eq);
  
  typedef struct{
    Bit#(addr_width) address;
    Bit#(2) transfer_size;
    Bit#(3) burst_mode;
    Access_type ld_st;
    Bit#(TAdd#(TLog#(cbuff_size),1)) token;
  } To_Memory#(numeric type addr_width,numeric type cbuff_size) deriving(Bits,Eq);

  typedef struct{
    Bit#(TMul#(8,word_size)) data_word;
    Bit#(1) bus_error;
    Bit#(addr_width) address;
    Bit#(TAdd#(TLog#(cbuff_size),1)) token;
  }From_Memory#(numeric type addr_width,numeric type word_size, numeric type block_size, numeric type cbuff_size) deriving(Bits,Eq);

  typedef struct {
    Bit#(TAdd#(TLog#(cbuff_size),1)) token;
    Bit#(TLog#(ways)) replace_block;
       Prediction prediction1;
    Prediction prediction2;
	Bit#(addr_width) pc;
  }Metadata#(numeric type addr_width,numeric type _word_size,numeric type _block_size,numeric type cbuff_size,numeric type ways) deriving (Bits,Eq);

  typedef struct{
    To_Memory#(addr_width,cbuff_size) request;
    Metadata#(addr_width,_word_size,_block_size,cbuff_size,_ways) metadata;
  }LoadBufferData#(numeric type addr_width,numeric type _word_size,numeric type _block_size,numeric type cbuff_size, numeric type _ways) deriving (Bits,Eq);
endpackage 
