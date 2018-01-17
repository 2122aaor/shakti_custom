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
  typedef enum {Load,Store} Access_type deriving (Bits,Eq,FShow);

  typedef struct{
    Bit#(addr_width) address;
  }From_Cpu#(numeric type addr_width) deriving(Bits,Eq);
  
  typedef struct{
    Bit#(TMul#(word_size,8)) data_word;
    Bit#(1) bus_error;
    Bit#(1) misaligned_error;
    Bit#(addr_width) address;
  }To_Cpu#(numeric type addr_width,numeric type word_size) deriving(Bits,Eq);

  typedef struct{
    Bit#(addr_width) address;
    Bit#(5) burst_length; 
    Access_type ld_st;
  } To_Memory#(numeric type addr_width, numeric type ways, numeric type block_size) deriving(Bits,Eq);

  typedef struct{
    Bit#(TMul#(8,word_size)) data_line;
    Bit#(1) bus_error;
    Bit#(1) misaligned_error;
    Bit#(addr_width) address;
  }From_Memory#(numeric type addr_width,numeric type word_size, numeric type block_size) deriving(Bits,Eq);

  typedef struct{
    Bit#(TMul#(8,TMul#(word_size,block_size))) data_line;
    Bit#(addr_width) address;
  }Recent_memory_response#(numeric type addr_width,numeric type word_size, numeric type block_size) deriving(Bits,Eq);

endpackage 
