/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Names : Abhinaya Agrawal
Email ID : agrawal.abhinaya@gmail.com
*/

// ================================================================
// bsc lib imports

import Vector :: *;
import TLM2   :: *;

`include "TLM.defines"
// ================================================================
// Project imports

import Req_Rsp :: *;
import TypeDefs :: *;
import ISA_Defs :: *;

`include "RVC.defines"
`include "macro.defines"

// ================================================================

typedef enum { BYTES1, BYTES2, BYTES3, BYTES4 } TLMBurstSize_Bytes deriving (Eq, Bits);

// Function to convert Data and Instruction lengths into bytes format as maintained by TLMRequest.burst_size
// TLMBurstSize #(`TLM_PRM) --> Bit #(TLog #(TDiv #(data_size), 8))
// For data_size = 32, TLMBurstSize --> Bit #(2)
//		       2'b00 => 1 byte burst
//		       2'b01 => 2 byte burst
//		       2'b10 => 3 byte burst
//		       2'b11 => 4 byte burst
function TLMBurstSize #(`TLM_PRM_REQ_CPU) reqSz_bytes_i (UInt#(8) sz);
   UInt #(8) bytes = sz>>3;
   TLMBurstSize_Bytes ret;
   case (bytes) matches
       1: ret = BYTES1;
       2: ret = BYTES2;
       3: ret = BYTES3;
	   4: ret = BYTES4;
	   default: ret = BYTES1; //@TODO
   endcase
   return pack(ret);
endfunction 

// Function to convert request size forwarded by CPU into Memory acceptable format
// For CPU: 'b00 -> 1 byte; For Mem: 'b01 -> 1 byte;
// This is required as TLMBurstSize follows this format
function Bit #(8) reqSz_bytes_mem (TLMBurstSize #(`TLM_PRM_REQ_CPU) sz);
   Bit #(8) n_b = extend(sz);
   return n_b + 1;
endfunction

// Memory Map
// Main Memory
Addr addr_base_main_memory = 'h80000000;
Addr addr_bounds_main_memory = 'hffffffff;
// RTC
Addr addr_base_rtc = 'h40000000;
Addr addr_bounds_rtc = 'h400000ff;
// Config String
Addr addr_base_config_memory = 'h0000100c;
Addr addr_bounds_config_memory = 'h0000ffff;
