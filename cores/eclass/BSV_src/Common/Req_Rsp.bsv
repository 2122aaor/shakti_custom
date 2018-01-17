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

package Req_Rsp;

// ================================================================
// This package defines Requests and Responses 
// ================================================================

// ----------------------------------------------------------------

import DefaultValue :: *;
import TLM2         :: *;

`include "TLM.defines"
`include "RVC.defines"

// ================================================================
// TLM Configurable parameters
// `define TLM_PRM_DCL numeric type id_size,   \
//		       numeric type addr_size, \
//		       numeric type data_size, \
//		       numeric type unit_size, \
//		       custom  

// `define TLM_PRM id_size, addr_size, data_size, unit_size, cstm_type

// `define TLM_PRM_STD 4, 32, 32, 10, Bit#(0)
// ================================================================

typedef enum { BITS8, BITS16, BITS32, BITS64, BITS128, BITS256, BITS512, BITS1024} TLMBSize
   deriving (Eq, Bits);

// Request types at targets and initiators
// At CPU: Format for Request to be SENT
typedef TLMRequest #(`TLM_PRM_REQ_CPU) Req_CPU;
typedef RequestDescriptor #(`TLM_PRM_REQ_CPU) Req_Desc_CPU;
typedef RequestData #(`TLM_PRM_REQ_CPU) Req_data_CPU;

// At Memory: Format for ANTICIPATED Response
typedef TLMRequest #(`TLM_PRM_REQ_MEM) Req_MEM;
typedef RequestDescriptor #(`TLM_PRM_REQ_MEM) Req_Desc_MEM;
typedef RequestData #(`TLM_PRM_REQ_MEM) Req_data_MEM;

// Response types at targets and initiators
// At CPU: Format for ANTICIPATED Response 
typedef TLMResponse #(`TLM_PRM_RSP_CPU) Rsp_CPU;

// At Memory: Format for Response to be SENT
typedef TLMResponse #(`TLM_PRM_RSP_MEM) Rsp_MEM;

// Instance of default value class used to establish default value for custom field. TLMBSize for now
instance DefaultValue #(TLMBSize);
   function defaultValue ();
      return BITS32;
   endfunction
endinstance

// ----------------
// Help functions to display requests

function Action display_TLMCommand (TLMCommand cmd);
   case (cmd)
      READ:    $write ("READ");
      WRITE:   $write ("WRITE");
      UNKNOWN: $write ("UNKNOWN");
   endcase
endfunction

function Action display_TLMBSize (TLMBSize sz);
   case (sz)
      BITS8    : $write ("8b");
      BITS16   : $write ("16b");
      BITS32   : $write ("32b");
      BITS64   : $write ("64b");
      BITS128  : $write ("128b");
      BITS256  : $write ("256b");
      BITS512  : $write ("512b");
      BITS1024 : $write ("1024b");
   endcase
endfunction

function Action display_cpu_Req (RequestDescriptor #(`TLM_PRM_REQ_CPU) desc);
    action
      $write ("Req_Descriptor{");
      display_TLMCommand (desc.command);
      $write (" %h %h ", desc.addr, desc.data);
    endaction
endfunction

// ================================================================
// Responses


// ----------------
// Help functions to display requests

function Action display_TLMStatus (TLMStatus status);
   case (status)
      SUCCESS      : $write ("SUCCESS");
      ERROR        : $write ("ERROR");
      NO_RESPONSE  : $write ("NO RESPONSE");
      UNKNOWN	   : $write ("UNKNOWN");
   endcase
endfunction

function Action display_mem_Rsp (TLMResponse #(`TLM_PRM_RSP_MEM) rsp);
   action
      $write ("Rsp{");
      display_TLMCommand (rsp.command);
      $write (" %h ", rsp.data);
      display_TLMStatus (rsp.status);
   endaction
endfunction

// ================================================================

endpackage: Req_Rsp
