package TLMReqRsp;

import TLM2	:: *;
import Axi	:: *;
`include "TLM2Defs.defines"

// TLM2 Request Response types for SHAKTI I-Class Processor

// Request Respose types for Memory
typedef TLMRequest #(`TLM_PRM_MEM_REQ) 			Req_Mem;
typedef RequestDescriptor #(`TLM_PRM_MEM_REQ) 	Req_Desc_Mem;
typedef RequestData #(`TLM_PRM_MEM_REQ) 	    Req_Data_Mem;

typedef TLMResponse #(`TLM_PRM_MEM_RSP) 		Rsp_Mem;

// Request Respose types for CPU
typedef TLMRequest #(`TLM_PRM_CPU_REQ) 			Req_CPU;
typedef RequestDescriptor #(`TLM_PRM_CPU_REQ) 	Req_Desc_CPU;
typedef RequestData #(`TLM_PRM_CPU_REQ) 	    Req_Data_CPU;

typedef TLMResponse #(`TLM_PRM_CPU_RSP) 		Rsp_CPU;

/*instance AxiConvert #(AxiLock, Bit #(64));
	function AxiLock toAxi(Bit #(64) value);
		return unpack(0);
	endfunction
	function Bit #(64) fromAxi(AxiLock value);
		return ?;
	endfunction
endinstance

instance AxiConvert #(AxiCache, Bit #(64));
	function AxiCache toAxi(Bit #(64) value);
		return unpack(0);
	endfunction
	function Bit #(64) fromAxi(AxiCache value);
		return ?;
	endfunction
endinstance

instance AxiConvert #(AxiProt, Bit #(64));
	function AxiProt toAxi(Bit #(64) value);
		return unpack(0);
	endfunction
	function Bit #(64) fromAxi(AxiProt value);
		return ?;
	endfunction
endinstance

instance AxiConvert #(AxiCustom, Bit #(64));
	function AxiCustom toAxi(Bit #(64) value);
		return unpack(value);
	endfunction
	function Bit #(64) fromAxi(AxiCustom value);
		return pack(value);
	endfunction
endinstance
*/
endpackage
