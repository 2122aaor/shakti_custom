package TLMReqRsp;

import TLM2	:: *;
import Axi	:: *;
`include "defined_parameters.bsv"

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

endpackage
