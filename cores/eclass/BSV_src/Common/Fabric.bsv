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

package Fabric;

// Bluespec library imports

import Vector       :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

// ================================================================
// Project imports

import TLM2		   :: *; // Using local copy only.
import AHB		   :: *; // Using local copy only.
import Utils       :: *;
import Req_Rsp     :: *;
import Sys_Configs :: *;

`include "TLM.defines"
`include "RVC.defines"

// ================================================================
// Typedefinitions for masters, slaves and fabrics

// Master and slave fabrics interfaces, for CPU and MEMORY
typedef AHBFabricMaster #(`TLM_PRM_AHB_REQ) AHBMasterFabric;
typedef AHBFabricSlave  #(`TLM_PRM_AHB_RSP) AHBSlaveFabric;

// Master and slave transactors, definition for CPU and MEMORY
typedef  AHBMasterXActor #(`TLM_RR_AHB, `TLM_PRM_REQ_CPU) AHBMasterActor;
typedef  AHBSlaveXActor  #(`TLM_RR_AHB, `TLM_PRM_RSP_MEM) AHBSlaveActor;

endpackage: Fabric
