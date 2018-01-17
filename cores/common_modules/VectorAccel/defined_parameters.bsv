/*
Copyright (c) 2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name     : Defined Parameters for VPU
Author Name     : Vinod G, Sumanth Sridhar
e-mail Id       : g.vinod1993@gmail.com, sumanthsridhar.009@gmail.com
Last updated on : 30th June 2016
*/




// RegFile defs
`define	VS_AddrWidth			6
`define	VA_AddrWidth			5

`define ROB_BUFFER_SIZE			16
`define STORE_BUFFER_SIZE		16

///////////////////////////////// Settings for static compilation /////////////////////////////////

// Scalar Unit

`define	USE_ROB

/* Defines whether write-back aribitration when multiple writes occur in a 
   single cycle shd be age-based or priority (of functional unit) based 
   NOTE: only priority arbitration supported when ROB not present	*/
	`define	Scalar_WriteBack_Priority // ifndef: Scalar_WriteBack_OldestFirst

/* List of all forwarding paths */
	// `define	FWD_TO_Decode
	`define	FWD_FROM_ScalarExec
	`define	FWD_FROM_WriteBack
	`define	FWD_FROM_MulDiv
	`define	FWD_FROM_SMU
	`define	FWD_FROM_FPU
	`define	FWD_FROM_VectorUnit
	`define	FWD_FROM_TOTAL			6
