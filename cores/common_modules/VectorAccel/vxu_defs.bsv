/*
Copyright (c) 2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name     : Parameters for Vector Execution Unit
Author Name     : Vinod G, Sumanth Sridhar
e-mail Id       : g.vinod1993@gmail.com, sumanthsridhar.009@gmail.com
Last updated on : 5th November 2016
*/

// constants
`define	L						2			// no of lanes
`define	B						4			// no of banks per lane
`define	W						64			// max precision (no of bits) per element
`define	w						16			// min precision = VRF_WIDTH/PRF_WIDTH = BANK_WIDTH/BANK_PRED_WIDTH
`define	HLEN					16			// hardware length = no of 'W'-bit entries in each line of VRF
`define	VRF_WIDTH				1024		// no of bits per VRF entry (width of VRF = W x HLEN)
`define	VRF_DEPTH				64			// no of entries in Vector RegFile (depth of VRF)
`define	PRF_WIDTH				64			// no of bits per entry in PRF (width of PRF) todo check this number
`define	PRF_DEPTH				16			// no of entries in Predicate RegFile (depth of PRF)

// bank constants
`define	BANK_WIDTH				128			// no of bits per bank entry (width of banks)
`define	BANK_DEPTH				64			// no of entries in Vector RegFile (depth of VRF)
`define	BANK_PRED_WIDTH			8			// no of bits of a predicate line per bank
`define	N_bankALUs				2			// no of ALUs per bank
// `define	MVP_128b						// define if 128b is to be supported
`define	MVP_64b								// define if 64b is to be supported
`define	MVP_32b								// define if 32b is to be supported
`define	MVP_16b								// define if 16b is to be supported
// `define	MVP_8b							// define if 8b is to be supported
`define	N_Entries_128b			0
`define	N_Entries_64b			32
`define	N_Entries_32b			16
`define	N_Entries_16b			16
`define	N_Entries_8b			0
`define	BRQ_DEPTH				8			// no of entries in the FIFO 'Bank Read Q'
`define	BPQ_DEPTH				8			// no of entries in the FIFO 'Bank Predicate Q'

// Latencies
`define	LAT_REGFILE		0
`define	LAT_ALU			1

// Config defs
