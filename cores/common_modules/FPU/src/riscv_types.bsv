/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name   	: Risc-V typedefs
Author's Name 	: Arjun C. Menon
e-mail id	: c.arjunmenon@gmail.com
Last updated on : 22nd October 2013

	This module consists of typedefs of commonly used structures and unions in various modules.
*/

package riscv_types;
	typedef union tagged{ 	
		void No_exception;					// indicates that ther was no exception generated
		Bool Invalid;						// indicates that the operation is invalid
		Bool Divide_by_Zero;					// indicates that the division operation is a divide by zero.
		Bool Overflow;						// indicates an overflow
		Bool Underflow;						// indicates an underflow
		Bool Inexact;						// indicates that the produced result is inexact
		}Exception deriving(Bits, Eq);

	typedef struct{
		Bit#(5) destination;					// holds the destination address of the operation in the registerfile
		Bit#(32) fsr;						// the file status register containing all the flags and control bits
		Bit#(64) final_result;					// the final result for the operation
		Exception exception; 					// indicates if any exception is generated.
		}Output_type deriving(Bits,Eq);				// data structure of the output FIFO.
endpackage
