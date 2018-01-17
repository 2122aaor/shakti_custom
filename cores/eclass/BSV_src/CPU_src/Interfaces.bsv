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

import TypeDefs :: *;
import ISA_Defs :: *;

interface CoreDebugIFC;
   method Action              reset;                            // Reset CPU to Initial state
   method ActionValue#(Bool)  reset_complete;                   // True if all CPU and all sub-modules are reset to initial state
   method Action              run_continue (Maybe #(Addr) mpc); // Execute all instructions until the end of instruction stream
   method Action              run_step     (Maybe #(Addr) mpc); // Execute next instruction only
   method Action              stop;                             // Stop CPU
   method CPU_Stop_Reason     stop_reason;                      // Returns the reason why CPU is currently stopped
   method Addr                read_pc;                          // Read Current PC. (Address of instruction to be fetched)
   method Addr                read_exec_pc;                     // Read Currently executed PC. (Address of instruction currently being executed, i.e., is in third stage)
   method Action              write_pc (Addr d);                // Update PC. Used by testbench to provide initial PC
   method Data                read_gpr (RegName r);             // Read a General-Purpose Register
   method Action              write_gpr (RegName r, Data d);    // Write into a General-Purpose Register
   method Action              req_read_memW (Addr addr);        // Request to read a word (or range) in data memory
   method ActionValue #(Data) rsp_read_memW ();                 // Response for a request generated using req_read_memW method
   method Action              write_memW (Addr addr, Data d);   // Write a word (or range) in data memory
   method CounterData         read_instret;                     // Read the number of instructions retired. This is the number of instructions already committed
   method CounterData         read_cycle;                       // Read number of cycles past
   method Action              set_verbosity (int verbosity);    // Defines the frequency with which management information is printed
endinterface

interface InterruptIFC;
   method Action timer   (Bool mtip);
   method Action software(Bool msip);
   method Action external(Bool meip);
endinterface

interface UartIFC;
   method Action  sin(Bit#(1) in);
   method Bit#(1) sout();
	method Bool		busy;
endinterface
