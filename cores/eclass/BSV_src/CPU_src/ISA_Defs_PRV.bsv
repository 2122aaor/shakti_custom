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

/* ==================================================================================
 
 RISC-V Defined Privilege Levels
 RISC-V ISA defines four Privilege Levels with Abbreviations and Encoding as below
 Level Encoding   Name      Abbreviation
 ----- --------   ----       ------------
   0     00    User        U
   1     01   Supervisor   S
   2     10   Hypervisor   H
   3     11   Machine      M
 This package defines functions and types necessary to implement User-Level 
 and Machine-Level Privileges

=====================================================================================*/

import ISA_Defs :: *;   // Needed for importing XPRLEN

// Privilege Level Declaration
Bit #(2) prvU = 0;
Bit #(2) prvS = 1;
Bit #(2) prvH = 2;
Bit #(2) prvM = 3;

typedef enum {U, S, H, M} Privilege_Level deriving (Bits, Eq);
typedef Funct12 CSR_Addr;

// CSR Map
typedef enum {
            // User-Level CSRs
            // ------

            /*User Trap Status*/
            USTATUS   = 'h000,
            UIE       = 'h004,
            UTVEC     = 'h005,

            /*User Trap Handling*/
            USCRATCH  = 'h040,
            UEPC      = 'h041,
            UCAUSE    = 'h042,
            UBADADDR  = 'h043,
            UIP       = 'h044,

            /*User Counter/Timers*/
            UCYCLE    = 'hc00,
            UTIME     = 'hc01,
            UINSTRET  = 'hc02,
            UCYCLEH   = 'hc80,
            UTIMEH    = 'hc81,
            UINSTRETH = 'hc82,

            // Machine-Level CSRs
            // ------
            /*Machine Information Registers*/
            MISA      = 'h301, 
            MVENDORID = 'hf11, 
            MARCHID   = 'hf12, 
            MIMPID    = 'hf13, 
            MHARTID   = 'hf14,

            /*Machine Trap Setup CSR*/
            MSTATUS  = 'h300,
            MEDELEG  = 'h302,
            MIDELEG  = 'h303,
            MIE      = 'h304,
            MTVEC    = 'h305,

            /*Machine Trap Handling*/ 
            MSCRATCH = 'h340,
            MEPC     = 'h341,
            MCAUSE   = 'h342,
            MBADADDR = 'h343,
            MIP      = 'h344,
            
            /*Machine Protection and Translation*/ 
            MBASE    = 'h380,
            MBOUND   = 'h381,
            MIBASE   = 'h382,
            MIBOUND  = 'h383,
            MDBASE   = 'h384,
            MDBOUND  = 'h385,
            
            /*Macine Timers and Counters*/ 
            MCYCLE   = 'hb00,
            //MTIME  = 'hf01,
            MINSTRET = 'hb02, 
            MCYCLEH  = 'hb80,
            //MTIMEH = 'hf81,
            MINSTRETH= 'hb82,
            
            /*Machine Counter Setup*/
            MUCOUNTEREN = 'h320, 
            MSCOUNTEREN = 'h321,
            MHCOUNTEREN = 'h322,

            /*Machine Counter-Delta Register*/
            MUCYCLE_DELTA     = 'h700,
            MUTIME_DELTA      = 'h701,
            MUINSTRET_DELTA   = 'h702,
            MUCYCLE_DELTAH    = 'h780,
            MUTIME_DELTAH     = 'h781,
            MUINSTRET_DELTAH  = 'h782,

            /*Non-Standard Machine Register*/
            MXUARTRX          = 'h77e,
            MXUARTTX          = 'h77f
} CSR deriving (Bits, Eq);

typedef Bit #(16) Interrupt_Vector;
typedef Bit #(16) Exception_Vector;

// Function to check if a given CSR is supported
   function Bool isValidCSR(CSR csr);
      Bool ret = (
         case (csr)
            USTATUS   : True;
            UIE       : True; 
            UTVEC     : True;
                        
            USCRATCH  : True;
            UEPC      : True;
            UCAUSE    : True;
            UBADADDR  : True;
            UIP       : True;
                       
            UCYCLE    : True;
            UTIME     : True;
            UINSTRET  : True;
            UCYCLEH   : True;
            UTIMEH    : True;
            UINSTRETH : True;
                      
            MISA      : True;
            MVENDORID : True;
            MARCHID   : True;
            MIMPID    : True;
            MHARTID   : True;
                     
            MSTATUS  :  True;
            MEDELEG  :  True;
            MIDELEG  :  True;
            MIE      :  True;
            MTVEC    :  True;
            
            MSCRATCH :  True;
            MEPC     :  True;
            MCAUSE   :  True;
            MBADADDR :  True;
            MIP      :  True;
            
            MBASE    :  True;
            MBOUND   :  True;
            MIBASE   :  True;
            MIBOUND  :  True;
            MDBASE   :  True;
            MDBOUND  :  True;
            
            MCYCLE   :  True;
            MINSTRET :  True;
            MCYCLEH  :  True;
            MINSTRETH:  True;
            
            MUCOUNTEREN : True;
                       
            MUCYCLE_DELTA     : True; 
            MUTIME_DELTA      : True;
            MUINSTRET_DELTA   : True;
            MUCYCLE_DELTAH    : True;
            MUTIME_DELTAH     : True;
            MUINSTRET_DELTAH  : True;

            // Non-standard Machine Register
            MXUARTTX : True;
            MXUARTRX : True;

            default: False;
         endcase
         );
      return ret;
   endfunction

function Bool hasCSRPermission(CSR_Addr csr, Bit#(2) prv, Bool write);
    CSR_Addr csr_index = pack(csr);
    return ((prv >= csr_index[9:8]) && (!write || (csr_index[11:10] != 2'b11)));
endfunction
