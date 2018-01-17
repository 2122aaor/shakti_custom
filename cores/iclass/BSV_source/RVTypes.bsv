/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 


Author Names : Rahul Bodduna
Email ID : rahul.bodduna@gmail.com
*/

package RVTypes;


`include "defined_parameters.bsv"
import DefaultValue::*;
import FShow::*;
import Vector::*;
import riscv_types:: *;

typedef Bit#(TDiv#(DataSz,8)) DataByteEn;
typedef Bit#(TLog#(TDiv#(DataSz,8))) DataByteSel; // Type of byte select value for Data
// Virtual address
typedef XLEN AddrSz;
typedef Bit#(AddrSz) Addr;

// Physical address
typedef 64 PAddrSz;
typedef Bit#(PAddrSz) PAddr;
typedef enum {
    B   = 2'b00,
		H   = 2'b01,
		W   = 2'b10,
		D   = 2'b11
} RVMemSize deriving (Bits, Eq, FShow);


`ifdef CONFIG_RV64
typedef 26 AsidSz;
`endif
`ifdef CONFIG_RV32
typedef 10 AsidSz;
`endif
typedef Bit#(AsidSz) Asid;

typedef enum {
    CSRustatus          = 12'h000,
    CSRuie              = 12'h004,
    CSRutvec            = 12'h005,
    CSRuscratch         = 12'h040,
    CSRuepc             = 12'h041,
    CSRucause           = 12'h042,
    CSRubadaddr         = 12'h043,
    CSRuip              = 12'h044,
    CSRfflags           = 12'h001,
    CSRfrm              = 12'h002,
    CSRfcsr             = 12'h003,
    CSRcycle            = 12'hc00,
    CSRtime             = 12'hc01,
    CSRinstret          = 12'hc02,
		CSRhpmcounter3			= 12'hc03,
		CSRhpmcounter4			= 12'hc04,
		CSRhpmcounter5			= 12'hc05,
		CSRhpmcounter6			= 12'hc06,
		CSRhpmcounter7			= 12'hc07,
		CSRhpmcounter8			= 12'hc08,
		CSRhpmcounter9			= 12'hc09,
		CSRhpmcounter10			= 12'hc0a,
		CSRhpmcounter11			= 12'hc0b,
		CSRhpmcounter12			= 12'hc0c,
		CSRhpmcounter13			= 12'hc0d,
		CSRhpmcounter14			= 12'hc0e,
		CSRhpmcounter15			= 12'hc0f,
		CSRhpmcounter16			= 12'hc10,
		CSRhpmcounter17			= 12'hc11,
		CSRhpmcounter18			= 12'hc12,
		CSRhpmcounter19			= 12'hc13,
		CSRhpmcounter20			= 12'hc14,
		CSRhpmcounter21			= 12'hc15,
		CSRhpmcounter22			= 12'hc16,
		CSRhpmcounter23			= 12'hc17,
		CSRhpmcounter24			= 12'hc18,
		CSRhpmcounter25			= 12'hc19,
		CSRhpmcounter26			= 12'hc1a,
		CSRhpmcounter27			= 12'hc1b,
		CSRhpmcounter28			= 12'hc1c,
		CSRhpmcounter29			= 12'hc1d,
		CSRhpmcounter30			= 12'hc1e,
		CSRhpmcounter31			= 12'hc1f,
    CSRcycleh           = 12'hc80,
    CSRtimeh            = 12'hc81,
    CSRinstreth         = 12'hc82,
		CSRhpmcounterh3			= 12'hc83,
		CSRhpmcounterh4			= 12'hc84,
		CSRhpmcounterh5			= 12'hc85,
		CSRhpmcounterh6			= 12'hc86,
		CSRhpmcounterh7			= 12'hc87,
		CSRhpmcounterh8			= 12'hc88,
		CSRhpmcounterh9			= 12'hc89,
		CSRhpmcounterh10		= 12'hc8a,
		CSRhpmcounterh11		= 12'hc8b,
		CSRhpmcounterh12		= 12'hc8c,
		CSRhpmcounterh13		= 12'hc8d,
		CSRhpmcounterh14		= 12'hc8e,
		CSRhpmcounterh15		= 12'hc8f,
		CSRhpmcounterh16		= 12'hc90,
		CSRhpmcounterh17		= 12'hc91,
		CSRhpmcounterh18		= 12'hc92,
		CSRhpmcounterh19		= 12'hc93,
		CSRhpmcounterh20		= 12'hc94,
		CSRhpmcounterh21		= 12'hc95,
		CSRhpmcounterh22		= 12'hc96,
		CSRhpmcounterh23		= 12'hc97,
		CSRhpmcounterh24		= 12'hc98,
		CSRhpmcounterh25		= 12'hc99,
		CSRhpmcounterh26		= 12'hc9a,
		CSRhpmcounterh27		= 12'hc9b,
		CSRhpmcounterh28		= 12'hc9c,
		CSRhpmcounterh29		= 12'hc9d,
		CSRhpmcounterh30		= 12'hc9e,
		CSRhpmcounterh31		= 12'hc9f,
    CSRsstatus          = 12'h100,
    CSRsedeleg          = 12'h102,
    CSRsideleg          = 12'h103,
    CSRsie              = 12'h104,
    CSRstvec            = 12'h105,
    CSRsscratch         = 12'h140,
    CSRsepc             = 12'h141,
    CSRscause           = 12'h142,
    CSRsbadaddr         = 12'h143,
    CSRsip              = 12'h144,
    CSRsptbr            = 12'h180,
    CSRhstatus          = 12'h200,
    CSRhedeleg          = 12'h202,
    CSRhideleg          = 12'h203,
    CSRhie              = 12'h204,
    CSRhtvec            = 12'h205,
    CSRhscratch         = 12'h240,
    CSRhepc             = 12'h241,
    CSRhcause           = 12'h242,
    CSRhbadaddr         = 12'h243,
    CSRhip              = 12'h244,
    CSRmisa             = 12'hf10,
    CSRmvendorid        = 12'hf11,
    CSRmarchid          = 12'hf12,
    CSRmimpid           = 12'hf13,
    CSRmhartid          = 12'hf14,
    CSRmstatus          = 12'h300,
    CSRmedeleg          = 12'h302,
    CSRmideleg          = 12'h303,
    CSRmie              = 12'h304,
    CSRmtvec            = 12'h305,
    CSRmscratch         = 12'h340,
    CSRmepc             = 12'h341,
    CSRmcause           = 12'h342,
    CSRmbadaddr         = 12'h343,
    CSRmip              = 12'h344,
    CSRmbase            = 12'h380,
    CSRmbound           = 12'h381,
    CSRmibase           = 12'h382,
    CSRmibound          = 12'h383,
    CSRmdbase           = 12'h384,
    CSRmdbound          = 12'h385,
    CSRmcycle           = 12'hb00,
    CSRminstret         = 12'hb02,
		CSRmhpmcounter3			= 12'hb03,
		CSRmhpmcounter4			= 12'hb04,
		CSRmhpmcounter5			= 12'hb05,
		CSRmhpmcounter6			= 12'hb06,
		CSRmhpmcounter7			= 12'hb07,
		CSRmhpmcounter8			= 12'hb08,
		CSRmhpmcounter9			= 12'hb09,
		CSRmhpmcounter10		= 12'hb0a,
		CSRmhpmcounter11		= 12'hb0b,
		CSRmhpmcounter12		= 12'hb0c,
		CSRmhpmcounter13		= 12'hb0d,
		CSRmhpmcounter14		= 12'hb0e,
		CSRmhpmcounter15		= 12'hb0f,
		CSRmhpmcounter16		= 12'hb10,
		CSRmhpmcounter17		= 12'hb11,
		CSRmhpmcounter18		= 12'hb12,
		CSRmhpmcounter19		= 12'hb13,
		CSRmhpmcounter20		= 12'hb14,
		CSRmhpmcounter21		= 12'hb15,
		CSRmhpmcounter22		= 12'hb16,
		CSRmhpmcounter23		= 12'hb17,
		CSRmhpmcounter24		= 12'hb18,
		CSRmhpmcounter25		= 12'hb19,
		CSRmhpmcounter26		= 12'hb1a,
		CSRmhpmcounter27		= 12'hb1b,
		CSRmhpmcounter28		= 12'hb1c,
		CSRmhpmcounter29		= 12'hb1d,
		CSRmhpmcounter30		= 12'hb1e,
		CSRmhpmcounter31		= 12'hb1f,
    CSRmcycleh          = 12'hb80,
    CSRminstreth        = 12'hb82,
		CSRmhpmcounterh3		= 12'hb83,
		CSRmhpmcounterh4		= 12'hb84,
		CSRmhpmcounterh5		= 12'hb85,
		CSRmhpmcounterh6		= 12'hb86,
		CSRmhpmcounterh7		= 12'hb87,
		CSRmhpmcounterh8		= 12'hb88,
		CSRmhpmcounterh9		= 12'hb89,
		CSRmhpmcounterh10		= 12'hb8a,
		CSRmhpmcounterh11		= 12'hb8b,
		CSRmhpmcounterh12		= 12'hb8c,
		CSRmhpmcounterh13		= 12'hb8d,
		CSRmhpmcounterh14		= 12'hb8e,
		CSRmhpmcounterh15		= 12'hb8f,
		CSRmhpmcounterh16		= 12'hb90,
		CSRmhpmcounterh17		= 12'hb91,
		CSRmhpmcounterh18		= 12'hb92,
		CSRmhpmcounterh19		= 12'hb93,
		CSRmhpmcounterh20		= 12'hb94,
		CSRmhpmcounterh21		= 12'hb95,
		CSRmhpmcounterh22		= 12'hb96,
		CSRmhpmcounterh23		= 12'hb97,
		CSRmhpmcounterh24		= 12'hb98,
		CSRmhpmcounterh25		= 12'hb99,
		CSRmhpmcounterh26		= 12'hb9a,
		CSRmhpmcounterh27		= 12'hb9b,
		CSRmhpmcounterh28		= 12'hb9c,
		CSRmhpmcounterh29		= 12'hb9d,
		CSRmhpmcounterh30		= 12'hb9e,
		CSRmhpmcounterh31		= 12'hb9f,
    CSRmucounteren      = 12'h320,
    CSRmscounteren      = 12'h321,
    CSRmhcounteren      = 12'h322,
		CSRmhpmevent3				= 12'h323,
		CSRmhpmevent4				= 12'h324,
		CSRmhpmevent5				= 12'h325,
		CSRmhpmevent6				= 12'h326,
		CSRmhpmevent7				= 12'h327,
		CSRmhpmevent8				= 12'h328,
		CSRmhpmevent9				= 12'h329,
		CSRmhpmevent10			= 12'h32a,
		CSRmhpmevent11			= 12'h32b,
		CSRmhpmevent12			= 12'h32c,
		CSRmhpmevent13			= 12'h32d,
		CSRmhpmevent14			= 12'h32e,
		CSRmhpmevent15			= 12'h32f,
		CSRmhpmevent16			= 12'h330,
		CSRmhpmevent17			= 12'h331,
		CSRmhpmevent18			= 12'h332,
		CSRmhpmevent19			= 12'h333,
		CSRmhpmevent20			= 12'h334,
		CSRmhpmevent21			= 12'h335,
		CSRmhpmevent22			= 12'h336,
		CSRmhpmevent23			= 12'h337,
		CSRmhpmevent24			= 12'h338,
		CSRmhpmevent25			= 12'h339,
		CSRmhpmevent26			= 12'h33a,
		CSRmhpmevent27			= 12'h33b,
		CSRmhpmevent28			= 12'h33c,
		CSRmhpmevent29			= 12'h33d,
		CSRmhpmevent30			= 12'h33e,
		CSRmhpmevent31			= 12'h33f
  } CSR deriving (Bits, Eq, FShow);

function Bool hasCSRPermission(CSR csr, Bit#(2) prv, Bool write);
    Bit#(12) csr_index = pack(csr);
    return ((prv >= csr_index[9:8]) && (!write || (csr_index[11:10] != 2'b11)));
endfunction

typedef enum {
    FenceI,
    SFenceVM
} IntraCoreFence deriving (Bits, Eq, FShow);

typedef struct {
    Bool sw; // successor wrtie
    Bool sr; // successor read
    Bool so; // successor output
    Bool si; // successor input
    Bool pw; // predecessor write
    Bool pr; // predecessor read
    Bool po; // predecessor output
    Bool pi; // predecessor input
} InterCoreFence deriving (Bits, Eq, FShow);

typedef union tagged {
    IntraCoreFence IntraCore;
    InterCoreFence InterCore;
} Fence deriving (Bits, Eq, FShow);

typedef struct {                                                                                                               
     Bool rv64;
     // ISA modes
     Bool h;
     Bool s;
     Bool u;
     // standard ISA extensions
     Bool m;
     Bool a;
     Bool f;
     Bool d;
     // non-standard extensions
     Bool x;
 } RiscVISASubset deriving (Bits, Eq, FShow);
 
 instance DefaultValue#(RiscVISASubset);
     function RiscVISASubset defaultValue = RiscVISASubset{
 `ifdef CONFIG_RV64
             rv64:   True,
 `else
             rv64:   False,
 `endif
             h:      False,
 `ifdef CONFIG_S
             s:      True,
 `else
             s:      False,
 `endif
 `ifdef CONFIG_U
             u:      True,
 `else
             u:      False,
 `endif
 `ifdef CONFIG_M
             m:      True,
 `else
             m:      False,
 `endif
 `ifdef CONFIG_A
             a:      True,
 `else
             a:      False,
 `endif
 `ifdef CONFIG_F
             f:      True,
 `else
             f:      False,
 `endif
 `ifdef CONFIG_D
             d:      True,
 `else
             d:      False,
 `endif
             x:      False
     };
 endinstance


function Data getMISA(RiscVISASubset isa);
     // include I by default
     Data misa = {2'b00, 0, 26'b00000000000000000100000000};
     if (isa.rv64) begin
         // rv64
         misa = misa | {2'b10, 0, 26'b00000000000000000000000000};
     end else begin
         // rv32
         misa = misa | {2'b01, 0, 26'b00000000000000000000000000};
     end
     if (isa.s) misa = misa | {2'b00, 0, 26'b00000001000000000000000000};
     if (isa.u) misa = misa | {2'b00, 0, 26'b00000100000000000000000000};
     if (isa.m) misa = misa | {2'b00, 0, 26'b00000000000001000000000000};
     if (isa.a) misa = misa | {2'b00, 0, 26'b00000000000000000000000001};
     if (isa.f) misa = misa | {2'b00, 0, 26'b00000000000000000000100000};
     if (isa.d) misa = misa | {2'b00, 0, 26'b00000000000000000000001000};
     return misa;
 endfunction


typedef enum {
    Gpr = 1'b0,
    Fpu = 1'b1
} RegType deriving (Bits, Eq, FShow);

typedef enum {
    None, I, S, SB, U, UJ, Z
} ImmType deriving (Bits, Eq, FShow);

typedef struct {
    Bit#(2) prv;
    Bit#(3) frm;
    Bool f_enabled;
    Bool x_enabled;
} CsrState deriving (Bits, Eq, FShow);

typedef struct {
    Bit#(2) prv;
    Asid    asid;
    Bit#(5) vm;
    Bool    mxr;
    Bool    pum;
    Addr    base;
    Addr    bound;
} VMInfo deriving (Bits, Eq, FShow);
instance DefaultValue#(VMInfo);
    function VMInfo defaultValue = VMInfo {prv: prvM, asid: 0, vm: 0, mxr: False, pum: False, base: 0, bound: 0};
endinstance

// Instead of making PMAs generic (like a massive struct), we are adding named
// PMAs as needed. Currently these PMAs are defined by device
typedef enum {
    MainMemory, // Cacheable, R, W, and X, all AMO supported
    IORom,      // Cacheable, R and X only, no AMO
    IODevice,   // R and W, but no AMO
    IOEmpty     // no R, W, or X
} PMA deriving (Bits, Eq, FShow);

function Bool isCacheable(PMA pma);
    return (case (pma)
                MainMemory, IORom: True;
                default: False;
            endcase);
endfunction

Bit#(2) prvU = 0;
Bit#(2) prvS = 1;
Bit#(2) prvH = 2;
Bit#(2) prvM = 3;

// Virtual Memory Types
Bit#(5) vmMbare = 0;
Bit#(5) vmMbb   = 1;
Bit#(5) vmMbbid = 2;
Bit#(5) vmSv32  = 8;
Bit#(5) vmSv39  = 9;
Bit#(5) vmSv48  = 10;
Bit#(5) vmSv57  = 11;
Bit#(5) vmSv64  = 12;

// uncached memory port
typedef struct {
    Bool            write;
    RVMemSize       size;
    PAddr           addr;
    Data            data;
} UncachedMemReq deriving (Bits, Eq, FShow);
typedef struct {
    Bool            write;
    Data            data;
} UncachedMemResp deriving (Bits, Eq, FShow);

typedef struct {
    Bit#(16) reserved;
    Bit#(20) ppn2;
    Bit#(9) ppn1;
    Bit#(9) ppn0;
    Bit#(2) reserved_sw;
    Bool d;
    Bool a;
    Bool g;
    Bool u;
    Bool x;
    Bool w;
    Bool r;
    Bool valid;
} PTE_Sv39 deriving (Eq, FShow); // Has custom Bits implementation
instance Bits#(PTE_Sv39, 64);
    function Bit#(64) pack(PTE_Sv39 x);
        return {x.reserved, x.ppn2, x.ppn1, x.ppn0, x.reserved_sw, pack(x.d), pack(x.a), pack(x.g), pack(x.u), pack(x.x), pack(x.w), pack(x.r), pack(x.valid)};
    endfunction
    function PTE_Sv39 unpack(Bit#(64) x);
        return (PTE_Sv39 {
                reserved:     x[63:48],
                ppn2:         x[47:28],
                ppn1:         x[27:19],
                ppn0:         x[18:10],
                reserved_sw:  x[9:8],
                d:            unpack(x[7]),
                a:            unpack(x[6]),
                g:            unpack(x[5]),
                u:            unpack(x[4]),
                x:            unpack(x[3]),
                w:            unpack(x[2]),
                r:            unpack(x[1]),
                valid:        unpack(x[0])
            });
    endfunction
endinstance
function Bool isLegalPTE(PTE_Sv39 pte);
    return pte.valid && !(pte.w && !(pte.r));
endfunction
function Bool isLeafPTE(PTE_Sv39 pte);
    return pte.valid && (pte.r || pte.w || pte.x);
endfunction
endpackage
