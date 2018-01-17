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

`include "defined_parameters.bsv"
import RVTypes::*;
import ConcatReg::*;
import ConfigReg::*;
import DefaultValue::*;
import RegUtil::*;
import Vector::*;
import riscv_types:: *;

typedef union tagged {
    struct {
        TrapCause exception;
        Addr      trapHandlerPC;
    } Exception;     // exception/interrupt redirection
    Addr RedirectPC; // non-trap redirection (xRET)
    Data CsrData;    // CSR read operations
    void None;       // all other operations
} CsrReturn deriving (Bits, Eq, FShow);

interface Ifc_CsrFile;
    // Read and Write ports
    // method Data rd(CSR csr);
    // Outputs for CSRs that the rest of the processor needs to know about
    method VMInfo vmI;
    method VMInfo vmD;
    method CsrState csrState; // prv, frm, f_enabled, x_enabled
    method Maybe#(InterruptCause) readyInterrupt; // TODO: fix this data type
    method ActionValue#(CsrReturn)
        wr( Addr pc,
            Maybe#(SystemInst) sysInst,
            CSR csr,
            Data data, // zimm or rval
            Addr addr, // badaddr
            Maybe#(TrapCause) trap, // exception or interrupt
            // indirect updates
            Bit#(5) fflags,
            Bool fpuDirty,
            Bool xDirty);

endinterface

module mkCsrFile#(
            Data hartid,                      // Compile-time constant
            Bit#(64) time_counter, Bool mtip, // From RTC
            Bool msip,                        // From IPI
            Bool meip                         // From interrupt controller
        )(Ifc_CsrFile);

    let verbose = False;
    File fout = stdout;

    RiscVISASubset isa = defaultValue;
    Bool is32bit       = valueOf(XLEN) == 32;
    Data mvendorid     = 0; // non-commercial
    Data marchid       = 0; // not implemented
    Data mimpid        = 0; // not implemented
    Addr default_mtvec = 'h0000_1000;
    Addr default_stvec = 'h0000_8000;

    // This is used to allow the readyInterruptSignal to be read after writing to the CSRF
    Wire#(Maybe#(InterruptCause)) readyInterruptWire <- mkDWire(tagged Invalid);

    Reg#(Bit#(2)) prv <- mkReg(prvM); // resets to machine mode

    // Counters
    Reg#(Bit#(64)) cycle_counter <- mkReg(0);
    Reg#(Bit#(64)) instret_counter <- mkReg(0);
    Reg#(Bit#(64)) event_counter <- mkReg(0);

    // Counter enables
    Reg#(Bit#(1)) u_ir_field <- mkReg(0);
    Reg#(Bit#(1)) u_tm_field <- mkReg(0);
    Reg#(Bit#(1)) u_cy_field <- mkReg(0);
`ifdef HYPERVISOR
    Reg#(Bit#(1)) h_ir_field <- mkReg(0);
    Reg#(Bit#(1)) h_tm_field <- mkReg(0);
    Reg#(Bit#(1)) h_cy_field <- mkReg(0);
`endif
`ifdef SUPERVISOR
    Reg#(Bit#(1)) s_ir_field <- mkReg(0);
    Reg#(Bit#(1)) s_tm_field <- mkReg(0);
    Reg#(Bit#(1)) s_cy_field <- mkReg(0);
`endif

    // FPU Fields
    Reg#(Bit#(5)) fflags_field  <- mkReg(0);
    Reg#(Bit#(3)) frm_field     <- mkReg(0);

    // vm fields
`ifdef CONFIG_RV64
    // XLEN = 64
    Reg#(Bit#(26)) asid_field      <- mkReg(0);
    Reg#(Bit#(38)) sptbr_ppn_field <- mkReg(0);
`else
    // XLEN = 32
    Reg#(Bit#(10)) asid_field      <- mkReg(0);
    Reg#(Bit#(22)) sptbr_ppn_field <- mkReg(0);
`endif

    // trap delegation fields
`ifdef SUPERVISOR
    Reg#(Bit#(12)) sedeleg_field <- mkReg(0);
    Reg#(Bit#(12)) sideleg_field <- mkReg(0);
`endif
`ifdef HYPERVISOR
    Reg#(Bit#(12)) hedeleg_field <- mkReg(0);
    Reg#(Bit#(12)) hideleg_field <- mkReg(0);
`endif
    Reg#(Bit#(12)) medeleg_field <- mkReg(0);
    Reg#(Bit#(12)) mideleg_field <- mkReg(0);

    // trap vector fields (same as CSR without bottom 2 bits)
    Reg#(Bit#(TSub#(XLEN,2))) mtvec_field <- mkReg(truncateLSB(default_mtvec));
    Reg#(Bit#(TSub#(XLEN,2))) stvec_field <- mkReg(truncateLSB(default_stvec));
`ifdef HYPERVISOR
    Reg#(Bit#(TSub#(XLEN,2))) htvec_field <- mkReg(truncateLSB(default_stvec));
`endif

    // mstatus fields
    Reg#(Bit#(5)) vm_field   <- mkReg(0); // WARL
    Reg#(Bit#(1)) mxr_field  <- mkReg(0);
    Reg#(Bit#(1)) pum_field  <- mkReg(0);
    Reg#(Bit#(1)) mprv_field <- mkReg(0);
    Reg#(Bit#(2)) xs_field   <- mkReg(0);
    Reg#(Bit#(2)) fs_field   <- mkReg(0);
    Reg#(Bit#(2)) mpp_field  <- mkReg(0);
`ifdef HYPERVISOR
    Reg#(Bit#(2)) hpp_field  <- mkReg(0);
    Reg#(Bit#(1)) hpie_field <- mkReg(0);
    Reg#(Bit#(1)) hie_field  <- mkReg(0);
`elsif
    Reg#(Bit#(2)) hpp_field  =  readOnlyReg(0);
    Reg#(Bit#(1)) hpie_field =  readOnlyReg(0);
    Reg#(Bit#(1)) hie_field  =  readOnlyReg(0);
`endif
`ifdef SUPERVISOR
    Reg#(Bit#(1)) spp_field  <- mkReg(0);
    Reg#(Bit#(1)) spie_field <- mkReg(0);
    Reg#(Bit#(1)) sie_field  <- mkReg(0);
`elsif
    Reg#(Bit#(2)) spp_field  =  readOnlyReg(0);
    Reg#(Bit#(1)) spie_field =  readOnlyReg(0);
    Reg#(Bit#(1)) sie_field  =  readOnlyReg(0);
`endif
    Reg#(Bit#(1)) mpie_field <- mkReg(0);
    Reg#(Bit#(1)) upie_field <- mkReg(0);
    Reg#(Bit#(1)) mie_field  <- mkReg(0);
    Reg#(Bit#(1)) uie_field  <- mkReg(0);
    Reg#(Bit#(1)) sd_field   =  readOnlyReg(pack((xs_field == 2'b11) || (fs_field == 2'b11)));

    // mie fields
    Reg#(Bit#(1)) meie_field <- mkReg(0);
    Reg#(Bit#(1)) msie_field <- mkReg(0);
    Reg#(Bit#(1)) mtie_field <- mkReg(0);
`ifdef HYPERVISOR
    Reg#(Bit#(1)) heie_field <-  mkReg(0);
    Reg#(Bit#(1)) hsie_field <-  mkReg(0);
    Reg#(Bit#(1)) htie_field <-  mkReg(0);
`elsif
    Reg#(Bit#(1)) heie_field =  readOnlyReg(0);
    Reg#(Bit#(1)) hsie_field =  readOnlyReg(0);
    Reg#(Bit#(1)) htie_field =  readOnlyReg(0);
`endif
`ifdef SUPERVISOR
    Reg#(Bit#(1)) stie_field <- mkReg(0);
    Reg#(Bit#(1)) ssie_field <- mkReg(0);
    Reg#(Bit#(1)) seie_field <- mkReg(0);
`elsif
    Reg#(Bit#(1)) stie_field = readOnlyReg(0);
    Reg#(Bit#(1)) ssie_field = readOnlyReg(0);
    Reg#(Bit#(1)) seie_field = readOnlyReg(0);
`endif
    Reg#(Bit#(1)) utie_field <- mkReg(0);
    Reg#(Bit#(1)) usie_field <- mkReg(0);
    Reg#(Bit#(1)) ueie_field <- mkReg(0);

    // mip fields
    Reg#(Bit#(1)) meip_field =  readOnlyReg(pack(meip));
    Reg#(Bit#(1)) mtip_field =  readOnlyReg(pack(mtip));
    Reg#(Bit#(1)) msip_field =  readOnlyReg(pack(msip));
`ifdef HYPERVISOR
    Reg#(Bit#(1)) heip_field <-  mkReg(0);
    Reg#(Bit#(1)) htip_field <-  mkReg(0);
    Reg#(Bit#(1)) hsip_field <-  mkReg(0);
`elsif
    Reg#(Bit#(1)) heip_field =  readOnlyReg(0);
    Reg#(Bit#(1)) htip_field =  readOnlyReg(0);
    Reg#(Bit#(1)) hsip_field =  readOnlyReg(0);
`endif
`ifdef SUPERVISOR
    Reg#(Bit#(1)) seip_field <- mkReg(0);
    Reg#(Bit#(1)) stip_field <- mkReg(0);
    Reg#(Bit#(1)) ssip_field <- mkReg(0);
`elsif
    Reg#(Bit#(1)) seip_field <- readOnlyReg(0);
    Reg#(Bit#(1)) stip_field <- readOnlyReg(0);
    Reg#(Bit#(1)) ssip_field <- readOnlyReg(0);
`endif
    Reg#(Bit#(1)) ueip_field <- mkReg(0);
    Reg#(Bit#(1)) utip_field <- mkReg(0);
    Reg#(Bit#(1)) usip_field <- mkReg(0);

    // Priv 1.9 CSRs

    // Machine Timers and Counters
    Reg#(Data) mcycle_csr   = readOnlyReg(truncate(cycle_counter));
    Reg#(Data) mtime_csr    = readOnlyReg(truncate(time_counter));
    Reg#(Data) minstret_csr = readOnlyReg(truncate(instret_counter));

    // Upper 32-bits of CSRs for RV32: (these will just be unused copies of
    // the other cycle/time/instret registers for RV64)
    Reg#(Data) mcycleh_csr   = readOnlyReg(truncateLSB(cycle_counter));
    Reg#(Data) mtimeh_csr    = readOnlyReg(truncateLSB(time_counter));
    Reg#(Data) minstreth_csr = readOnlyReg(truncateLSB(instret_counter));

    // User FPU
    Reg#(Data) fflags_csr = addWriteSideEffect(zeroExtendReg(fflags_field), fs_field._write(2'b11));
    Reg#(Data) frm_csr    = addWriteSideEffect(zeroExtendReg(frm_field), fs_field._write(2'b11));
    Reg#(Data) fcsr_csr   = addWriteSideEffect(zeroExtendReg(concatReg2(frm_field, fflags_field)), fs_field._write(2'b11));

    // User Timers and Counters
    Reg#(Data) cycle_csr   = readOnlyReg(truncate(cycle_counter));
    Reg#(Data) time_csr    = readOnlyReg(truncate(time_counter));
    Reg#(Data) instret_csr = readOnlyReg(truncate(instret_counter));

    // Upper 32-bits of CSRs for RV32: (these will just be unused copies of
    // the other cycle/time/instret registers for RV64)
    Reg#(Data) cycleh_csr   = readOnlyReg(truncateLSB(cycle_counter));
    Reg#(Data) timeh_csr    = readOnlyReg(truncateLSB(time_counter));
    Reg#(Data) instreth_csr = readOnlyReg(truncateLSB(instret_counter));

    // Supervisor
    Reg#(Data) sstatus_csr =  concatReg20(
            sd_field,
            readOnlyReg(0), // flexible width to support XLEN = 32 or 64
            vm_field,
            readOnlyReg(4'b0),
            readOnlyReg(1'b0), pum_field, readOnlyReg(1'b0), // memory privilege
            xs_field, fs_field, // coprocessor states
            readOnlyReg(2'b0), readOnlyReg(2'b0), spp_field, // previous privileges
            readOnlyReg(1'b0), readOnlyReg(1'b0), spie_field, upie_field, // previous interrupt enables
            readOnlyReg(1'b0), readOnlyReg(1'b0), sie_field, uie_field); // interrupt enables
    Reg#(Data) sedeleg_csr  =  concatReg2(readOnlyReg(0), sedeleg_field); // WARL - 12 legal exceptions
    Reg#(Data) sideleg_csr  =  concatReg2(readOnlyReg(0), sideleg_field); // WARL - 12 legal interrupts
    Reg#(Data) sie_csr      =  concatReg13(
            readOnlyReg(0),
            readOnlyReg(1'b0), readOnlyReg(1'b0), seie_field, ueie_field,
            readOnlyReg(1'b0), readOnlyReg(1'b0), stie_field, utie_field,
            readOnlyReg(1'b0), readOnlyReg(1'b0), ssie_field, usie_field);
    Reg#(Data) stvec_csr    =  concatReg2(stvec_field, readOnlyReg(2'd0));
    Reg#(Data) sscratch_csr <- mkReg(0);
    Reg#(Data) sepc_csr     <- mkReg(0);
    Reg#(Data) scause_csr   <- mkReg(0);
    Reg#(Data) sbadaddr_csr <- mkReg(0);
    Reg#(Data) sip_csr      =  concatReg13(
            readOnlyReg(0),
            readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(seip_field), readOnlyReg(ueip_field),
            readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(stip_field), readOnlyReg(utip_field),
            readOnlyReg(1'b0), readOnlyReg(1'b0), ssip_field, usip_field);

    Reg#(Data) sptbr_csr    = concatReg2(asid_field, sptbr_ppn_field);

		// Hypervisor
    Reg#(Data) hstatus_csr =  concatReg20(
            sd_field,
            readOnlyReg(0), // flexible width to support XLEN = 32 or 64
            vm_field,
            readOnlyReg(4'b0),
            readOnlyReg(1'b0), pum_field, readOnlyReg(1'b0), // memory privilege
            xs_field, fs_field, // coprocessor states
            readOnlyReg(2'b0), hpp_field, spp_field,// previous privileges
            readOnlyReg(1'b0), hpie_field, spie_field, upie_field, // previous interrupt enables
            readOnlyReg(1'b0), hie_field, sie_field, uie_field); // interrupt enables
    Reg#(Data) hedeleg_csr  =  concatReg2(readOnlyReg(0), hedeleg_field); // WARL - 12 legal exceptions
    Reg#(Data) hideleg_csr  =  concatReg2(readOnlyReg(0), hideleg_field); // WARL - 12 legal interrupts
    Reg#(Data) hie_csr      =  concatReg13(
            readOnlyReg(0),
            readOnlyReg(1'b0), heie_field, seie_field, ueie_field,
            readOnlyReg(1'b0), htie_field, stie_field, utie_field,
            readOnlyReg(1'b0), hsie_field, ssie_field, usie_field);
    Reg#(Data) htvec_csr    =  concatReg2(htvec_field, readOnlyReg(2'd0));
    Reg#(Data) hscratch_csr <- mkReg(0);
    Reg#(Data) hepc_csr     <- mkReg(0);
    Reg#(Data) hcause_csr   <- mkReg(0);
    Reg#(Data) hbadaddr_csr <- mkReg(0);
    Reg#(Data) hip_csr      =  concatReg13(
            readOnlyReg(0),
            readOnlyReg(1'b0), readOnlyReg(heip_field),readOnlyReg(seip_field), readOnlyReg(ueip_field),
            readOnlyReg(1'b0), readOnlyReg(htip_field),readOnlyReg(stip_field), readOnlyReg(utip_field),
            readOnlyReg(1'b0), hsip_field, ssip_field, usip_field);


    // Machine Information Registers
    Reg#(Data) misa_csr      = readOnlyReg(getMISA(isa));
    Reg#(Data) mvendorid_csr = readOnlyReg(mvendorid);
    Reg#(Data) marchid_csr   = readOnlyReg(marchid);
    Reg#(Data) mimpid_csr    = readOnlyReg(mimpid);
    Reg#(Data) mhartid_csr   = readOnlyReg(hartid);

    // Machine Trap Setup
    Reg#(Data) mstatus_csr =  concatReg20(
            sd_field,
            readOnlyReg(0),
            vm_field,
            readOnlyReg(4'b0),
            mxr_field, pum_field, mprv_field, // memory privilege
            xs_field, fs_field, // coprocessor states
            mpp_field, hpp_field, spp_field, // previous privileges
            mpie_field, hpie_field, spie_field, upie_field, // previous interrupt enables
            mie_field, hie_field, sie_field, uie_field); // interrupt enables
    Reg#(Data) medeleg_csr =  concatReg2(readOnlyReg(0), medeleg_field); // WARL - 12 legal exceptions
    Reg#(Data) mideleg_csr =  concatReg2(readOnlyReg(0), mideleg_field); // WARL - 12 legal interrupts
    Reg#(Data) mie_csr     =  concatReg13(
            readOnlyReg(0),
            meie_field, heie_field, seie_field, ueie_field,
            mtie_field, htie_field, stie_field, utie_field,
            msie_field, hsie_field, ssie_field, usie_field);
    Reg#(Data) mtvec_csr   =  concatReg2(mtvec_field, readOnlyReg(2'd0));

    // Machine Trap Handling
    Reg#(Data) mscratch_csr <- mkReg(0);
    Reg#(Data) mepc_csr     <- mkReg(0);
    Reg#(Data) mcause_csr   <- mkReg(0);
    Reg#(Data) mbadaddr_csr <- mkReg(0);
    Reg#(Data) mip_csr      =  concatReg13(
            readOnlyReg(0),
            readOnlyReg(meip_field), readOnlyReg(heip_field), readOnlyReg(seip_field), readOnlyReg(ueip_field),
            readOnlyReg(mtip_field), htip_field, stip_field, utip_field,
            readOnlyReg(msip_field), hsip_field, ssip_field, usip_field);

    // Machine Protection and Translation
    Reg#(Data) mbase_csr   <- mkReg(0);
    Reg#(Data) mbound_csr  <- mkReg(0);
    Reg#(Data) mibase_csr  <- mkReg(0);
    Reg#(Data) mibound_csr <- mkReg(0);
    Reg#(Data) mdbase_csr  <- mkReg(0);
    Reg#(Data) mdbound_csr <- mkReg(0);

		Integer hpm_offset = 3;

		Reg#(Data) hpmcounter_csr[29];
		for(Integer i =0; i <=28; i=i+1)
			hpmcounter_csr[i] <- mkReg(0);

		Reg#(Data) hpmcounterh_csr[29];
		for(Integer i =0; i <=28; i=i+1)
			hpmcounterh_csr[i] <- mkReg(0);

		Reg#(Data) mhpmcounter_csr[29];
		for(Integer i =0; i <=28; i=i+1)
			mhpmcounter_csr[i] <- mkReg(0);

		Reg#(Data) mhpmcounterh_csr[29];
		for(Integer i =0; i <=28; i=i+1)
			mhpmcounterh_csr[i] <- mkReg(0);

		Reg#(Data) mhpmevent_csr[29];
		for(Integer i =0; i <=28; i=i+1)
			mhpmevent_csr[i] <- mkReg(0);

		Reg#(Bit#(29)) mucounteren_field <- mkReg(0);
`ifdef HYPERVISOR
		Reg#(Bit#(29)) mhcounteren_field <- mkReg(0);
		Reg#(Bit#(29)) mscounteren_field <- mkReg(0);
`elsif SUPERVISOR
		Reg#(Bit#(29)) mscounteren_field <- mkReg(0);
`endif

		Reg#(Data) mucounteren_csr = concatReg5(readOnlyReg(0), 
																				u_ir_field, u_tm_field, u_cy_field, mucounteren_field); 
`ifdef HYPERVISOR
		Reg#(Data) mhcounteren_csr = concatReg5(readOnlyReg(0), h_ir_field, h_tm_field,
																														 h_cy_field, mhcounteren_field); 
		Reg#(Data) mscounteren_csr = concatReg5(readOnlyReg(0), s_ir_field, s_tm_field,
																														 s_cy_field, mscounteren_field); 
`elsif SUPERVISOR
		Reg#(Data) mscounteren_csr = concatReg5(readOnlyReg(0), s_ir_field, s_tm_field,
																														 s_cy_field, mscounteren_field); 
`endif

    function Reg#(Data) getCSR(CSR csr);
        return (case (csr)
                CSRfflags:              fflags_csr;
                CSRfrm:                 frm_csr;
                CSRfcsr:                fcsr_csr;
                CSRcycle:               cycle_csr;
								CSRtime:                time_csr;
                CSRinstret:             instret_csr;
                CSRhpmcounter3:					hpmcounter_csr[0];
								CSRhpmcounter4:					hpmcounter_csr[1];
								CSRhpmcounter5:					hpmcounter_csr[2];
								CSRhpmcounter6:					hpmcounter_csr[3];
								CSRhpmcounter7:					hpmcounter_csr[4];
								CSRhpmcounter8:					hpmcounter_csr[5];
								CSRhpmcounter9:					hpmcounter_csr[6];
								CSRhpmcounter10:				hpmcounter_csr[7];
								CSRhpmcounter11:				hpmcounter_csr[8];
								CSRhpmcounter12:				hpmcounter_csr[9];
								CSRhpmcounter13:				hpmcounter_csr[10];
								CSRhpmcounter14:				hpmcounter_csr[11];
								CSRhpmcounter15:				hpmcounter_csr[12];
								CSRhpmcounter16:				hpmcounter_csr[13];
								CSRhpmcounter17:				hpmcounter_csr[14];
								CSRhpmcounter18:				hpmcounter_csr[15];
								CSRhpmcounter19:				hpmcounter_csr[16];
								CSRhpmcounter20:				hpmcounter_csr[17];
								CSRhpmcounter21:				hpmcounter_csr[18];
								CSRhpmcounter22:				hpmcounter_csr[19];
								CSRhpmcounter23:				hpmcounter_csr[20];
								CSRhpmcounter24:				hpmcounter_csr[21];
								CSRhpmcounter25:				hpmcounter_csr[22];
								CSRhpmcounter26:				hpmcounter_csr[23];
								CSRhpmcounter27:				hpmcounter_csr[24];
								CSRhpmcounter28:				hpmcounter_csr[25];
								CSRhpmcounter29:				hpmcounter_csr[26];
								CSRhpmcounter30:				hpmcounter_csr[27];
								CSRhpmcounter31:				hpmcounter_csr[28];
                CSRinstreth:            instreth_csr;
                CSRcycleh:              cycleh_csr;
                CSRtimeh:               timeh_csr;
								CSRhpmcounterh3:				hpmcounterh_csr[0];
								CSRhpmcounterh4:				hpmcounterh_csr[1];
								CSRhpmcounterh5:				hpmcounterh_csr[2];
								CSRhpmcounterh6:				hpmcounterh_csr[3];
								CSRhpmcounterh7:				hpmcounterh_csr[4];
								CSRhpmcounterh8:				hpmcounterh_csr[5];
								CSRhpmcounterh9:				hpmcounterh_csr[6];
								CSRhpmcounterh10:				hpmcounterh_csr[7];
								CSRhpmcounterh11:				hpmcounterh_csr[8];
								CSRhpmcounterh12:				hpmcounterh_csr[9];
								CSRhpmcounterh13:				hpmcounterh_csr[10];
								CSRhpmcounterh14:				hpmcounterh_csr[11];
								CSRhpmcounterh15:				hpmcounterh_csr[12];
								CSRhpmcounterh16:				hpmcounterh_csr[13];
								CSRhpmcounterh17:				hpmcounterh_csr[14];
								CSRhpmcounterh18:				hpmcounterh_csr[15];
								CSRhpmcounterh19:				hpmcounterh_csr[16];
								CSRhpmcounterh20:				hpmcounterh_csr[17];
								CSRhpmcounterh21:				hpmcounterh_csr[18];
								CSRhpmcounterh22:				hpmcounterh_csr[19];
								CSRhpmcounterh23:				hpmcounterh_csr[20];
								CSRhpmcounterh24:				hpmcounterh_csr[21];
								CSRhpmcounterh25:				hpmcounterh_csr[22];
								CSRhpmcounterh26:				hpmcounterh_csr[23];
								CSRhpmcounterh27:				hpmcounterh_csr[24];
								CSRhpmcounterh28:				hpmcounterh_csr[25];
								CSRhpmcounterh29:				hpmcounterh_csr[26];
								CSRhpmcounterh30:				hpmcounterh_csr[27];
								CSRhpmcounterh31:				hpmcounterh_csr[28];
                CSRsstatus:             sstatus_csr;
                CSRsedeleg:             sedeleg_csr;
                CSRsideleg:             sideleg_csr;
                CSRsie:                 sie_csr;
                CSRstvec:               stvec_csr;
                CSRsscratch:            sscratch_csr;
                CSRsepc:                sepc_csr;
                CSRscause:              scause_csr;
                CSRsbadaddr:            sbadaddr_csr;
                CSRsip:                 sip_csr;
                CSRsptbr:               sptbr_csr;
								CSRhstatus:             hstatus_csr;
                CSRhedeleg:             hedeleg_csr;
                CSRhideleg:             hideleg_csr;
                CSRhie:                 hie_csr;
                CSRhtvec:               htvec_csr;
                CSRhscratch:            hscratch_csr;
                CSRhepc:                hepc_csr;
                CSRhcause:              hcause_csr;
                CSRhbadaddr:            hbadaddr_csr;
                CSRhip:                 hip_csr;
                CSRmisa:                misa_csr;
                CSRmvendorid:           mvendorid_csr;
                CSRmarchid:             marchid_csr;
                CSRmimpid:              mimpid_csr;
                CSRmhartid:             mhartid_csr;
                CSRmstatus:             mstatus_csr;
                CSRmedeleg:             medeleg_csr;
                CSRmideleg:             mideleg_csr;
                CSRmie:                 mie_csr;
                CSRmtvec:               mtvec_csr;
                CSRmscratch:            mscratch_csr;
                CSRmepc:                mepc_csr;
                CSRmcause:              mcause_csr;
                CSRmbadaddr:            mbadaddr_csr;
                CSRmip:                 mip_csr;
                CSRmbase:               mbase_csr;
                CSRmbound:              mbound_csr;
                CSRmibase:              mibase_csr;
                CSRmibound:             mibound_csr;
                CSRmdbase:              mdbase_csr;
                CSRmdbound:             mdbound_csr;
                CSRmcycle:              mcycle_csr;
                CSRminstret:            minstret_csr;
								CSRmhpmcounter3:				mhpmcounter_csr[0];	
								CSRmhpmcounter4:				mhpmcounter_csr[1];	
								CSRmhpmcounter5:				mhpmcounter_csr[2];	
								CSRmhpmcounter6:				mhpmcounter_csr[3];	
								CSRmhpmcounter7:				mhpmcounter_csr[4];	
								CSRmhpmcounter8:				mhpmcounter_csr[5];	
								CSRmhpmcounter9:				mhpmcounter_csr[6];	
								CSRmhpmcounter10:				mhpmcounter_csr[7];	
								CSRmhpmcounter11:				mhpmcounter_csr[8];	
								CSRmhpmcounter12:				mhpmcounter_csr[9];	
								CSRmhpmcounter13:				mhpmcounter_csr[10];	
								CSRmhpmcounter14:				mhpmcounter_csr[11];	
								CSRmhpmcounter15:				mhpmcounter_csr[12];	
								CSRmhpmcounter16:				mhpmcounter_csr[13];	
								CSRmhpmcounter17:				mhpmcounter_csr[14];	
								CSRmhpmcounter18:				mhpmcounter_csr[15];	
								CSRmhpmcounter19:				mhpmcounter_csr[16];	
								CSRmhpmcounter20:				mhpmcounter_csr[17];	
								CSRmhpmcounter21:				mhpmcounter_csr[18];	
								CSRmhpmcounter22:				mhpmcounter_csr[19];	
								CSRmhpmcounter23:				mhpmcounter_csr[20];	
								CSRmhpmcounter24:				mhpmcounter_csr[21];	
								CSRmhpmcounter25:				mhpmcounter_csr[22];	
								CSRmhpmcounter26:				mhpmcounter_csr[23];	
								CSRmhpmcounter27:				mhpmcounter_csr[24];	
								CSRmhpmcounter28:				mhpmcounter_csr[25];	
								CSRmhpmcounter29:				mhpmcounter_csr[26];	
								CSRmhpmcounter30:				mhpmcounter_csr[27];	
								CSRmhpmcounter31:				mhpmcounter_csr[28];	
    						CSRmcycleh:        			mcycleh_csr;  
    						CSRminstreth:      			minstreth_csr;  
								CSRmhpmcounterh3:				mhpmcounterh_csr[0];	
								CSRmhpmcounterh4:				mhpmcounterh_csr[1];	
								CSRmhpmcounterh5:				mhpmcounterh_csr[2];	
								CSRmhpmcounterh6:				mhpmcounterh_csr[3];	
								CSRmhpmcounterh7:				mhpmcounterh_csr[4];	
								CSRmhpmcounterh8:				mhpmcounterh_csr[5];	
								CSRmhpmcounterh9:				mhpmcounterh_csr[6];	
								CSRmhpmcounterh10:			mhpmcounterh_csr[7];	
								CSRmhpmcounterh11:			mhpmcounterh_csr[8];	
								CSRmhpmcounterh12:			mhpmcounterh_csr[9];	
								CSRmhpmcounterh13:			mhpmcounterh_csr[10];	
								CSRmhpmcounterh14:			mhpmcounterh_csr[11];	
								CSRmhpmcounterh15:			mhpmcounterh_csr[12];	
								CSRmhpmcounterh16:			mhpmcounterh_csr[13];	
								CSRmhpmcounterh17:			mhpmcounterh_csr[14];	
								CSRmhpmcounterh18:			mhpmcounterh_csr[15];	
								CSRmhpmcounterh19:			mhpmcounterh_csr[16];	
								CSRmhpmcounterh20:			mhpmcounterh_csr[17];	
								CSRmhpmcounterh21:			mhpmcounterh_csr[18];	
								CSRmhpmcounterh22:			mhpmcounterh_csr[19];	
								CSRmhpmcounterh23:			mhpmcounterh_csr[20];	
								CSRmhpmcounterh24:			mhpmcounterh_csr[21];	
								CSRmhpmcounterh25:			mhpmcounterh_csr[22];	
								CSRmhpmcounterh26:			mhpmcounterh_csr[23];	
								CSRmhpmcounterh27:			mhpmcounterh_csr[24];	
								CSRmhpmcounterh28:			mhpmcounterh_csr[25];	
								CSRmhpmcounterh29:			mhpmcounterh_csr[26];	
								CSRmhpmcounterh30:			mhpmcounterh_csr[27];	
								CSRmhpmcounterh31:			mhpmcounterh_csr[28];	
    						CSRmucounteren:					mucounteren_csr; 
    						CSRmscounteren:      		mscounteren_csr; 
    						CSRmhcounteren:      		mscounteren_csr; 
								CSRmhpmevent3:					mhpmevent_csr[0]; 		
								CSRmhpmevent4:				  mhpmevent_csr[1]; 
								CSRmhpmevent5:				  mhpmevent_csr[2]; 
								CSRmhpmevent6:				  mhpmevent_csr[3]; 
								CSRmhpmevent7:				  mhpmevent_csr[4]; 
								CSRmhpmevent8:				  mhpmevent_csr[5]; 
								CSRmhpmevent9:				  mhpmevent_csr[6]; 
								CSRmhpmevent10:			    mhpmevent_csr[7]; 
								CSRmhpmevent11:			    mhpmevent_csr[8]; 
								CSRmhpmevent12:			    mhpmevent_csr[9]; 
								CSRmhpmevent13:			    mhpmevent_csr[10];
								CSRmhpmevent14:			    mhpmevent_csr[11];
								CSRmhpmevent15:			    mhpmevent_csr[12];
								CSRmhpmevent16:			    mhpmevent_csr[13];
								CSRmhpmevent17:			    mhpmevent_csr[14];
								CSRmhpmevent18:			    mhpmevent_csr[15];
								CSRmhpmevent19:			    mhpmevent_csr[16];
								CSRmhpmevent20:			    mhpmevent_csr[17];
								CSRmhpmevent21:			    mhpmevent_csr[18];
								CSRmhpmevent22:			    mhpmevent_csr[19];
								CSRmhpmevent23:			    mhpmevent_csr[20];
								CSRmhpmevent24:			    mhpmevent_csr[21];
								CSRmhpmevent25:			    mhpmevent_csr[22];
								CSRmhpmevent26:			    mhpmevent_csr[23];
								CSRmhpmevent27:			    mhpmevent_csr[24];
								CSRmhpmevent28:			    mhpmevent_csr[25];
								CSRmhpmevent29:			    mhpmevent_csr[26];
								CSRmhpmevent30:			    mhpmevent_csr[27];
								CSRmhpmevent31:			    mhpmevent_csr[28];
                default:                (readOnlyReg(0));
            endcase);
    endfunction

    function Bool isLegalCSR(CSR csr);
        return (case (csr)
                CSRfflags:              (fs_field != 0);
                CSRfrm:                 (fs_field != 0);
                CSRfcsr:                (fs_field != 0);
                CSRcycle:               (u_cy_field == 1);
                CSRtime:                (u_tm_field == 1);
                CSRinstret:             (u_ir_field == 1);
								CSRhpmcounter3:					(((mucounteren_csr[0+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[0+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[0+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter4:					(((mucounteren_csr[1+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[1+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[1+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter5:					(((mucounteren_csr[2+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[2+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[2+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter6:					(((mucounteren_csr[3+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[3+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[3+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter7:					(((mucounteren_csr[4+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[4+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[4+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter8:					(((mucounteren_csr[5+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[5+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[5+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter9:					(((mucounteren_csr[6+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[6+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[6+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter10:				(((mucounteren_csr[7+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[7+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[7+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter11:				(((mucounteren_csr[8+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[8+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[8+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter12:				(((mucounteren_csr[9+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[9+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[9+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter13:				(((mucounteren_csr[10+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[10+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[10+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter14:				(((mucounteren_csr[11+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[11+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[11+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter15:				(((mucounteren_csr[12+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[12+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[12+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter16:				(((mucounteren_csr[13+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[13+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[13+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter17:				(((mucounteren_csr[14+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[14+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[14+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter18:				(((mucounteren_csr[15+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[15+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[15+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter19:				(((mucounteren_csr[16+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[16+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[16+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter20:				(((mucounteren_csr[17+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[17+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[17+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter21:				(((mucounteren_csr[18+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[18+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[18+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter22:				(((mucounteren_csr[19+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[19+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[19+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter23:				(((mucounteren_csr[20+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[20+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[20+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter24:				(((mucounteren_csr[21+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[21+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[21+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter25:				(((mucounteren_csr[22+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[22+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[22+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter26:				(((mucounteren_csr[23+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[23+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[23+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter27:				(((mucounteren_csr[24+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[24+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[24+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter28:				(((mucounteren_csr[25+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[25+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[25+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter29:				(((mucounteren_csr[26+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[26+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[26+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter30:				(((mucounteren_csr[27+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[27+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[27+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounter31:				(((mucounteren_csr[28+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[28+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[28+hpm_offset] == 1) && (prv == prvH)));
                CSRcycleh:              ((u_cy_field == 1) && is32bit);
                CSRtimeh:               ((u_tm_field == 1) && is32bit);
                CSRinstreth:            ((u_ir_field == 1) && is32bit);
								CSRhpmcounterh3:				(((mucounteren_csr[0+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[0+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[0+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh4:				(((mucounteren_csr[1+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[1+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[1+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh5:				(((mucounteren_csr[2+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[2+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[2+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh6:				(((mucounteren_csr[3+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[3+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[3+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh7:				(((mucounteren_csr[4+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[4+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[4+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh8:				(((mucounteren_csr[5+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[5+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[5+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh9:				(((mucounteren_csr[6+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[6+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[6+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh10:				(((mucounteren_csr[7+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[7+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[7+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh11:				(((mucounteren_csr[8+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[8+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[8+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh12:				(((mucounteren_csr[9+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[9+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[9+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh13:				(((mucounteren_csr[10+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[10+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[10+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh14:				(((mucounteren_csr[11+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[11+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[11+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh15:				(((mucounteren_csr[12+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[12+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[12+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh16:				(((mucounteren_csr[13+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[13+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[13+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh17:				(((mucounteren_csr[14+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[14+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[14+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh18:				(((mucounteren_csr[15+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[15+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[15+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh19:				(((mucounteren_csr[16+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[16+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[16+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh20:				(((mucounteren_csr[17+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[17+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[17+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh21:				(((mucounteren_csr[18+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[18+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[18+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh22:				(((mucounteren_csr[19+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[19+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[19+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh23:				(((mucounteren_csr[20+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[20+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[20+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh24:				(((mucounteren_csr[21+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[21+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[21+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh25:				(((mucounteren_csr[22+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[22+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[22+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh26:				(((mucounteren_csr[23+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[23+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[23+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh27:				(((mucounteren_csr[24+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[24+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[24+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh28:				(((mucounteren_csr[25+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[25+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[25+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh29:				(((mucounteren_csr[26+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[26+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[26+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh30:				(((mucounteren_csr[27+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[27+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[27+hpm_offset] == 1) && (prv == prvH)));
								CSRhpmcounterh31:				(((mucounteren_csr[28+hpm_offset] == 1) && (prv == prvU)) || ((mscounteren_csr[28+hpm_offset] == 1) && (prv == prvS)) || ((mhcounteren_csr[28+hpm_offset] == 1) && (prv == prvH)));
                CSRsstatus:             True;
                CSRsedeleg:             True;
                CSRsideleg:             True;
                CSRsie:                 True;
                CSRstvec:               True;
                CSRsscratch:            True;
                CSRsepc:                True;
                CSRscause:              True;
                CSRsbadaddr:            True;
                CSRsip:                 True;
                CSRsptbr:               True;
                CSRhstatus:             True;
                CSRhedeleg:             True;
                CSRhideleg:             True;
                CSRhie:                 True;
                CSRhtvec:               True;
                CSRhscratch:            True;
                CSRhepc:                True;
                CSRhcause:              True;
                CSRhbadaddr:            True;
                CSRhip:                 True;
                CSRmisa:                True;
                CSRmvendorid:           True;
                CSRmarchid:             True;
                CSRmimpid:              True;
                CSRmhartid:             True;
                CSRmstatus:             True;
                CSRmedeleg:             True;
                CSRmideleg:             True;
                CSRmie:                 True;
                CSRmtvec:               True;
                CSRmscratch:            True;
                CSRmepc:                True;
                CSRmcause:              True;
                CSRmbadaddr:            True;
                CSRmip:                 True;
                CSRmbase:               True;
                CSRmbound:              True;
                CSRmibase:              True;
                CSRmibound:             True;
                CSRmdbase:              True;
                CSRmdbound:             True;
                CSRmcycle:              True;
                CSRminstret:            True;
								CSRmhpmcounter3:			  True;		
								CSRmhpmcounter4:			  True;		
								CSRmhpmcounter5:			  True;		
								CSRmhpmcounter6:			  True;		
								CSRmhpmcounter7:			  True;		
								CSRmhpmcounter8:			  True;		
								CSRmhpmcounter9:			  True;		
								CSRmhpmcounter10:			  True;		
								CSRmhpmcounter11:			  True;		
								CSRmhpmcounter12:			  True;		
								CSRmhpmcounter13:			  True;	
								CSRmhpmcounter14:			  True;	
								CSRmhpmcounter15:			  True;	
								CSRmhpmcounter16:			  True;	
								CSRmhpmcounter17:			  True;	
								CSRmhpmcounter18:			  True;	
								CSRmhpmcounter19:			  True;	
								CSRmhpmcounter20:			  True;	
								CSRmhpmcounter21:			  True;	
								CSRmhpmcounter22:			  True;	
								CSRmhpmcounter23:			  True;	
								CSRmhpmcounter24:			  True;	
								CSRmhpmcounter25:			  True;	
								CSRmhpmcounter26:			  True;	
								CSRmhpmcounter27:			  True;	
								CSRmhpmcounter28:			  True;	
								CSRmhpmcounter29:			  True;	
								CSRmhpmcounter30:			  True;	
								CSRmhpmcounter31:			  True;	
                CSRmcycleh:             is32bit;
                CSRminstreth:           is32bit;
								CSRmhpmcounterh3:			  True;	
								CSRmhpmcounterh4:			  True;	
								CSRmhpmcounterh5:			  True;	
								CSRmhpmcounterh6:			  True;	
								CSRmhpmcounterh7:			  True;	
								CSRmhpmcounterh8:			  True;	
								CSRmhpmcounterh9:			  True;	
								CSRmhpmcounterh10:		  True;	
								CSRmhpmcounterh11:		  True;	
								CSRmhpmcounterh12:		  True;	
								CSRmhpmcounterh13:		  True;		
								CSRmhpmcounterh14:		  True;		
								CSRmhpmcounterh15:		  True;		
								CSRmhpmcounterh16:		  True;		
								CSRmhpmcounterh17:		  True;		
								CSRmhpmcounterh18:		  True;		
								CSRmhpmcounterh19:		  True;		
								CSRmhpmcounterh20:		  True;		
								CSRmhpmcounterh21:		  True;		
								CSRmhpmcounterh22:		  True;		
								CSRmhpmcounterh23:		  True;		
								CSRmhpmcounterh24:		  True;		
								CSRmhpmcounterh25:		  True;		
								CSRmhpmcounterh26:		  True;		
								CSRmhpmcounterh27:		  True;		
								CSRmhpmcounterh28:		  True;		
								CSRmhpmcounterh29:		  True;		
								CSRmhpmcounterh30:		  True;		
								CSRmhpmcounterh31:		  True;		
                CSRmucounteren:         True;
`ifdef SUPERVISOR
								CSRmscounteren:         True;
`endif
`ifdef HIPERVISOR
								CSRmhcounteren:					True;
`endif
								CSRmhpmevent3:				  True;	 	
								CSRmhpmevent4:				  True;  
								CSRmhpmevent5:				  True;  
								CSRmhpmevent6:				  True;  
								CSRmhpmevent7:				  True;  
								CSRmhpmevent8:				  True;  
								CSRmhpmevent9:				  True;  
								CSRmhpmevent10:			    True;  
								CSRmhpmevent11:			    True;  
								CSRmhpmevent12:			    True;  
								CSRmhpmevent13:			    True;  
								CSRmhpmevent14:			    True;  
								CSRmhpmevent15:			    True;  
								CSRmhpmevent16:			    True;  
								CSRmhpmevent17:			    True;  
								CSRmhpmevent18:			    True;  
								CSRmhpmevent19:			    True;  
								CSRmhpmevent20:			    True;  
								CSRmhpmevent21:			    True;  
								CSRmhpmevent22:			    True;  
								CSRmhpmevent23:			    True;  
								CSRmhpmevent24:			    True;  
								CSRmhpmevent25:			    True;  
								CSRmhpmevent26:			    True;  
								CSRmhpmevent27:			    True;  
								CSRmhpmevent28:			    True;  
								CSRmhpmevent29:			    True;  
								CSRmhpmevent30:			    True;  
								CSRmhpmevent31:			    True;  
								
                default:                False;
            endcase);
    endfunction

    function Maybe#(InterruptCause) readyInterruptFunc();
        Bit#(12) ready_interrupts = truncate(mip_csr) & truncate(mie_csr);
				Bit#(12) ready_hypervisor_interrupts = 12'h000;
				Bit#(12) ready_supervisor_interrupts = 12'h000;
				Bool hypervisor_interrupts_enabled = False;
				Bool supervisor_interrupts_enabled = False;
        // machine mode
        let ready_machine_interrupts = ready_interrupts & ~truncate(mideleg_csr);
        Bool machine_interrupts_enabled = (mie_field == 1) || (prv < prvM);
`ifdef HYPERVISOR
				//hypervisor mode
				ready_hypervisor_interrupts = ready_interrupts & truncate(mideleg_csr) & ~truncate(hideleg_csr);
        hypervisor_interrupts_enabled = ((hie_field == 1) && (prv == prvH)) || (prv < prvH);
				// supervisor mode
        ready_supervisor_interrupts = ready_interrupts & truncate(mideleg_csr) & truncate(hideleg_csr) & ~truncate(sideleg_csr);
        supervisor_interrupts_enabled = ((sie_field == 1) && (prv == prvS)) || (prv < prvS);
        // user mode
        let ready_user_interrupts = ready_interrupts & truncate(mideleg_csr) & truncate(hideleg_csr) & truncate(sideleg_csr);
        let user_interrupts_enabled = (uie_field == 1) && (prv == prvU);
`elsif SUPERVISOR
        // supervisor mode
        ready_supervisor_interrupts = ready_interrupts & truncate(mideleg_csr) & ~truncate(sideleg_csr);
        supervisor_interrupts_enabled = ((sie_field == 1) && (prv == prvS)) || (prv < prvS);
        // user mode
        let ready_user_interrupts = ready_interrupts & truncate(mideleg_csr) & truncate(sideleg_csr);
        let user_interrupts_enabled = (uie_field == 1) && (prv == prvU);
`else
        // user mode
        let ready_user_interrupts = ready_interrupts & truncate(mideleg_csr);
        let user_interrupts_enabled = (uie_field == 1) && (prv == prvU);
`endif
        // combined
        ready_interrupts = (machine_interrupts_enabled ? ready_machine_interrupts : 0)
                            | (supervisor_interrupts_enabled ? ready_supervisor_interrupts : 0)
                            | (hypervisor_interrupts_enabled ? ready_hypervisor_interrupts : 0)
                            | (user_interrupts_enabled ? ready_user_interrupts : 0);
        // format pendingInterrupt value to return
        Maybe#(InterruptCause) ret = tagged Invalid;
        if (ready_interrupts != 0) begin
            // pack/unpack type conversion:
            // UInt#(TLog#(TAdd#(12,1))) == UInt#(4) -> Bit#(4) -> InterruptCause
            ret = tagged Valid unpack(pack(countZerosLSB(ready_interrupts)));
        end
        return ret;
    endfunction

    // RULES
    ////////////////////////////////////////////////////////

    rule setReadyInterruptWire;
        readyInterruptWire <= readyInterruptFunc();
    endrule

    rule incrementCycle;
        cycle_counter <= cycle_counter + 1;
    endrule

    // METHODS
    ////////////////////////////////////////////////////////

    method VMInfo vmI;
        Bit#(5) vm = (prv == prvM) ? vmMbare : vm_field;
        Addr base = (case (vm)
                        vmMbare: 0;
                        vmMbb: mbase_csr;
                        vmMbbid: mibase_csr;
                        // all paged virtual memory modes
`ifdef CONFIG_RV64
                        default: {0, sptbr_ppn_field, 12'd0};
`else
                        default: truncate({sptbr_ppn_field, 12'd0}); // TODO: allow Addr to be Bit#(34) for rv32
`endif
                    endcase);
        Addr bound = (case (vm)
                        vmMbb: mbound_csr;
                        vmMbbid: mibound_csr;
                        default: -1;
                    endcase);
        return VMInfo{ prv: prv, asid: asid_field, vm: vm, mxr: unpack(mxr_field), pum: unpack(pum_field), base: base, bound: bound };
    endmethod

    method VMInfo vmD;
        Bit#(2) vm_prv = (mprv_field == 1) ? mpp_field : prv;
        Bit#(5) vm = (vm_prv == prvM) ? vmMbare : vm_field;
        Addr base = (case (vm)
                        vmMbare: 0;
                        vmMbb: mbase_csr;
                        vmMbbid: mdbase_csr;
                        // all paged virtual memory modes
`ifdef CONFIG_RV64
                        default: {0, sptbr_ppn_field, 12'd0};
`else
                        default: truncate({sptbr_ppn_field, 12'd0}); // TODO: allow Addr to be Bit#(34) for rv32
`endif
                    endcase);
        Addr bound = (case (vm)
                        vmMbb: mbound_csr;
                        vmMbbid: mdbound_csr;
                        default: -1;
                    endcase);
        return VMInfo{ prv: vm_prv, asid: asid_field, vm: vm, mxr: unpack(mxr_field), pum: unpack(pum_field), base: base, bound: bound };
    endmethod

    method CsrState csrState = CsrState {prv: prv, frm: frm_field, f_enabled: (fs_field != 0), x_enabled: (xs_field != 0)};

    method Maybe#(InterruptCause) readyInterrupt;
        return readyInterruptWire;
    endmethod

    method ActionValue#(CsrReturn) wr(
            Addr pc, // pc of current exception
            Maybe#(SystemInst) sysInst,
            CSR csr,
            Data data, // zimm or rval
            Addr addr, // badaddr
            Maybe#(TrapCause) trap, // exception or interrupt
            // indirect updates
            Bit#(5) fflags,
            Bool fpuDirty,
            Bool xDirty);

        Maybe#(TrapCause) trapToTake = trap;
        if (!isValid(trapToTake) &&& sysInst matches tagged Valid .validSysInst) begin
            case (validSysInst)
                ECall:  trapToTake = tagged Valid (case (prv)
                                                    prvU: (tagged Exception EnvCallU);
                                                    prvS: (tagged Exception EnvCallS);
                                                    prvH: (tagged Exception EnvCallH);
                                                    prvM: (tagged Exception EnvCallM);
                                                endcase);
                // URet and HRet are not supported
                URet: trapToTake = tagged Valid (tagged Exception IllegalInst);
                SRet:
                    begin
                        if (prv < prvS) begin
                            trapToTake = tagged Valid (tagged Exception IllegalInst);
                        end
                    end
                HRet: 
                    begin
                        if (prv < prvH) begin
                            trapToTake = tagged Valid (tagged Exception IllegalInst);
                        end
                    end
                MRet:
                    begin
                        if (prv < prvM) begin
                            trapToTake = tagged Valid (tagged Exception IllegalInst);
                        end
                    end
                EBreak: trapToTake = tagged Valid (tagged Exception Breakpoint);
                CSRRW, CSRRS, CSRRC, CSRR, CSRW:
                    begin
                        Bool read = (validSysInst != CSRW);
                        Bool write = (validSysInst != CSRR);
                        if (!isLegalCSR(csr) || !hasCSRPermission(csr, prv, write)) begin
                            trapToTake = tagged Valid (tagged Exception IllegalInst);
                        end
                    end
            endcase
        end

        // Three case from this point onward:
        // 1) Trap (exception or interrupt)
        // 2) Non-trapping system instruction
        //      increment instret
        // 3) Normal instruction
        //      increment instret
        //      update fs and fflags if necessary
        if (trapToTake matches tagged Valid .validTrap) begin
						Bool delegToH = False;
						Bool delegToS = False;
`ifdef HYPERVISOR
            // Traps
            // delegate to S if the prv <= S and the corresponding deleg bit is set
            delegToH = prv <= prvH && (case (validTrap) matches
                    tagged Exception .exceptionCause: (((medeleg_csr >> pack(exceptionCause)) & 1) != 0);
                    tagged Interrupt .interruptCause: (((mideleg_csr >> pack(interruptCause)) & 1) != 0);
                endcase);
						if(delegToH) begin
`endif			
`ifdef SUPERVISOR
							// Traps
            	// delegate to S if the prv <= S and the corresponding deleg bit is set
            	delegToS = prv <= prvS && (case (validTrap) matches
            	        tagged Exception .exceptionCause: (((medeleg_csr >> pack(exceptionCause)) & 1) != 0);
            	        tagged Interrupt .interruptCause: (((mideleg_csr >> pack(interruptCause)) & 1) != 0);
            	    endcase);
`endif
`ifdef HYPERVISOR
						end
`endif
						Addr newPC = mtvec_csr;
            if (delegToS) begin
								newPC = stvec_csr;
                // trap to prvS
                sepc_csr <= pc;
                scause_csr <= toCauseCSR(validTrap);
                case (validTrap)
                    tagged Exception InstAddrMisaligned,
                    tagged Exception InstAccessFault,
                    tagged Exception LoadAddrMisaligned,
                    tagged Exception LoadAccessFault,
                    tagged Exception StoreAddrMisaligned,
                    tagged Exception StoreAccessFault:
                        sbadaddr_csr <= addr;
                endcase
                // update mstatus fields
                spp_field <= (prv == prvU) ? 0 : 1;
                spie_field <= (prv == prvU) ? uie_field : sie_field;
                sie_field <= 0;
                // update privilege
                prv <= prvS;
						end
						else if(delegToH) begin
								newPC = htvec_csr;
                // trap to prvS
                hepc_csr <= pc;
                hcause_csr <= toCauseCSR(validTrap);
                case (validTrap)
                    tagged Exception InstAddrMisaligned,
                    tagged Exception InstAccessFault,
                    tagged Exception LoadAddrMisaligned,
                    tagged Exception LoadAccessFault,
                    tagged Exception StoreAddrMisaligned,
                    tagged Exception StoreAccessFault:
                        hbadaddr_csr <= addr;
                endcase
                // update mstatus fields
                hpp_field <= prv;
                hpie_field <= (case(prv) 
																prvU : uie_field; 
																prvS : sie_field;
																prvH : hie_field;
															endcase);
                hie_field <= 0;
                // update privilege
                prv <= prvH;
            end 
						else begin
                // trap to prvM
                mepc_csr <= pc;
                mcause_csr <= toCauseCSR(validTrap);
                case (validTrap)
                    tagged Exception InstAddrMisaligned,
                    tagged Exception InstAccessFault,
                    tagged Exception LoadAddrMisaligned,
                    tagged Exception LoadAccessFault,
                    tagged Exception StoreAddrMisaligned,
                    tagged Exception StoreAccessFault:
                        mbadaddr_csr <= addr;
                endcase
                // update mstatus fields
                mpp_field <= prv;
                mpie_field <= (case (prv)
                                prvU: uie_field;
                                prvS: sie_field;
                                prvH: hie_field;
                                prvM: mie_field;
                            endcase);
                mie_field <= 0;
                // update privilege
                prv <= prvM;
            end
            return tagged Exception {exception: validTrap, trapHandlerPC: newPC};
        end 
				else if (sysInst matches tagged Valid .validSysInst) begin
            // Non-trapping system instructions
            instret_counter <= instret_counter + 1;
            case (validSysInst)
                SRet:
                    begin
                        let next_prv = spp_field == 1 ? prvS : prvU;
                        case (next_prv)
                            prvU: uie_field <= spie_field;
                            prvS: sie_field <= spie_field;
                        endcase
                        spie_field <= 0;
                        spp_field <= 0;
                        prv <= next_prv;
                        Addr newPC = sepc_csr;
                        return tagged RedirectPC newPC;
                    end
                HRet:
                    begin
                        let next_prv = hpp_field;
                        case (next_prv)
                            prvU: uie_field <= hpie_field;
                            prvS: sie_field <= hpie_field;
                            prvM: mie_field <= hpie_field;
                        endcase
                        hpie_field <= 0;
                        hpp_field <= prvU;
                        prv <= next_prv;
                        Addr newPC = mepc_csr;
                        return tagged RedirectPC newPC;
                    end
                MRet:
                    begin
                        let next_prv = mpp_field;
                        case (next_prv)
                            prvU: uie_field <= mpie_field;
                            prvS: sie_field <= mpie_field;
                            prvM: mie_field <= mpie_field;
                        endcase
                        mpie_field <= 0;
                        mpp_field <= prvU;
                        prv <= next_prv;
                        Addr newPC = hepc_csr;
                        return tagged RedirectPC newPC;
                    end
                CSRRW, CSRRS, CSRRC, CSRR, CSRW:
                    begin
                        // Not used at the moment, just a place holder in case
                        // any read side-effects are needed in the CSRs.
                        Bool read = (validSysInst != CSRW);
                        Bool write = (validSysInst != CSRR);
                        // CSR read/write operation
                        let oldVal = getCSR(csr)._read;
                        let newVal = (case(validSysInst)
                                CSRW, CSRR, CSRRW: data;
                                CSRRS: (oldVal | data);
                                CSRRC: (oldVal & (~data));
                            endcase);
                        if (write) begin
                            getCSR(csr)._write(newVal);
                        end
                        return tagged CsrData oldVal;
                    end
                default:
                    begin
                        return tagged None;
                    end
            endcase
        end 
				else begin
            // Normal instruction
            // update fflags, fs, and xs if necessary
            if ((fflags | fflags_field) != fflags_field) begin
                fflags_field <= fflags_field | fflags;
                fpuDirty = True;
            end
            if (fpuDirty) begin
                if (fs_field == 0) begin
                    // fs_field shouldn't be 00
                    $fdisplay(stderr, "[ERROR] CSRFile: fpuDirty is true, but fs_field is set to 0");
                end
                fs_field <= 2'b11;
            end
            if (xDirty) begin
                xs_field <= 2'b11;
            end
            instret_counter <= instret_counter + 1;
            return tagged None;
        end
    endmethod
endmodule
