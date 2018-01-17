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

package CSRFile;

// ================================================================
// Bluespec libraries

import RegFile       :: *;
import FIFOF         :: *;
import SpecialFIFOs  :: *;
import DefaultValue  :: *;
import GetPut        :: *;

// ================================================================
// Project imports

import TLM2          :: *; // Using local copy
import Req_Rsp       :: *;
import Sys_Configs   :: *;
import Vector        :: *;
import RegUtil       :: *;
import ConcatReg     :: *;
import TypeDefs      :: *;
import Interfaces    :: *;
import myRS232       :: *;

import ISA_Defs      :: *;
import ISA_Defs_PRV  :: *;   // Privilege ISA defintions

`include "TLM.defines"
`include "RVC.defines"
`include "macro.defines"

`define BAUD_RATE 130
// ================================================================

interface CSR_IFC;
   method CSRState read_csr_state;
   method VMInfo read_vmInfo_I;
   method VMInfo read_vmInfo_D;
   method Maybe #(MCause_Interrupt) read_pending_interrupt;

   method ActionValue #(CSRReturn) try_system_instruction (CSRData data);
   method ActionValue #(Maybe #(Addr)) try_pending_trap (TrapData trap, Addr pc);

   interface InterruptIFC interrupt_ifc;
   method Action write_counter_time (CounterData data);
   
   // UART interface
   interface UartIFC uart_ifc;
endinterface

module mkCSRFile #(Data hartid, CounterData counter_cycle, CounterData counter_instret)(CSR_IFC); // From interrupt controller
    
   // Machine information 
   Data mvendorid     = 0; // non-commercial
   Data marchid       = 0; // not implemented
   Data mimpid        = 0; // not implemented
   Addr default_mtvec = 'h0000_1000;
   Addr default_utvec = 'h0000_8000;

   Wire #(Bool) wr_mtip <- mkDWire(False);
   Wire #(Bool) wr_msip <- mkDWire(False);
   Wire #(Bool) wr_meip <- mkDWire(False);

   Wire #(CounterData) wr_counter_time <- mkWire;

   // Current Privilege Level
   Reg#(Bit#(2)) rg_prv[2] <- mkCReg(2, prvM); // resets to machine mode
   
   // Counter enables
   Reg#(Bit#(1)) rg_u_ir <- mkReg(0);
   Reg#(Bit#(1)) rg_u_tm <- mkReg(0);
   Reg#(Bit#(1)) rg_u_cy <- mkReg(0);
   Reg#(Bit#(1)) rg_s_ir <- mkReg(0);
   Reg#(Bit#(1)) rg_s_tm <- mkReg(0);
   Reg#(Bit#(1)) rg_s_cy <- mkReg(0);

   // trap delegation fields
   Reg#(Bit#(12)) rg_medeleg <- mkReg(0);
   Reg#(Bit#(12)) rg_mideleg <- mkReg(0);

   // trap vector fields (same as CSR without bottom 2 bits)
   Reg#(Bit#(TSub#(XLEN,2))) rg_mtvec <- mkReg(truncateLSB(default_mtvec));
   Reg#(Bit#(TSub#(XLEN,2))) rg_utvec <- mkReg(truncateLSB(default_utvec));

   // mstatus fields
   Reg#(Bit#(5)) rg_vm   <- mkReg(0); // WARL
   Reg#(Bit#(1)) rg_mxr  <- mkReg(0); // Not required
   Reg#(Bit#(1)) rg_pum  <- mkReg(0); // Not required
   Reg#(Bit#(1)) rg_mprv <- mkReg(0);
   Reg#(Bit#(2)) rg_xs   <- mkReg(0);
   Reg#(Bit#(2)) rg_fs   <- mkReg(0);
   Reg#(Bit#(2)) rg_mpp[2]  <- mkCReg(2, 0);
   Reg#(Bit#(2)) rg_hpp     = readOnlyReg(0);
   Reg#(Bit#(1)) rg_spp     = readOnlyReg(0);
   Reg#(Bit#(1)) rg_mpie[2] <- mkCReg(2, 0);
   Reg#(Bit#(1)) rg_hpie    = readOnlyReg(0);
   Reg#(Bit#(1)) rg_spie    = readOnlyReg(0);
   Reg#(Bit#(1)) rg_upie[2] <- mkCReg(2, 0);
   Reg#(Bit#(1)) rg_mie[2]  <- mkCReg(2, 0);
   Reg#(Bit#(1)) rg_hie     = readOnlyReg(0);
   Reg#(Bit#(1)) rg_sie     = readOnlyReg(0);
   Reg#(Bit#(1)) rg_uie[2]  <- mkCReg(2, 0);
   Reg#(Bit#(1)) rg_sd   =  readOnlyReg(pack((rg_xs == 2'b11) || (rg_fs == 2'b11)));

   // mie fields
   Reg#(Bit#(1)) rg_meie <- mkReg(0);
   Reg#(Bit#(1)) rg_heie =  readOnlyReg(0);
   Reg#(Bit#(1)) rg_seie <- mkReg(0);
   Reg#(Bit#(1)) rg_ueie <- mkReg(0);
   Reg#(Bit#(1)) rg_mtie <- mkReg(0);
   Reg#(Bit#(1)) rg_htie =  readOnlyReg(0);
   Reg#(Bit#(1)) rg_stie <- mkReg(0);
   Reg#(Bit#(1)) rg_utie <- mkReg(0);
   Reg#(Bit#(1)) rg_msie <- mkReg(0);
   Reg#(Bit#(1)) rg_hsie =  readOnlyReg(0);
   Reg#(Bit#(1)) rg_ssie <- mkReg(0);
   Reg#(Bit#(1)) rg_usie <- mkReg(0);

   // mip fields
   Reg#(Bit#(1)) rg_meip =  readOnlyReg(pack(wr_meip));
   Reg#(Bit#(1)) rg_heip =  readOnlyReg(0);
   Reg#(Bit#(1)) rg_seip <- mkReg(0);
   Reg#(Bit#(1)) rg_ueip <- mkReg(0);
   Reg#(Bit#(1)) rg_mtip =  readOnlyReg(pack(wr_mtip));
   Reg#(Bit#(1)) rg_htip =  readOnlyReg(0);
   Reg#(Bit#(1)) rg_stip <- mkReg(0);
   Reg#(Bit#(1)) rg_utip <- mkReg(0);
   Reg#(Bit#(1)) rg_msip =  readOnlyReg(pack(wr_msip));
   Reg#(Bit#(1)) rg_hsip =  readOnlyReg(0);
   Reg#(Bit#(1)) rg_ssip <- mkReg(0);
   Reg#(Bit#(1)) rg_usip <- mkReg(0);

   // Priv 1.9 CSRs

   // User Trap Status
   Reg#(Data) csr_ustatus =  concatReg20(
           rg_sd,
           readOnlyReg(0),
           rg_vm,
           readOnlyReg(4'b0),
           rg_mxr, rg_pum, rg_mprv, // memory privilege
           rg_xs, rg_fs, // coprocessor states
           readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), // previous privileges
           readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), rg_upie[0], // previous interrupt enables
           readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), rg_uie[0]); // interrupt enables

   Reg#(Data) csr_uie =  concatReg13(
           readOnlyReg(0),
           readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), rg_ueie,
           readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), rg_utie,
           readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), rg_usie);

   Reg#(Data) csr_utvec = concatReg2(rg_utvec, readOnlyReg(2'b0));
   
   // User Trap Handling
   Reg#(Data) csr_uscratch <- mkReg(0);
   Reg#(Data) csr_uepc[2]     <- mkCReg(2, 0);
   Reg#(Data) csr_ucause[2]   <- mkCReg(2, 0);
   Reg#(Data) csr_ubadaddr[2] <- mkCReg(2, 0);
   Reg#(Data) csr_uip      =  concatReg13(
           readOnlyReg(0),
           readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(rg_ueip),
           readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(rg_utip),
           readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), rg_usip);


   // Machine Timers and Counters. TODO Fix these counters. Make these read/write registers
   Reg#(Data) csr_mcycle[3]   <- mkCReg(3, 0);
   Reg#(Data) csr_minstret[3] <- mkCReg(3, 0);

   // Upper 32-bits of CSRs for RV32: (these will just be unused copies of
   // the other cycle/time/instret registers for RV64)
   Reg#(Data) csr_mcycleh[3]   <- mkCReg(3, 0);
   Reg#(Data) csr_minstreth[3] <- mkCReg(3, 0);
   
   rule rl_update_counters;
      csr_mcycle[0] <= truncate(counter_cycle);
      csr_mcycleh[0] <= truncateLSB(counter_cycle);
      csr_minstret[0] <= truncate(counter_instret);
      csr_minstreth[0] <= truncateLSB(counter_instret);
   endrule

   Reg#(Data) csr_cycle   = readOnlyReg(csr_mcycle[1]);
   Reg#(Data) csr_time    = readOnlyReg(truncate(wr_counter_time));
   Reg#(Data) csr_instret = readOnlyReg(csr_minstret[1]);

   // Upper 32-bits of CSRs for RV32: (these will just be unused copies of
   // the other cycle/time/instret registers for RV64)
   Reg#(Data) csr_cycleh   = readOnlyReg(csr_mcycleh[1]);
   Reg#(Data) csr_timeh    = readOnlyReg(truncateLSB(wr_counter_time));
   Reg#(Data) csr_instreth = readOnlyReg(csr_minstreth[1]);

   // Machine Information Registers
   Reg#(Data) csr_misa      <- mkReg(32'h40141129); //For torture: mkReg(32'h40141129); //For BRF(and E): mkReg(32'h40101100);
   Reg#(Data) csr_mvendorid = readOnlyReg(mvendorid);
   Reg#(Data) csr_marchid   = readOnlyReg(marchid);
   Reg#(Data) csr_mimpid    = readOnlyReg(mimpid);
   Reg#(Data) csr_mhartid   = readOnlyReg(hartid);

   // Machine Trap Setup
   Reg#(Data) csr_mstatus =  concatReg20(
           rg_sd,
           readOnlyReg(0),
           rg_vm,
           readOnlyReg(4'b0),
           rg_mxr, rg_pum, rg_mprv, // memory privilege
           rg_xs, rg_fs, // coprocessor states
           rg_mpp[0], rg_hpp, rg_spp, // previous privileges
           rg_mpie[0], rg_hpie, rg_spie, rg_upie[0], // previous interrupt enables
           rg_mie[0], rg_hie, rg_sie, rg_uie[0]); // interrupt enables
   Reg#(Data) csr_medeleg =  concatReg2(readOnlyReg(0), rg_medeleg); // WARL - 12 legal exceptions
   Reg#(Data) csr_mideleg =  concatReg2(readOnlyReg(0), rg_mideleg); // WARL - 12 legal interrupts
   Reg#(Data) csr_mie     =  concatReg13(
           readOnlyReg(0),
           rg_meie, rg_heie, rg_seie, rg_ueie,
           rg_mtie, rg_htie, rg_stie, rg_utie,
           rg_msie, rg_hsie, rg_ssie, rg_usie);
   Reg#(Data) csr_mtvec = concatReg2(rg_mtvec, readOnlyReg(2'd0));

   // Machine Trap Handling
   Reg#(Data) csr_mscratch    <- mkReg(0);
   Reg#(Data) csr_mepc[2]     <- mkCReg(2, 0);
   Reg#(Data) csr_mcause[2]   <- mkCReg(2, 0);
   Reg#(Data) csr_mbadaddr[2] <- mkCReg(2, 0);
   Reg#(Data) csr_mip       =  concatReg13(
           readOnlyReg(0),
           readOnlyReg(rg_meip), readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(rg_ueip),
           readOnlyReg(rg_mtip), readOnlyReg(1'b0), readOnlyReg(1'b0), rg_utip,
           readOnlyReg(rg_msip), readOnlyReg(1'b0), readOnlyReg(1'b0), rg_usip);

   // Machine Protection and Translation
   Reg#(Data) csr_mbase   <- mkReg(0);
   Reg#(Data) csr_mbound  <- mkReg(0);
   Reg#(Data) csr_mibase  <- mkReg(0);
   Reg#(Data) csr_mibound <- mkReg(0);
   Reg#(Data) csr_mdbase  <- mkReg(0);
   Reg#(Data) csr_mdbound <- mkReg(0);

   // Machine Counter Setup
   Reg#(Data) csr_mucounteren = concatReg4(readOnlyReg(0), rg_u_ir, rg_u_tm, rg_u_cy);

   // =====================================
   // UART
   // =====================================
   function Reg#(t) writeSideEffect(Reg#(t) r, Action a);
    return (interface Reg;
            method t _read = r._read;
            method Action _write(t x);
                r._write(x);
                a;
            endmethod
        endinterface);
   endfunction

   Wire#(Bool)    wr_send_to_uart <-mkDWire(False);
   Wire#(Bit#(1)) wr_transmission_notfull <-mkDWire(0);
   Reg#(Bit#(8))  rg_tx[2] <-mkCReg(2,0);
   Reg#(Bit#(9))  rg_rx[2] <-mkCReg(2,0);
   Reg#(Bit#(32)) csr_mxuarttx = writeSideEffect(concatReg3(readOnlyReg(23'd0),readOnlyReg(wr_transmission_notfull),rg_tx[0]),wr_send_to_uart._write(True));
   Reg#(Bit#(32)) csr_mxuartrx = (concatReg2(readOnlyReg(23'd0),readOnlyReg(rg_rx[0])));
 
   UART#(3) uart <-mkUART(8,NONE,STOP_1,`BAUD_RATE); // charasize,Parity,Stop Bits,BaudRate
   rule send_transimission_data0(wr_send_to_uart);
     uart.rx.put(rg_tx[1]);
   endrule
   rule uart_tx_connection;
     wr_transmission_notfull<=pack(uart.transmission_fifo_notfull);
   endrule
   rule uart_rx_connectionif(rg_rx[1][8]==0);
     let x<-uart.tx.get;
     rg_rx[1]<={1'b1,x};
   endrule
   // ====================================

   Reg#(Data) rg_tmp <- mkReg(0);

   rule rl_print_csrs(rg_tmp < csr_minstret[0]);
      rg_tmp <= csr_minstret[0];
      //$display($time, " CSR: MCAUSE: %h", csr_mcause[0]);
      //$display($time, " CSR: MSTATUS: %h", csr_mstatus);
      //$display($time, " CSR: MCYCLE: %h", {csr_mcycleh[0], csr_mcycle[0]});
      //$display($time, " CSR: MINSTRET: 0x%h (%0d), MCAUSE: 0x%h, PRV: %h, uEPC: %h, mEPC: %h", {csr_minstreth[0], csr_minstret[0]}, {csr_minstreth[0], csr_minstret[0]}, csr_mcause[0], rg_prv[0], csr_uepc[0], csr_mepc[0]);
      //$display($time, " CSR: MIP: %h (%b)", csr_mip, csr_mip);
      //$display($time, " CSR: MIE: %h (%b)", csr_mie, csr_mie);
   endrule

   // Function to access a CSR given an index
   function Reg #(Data) get_csr(CSR csr);
      Reg #(Data) ret = (
         case (csr)
            /*User Trap Status*/
            USTATUS   : csr_ustatus;
            UIE       : csr_uie;
            UTVEC     : csr_utvec;

            /*User Trap Handling*/
            USCRATCH  : csr_uscratch; 
            UEPC      : csr_uepc[0];
            UCAUSE    : csr_ucause[0];
            UBADADDR  : csr_ubadaddr[0];
            UIP       : csr_uip;

            /*User Counter/Timers*/
            UCYCLE    : csr_cycle;
            UTIME     : csr_time;
            UINSTRET  : csr_instret;
            UCYCLEH   : csr_cycleh;
            UTIMEH    : csr_timeh;
            UINSTRETH : csr_instreth;

            // Machine-Level CSRs
            // ------
            /*Machine Information Registers*/
            MISA      : csr_misa; 
            MVENDORID : csr_mvendorid; 
            MARCHID   : csr_marchid; 
            MIMPID    : csr_mimpid; 
            MHARTID   : csr_mhartid;

            /*Machine Trap Setup CSR*/
            MSTATUS  : csr_mstatus;
            MEDELEG  : csr_medeleg;
            MIDELEG  : csr_mideleg;
            MIE      : csr_mie;
            MTVEC    : csr_mtvec;

            /*Machine Trap Handling*/ 
            MSCRATCH : csr_mscratch;
            MEPC     : csr_mepc[0];
            MCAUSE   : csr_mcause[0];
            MBADADDR : csr_mbadaddr[0];
            MIP      : csr_mip;
            
            /*Machine Protection and Translation*/ 
            MBASE    : csr_mbase;
            MBOUND   : csr_mbound;
            MIBASE   : csr_mibase;
            MIBOUND  : csr_mibound;
            MDBASE   : csr_mdbase;
            MDBOUND  : csr_mdbound;
            
            /*Macine Timers and Counters*/ 
            MCYCLE   : csr_mcycle[1];
            MINSTRET : csr_minstret[1]; 
            MCYCLEH  : csr_mcycleh[1];
            MINSTRETH: csr_minstreth[1];
            
            /*Machine Counter Setup*/
            MUCOUNTEREN : csr_mucounteren; 
        
            // Non-standard Machine Register
            /*UART*/
            MXUARTTX : csr_mxuarttx;
            MXUARTRX : csr_mxuartrx;

            default: readOnlyReg(32'b0);
         endcase
      );
      return ret;
   endfunction
   
   // Check pending interrupts
   function Maybe #(MCause_Interrupt) fn_chk_pending_interrupt();
     Bit#(12) pending_interrupts = truncate(csr_mip) & truncate(csr_mie);
     // machine mode
     let pending_machine_interrupts = pending_interrupts & ~truncate(csr_mideleg);
     let machine_interrupts_enabled = (rg_mie[0] == 1) || (rg_prv[0] < prvM);
     // user mode
     let pending_user_interrupts = pending_interrupts & truncate(csr_mideleg);
     let user_interrupts_enabled = (rg_uie[0] == 1) && (rg_prv[0] == prvU);
     // combined
     pending_interrupts = (machine_interrupts_enabled ? pending_machine_interrupts : 0)
                         | (user_interrupts_enabled ? pending_user_interrupts : 0);
     // format pendingInterrupt value to return
     Maybe#(MCause_Interrupt) ret = tagged Invalid;
     if (pending_interrupts != 0) begin
         // pack/unpack type conversion:
         // UInt#(TLog#(TAdd#(12,1))) == UInt#(4) -> Bit#(4) -> InterruptCause
         ret = tagged Valid unpack(pack(countZerosLSB(pending_interrupts)));
     end
     return ret;
   endfunction
   
   Maybe #(MCause_Interrupt) pending_interrupt = fn_chk_pending_interrupt;

   function Bool valid_csr_access(CSR_Addr csr_addr, RegName operand);
      Bool ret = ((!isValidCSR(unpack(csr_addr))) || !hasCSRPermission(unpack(csr_addr), rg_prv[0], (operand != 0) ? True:False));
      return !ret;
   endfunction

   method Maybe #(MCause_Interrupt) read_pending_interrupt;
      return pending_interrupt;
   endmethod
   
   method ActionValue #(Maybe #(Addr)) try_pending_trap (TrapData t, Addr pc);
      //let pc = t.pc;
      let bad_addr = fromMaybe(?, t.bad_addr);
      let trapToTake = t.trap;

      Maybe #(Addr) redirect_pc = tagged Invalid;

      begin
         Data cause = 0;
         Bit #(TSub #(XLEN, 1)) cause_code = 0;
         Bit #(1) cause_type = 0;
         case (trapToTake) matches
            tagged Interrupt .i: begin cause_type = 1; cause_code = zeroExtend(pack(i)); end
            tagged Exception .e: begin cause_type = 0; cause_code = zeroExtend(pack(e)); end
         endcase
         cause = {cause_type, cause_code};
         // Check if the trap was delegated
         Bool delegToU = rg_prv[1] <= prvU && (case (trapToTake) matches
                 tagged Exception .exceptionCause: (((csr_medeleg >> pack(exceptionCause)) & 1) != 0);
                 tagged Interrupt .interruptCause: (((csr_mideleg >> pack(interruptCause)) & 1) != 0);
             endcase);
         if (delegToU) begin
            // Update 'ucause' csr
            csr_ucause[1] <= cause;
            // Build redirect pc
            redirect_pc = tagged Valid csr_utvec;
            // Update 'uepc' csr
            csr_uepc[1] <= pc;
            // Update 'ubadaddr' csr
            case (trapToTake)
                tagged Exception MCAUSE_INSTRN_ADDR_MISALN,
                tagged Exception MCAUSE_INSTRN_ACCESS_FAULT,
                tagged Exception MCAUSE_LOAD_ADDR_MISALN,
                tagged Exception MCAUSE_LOAD_ACCESS_FAULT,
                tagged Exception MCAUSE_STORE_ADDR_MISALN,
                tagged Exception MCAUSE_STORE_ACCESS_FAULT:
                    csr_ubadaddr[1] <= bad_addr; 
            endcase
            rg_uie[1] <= 0;
            rg_prv[1] <= prvU;
         end
         else begin
            // Update 'mcause' csr
            csr_mcause[1] <= cause;
            // Build redirect pc
            redirect_pc = tagged Valid csr_mtvec;
            // Update 'uepc' csr
            csr_mepc[1] <= pc;
            // Update 'ubadaddr' csr
            case (trapToTake)
                tagged Exception MCAUSE_INSTRN_ADDR_MISALN,
                tagged Exception MCAUSE_INSTRN_ACCESS_FAULT,
                tagged Exception MCAUSE_LOAD_ADDR_MISALN,
                tagged Exception MCAUSE_LOAD_ACCESS_FAULT,
                tagged Exception MCAUSE_STORE_ADDR_MISALN,
                tagged Exception MCAUSE_STORE_ACCESS_FAULT:
                    csr_mbadaddr[1] <= bad_addr; 
            endcase
            rg_mie[1] <= 0;
            rg_prv[1] <= prvM;
            rg_mpp[1] <= rg_prv[1];
            rg_mpie[1] <= (case (rg_prv[1])
                        prvU: rg_uie[1];
                        prvS: rg_sie;
                        prvH: rg_hie;
                        prvM: rg_mie[1];
                     endcase);
         end
      end
      return redirect_pc;
   endmethod

   method ActionValue #(CSRReturn) try_system_instruction (CSRData data);
      CSRReturn ret = ?;
      Maybe #(MCause_Trap) trapToTake = tagged Invalid;
      let csr = get_csr(unpack(data.csr_addr));
      Maybe #(Addr) newPC = tagged Invalid;

      if(data.opcode == op_SYSTEM) begin
         Bool match1 = True;
         Bool match2 = True;
         case ({data.csr_addr, data.funct3})
            {f12_ECALL, f3_ECALL}   : trapToTake = tagged Valid tagged Exception
                                       (case (rg_prv[0])
                                          prvU: MCAUSE_ENV_CALL_U;
                                          //prvS: tagged Valid MCAUSE_ENV_CALL_S;
                                          //prvH: tagged Valid MCAUSE_ENV_CALL_H;
                                          prvM: MCAUSE_ENV_CALL_M;
                                       endcase);
            {f12_EBREAK, f3_EBREAK} : trapToTake = tagged Valid tagged Exception MCAUSE_BREAKPOINT;
            {f12_URET, f3_URET}     : begin
                                          let next_prv = prvU; // Not required. Next level will automatically be U-mode
                                          rg_uie[0] <= rg_upie[0];
                                          rg_upie[0] <= 1;
                                          rg_prv[0] <= prvU;
                                          ret = tagged SysD tagged Redirect csr_uepc[0];
                                          newPC = tagged Valid csr_uepc[0];
                                      end
            {f12_SRET, f3_SRET}     : trapToTake = tagged Valid tagged Exception MCAUSE_ILLEGAL_INSTRN;
            {f12_HRET, f3_HRET}     : trapToTake = tagged Valid tagged Exception MCAUSE_ILLEGAL_INSTRN;
            {f12_MRET, f3_MRET}     : if(rg_prv[0] < prvM) trapToTake = tagged Valid tagged Exception MCAUSE_ILLEGAL_INSTRN;
                                      else begin 
                                          let next_prv = rg_mpp[0];
                                          case(next_prv)
                                             prvU: rg_uie[0] <= rg_mpie[0];
                                             prvM: rg_mie[0] <= rg_mpie[0];
                                          endcase
                                          rg_mpie[0] <= 1;
                                          rg_mpp[0] <= prvU;
                                          rg_prv[0] <= next_prv;
                                          newPC = tagged Valid csr_mepc[0];
                                          ret = tagged SysD tagged Redirect csr_mepc[0];
                                      end
            //{f12_WFI, f3_WFI}        : begin wr_wfi <= True; ret = tagged Valid WaitForInterrupt; end
            default: match1 = False;
         endcase

         case (data.funct3)
            f3_CSRRW  : if (valid_csr_access(data.csr_addr, data.rs1)) begin if (data.rs1 != 0) csr <= data.data;           ret = tagged SysD tagged Value csr; end
                        else trapToTake = tagged Valid tagged Exception MCAUSE_ILLEGAL_INSTRN;
            f3_CSRRS  : if (valid_csr_access(data.csr_addr, data.rs1)) begin if (data.rs1 != 0) csr <= data.data | csr;     ret = tagged SysD tagged Value csr; end
                        else trapToTake = tagged Valid tagged Exception MCAUSE_ILLEGAL_INSTRN;
            f3_CSRRC  : if (valid_csr_access(data.csr_addr, data.rs1)) begin if (data.rs1 != 0) csr <= data.data & (~csr);  ret = tagged SysD tagged Value csr; end
                        else trapToTake = tagged Valid tagged Exception MCAUSE_ILLEGAL_INSTRN;
            f3_CSRRWI : if (valid_csr_access(data.csr_addr, data.rs1)) begin if (data.zimm != 0) csr <= data.zimm;          ret = tagged SysD tagged Value csr; end
                        else trapToTake = tagged Valid tagged Exception MCAUSE_ILLEGAL_INSTRN;
            f3_CSRRSI : if (valid_csr_access(data.csr_addr, data.rs1)) begin if (data.zimm != 0) csr <= data.zimm | csr;    ret = tagged SysD tagged Value csr; end
                        else trapToTake = tagged Valid tagged Exception MCAUSE_ILLEGAL_INSTRN;
            f3_CSRRCI : if (valid_csr_access(data.csr_addr, data.rs1)) begin if (data.zimm != 0) csr <= data.zimm & (~csr); ret = tagged SysD tagged Value csr; end
                        else trapToTake = tagged Valid tagged Exception MCAUSE_ILLEGAL_INSTRN;
            default: match2 = False;
         endcase

         if(!match1 && !match2) trapToTake = tagged Valid tagged Exception MCAUSE_ILLEGAL_INSTRN;

         if (trapToTake matches tagged Valid .trap) ret = tagged TrapD TrapData{bad_addr: tagged Invalid, trap: trap};
      end
      return ret;
   endmethod

   method CSRState read_csr_state = CSRState {prv: rg_prv[0], /*frm: frm_field,*/ f_enabled: (rg_fs != 0), x_enabled: (rg_xs != 0)};

   // TODO Fix memory translation behaviour
   // Example case: when using mbbid mode, the high bit of base csr should be set but ignored when translating
   method VMInfo read_vmInfo_I;
      Bit#(5) vm = (rg_prv[0] == prvM) ? vmMbare : rg_vm;
      Addr base = (case (vm)
                      vmMbare: 0;
                      vmMbb: csr_mbase;
                      vmMbbid: csr_mibase;
                  endcase);
      Addr bound = (case (vm)
                      vmMbb: csr_mbound;
                      vmMbbid: csr_mibound;
                      default: -1;
                  endcase);
      return VMInfo{prv: rg_prv[0], /*asid: asid_field,*/ vm: vm, mxr: unpack(rg_mxr), pum: unpack(rg_pum), base: base, bound: bound};
   endmethod

   method VMInfo read_vmInfo_D;
      Bit#(2) vm_prv = (rg_mprv == 1) ? rg_mpp[0] : rg_prv[0];
      Bit#(5) vm = (vm_prv == prvM) ? vmMbare : rg_vm;
      Addr base = (case (vm)
                      vmMbare: 0;
                      vmMbb: csr_mbase;
                      vmMbbid: csr_mdbase;
                  endcase);
      Addr bound = (case (vm)
                      vmMbb: csr_mbound;
                      vmMbbid: csr_mdbound;
                      default: -1;
                  endcase);
      return VMInfo{ prv: vm_prv, /*asid: asid_field,*/ vm: vm, mxr: unpack(rg_mxr), pum: unpack(rg_pum), base: base, bound: bound };
   endmethod
   
   interface InterruptIFC interrupt_ifc;
      method Action timer(mtip);
         wr_mtip <= mtip;
      endmethod

      method Action software(msip);
         wr_msip <= msip;
      endmethod

      method Action external(meip);
         wr_meip <= meip;
      endmethod
   endinterface
   
   method Action write_counter_time (CounterData data);
      wr_counter_time <= data;
   endmethod
   
   // UART interface
   interface UartIFC uart_ifc;
      method Action sin(Bit#(1) in);
         uart.rs232.sin(in);
      endmethod

      method Bit#(1) sout();
         return uart.rs232.sout;
      endmethod

      method Bool busy;
         return uart.busy;
      endmethod
   endinterface
endmodule

endpackage
