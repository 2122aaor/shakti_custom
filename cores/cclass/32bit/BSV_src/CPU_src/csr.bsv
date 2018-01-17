package csr;

  import defined_types::*;
	`include "defined_parameters.bsv"
  import ConfigReg::*;
  import myRS232::*;
  import GetPut            ::*;
  import ConcatReg ::*;
  interface Ifc_csr;
    method ActionValue #(CSRResult) csr_access(CSRInsn data);
    method    Action      sin(Bit#(1) in);
    method    Bit#(1)     sout();
		method 	Action mtip(Bit#(1) mtip1);
  endinterface

  function Reg#(t) readOnlyReg(t r);
    return (interface Reg;
            method t _read = r;
            method Action _write(t x) = noAction;
        endinterface);
  endfunction

  function Reg#(t) writeSideEffect(Reg#(t) r, Action a);
    return (interface Reg;
            method t _read = r._read;
            method Action _write(t x);
                r._write(x);
                a;
            endmethod
        endinterface);
endfunction

  (*synthesize*)
  module mkcsr#( Bit#(1) meip, Bit#(1) ueip)(Ifc_csr);

  `ifdef simulate
    Reg#(Bool) wr_endsimulation <-mkReg(False);
  `endif
  /////////////////////////////// Machine level register /////////////////////////
  // Current Privilege Level
	Reg#(Privilege_mode) rg_prv <- mkReg(Machine); // resets to machine mode

  Reg#(Bit#(32)) csr_mvendorid = readOnlyReg(0);
  Reg#(Bit#(32)) csr_marchid = readOnlyReg(0);
  Reg#(Bit#(32)) csr_mimpid = readOnlyReg(0);
  Reg#(Bit#(32)) csr_mhartid = readOnlyReg(0);
  Reg#(Bit#(32)) csr_misa <- mkReg(`MISA_BITS);
   
  // trap vector fields (same as CSR without bottom 2 bits)
  Reg#(Bit#(30)) rg_mtvec <- mkReg(`MTVEC_DEFUALT);
  Reg#(Bit#(32)) csr_mtvec=concatReg2(rg_mtvec,readOnlyReg(2'b0));

  // mstatus fields
  Reg#(Bit#(5)) rg_vm	 <- mkReg(0); // WARL
  Reg#(Bit#(1)) rg_mxr  <- mkReg(0); // Not required
  Reg#(Bit#(1)) rg_pum  <- mkReg(0); // Not required
  Reg#(Bit#(1)) rg_mprv <- mkReg(0);
  Reg#(Bit#(2)) rg_xs	 = readOnlyReg(0);
  Reg#(Bit#(2)) rg_fs	 <- mkReg(2'b01);
  Reg#(Bit#(2)) rg_mpp	 <- mkReg(0);
  Reg#(Bit#(2)) rg_hpp	 =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_spp	 =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_mpie <- mkReg(0);
  Reg#(Bit#(1)) rg_hpie =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_spie =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_upie <- mkReg(0);
  Reg#(Bit#(1)) rg_mie	 <- mkReg(0);
  Reg#(Bit#(1)) rg_hie	 =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_sie	 =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_uie	 <- mkReg(0);
  Reg#(Bit#(1)) rg_sd	 =  readOnlyReg(pack((rg_xs == 2'b11) || (rg_fs == 2'b11)));
  Reg#(Bit#(32)) csr_mstatus =  concatReg20(
           rg_sd,
           readOnlyReg(0),
           rg_vm,
           readOnlyReg(4'b0),
           rg_mxr, rg_pum, rg_mprv, // memory privilege
           rg_xs, rg_fs, // coprocessor states
           rg_mpp, rg_hpp, rg_spp, // previous privileges
           rg_mpie, rg_hpie, rg_spie, rg_upie, // previous interrupt enables
           rg_mie, rg_hie, rg_sie, rg_uie); // interrupt enables

  // trap delegation fields
  Reg#(Bit#(12)) rg_medeleg<-mkReg(0);
  Reg#(Bit#(12)) rg_mideleg<-mkReg(0);
  Reg#(Bit#(32)) csr_medeleg = concatReg2(readOnlyReg(0),rg_medeleg);
  Reg#(Bit#(32)) csr_mideleg = concatReg2(readOnlyReg(0),rg_mideleg);

  // mie fields
  Reg#(Bit#(1)) rg_meie <- mkReg(0);
  Reg#(Bit#(1)) rg_heie =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_seie =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_ueie <- mkReg(0);
  Reg#(Bit#(1)) rg_mtie <- mkReg(0);
  Reg#(Bit#(1)) rg_htie =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_stie =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_utie <- mkReg(0);
  Reg#(Bit#(1)) rg_msie <- mkReg(0);
  Reg#(Bit#(1)) rg_hsie =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_ssie =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_usie <- mkReg(0);
  Reg#(Bit#(32)) csr_mie     =  concatReg13(
           readOnlyReg(0),
           rg_meie, rg_heie, rg_seie, rg_ueie,
           rg_mtie, rg_htie, rg_stie, rg_utie,
           rg_msie, rg_hsie, rg_ssie, rg_usie);

  // mip fields
  Reg#(Bit#(1)) rg_meip =  readOnlyReg(pack(meip));
  Reg#(Bit#(1)) rg_heip =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_seip =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_ueip =  readOnlyReg(pack(ueip));
	Wire#(Bit#(1)) wr_mtip <-mkDWire(0);
  Reg#(Bit#(1)) rg_mtip =  readOnlyReg(pack(wr_mtip));
  Reg#(Bit#(1)) rg_htip =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_stip =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_utip <- mkReg(0);
  Reg#(Bit#(1)) rg_msip =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_hsip =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_ssip =  readOnlyReg(0);
  Reg#(Bit#(1)) rg_usip <- mkReg(0);
  
  Reg#(Bit#(32)) csr_mcycle[2]<-mkCReg(2,0);
  Reg#(Bit#(32)) csr_mcycleh[2]<-mkCReg(2,0);
  Reg#(Bit#(32)) csr_minstret[2]<-mkCReg(2,0);
  Reg#(Bit#(32)) csr_minstreth[2]<-mkCReg(2,0);

  // Machine Trap Handling
  Reg#(Bit#(32)) csr_mscratch <- mkReg(0);
  Reg#(Bit#(32)) csr_mepc     <- mkReg(0);
  Reg#(Bit#(32)) csr_mcause   <- mkReg(0);
  Reg#(Bit#(32)) csr_mbadaddr <- mkReg(0);
  Reg#(Bit#(32)) csr_mip      =  concatReg13(
           readOnlyReg(0),
           readOnlyReg(rg_meip), rg_heip, rg_seip, rg_ueip,
           readOnlyReg(rg_mtip), rg_htip, rg_stip, rg_utip,
           readOnlyReg(rg_msip), rg_hsip, rg_ssip, rg_usip);
  
  // Machine Protection and Translation
  Reg#(Bit#(32)) csr_mbase   <- mkReg(0);
  Reg#(Bit#(32)) csr_mbound  <- mkReg(0);
  Reg#(Bit#(32)) csr_mibase  <- mkReg(0);
  Reg#(Bit#(32)) csr_mibound <- mkReg(0);
  Reg#(Bit#(32)) csr_mdbase  <- mkReg(0);
  Reg#(Bit#(32)) csr_mdbound <- mkReg(0);

	// Counter enables
  Reg#(Bit#(1)) rg_u_ir <- mkReg(0);
  Reg#(Bit#(1)) rg_u_tm <- mkReg(0);
  Reg#(Bit#(1)) rg_u_cy <- mkReg(0);
  // Machine Counter Setup
  Reg#(Bit#(32)) csr_mucounteren = concatReg4(readOnlyReg(0), rg_u_ir, rg_u_tm, rg_u_cy);
  //////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// User level registers ///////////////////////////////////

  Reg#(Bit#(30)) rg_utvec <- mkReg(truncateLSB(`UTVEC_DEFAULT));
  Reg#(Bit#(32)) csr_utvec=concatReg2(rg_utvec,readOnlyReg(2'b0));

  Reg#(Bit#(32)) csr_ustatus =  concatReg20(
           rg_sd,
           readOnlyReg(0),
           rg_vm,
           readOnlyReg(4'b0),
           rg_mxr, rg_pum, rg_mprv, // memory privilege
           rg_xs, rg_fs, // coprocessor states
           readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), // previous privileges
           readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), rg_upie, // previous interrupt enables
           readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), rg_uie); // interrupt enables
  Reg#(Bit#(32)) csr_uie =  concatReg13(
           readOnlyReg(0),
           readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), rg_ueie,
           readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), rg_utie,
           readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), rg_usie);

	// User Trap Handling
	Reg#(Bit#(32)) csr_uscratch <- mkReg(0);
  Reg#(Bit#(32)) csr_uepc     <- mkReg(0);
  Reg#(Bit#(32)) csr_ucause   <- mkReg(0);
  Reg#(Bit#(32)) csr_ubadaddr <- mkReg(0);
  Reg#(Bit#(32)) csr_uip      =  concatReg13(
          readOnlyReg(0),
          readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(rg_ueip),
          readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(rg_utip),
          readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(1'b0), rg_usip);
  Reg#(Bit#(32)) csr_uinstret=readOnlyReg(csr_minstret[1]);
  Reg#(Bit#(32)) csr_uinstreth=readOnlyReg(csr_minstreth[1]);
  Reg#(Bit#(32)) csr_ucycle=readOnlyReg(csr_mcycle[1]);
  Reg#(Bit#(32)) csr_ucycleh=readOnlyReg(csr_mcycleh[1]);

  Reg#(Bit#(5)) rg_fflags[2]<-mkCReg(2,0);
  Reg#(Bit#(3)) rg_frm<-mkReg(0);
  Reg#(Bit#(32)) csr_fcsr = writeSideEffect(concatReg3(readOnlyReg(24'd0),rg_frm,rg_fflags[1]),rg_fs._write(2'b11));
  Reg#(Bit#(32)) csr_fflags=writeSideEffect(concatReg2(readOnlyReg(27'd0),rg_fflags[1]),rg_fs._write(2'b11));
  Reg#(Bit#(32)) csr_frm =  writeSideEffect(concatReg2(readOnlyReg(29'd0),rg_frm),rg_fs._write(2'b11));
  

  //////////////////////////////////////////////////////////////////////////////////////////
  
  /////////////////////////////////////////////////////// UART related wires and registers //////////////////////////////////////////////////
  Wire#(Bool) wr_send_to_uart <-mkDWire(False);
  Wire#(Bit#(1)) wr_transmission_notfull <-mkDWire(0);
  Reg#(Bit#(8)) rg_tx[2] <-mkCReg(2,0);
  Reg#(Bit#(9)) rg_rx[2] <-mkCReg(2,0);
  Reg#(Bit#(32)) csr_uarttx = writeSideEffect(concatReg3(readOnlyReg(23'd0),readOnlyReg(wr_transmission_notfull),rg_tx[0]),wr_send_to_uart._write(True));
  Reg#(Bit#(32)) csr_uartrx = (concatReg2(readOnlyReg(23'd0),readOnlyReg(rg_rx[0])));
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  //////////////////////////////// UART Debugger /////////////////////////////////
      UART#(3) uart <-mkUART(8,NONE,STOP_1,`BAUD_RATE); // charasize,Parity,Stop Bits,BaudRate
      rule send_transimission_data0(wr_send_to_uart);
        $display($time,"\t Putting Data :%s in FIFO for transmision",wr_send_to_uart);
        uart.rx.put(rg_tx[1]);
      endrule
      rule uart_tx_connection;
        wr_transmission_notfull<=pack(uart.transmission_fifo_notfull);
      endrule
      rule uart_rx_connectionif(rg_rx[1][8]==0);
        let x<-uart.tx.get;
        rg_rx[1]<={1'b1,x};
        $display($time,"\tReceived Character: %s",x);
      endrule
  ////////////////////////////////////////////////////////////////////////////////
 
  function Reg#(Bit#(32)) read_csr(Bit#(12) address);
   Reg#(Bit#(32)) csr=(
    case(address)
				`USTATUS	    : csr_ustatus;
				`UIE		      : csr_uie; 
				`UTVEC		    : csr_utvec;
				`USCRATCH	    : csr_uscratch;
				`UEPC		      : csr_uepc;
				`UCAUSE	      : csr_ucause;
				`UBADADDR	    : csr_ubadaddr;
				`UIP		      : csr_uip;
        `FRM          : csr_frm;
        `FFLAGS       : csr_fflags;
        `FCSR         : csr_fcsr;
				`UCYCLE 	    : csr_ucycle;
			//	`UTIME		    : True;
				`UINSTRET	    : csr_uinstret;
				`UCYCLEH	    : csr_ucycleh;
			//	`UTIMEH	      : True;
				`UINSTRETH    : csr_uinstreth;
				`MISA		      : csr_misa;
				`MVENDORID    : csr_mvendorid;
				`MARCHID	    : csr_marchid;
				`MIMPID	      : csr_mimpid;
				`MHARTID	    : csr_mhartid;
				`MSTATUS	    : csr_mstatus;
				`MEDELEG	    : csr_medeleg;
				`MIDELEG	    : csr_mideleg;
				`MIE		      : csr_mie;
				`MTVEC		    : csr_mtvec;
				`MSCRATCH	    : csr_mscratch;
				`MEPC		      : csr_mepc;
				`MCAUSE	      : csr_mcause;
				`MBADADDR     : csr_mbadaddr;
				`MIP		      : csr_mip;
				`MBASE		    : csr_mbase;
				`MBOUND	      : csr_mbound;
				`MIBASE	      : csr_mibase;
				`MIBOUND	    : csr_mibound;
				`MDBASE	      : csr_mdbase;
				`MDBOUND	    : csr_mdbound;
				`MCYCLE	      : csr_mcycle[1];
				`MINSTRET     : csr_minstret[1];
				`MCYCLEH      : csr_mcycleh[1];
				`MINSTRETH    : csr_minstreth[1];
				`MUCOUNTEREN	: csr_mucounteren;
        `UARTTX       : csr_uarttx;
        `UARTRX       : csr_uartrx;
      default:  readOnlyReg(0);
    endcase
    ); 
    return csr;
  endfunction
	
  function Bool isValidCSR(Bit#(12) csr);
		Bool ret = (
			case (csr)
				`USTATUS	 : True;
				`UIE		 : True; 
				`UTVEC		 : True;
				`USCRATCH	 : True;
				`UEPC		 : True;
				`UCAUSE	 : True;
				`UBADADDR	 : True;
				`UIP		 : True;
        `FRM: True;
        `FFLAGS:True;
        `FCSR:True;
				`UCYCLE 	 : True;
				`UTIME		 : True;
				`UINSTRET	 : True;
				`UCYCLEH	 : True;
				`UTIMEH	 : True;
				`UINSTRETH : True;
				`MISA		 : True;
				`MVENDORID : True;
				`MARCHID	 : True;
				`MIMPID	 : True;
				`MHARTID	 : True;
				`MSTATUS	:  True;
				`MEDELEG	:  True;
				`MIDELEG	:  True;
				`MIE		:  True;
				`MTVEC		:  True;
				`MSCRATCH	:  True;
				`MEPC		:  True;
				`MCAUSE	:  True;
				`MBADADDR :  True;
				`MIP		:  True;
				`MBASE		:  True;
				`MBOUND	:  True;
				`MIBASE	:  True;
				`MIBOUND	:  True;
				`MDBASE	:  True;
				`MDBOUND	:  True;
				`MCYCLE	:  True;
				`MINSTRET :  True;
				`MCYCLEH  :  True;
				`MINSTRETH:  True;
				`MUCOUNTEREN	: True;
        `UARTTX       :True;
        `UARTRX       :True;
				// Non-standard Machine Register
				//`MXUARTTX : True;
				//`MXUARTRX : True;

				default: False;
			endcase
			);
		return ret;
	endfunction
  
  function Bool hasCSRPermission(Bit#(12) address, Privilege_mode prv, Bool write);
    Bit#(12) csr_index = pack(address);
    return ((pack(prv) >= csr_index[9:8]) && (!write || (csr_index[11:10] != 2'b11)));
  endfunction
   
  // if the operand is not 0 then the instruction will perform a write on the CSR.
	function Bool valid_csr_access(Bit#(12) csr_addr, Bit#(5) operand);
		Bool ret = ((!isValidCSR(unpack(csr_addr))) || !hasCSRPermission(unpack(csr_addr), rg_prv, (operand != 0) ? True:False));
		return !ret;
	endfunction

  rule increment_cycle_counter;
    csr_mcycle[0]<=csr_mcycle[0]+1;
    if(csr_mcycle[0]==32'hFFFFFFFF)
      csr_mcycleh[0]<=csr_mcycleh[0]+1;
  endrule
  
	// Check pending interrupts
	function Maybe #(Interrupt_cause) fn_chk_pending_interrupt();
     Bit#(12) pending_interrupts = truncate(csr_mip) & truncate(csr_mie);
     // machine mode
     let pending_machine_interrupts = pending_interrupts & ~truncate(csr_mideleg);
     let machine_interrupts_enabled = (rg_mie == 1) || (pack(rg_prv) < pack(Machine));
     // user mode
     let pending_user_interrupts = pending_interrupts & truncate(csr_mideleg);
     let user_interrupts_enabled = (rg_uie == 1) && (rg_prv == User);
     // combined
     pending_interrupts = (machine_interrupts_enabled ? pending_machine_interrupts : 0)
                         | (user_interrupts_enabled ? pending_user_interrupts : 0);
     // format pendingInterrupt value to return
     Maybe#(Interrupt_cause) ret = tagged Invalid;
     if (pending_interrupts != 0) begin
         ret = tagged Valid unpack(pack(countZerosLSB(pending_interrupts)));
     end
     return ret;
	endfunction

  `ifdef simulate
//		rule allways_disp;
//				$display("MINSTRET: %d\t MCYCLE: %d\n",csr_minstret[0],csr_mcycle[0]);
//		endrule
    rule end_simulations(wr_endsimulation);
			if(!uart.transmission_fifo_notEmpty)begin
				$display("MINSTRET: %d\n MCYCLE: %d\n",csr_minstret[0],csr_mcycle[0]);
        $finish(0);
			end
    endrule
  `endif
	
  method ActionValue #(CSRResult) csr_access(CSRInsn data);
    let ret = CSRResult{address:0,redirect:False,destination_value:data.rd_data};
    let pending_interrupt = fn_chk_pending_interrupt;
    Maybe#(Trap_info) take_trap =tagged Invalid;
    
    let csr_addr=data.rd_data[16:5];
    let rs1 = data.rd_data[4:0];
    let funct3=data.rd_data[19:17];
    if(pending_interrupt matches tagged Valid .interrupt) begin
      take_trap=tagged Valid (Trap_info{trap:tagged Interrupt interrupt,badaddr:0});
      $display($time,"\tCSR: Pending interrupts :%h",interrupt);
    end
    else if(data.exception matches tagged Valid .exception)begin
      take_trap=tagged Valid (Trap_info{trap:tagged Exception exception,badaddr:0});
      $display($time,"\tCSR: Instruction generated Exception :%h",exception);
    end
    else if(data.is_privilege)begin // This means it is a system instruction;
      case(funct3)
        'd0:
          case (csr_addr)
            'h000: // ECALL
              take_trap=tagged Valid (Trap_info{trap:tagged Exception(case(rg_prv) User: Ecall_from_user;Machine:Ecall_from_machine;endcase),badaddr:0});
            'h001: // EBREAK
              take_trap=tagged Valid (Trap_info{trap:tagged Exception Breakpoint,badaddr:0});
            'h302: // MRET
              if(rg_prv!=Machine) take_trap= tagged Valid (Trap_info{trap:tagged Exception Illegal_inst,badaddr:0});
          endcase
        'd1,'d2,'d3,'d4,'d5,'d6,'d7:
          if(!valid_csr_access(csr_addr,rs1)) take_trap= tagged Valid (Trap_info{trap:tagged Exception Illegal_inst,badaddr:0});
      endcase
    end

    if(take_trap matches tagged Valid .trap)begin
      $display($time,"\tTaking Trap");
      let bad_addr = trap.badaddr;
      let trapToTake = trap.trap;
      `ifdef simulate
        if(trapToTake matches tagged Exception .x &&& x == Illegal_inst)begin
          $display($time,"\tCSR: Illegal instruction");
          wr_endsimulation<=True;
        end
      `endif
      ret.redirect=True;
      Bit#(32) cause = 0;
      Bit #(TSub #(32, 1)) cause_code = 0;
      Bit #(1) cause_type = 0;
      case (trapToTake) matches
        tagged Interrupt .i: begin cause_type = 1; cause_code = zeroExtend(pack(i)); end
        tagged Exception .e: begin cause_type = 0; cause_code = zeroExtend(pack(e)); end
      endcase
      cause = {cause_type, cause_code};
      // Check if the trap was delegated
      Bool delegToU = (pack(rg_prv) <= pack(User)) && (case (trapToTake) matches
              tagged Exception .exceptionCause: (((csr_medeleg >> pack(exceptionCause)) & 1) != 0);
              tagged Interrupt .interruptCause: (((csr_mideleg >> pack(interruptCause)) & 1) != 0);
          endcase);
      if (delegToU) begin
        $display($time,"\tCSR: Trap Delegated to User. utvec: %h cause: %d csr_uepc: %h medeleg: %h rg_prv: %h mtvec: %h",csr_utvec,cause,data.pc,csr_medeleg,rg_prv,csr_mtvec);
        csr_ucause <= cause; // Update 'ucause' csr
        ret.address = csr_utvec;// Build redirect pc
        csr_uepc <= data.pc;// Update 'uepc' csr
        case (trapToTake)// Update 'ubadaddr' csr
          tagged Exception Inst_addr_misaligned,
          tagged Exception Inst_access_fault,
          tagged Exception Load_addr_misaligned,
          tagged Exception Load_access_fault,
          tagged Exception Store_addr_misaligned,
          tagged Exception Store_access_fault:  csr_ubadaddr <= bad_addr; 
        endcase
        rg_uie <= 0;
        rg_prv <= User;
      end
      else begin
        csr_mcause <= cause;
        $display($time,"\tCSR: Trap Delegated to User. utvec: %h cause: %d csr_uepc: %h",csr_utvec,cause,data.pc);
        ret.address = csr_mtvec;
        csr_mepc <= data.pc;
        case (trapToTake)
          tagged Exception Inst_addr_misaligned,
          tagged Exception Inst_access_fault,
          tagged Exception Load_addr_misaligned,
          tagged Exception Load_access_fault,
          tagged Exception Store_addr_misaligned,
          tagged Exception Store_access_fault: csr_mbadaddr <= bad_addr; 
        endcase
        rg_mie <= 0;
        rg_prv <= Machine;
        rg_mpp <= pack(rg_prv);
        rg_mpie <= (case (rg_prv)  User: rg_uie;  Machine: rg_mie;  endcase);
      end
    end
    else if(data.is_privilege==False)begin
      $display($time,"\tCSR: Normal Instruction");
      csr_minstret[0]<=csr_minstret[0]+1;
      if(csr_minstret[0]==32'hFFFFFFFF)
        csr_minstreth[0]<=csr_minstreth[0]+1;
      let fpudirty=False;
      if((data.fflags|rg_fflags[0])!=rg_fflags[0])begin
        rg_fflags[0]<=data.fflags|rg_fflags[0];
        fpudirty=True;
      end
      if(fpudirty)
        if(rg_fs==2'b0)begin
          $display("Error: FPU id Dirty and FX field is 0");
		end
    end
    else begin
      $display($time,"\tCSR: Performing CSR Instructions");
      let csr = read_csr(unpack(csr_addr)); // read the CSR register first.
      $display($time,"\tCSR: Addr: %h, Funct3: %d, Value: %h Rs1: %h",csr_addr,funct3,csr,rs1);
      if(funct3==0)begin
        case (csr_addr)
          'h302:begin  // MRET
              Privilege_mode next_prv =unpack(rg_mpp);
              case(next_prv)
                User: rg_uie <= rg_mpie;
                Machine: rg_mie <= rg_mpie;
              endcase
              rg_mpie <= 1;
              rg_mpp <= pack(User);
              rg_prv <= next_prv;
              ret.address=csr_mepc;
              ret.redirect=True;
          end
					'h002:begin // URET
						rg_uie<=rg_upie;
						rg_upie<=1;
						rg_prv<=User;
						ret.address=csr_uepc;
						ret.redirect=True;
					end
        endcase
      end
      else begin
        ret.destination_value=csr; 
        if(csr_addr=='h77e)
          rg_rx[0]<=0;
        if(rs1!=0)
          case(funct3)
            'd1: csr <= data.rs1_data;           // CSRRW
            'd2: csr <= data.rs1_data | csr;// CSRRS 
            'd3: csr <= data.rs1_data & (~csr);    //CSRRC 
            'd5: csr <= zeroExtend(rs1);          // CSRRWI 
            'd6: csr <= zeroExtend(rs1) | csr;    // CSRRSI 
            'd7: csr <= zeroExtend(rs1) & (~csr); 
          endcase
        end
      end
    return ret;
  endmethod

  method    Action      sin(Bit#(1) in);
    uart.rs232.sin(in);
  endmethod
  method    Bit#(1)     sout();
   return uart.rs232.sout;
  endmethod
	method 	Action mtip(Bit#(1) mtip1);
		wr_mtip<=mtip1;
	endmethod
  endmodule
endpackage
