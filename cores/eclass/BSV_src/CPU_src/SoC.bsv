/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Name : Abhinaya Agrawal
Email ID : agrawal.abhinaya@gmail.com
*/

/* ================================================================

// Top module: mkTestbench
// -----------------------
// module mkTestbench initializes the system by reseting CPU, setting initial Program Counter (PC) and initializing Instruction and Data Memories
// It then sets up the interconnection between CPU, various memories and peripherals. Currently, ARM's AMBA AHB bus is being used
// Testbench currently supports execution in two modes: 1. DEBUG Environment 2. VERIFICATION Environment

// DEBUG Environment:  
// ------------------
// This module initializes Memory_model and CPU and provides 
// testbench for the Processor. Also establishes the interconnection
// between CPU and MEMORY

================================================================ */

// ================================================================

// Bluespec libraries

import Vector       :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import StmtFSM      :: *;

// ================================================================ 

// ================================================================
// Project imports

import TLM2					:: *;	// Using local copy.
import AHB					:: *;	// Using local copy. 
import Utils				:: *;
import Req_Rsp				:: *;
import Sys_Configs		:: *;
import C_import_decls	:: *;

import ISA_Defs			:: *;
import CPU					:: *;
import Memory_Model		:: *;
import TypeDefs			:: *;
import Interfaces 		:: *;
import Fabric				:: *;
import RTC_AHB					:: *;
//import Host					:: *;
import myRS232				:: *;
import Proc					:: *;

`include "TLM.defines"
`include "RVC.defines"
`include "macro.defines"

// ================================================================
// Interactive commands from the console
// Commands are returned by the imported C function: c_console_command ()
// which returns a Vector #(10, Bit #(64))
// [0] is the arg count (>= 1)
// [1] is the command, see codes below
// [2..9] are the optional arguments (up to 8 args)

Bit #(32) cmd_continue   = 0;
Bit #(32) cmd_dump       = 1;
Bit #(32) cmd_quit       = 2;
Bit #(32) cmd_reset      = 3;
Bit #(32) cmd_step       = 4;
Bit #(32) cmd_step_until = 5;
Bit #(32) cmd_verbosity  = 6;    // arg: verbosity level
Bit #(32) cmd_dump_mem   = 7;

// ================================================================
// Testbench module

interface SoC_IFC;
   interface UartIFC uart_ifc;
endinterface

(*synthesize*)
module mkSoC (SoC_IFC);

	// Masters
	// =======
	Proc_IFC proc <- mkProc;

	// Slaves
	// ======
	RTC#(1) rtc <- mkRTC_RV32; // Real Time Clock
	Memory_IFC#(12) config_memory <- mkMemory_Model(`CONFIG_MEM_INIT_FILE, 32'h0000100c); // Config String
   Memory_IFC#(20) mem <- mkMemory_Model(`DATA_MEM_INIT_FILE, 32'h80000000); // Main memory. Parameters:: Size, init file name, base addr

	// Misc.
	// =====
   Reg #(File) file_arf_dump <- mkRegU; // Architectural Register File dump target
	
	rule rl_post_interrupt;
		proc.interrupt_ifc.timer(rtc.timerInterrupt[0]);
		proc.write_counter_time(rtc.timerValue);
	endrule

   // AHB Bus Interconnect
	// ====================

	// Helper functions
	function Bool addr_match_main_mem (AHBAddr #(`TLM_PRM_RSP_MEM) value);
		return (value >= addr_base_main_memory && value <= addr_bounds_main_memory) ? True : False;
	endfunction

	function Bool addr_match_rtc (AHBAddr #(`TLM_PRM_RSP_MEM) value);
		return (value >= addr_base_rtc && value <= addr_bounds_rtc) ? True : False;
	endfunction
	
	function Bool addr_match_mem_config (AHBAddr #(`TLM_PRM_RSP_MEM) value);
		return (value >= addr_base_config_memory && value <= addr_bounds_config_memory) ? True : False; // XXX High address is random. What should it be?
	endfunction
	
	function AHBMasterFabric funct1(AHBMasterActor a);
		return a.fabric;
	endfunction
	
	function AHBSlaveFabric funct2(AHBSlaveActor a);
		return a.fabric;
	endfunction
	
	// Instantaite transactors for TLM Masters and Slaves
	Vector #(2, AHBMasterActor) masterX <- replicateM(mkAHBMaster);
	Vector #(3, AHBSlaveActor)  slaveX = newVector;
	
	// Establish connectivity between Bus, Masters and Slaves
	mkConnection(proc.bus_ifc, masterX[0].tlm);

	// Make Slaves
	AHBSlaveActor slave_mem_data   <- mkAHBSlave(addr_match_main_mem);
	AHBSlaveActor slave_rtc			 <- mkAHBSlave(addr_match_rtc);
	AHBSlaveActor slave_mem_config <- mkAHBSlave(addr_match_mem_config);

	slaveX[0] = slave_mem_data;
	slaveX[1] = slave_rtc;
	slaveX[2] = slave_mem_config;
   
   mkConnection(slaveX[0].tlm, mem.bus_ifc);
   mkConnection(slaveX[1].tlm, rtc.bus_ifc);
   mkConnection(slaveX[2].tlm, config_memory.bus_ifc);
	
	// Instantiate bus interface
	Empty ahb_bus <- mkAHBBus (map(funct1, masterX), map(funct2, slaveX));
 
   // ----------
   // Check requested mode of operation of the CPU
   // Structure - `ifdef CPU_MODE_NORMAL run cpu in normal mode
   //				`else
   //                  `ifdef CPU_MODE_DEBUG run cpu in debug mode
   //					`endif
   //					`ifdef CPU_MODE_VERIFICATION run cpu in verification mode
   //					`endif
   //			  `endif // for CPU_MODE_NORMAL

	let halt = (proc.halt && !proc.uart_ifc.busy);

   // If the following condition is met, the CPU executes in Normal Mode
   // This mode allows execution of instructions as and when they arrive. At all other times, CPU must run ideally (perform NOPs?)
   `ifdef CPU_MODE_NORMAL
		mkAutoFSM (
		// Statement to initialize CPU
		seq
			action // Reset CPU
				$display ($time, " TB: Initializing CPU");
				proc.debug_ifc.reset;	// True sets mode to debug
			endaction
			
			action // Set initial PC
				Addr start_pc = `PC_INIT_VALUE;
				let reset_done <- proc.debug_ifc.reset_complete;
				if(reset_done == True) proc.debug_ifc.write_pc (start_pc);
				$display ($time, " TB: Setting initial PC to: %0h", start_pc);
			endaction
			
			while(!halt) seq
				noAction;
			endseq
		endseq
		);

	`else
  
   // ============= COMMON DEFINTIONS ====================================

   // -------
   // Miscellanious registers
  
   // Loop iterators
   Reg #(Bit #(6)) rg_xi <- mkRegU;
   Reg #(Bit #(6)) rg_xj <- mkRegU;

   // Registers needed for generating output similar to that generated by SPIKE ISS
   Bit #(64) dump_pc = extend((proc.debug_ifc.read_exec_pc));
   Reg #(Bit #(64)) rg_dump_gpr <- mkRegU;

   // Fuction to display Architectural Regfile Entry
   function Action show64b (String pre, Data x, String post);
      action
	 $write ("%s", pre);
	 Bool leading = True;
	 for (Integer j = 7; j > 0; j = j - 1) begin
		 if (leading && (x [31:28] == 0))
			 $write (" ");
		 else begin
			 $write ("%1h", x [31:28]);
			 leading = False;
		 end
		 x = x << 4;
	 end
	 $write ("%1h%s", x [31:28], post);
      endaction
   endfunction

   // Statement to dump current CPU state to console
   Stmt dump_arch_state =
      seq
	 action
		 let pc      = proc.debug_ifc.read_pc;
		 let instret = proc.debug_ifc.read_instret;
		 let cycle   = proc.debug_ifc.read_cycle;
		$display;
		 $write ("    instret: %0d, CPU cycles: %0d", instret, cycle);
		 show64b ("    PC: ", pc, "\n");
		$display;
	 endaction
	 for (rg_xj <= 0; rg_xj < 32; rg_xj <= rg_xj + 4) seq
		 $write   ("    %2d:", rg_xj);
		 show64b (" ", proc.debug_ifc.read_gpr (truncate (rg_xj)), "");
		 show64b (" ", proc.debug_ifc.read_gpr (truncate (rg_xj) + 1), "");
		 show64b (" ", proc.debug_ifc.read_gpr (truncate (rg_xj) + 2), "");
		 show64b (" ", proc.debug_ifc.read_gpr (truncate (rg_xj) + 3), "\n");
	 endseq
      endseq;


   // Statement to dump current CPU state to file
   Stmt dump_arch_state_to_file =
		seq
		 action
			$fwrite (file_arf_dump, "PC = %h\n", (dump_pc));
		 endaction
			
			for (rg_xi <= 1; rg_xi < 32; rg_xi <= rg_xi + 1) seq
			rg_dump_gpr <= signExtend(proc.debug_ifc.read_gpr (truncate(rg_xi)));
				$fwrite (file_arf_dump, "REG %2d %h\n", rg_xi, rg_dump_gpr);
			endseq

		 action
			$fwrite (file_arf_dump, "\n");
		 endaction
		endseq;

   // Statement to initialize CPU
   Stmt initialize_cpu
   = seq
		action // Reset CPU
			$display ($time, " TB: Initializing CPU");
			proc.debug_ifc.reset;	// True sets mode to debug
		endaction
		
		action // Set initial PC
			Addr start_pc = `PC_INIT_VALUE;
			let reset_done <- proc.debug_ifc.reset_complete;
			if(reset_done == True) proc.debug_ifc.write_pc (start_pc);
			$display ($time, " TB: Setting initial PC to: %0h", start_pc);
		endaction
	endseq;
		
	Stmt initialize_memory = seq
		action // Display initial memory allocation
			//$display ($time, " TB: Initializing Instruction memory [%0h..%0h]", imem_base_addr, 32'hffffffff/*imem_max_addr*/);
			//$display ($time, " TB: Initializing Data memory [%0h..%0h]", dmem_base_addr, 32'hffffffff/*dmem_max_addr*/);
		endaction
   endseq

   // ============= END: COMMON DEFINTIONS ===============================

   // If the following condition is met, the CPU executes in Software Debug mode
   // This mode allows step-by-step execution of instruction and dumping of the Architectural Register File
		`ifdef CPU_MODE_DEBUG;

   // =================== SOFT-DEBUG ENVIRONMENT ====================================

   // ----------------
   // Main behavior: process a queue of interactive commands
   Reg #(Vector #(10, Bit #(32))) rg_console_command <- mkRegU;
   Bit #(32) console_argc     = rg_console_command [0];
   Bit #(32) console_command  = rg_console_command [1];
   Bit #(32) console_arg_1    = rg_console_command [2];
   Bit #(32) console_arg_2    = rg_console_command [3];
   Bit #(32) console_arg_3    = rg_console_command [4];
   Bit #(32) console_arg_4    = rg_console_command [5];
   Bit #(32) console_arg_5    = rg_console_command [6];
   Bit #(32) console_arg_6    = rg_console_command [7];
   Bit #(32) console_arg_7    = rg_console_command [8];
   Bit #(32) console_arg_8    = rg_console_command [9];

   // Miscellanious register
   Reg #(Addr) rg_addr <- mkRegU;
   
   // An automatic Finite-State-Machine emulates the testbench for Soft-Debug mode
   // Execution starts from the first clock cycle
   // CPU, and Instruction and Data Memories are initialized. Thereafter, the console waits for inputs provided through terminal to proceed
   mkAutoFSM (
    seq
	
	 // Initialize CPU
	 initialize_cpu;

	 // Initialize Memory
	 initialize_memory;

	 // Run
	 while (!halt) seq
		 action // Read command from console
			 let cmd <- c_get_console_command;
			 rg_console_command <= cmd;
		 endaction

		// If command is continue, execute all instructions without breaking
		 if (console_command == cmd_continue) seq
			 action
		  Maybe #(Data) mpc = (  (console_argc == 1)
						? tagged Invalid
						: tagged Valid console_arg_1);
		  proc.debug_ifc.run_continue (mpc);
			 endaction
			 display_stop_reason (" TB: Stop reason: ", proc.debug_ifc.stop_reason, "\n");
			 dump_arch_state;
		 endseq

		// If command is dump, dump the Architectural Register File (here, dump to console)
		 else if (console_command == cmd_dump)
			 dump_arch_state;

		// If command is quit, terminate execution
		 else if (console_command == cmd_quit)
			 break;

		// If commnad is reset, reset CPU to initial state
		 else if (console_command == cmd_reset)
			 initialize_cpu;

		// If command is step, execute the next instruction to completion
		// Currently, even for instructions that take multiple cycles to complete (eg. LOAD/STORE), the execution terminates after the first cycle
		// @TODO this behaviour may be fixed by using 'rg_instret' of module CPU if the desired behaviour is to run the instruction to completion
		 else if (console_command == cmd_step) seq
			 action
		  Maybe #(Addr) mpc = (  (console_argc == 1)
						? tagged Invalid
						: tagged Valid console_arg_1);
		  proc.debug_ifc.run_step (mpc);
			 endaction
			 action
		  display_stop_reason (" TB: Stop reason: ", proc.debug_ifc.stop_reason, "\n");
		  $display ("");
			 endaction
		 endseq

		// If command is step_until, execute 'n' instructions. 'n' is a command line argument
		 else if (console_command == cmd_step_until) seq
			 while ((truncate(proc.debug_ifc.read_instret) < console_arg_1) && (proc.debug_ifc.stop_reason != CPU_STOP_EXIT))
		  proc.debug_ifc.run_step (tagged Invalid);
			 action
		  display_stop_reason (" TB: Stop reason: ", proc.debug_ifc.stop_reason, "\n");
		  $display ("");
			 endaction
			 dump_arch_state;
		 endseq

		// If command is verbosity, change the verbosity of the CPU to 'v'. 'v' is a command line argument
		// 'verbosity' is a variable used to control the amount of messages printed during execution, mainly used for debugging purposes
		// if 'verbosity' is set to 0, no messages will be displayed on the console (desired behaviour)
		 else if (console_command == cmd_verbosity) action
			 int v = unpack (truncate (console_arg_1));
			 $display ($time, " TB: Setting verbosity to %0d", v);
			 proc.debug_ifc.set_verbosity (v);
		 endaction

		// If command is dump_memory, dump the data memory region in the range specified through command line (here, to the console)
		 else if (console_command == cmd_dump_mem)
			 for (rg_addr <= (console_arg_1 & 32'hFFFF_FFFC);
			 rg_addr < console_arg_2;
			 rg_addr <= rg_addr + 4)
			 seq
		  proc.debug_ifc.req_read_memW (rg_addr);
		  action
			  let d <- proc.debug_ifc.rsp_read_memW;
			  $display ($time, " TB: %08h: %08h", rg_addr, d);
		  endaction
			 endseq

		// If command is unknown, print error message and ignore the command
		 else
			 $display ("Error: Ignored unknown command: %0d", console_command);
	 endseq
      endseq
      );

	// ==================== END: SOFT DEBUG ENVIRONMENT ================================

	`endif // CPU_MODE_DEBUG

    // ----------
    // Check requested mode of operation of the CPU
    // If the following condition is met, the CPU executes in Verification mode
	// This mode allows the CPU to run to completion (end of instruction stream) and dump the Architectural Register File and Data Memory (here, to dump files)

	`ifdef CPU_MODE_VERIFICATION;
	
	// ==================== VERIFICATION ENVIRONMENT =====================================

	// Miscellanious registers
	Reg #(CounterData) rg_prev_instret <- mkReg(0);			// Number of instructions retired before current step
	Reg #(CounterData) rg_instret <- mkReg(0);			// Number of instructions retired after current step

	mkAutoFSM (
		seq
			initialize_cpu;
			initialize_memory;

			action
				// Open Architectural Register File dump target file with write access
				File file_handle <- $fopen (`FILE_REG_DUMP, "w");

				// Validate dump target
				if (file_handle == InvalidFile) action // Exit if invalid
					$display($time, " TB: Cannot open file '%s' for dumping state. Exiting!", `FILE_REG_DUMP);
					$finish();
				endaction				
				else  // If valid: update handle
					file_arf_dump <= file_handle;
			endaction

			// Execute all instructions untill the end of instruction stream
			// End of instruction stream is currently marked by 'illegal' instruction (0x0000006F)
			// One instruction is retired at a time using 'run_step' method of the CPU Debug interface
			// At the end of each step, current state of the Architectural Register File is printed to a file
			while (True) seq
				rg_prev_instret <= proc.debug_ifc.read_instret;
				proc.debug_ifc.run_step(tagged Invalid);
				if(proc.debug_ifc.read_instret - rg_prev_instret > 0) seq // IF condition ensures redundant states are not printed
														// Useful for multi-cycle instructions and when a branch instruction flushes the pipeline
					rg_instret <= unpack(proc.debug_ifc.read_instret);		
					dump_arch_state_to_file;
					if(halt) break;
				endseq
			endseq
		endseq
	);

    // ==================== END: VERIFICATION ENVIRONMENT ================================

	`endif // CPU_MODE_VERIFICATION
	`endif // CPU_MODE_NORMAL

   interface UartIFC uart_ifc;
      method Action sin(Bit#(1) in);
         proc.uart_ifc.sin(in);
      endmethod

      method Bit#(1) sout;
         return proc.uart_ifc.sout;
      endmethod
   endinterface
endmodule
// ================================================================
