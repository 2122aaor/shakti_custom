## Introduction

The C-Class processor is a member of the SHAKTI processor systems. Two variants of the C-class are available : 32-bit and 64-bit version.
This particular doc-series provides details of the 32-bit variant : C-32. 

## Features of C-32 

* The entire design is designed using Bluespec System Verilog (BSV).
* As of now supports RV32IMF (Atomic instruction support to be added asap)
* Compliant with the Privilege-1.9.1 spec and ISA-2.1 spec.
* Harvard architecture with separate Instruction and Data caches.
* Both the caches are blocking and PIPT in nature to reduce area overheads.
* The C-32 core as of now sits on the AHB bus as a master providing easy integration with peripherals.
* All RV32I instructions execute in a single cycle.
* The multiplier and divider circuits are sequential in nature but highly paramtertized to allow easy change in the number of cycles required to execute a single multiplication/division instruction.
* The C-32 provides support for only the machine and user modes. 
* A tournament branch predictor is also available to imrpvode throughput.
* A UART has been integrated with the special CSR regsiters to allow debugging on FPGAs through host PC (JTAG replacement to be added soon).
* Benchmarks such as dhrystone and those provided in the riscv-test suit have been successfully tested on C-32.
* The current C-32 code line has also been ported on the nexys4_ddr fpga board.

## [Block Diagram](block_diagram.md)
