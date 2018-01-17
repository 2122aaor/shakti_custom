Benchmarks for C-32
========================================================

These tests are borrowed from the riscv-tests suite from Berkeley (https://github.com/riscv/riscv-tests/tree/master/benchmarks).
However, some modifications are done the syscalls.c file. Since C-32 uses a UART to commuincate
with the host pc, the putchar function (which initially called the spike based fesvr) has been modified
to send characters directly via the UART based CSR registers. Simlar changes to other functions to read
and print special registers have been made.

Requirements
------------------------------------------------------

This projects assumes you have installd the latest version of riscv32-unknown-elf-\* tool chain with RV32G support.
Please refer to the riscv-gnu-toolchain repo (https://github.com/riscv/riscv-gnu-toolchain) for further details on installation


How to use makefile
--------------------------------------------------------
1. make shakti
    * This will compile all the benchmarks with syscalls.c defined for shakti. (where primarily printf calls are uart based)

2. make spike
    * This will compile all the benchmarks with syscalls.c defined for spike. The executables (\*.riscv files) can be run on spike using "spike --isa=rv32g benchmark.riscv" command.

The above commands will create the outputfiles folder containing the following files for each benchmark:

* \*.riscv
	This file is the elf executable of the respective benchmark.

* \*.riscv.dump
	This file is the disassembled file containing all the sections of the elf of the respective benchmark.

* \*.riscv.hex
	This file is generated using the elf2hex command on the elf executable and used for RTL simulation and synthesis. These files can directly
	be copied into the nexys4_ddr/constraints/code.mem file before synthesis to run them on the FPGA without any modifications.


Benchmarks to be added
--------------------------------------------------------

Following are the list of benchmarks we are working of for porting onto the C-32

* Coremarks Floating
* FreeRTOS

