I-Class Core
--------------

Requirements
-------------
The code is written in BSV (Bluespec System Verilog). 
In order to compile the code or run the bluesim simulation  you need a Bluespec License (please contact Bluspec Inc)
The compilation can generate 2 types of output file - C files that can be run on the bluespec simulator,bluesim and verilog.
The verilog files can be used in any verilog tool-flow.

Compilation and Simulation
---------
To generate verilog files enter "make compile_verilog"
To simulate the core using verilog code with modelsim enter "make simulate_verilog"

Features
----------
1. I Class is a  64-bit variant, dual issue varinat  of Shakti processor family and features a pipeline of depth of 8 stages. 
   The 8 stages of the pipeline are -  Fetch, Decode, Rename(Map), Wakeup, Select, Drive, Execute, Commit. Each of the pipeline stages takes a single cycle to execute.
2. The core supports all standard instructions of the  RISC-V ISA for RV32I, RV64I and RV32M modes.
3. Each instruction has its operands renamed in-order but  issued out of order to the execution units. Commit is  in-order.
4. Register Renaming is done through a merged register file approach. Merged register files store both the architectural register values and speculated values. 
   The number of architectural registers are 32 and the number of physical registers are 64. A buffer (register alias table)  maintains the architectural register to physical register map.
5. The Branch Predictor is a Tournament Branch predictor. It has Bimodal and Global predictors. More advanced BP schemes will be added shortly.
6. The Functional Units are parameterised. The current design uses 2 Arithmetic and Logical Units, 1 Branch Unit, a Load Store Unit.
7. The I-Cache and D-Cache use a PIPT scheme and are parameterized. They will be changed to a  VIPT scheme once the MMU is added. 
   Each cache is nominally configured at 32KB. The cache is fully parameterised in terms of the size of the cache, 
   associativity, number of blocks within a cache line, number of sets, etc. The caches are implemented using BRAMs provided in the Bluespec library. 
   These BRAMs have a direct correlation to the FPGA based Block RAMs, easing the translation to an FPGA based design.
8. This variant supports only the machine mode. 

