#C CLASS PROCESSOR 

**Specs**
1. 5-Stage In-order Pipeline supporting RV32IM (AFD will be added soon)
2. Tournament branch predictor added as an extra stage
3. USB based debugger
4. AAPG automated torture cases

**NOTE**
Go to AAPG folder to run regressive torture test cases for the processor.

**Makefile Commands**
1. make module=mkTb_core file=Tb_core compile_bluesim link_bluesim
  * This will compile and link the BSV source and create a out and out.so binary in the bin folder
2. make
  * This will simply compile, link and simulate the entire RTL.
  * mem.hex(data_memory) and code.hex(instruction_memory) are required for successfull completion.
  * Simulation will end when *0000006f* instruction is encountered.

