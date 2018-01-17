### RISC-V TORTURE TESTS FOR SHAKTI ###
---------------------------------

This project is derived from Berkeley's riscv-torture repo: https://github.com/ucb-bar/riscv-torture. The files in the repo have been modified using the patches suggested
by cliffordwolf in  to generate only rv32g instructions. Atomic instructions have been switched off though.

Further changes in the riscv_test.hex have been made to terminate on a self-loop when a particular test has ended.

### TO RUN RISCV-TORTURE TESTS ###
---------------------------------

*	To run multiple tests (default is 1). The relevant files will be created in the output folder.

	make RUNS=100

* To clean all the files
	
	make clean





