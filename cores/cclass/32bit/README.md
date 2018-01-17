#C CLASS PROCESSOR 

**Specs**
---------
1. 5-Stage In-order Pipeline supporting RV32IMF (A will be added soon)
2. Tournament branch predictor added as an extra stage
3. USB based debugger
4. AAPG automated torture cases

**NOTE**
---------
Go to AAPG folder to run regressive torture test cases for the processor.
The iverilog, ncverilog and modelsim folders hold scripts required to run verilog simulations
in the respective tools.

**Makefile Commands**
------------------------

*	This will compile and link the code using bluesim. The linked binary will be present in the bin/ folder

		make 
		
*	The following commands will compile the BSV sources and link the code to create an executable in bluesim, ncverilog, modelsim or iverilog respectively. AAPG is then run the executables.

		make AAPG_bsim
		
		make AAPG_ncverilog
		
		make AAPG_msim
		
		make AAPG_iverilog
		
*	This command will simply compile the bluespec code to generate the verilog files for simulation and copy the necessary verilog library files from the bluespec folder to enable verilog simulations. The verilog files will be created in the verilog folder.

		make generate_verilog
		
*	This command will simply compile the bluespec code to generate the verilog files for synthesis and copy the necessary verilog library files from the bluespec folder to enable verilog synthesis. The verilog files will be created in the verilog_fpga folder. Simulations will not work using these verilog files.
		
		make generate_verilog_fpga
		
*	The folloing commands will run csmith on Bluesim, NCVERILOG, MODELSIM and IVERILOG respectively. RUNS value can be set as desired.

		make csmith_bsim RUNS=100
		
		make csmith_ncverilog RUNS=100
		
		make csmith_msmim RUNS=100
		
		make csmith_iverilog RUNS=100
		
*	The folloing commands will run riscv-torture-tests on Bluesim, NCVERILOG, MODELSIM and IVERILOG respectively. RUNS value can be set as desired.

		make torture_bsim RUNS=100
		
		make torture_ncverilog RUNS=100
		
		make torture_msmim RUNS=100
		
		make torture_iverilog RUNS=100
		
* This command will delete all files in bin and BSV_src/build_bsim folders.

		make clean_bsim
		
* This command will delete all files in verilog, verilog_fpga, bin and BSV_src/build_bsim folders.

		make clean_verilog
		
* This command will delete all generated files in the verification folder as well in addition to performing clean_verilog

		make restore
		

*	For any of the above compile/generate\_verilog\* commands you can send the following arguments to the make targets: 

		bpu=enable # This enables the branch predictor
		isa=RV32I # supports only the base integer isa
		isa=RV32IM # above + multiplication and division
		isa=RV32IMA # above + atomix support
		isa=RV32IMAF # above + Single precision floating point.
		axi=enable # uses the AXI3 bus instead of the AHB bus.

		make AAPG_bsim bpu=enable isa=RV32IMA axi=enable



