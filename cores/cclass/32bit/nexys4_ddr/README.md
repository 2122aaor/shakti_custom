Porting SHAKTI C-32 on the Digilent NEXYS4-DDR Board
========================================================

As of now the C-32 contains a basic UART port. The interface to this UART port
has been mapped to CSRs as of now. This UART port is used to communicate with
the host system over the USB.

This project assumes you have **vivado** in your $PATH and also the
requiredcable drivers installed on the host system.

The structure and the scripts used in this project are based on 
lowRISC's nexys4-ddr board demo. 

Folder structure:
-----------------

* constraints

	contains the pin map xdc file and the code.mem file which is the loaded into the test-memory of the processor.
	The mem file of the application that needs to run on the C-32 needs to be copied into C-32 before running make.

* Top.v
	
	This file instantiates the C-32 core and clock divider. The clock divider converts the 100MHz crystal clock to 20MHz to be used by the core.
	With the core running at 20MHz the UART of the core has been configured to run at a Baudrate of 9600, 8-bits, No parity and 1-stop bit

* \*.tcl
	
	These Tcl files are used to create/run vivado projects, synthesis and programming runs and used by the Makefile.


How to use makefile ( in sequential steps )
--------------------------------------------------------

* Create a Vivado project

        make project

* Generate bit-stream for downloading 

        make bitstream

* Open the Vivado GUI with the project (optional step)

        make vivado

* To program the Nexys4-DDR board with the generated bit file. 

        make program

* To search for the application BRAM and find its location.
	
				make search-bram

* To update the application BRAM in the bit file with a new application ( This assumes the code.mem in the constraints folder is the new application memory)
			
				make bit-update

