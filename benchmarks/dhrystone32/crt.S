	.text
	.globl _start	
_start:
	auipc sp, 0x20
	
	# Enabling FPU
	lui	t0,0x3					
	csrs	mstatus,t0			
	addi t0, x0, 0
	
	call main
	j _endProgram

