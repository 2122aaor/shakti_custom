.globl _endProgram
.align 6

_endProgram:
	nop
	csrw mtohost, 1;
1:
	j 1b
