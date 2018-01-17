.globl _endProgram
.align 3

_endProgram:
	nop
	csrw mtohost, 1;
1:
	j 1b
