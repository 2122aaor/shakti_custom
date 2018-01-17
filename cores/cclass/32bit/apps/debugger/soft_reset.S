# This function resets all the registers to 0 and calls _start program. Memory contents are not affected by the command.
.text
.align	4
.globl soft_reset

soft_reset:
    
	addi x1, x0,0x0 
	addi x2, x0,0x0 
	addi x3, x0,0x0
	addi x4, x0,0x0
	addi x5, x0,0x0
	addi x6, x0,0x0
	addi x7, x0,0x0
	addi x8, x0,0x0
	addi x9, x0,0x0
	addi x10, x0,0x0
	addi x11, x10,0x0 
	addi x12, x11,0x0
	addi x13, x12,0x0
	addi x14, x13,0x0
	addi x15, x14,0x0
	addi x16, x15,0x0
	addi x17, x16,0x0
	addi x18, x17,0x0
	addi x19, x18,0x0
	addi x20, x19,0x0
	addi x21, x20,0x0
	addi x22, x21,0x0
	addi x23, x22,0x0
	addi x24, x23,0x0
	addi x25, x24,0x0
	addi x26, x25,0x0
	addi x28, x26,0x0
	addi x29, x28,0x0
	addi x30, x29,0x0
	addi x31, x30,0x0
  beq x0,x0,_start

