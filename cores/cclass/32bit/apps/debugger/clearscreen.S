#this function clears the display screen
#the syntax to call this function is CLRS

.text
.align	4
.globl clearscreen

clearscreen:

# Registers modified are : x10, x30, x6, x7, x8

addi sp, sp, -20
sw ra, 0(sp)
sw x6, 4(sp)
sw x7, 8(sp)
sw x8, 12(sp)
sw x10, 16(sp)
sw x30, 20(sp)


 li x6, 0xa
 li x7, 100
 li x8, 0x00
 
 
 PrintHundred:
 
 waitc:
	csrrs x10,0x77f,x0   
	andi x30,x10,0x100     
	beq x30,x0,waitc 
	csrrw x0,0x77f,x6
	
	addi x8, x8, 0x01
	
	bne x8, x7, PrintHundred
	
exit_clearscreen: 
  lw ra,0(sp)
  lw x6, 4(sp)
  lw x7, 8(sp)
  lw x8, 12(sp)
  lw x10, 16(sp)
  lw x30, 20(sp)
  
  addi sp,sp,20
  
  ret	
