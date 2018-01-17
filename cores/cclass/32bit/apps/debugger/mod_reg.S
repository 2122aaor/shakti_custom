#this function modifies the value of a single register.
#the syntax of the function is MODR <new value in 8 bit hex format> <the register index in 3 bit decimal format>
#initially all the register values are stored in the stack in order to prevent the values from getting changed and then the offset for the #particular register value is calculated bby multiplying the index value by 4.
#in order to display the address and value in the right order, the required nibbles are shifted appropriately to the left most 4 bits by the #use of the nibble counter and a variable shift value.  
#the same 'print' subroutine is used to print both the address and value and hence in order to differentiate between the two a seperate #register is used.

.text
.align	4
.globl modify_register

modify_register:
	
	addi sp,sp,-124
	
	sw x0, 0(sp)        #store register values into memory locations
	sw x1, 4(sp)
	sw x2, 8(sp)
	sw x3, 12(sp)
	sw x4, 16(sp)
	sw x5, 20(sp)
	sw x6, 24(sp)
	sw x7, 28(sp)
	sw x8, 32(sp)
	sw x9, 36(sp)
	sw x10, 40(sp)
	sw x11, 44(sp)
	sw x12, 48(sp)
	sw x13, 52(sp)
	sw x14, 56(sp)
	sw x15, 60(sp)
	sw x16, 64(sp)
	sw x17, 68(sp)
	sw x18, 72(sp)
	sw x19, 76(sp)
	sw x20, 80(sp)
	sw x21, 84(sp)
	sw x22, 88(sp)
	sw x23, 92(sp)
	sw x24, 96(sp)
	sw x25, 100(sp)
	sw x26, 104(sp)
	sw x27, 108(sp)
	sw x28, 112(sp)
	sw x29, 116(sp)
	sw x30, 120(sp)
	sw x31, 124(sp)

	li s9,0x00
	li s7, 0x00
	li s10, 0x00

	addi s9, x20, 0x00  	#store the decimal representation of the index into s9
	addi s7, x18, 0x00  	#store the hex representation of the index into s7
        addi s10, x19, 0x00 	#store the new value into s10

	li a0, 0x04		#byte threshold     
	li a6, 0x00		#byte counter used to set the offset
	li a2, 0x00 		#offset value
	
Add:	add a2, s7, a2		#the add routine calculates index*4 in order to know the memory byte to be accessed
	addi a6, a6, 1
	bne a6,a0, Add
	
	addi a4, sp, 0		#store the sp value into a4
	add  a4, a4, a2		#add the offset to a4 to get the address of the required register
	
	#li a0, 0x04        	#byte threshold  
	li s1, 0x08        	#nibble threshold
	li s5, 0x0a        	#ascii value for 'a'
	
main_sub:
	li t6,  4   		#value by which the register value has to be shifted	
	li s2, 0x3a		#ascii value for ':'
	li s3, 0x06        	#nibble counter starts from 6 because we only have to display the index value of the register which is 3 digits
	addi a1, s9, 0x00	#store s9 into a1
	li t3, 0x00		#to differentiate between memory address display and memory value display.

loop:
	srl s4, a1, t6		#shift a1 by the required value and store it in s4.
	addi t6, t6, -4		#decrement the shift value by 4.
	andi s4, s4, 0x0f  	#to mask the first nibble alone
	bge s4, s5, alpha  	#check if alphabet
	addi s4, s4, 0x30  	#offset for numbers
	beq a0, a0, print	#jump to print subroutine.

alpha:
	addi s4, s4, 0x57  	#offset for alphabets
	beq a0, a0, print	#jump to print subroutine.
    	
print:	
        jal t5, wait       	#call the wait loop
	csrrw x0,0x77f,s4 	#output the value via reg_tx
	addi s3, s3, 0x01  	#upgrade the nibble counter
	bne s3, s1, loop   	#check if all 8 nibbles are over
	beq t3, s5, exit5   	#jump to next if the display was for the value
	addi s4, s2, 0x00  	#store s2 value in s4 i.e. store ascii value for ':' in s4.

spaces:
	jal t5, wait		#call the wait loop.
	csrrw x0, 0x77f, s4	#output the value via reg_tx.
	sw s10, 0(a4)		#store the new value into the memory address.
	
	lw t1, 0(a4)		#load the value stored in the memory address into t1.
	li t6, 28		#reset the nibble offset to 28 for memory value display.
	addi a1, t1, 0x00	#duplicate the value in t1 into a1.
	li t3, 0x0a		#make t3 '0a' to indicate that the diplay is for memory value and not memory address.
	li s3, 0x00		#reset nibble counter to 0.
	beq a0, a0, loop	#go to subroutine 'loop' to print the value.

wait:
	csrrs s8,0x77f,x0   
	andi s6,s8,0x100     
	beq s6,x0,wait
	jr t5

exit5:    

       	lw x0, 0(sp)       #store the values of the registers back from the memory.
	lw x1, 4(sp)
	lw x2, 8(sp)
	lw x3, 12(sp)
	lw x4, 16(sp)
	lw x5, 20(sp)
	lw x6, 24(sp)
	lw x7, 28(sp)
	lw x8, 32(sp)
	lw x9, 36(sp)
	lw x10, 40(sp)
	lw x11, 44(sp)
	lw x12, 48(sp)
	lw x13, 52(sp)
	lw x14, 56(sp)
	lw x15, 60(sp)
	lw x16, 64(sp)
	lw x17, 68(sp)
	lw x18, 72(sp)
	lw x19, 76(sp)
	lw x20, 80(sp)
	lw x21, 84(sp)
	lw x22, 88(sp)
	lw x23, 92(sp)
	lw x24, 96(sp)
	lw x25, 100(sp)
	lw x26, 104(sp)
	lw x27, 108(sp)
	lw x28, 112(sp)
	lw x29, 116(sp)
	lw x30, 120(sp)
	lw x31, 124(sp)
	
	addi sp,sp,124		# increment sp value.
	ret			#return to the main program.

	
