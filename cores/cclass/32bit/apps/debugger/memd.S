#this function displays the value stored in the memory addresses specified by the user.
#the syntax to call this function is DMEM <starting address in  bit hex format> <number of memory addresses to be displayed (index - 3 bit #decimal value)>
#in order to display the address and value in the right order, the required nibbles are shifted appropriately to the left most 4 bits by the #use of the nibble counter and a variable shift value.  
#the same 'print' subroutine is used to print both the address and value and hence in order to differentiate between the two a seperate #register is used.
#the display prints four memory values in a line with an 8 space gap between each values.

.text
.align	4
.globl display_memory
display_memory:

	addi sp, sp, -72	 #decrement stack for store initial values of registers.
	sw ra, 72(sp)		 #store the values of the registers being used into the memory.
	sw s8, 68(sp)	
	sw s7, 64(sp)
	sw s6, 60(sp)
	sw s5, 56(sp)
	sw s4, 52(sp)
	sw s3, 48(sp)
	sw s2, 44(sp)
	sw s1, 40(sp)
	sw t6, 36(sp)
	sw t5, 32(sp)
	sw t4, 28(sp)
	sw t3, 24(sp)
	sw t2, 20(sp)
	sw t1, 16(sp)
	sw a4, 12(sp)
	sw a3, 8(sp)
	sw a1, 4(sp)	
	sw a0, 0(sp)           
	
	li a3, 0x00
	li a4, 0x00
	
	addi a3, x18, 0x00	#store the index value into a3.
	addi a4, x19, 0x00	#store the starting address into a4.
	
	li a0, 0x04        	#byte threshold. 
	li s2, 0x20		#stores the ascii value of space into s2.
	li t2, 0x00	   	#counter to keep track of the number of mem locations
	li s1, 0x08        	#nibble threshold
	li s5, 0x0a        	#stores the ascii value of 'a' into s5.
	li t4, 0x00        	#line change counter
	li a1, 0x00
       
 
main_sub:
	li t6, 28		#shift value for the first nibble (to bring the leftmost nibble to the least significant nibble).
	li s2, 0x3a		#stores the ascii value of ':' in s2.
	addi t4, t4, 0x01	#increment the line change counter to indicate that a memory value is going to be printed.
	addi a1, a4, 0x00  	#stores the address value in a1
	li s3, 0x00        	#nibble counter
	li s7, 0x00        	#byte counter
	li t3, 0x00		#to differentiate between memory address display and memory value display.
loop:
	srl s4, a1, t6		#shift a1 by the required value and store it in s4.
	addi t6, t6, -4		#decrement the shift value by 4.
	andi s4, s4, 0x0f  	#to mask the first nibble alone.
	bge s4, s5, alpha1  	#check if alphabet.
	addi s4, s4, 0x30  	#offset for numbers.
	beq a0, a0, print	#jump to print subroutine.

alpha1:
	addi s4, s4, 0x57  	#offset for alphabets.
	beq a0, a0, print 	#jump the to print subroutine.
    	
print:	
  jal t5, wait       	#call the wait loop.
  nop
	csrrw x0,0x77f,s4 	#output the value via reg_tx.
	addi s3, s3, 0x01  	#upgrade the nibble counter.
	bne s3, s1, loop   	#check if all 8 nibbles are over, if not go to the subroutine 'loop' inorder to print the next nibble.
	beq t3, s5, next   	#jump to subroutine 'next' if the display was for the value, else proceed to display the value.
	addi s4, s2, 0x00 	#store s2 value in s4 i.e. store ascii value for ':' in s4.

spaces:
	jal t5, wait		#call the wait loop.
	csrrw x0, 0x77f, s4	#output the value via reg_tx.

	lw t1, 0(a4)		#load the value stored in the memory address into t1.
	
	li t6, 28		#reset the nibble offset to 28 for memory value display.
	addi a1, t1, 0x00	#duplicate the value in t1 into a1.
	li t3, 0x0a		#make t3 '0a' to indicate that the diplay is for memory value and not memory address.
	li s3, 0x00		#reset nibble counter to 0.
	beq a0, a0, loop	#go to subroutine 'loop' to print the value.

next:	li s2, 0x9		#store the ascii value of horizontal tab in to s2.
	jal t5, wait		#call the wait loop.
	csrrw x0, 0x77f, s2	#output the value via reg_tx.
	
	beq t4, a0, line_change	#if four memory values have been displayed, print a line feed.
next1:	
	addi a4, a4, 0x04	#increment the memory address by 4 to access the next value.
	addi t2, t2, 0x01	#increment the memory index counter.
	blt t2, a3, main_sub	#check if the index has reached the index threshold if not go back to preint the next memory value else go to 					#end of function.
	beq a0, a0, exit2 	#go to end of function.

line_change:
	li t4, 0xa		#store the ascii value of line feed in t4.
	jal t5, wait		#call the wait loop.
	csrrw x0, 0x77f, t4	#output the value via reg_tx.

	li t4, 0x00		#clear the value in t4 as it is also the line change counter.
	beq a0, a0, next1	#go to subroutine 'next1' to get the next memory addrress.
wait:
	csrrs s8,0x77f,x0   	
	andi s6,s8,0x100     
	beq s6,x0,wait
	jr t5

exit2:
	
	lw ra, 72(sp)		 #store the values of the registers back from the memory.
	lw s8, 68(sp)
	lw s7, 64(sp)
	lw s6, 60(sp)
	lw s5, 56(sp)
	lw s4, 52(sp)
	lw s3, 48(sp)
	lw s2, 44(sp)
	lw s1, 40(sp)
	lw t6, 36(sp)
	lw t5, 32(sp)
	lw t4, 28(sp)
	lw t3, 24(sp)
	lw t2, 20(sp)
	lw t1, 16(sp)
	lw a4, 12(sp)
	lw a3, 8(sp)
	lw a1, 4(sp)	
	lw a0, 0(sp) 

    	addi sp,sp,72		# increment sp value.
    	ret			#return to the main program.
	
