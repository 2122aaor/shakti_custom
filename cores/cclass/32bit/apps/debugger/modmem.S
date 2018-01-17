#this function modifies the value stored in the memory addresses specified by the user and displays the modifies memory addresses.
#the syntax to call this function is MODM <starting address in  bit hex format> <number of memory addresses to be displayed (index - 3 bit #decimal value)> <new value in 8 bit hex format>
#in order to display the address and value in the right order, the required nibbles are shifted appropriately to the left most 4 bits by the #use of the nibble counter and a variable shift value.  
#the same 'print' subroutine is used to print both the address and value and hence in order to differentiate between the two a seperate #register is used.
#the display prints four memory values in a line with an 8 space gap between the values.

.text
.align	4
.globl modmem

modmem:   
	addi sp, sp, -4	 #decrement stack for store initial values of registers.
	sw ra, 4(sp)		 #store the values of the registers being used into the memory.
	sw t2, 0(sp)

  lui t2,0x0 
main_sub:
	sw x20, 0(x19)		#store the new value into the memory address.
	addi x19, x19, 0x04	#increment the memory address by 4 to access the next value.
	addi t2, t2, 0x01	#increment the memory index counter.
	bne t2, x18, main_sub	#check if the index has reached the index threshold if not go back to preint the next memory value else go to 					#end of function.
	
exit3:
	lw ra, 4(sp)		 #store the values of the registers back from the memory.
	lw t2, 0(sp)

  addi sp,sp,4		# increment sp value.
  ret			#return to the main program.
	
