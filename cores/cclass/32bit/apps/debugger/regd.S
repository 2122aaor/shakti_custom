#this function displays the value stored in the 32 registers
#the syntax to call this function is DREG 
#in order to display the index and value in the right order, the required nibbles are shifted appropriately to the left most 4 bits by the #use of the nibble counter and a variable shift value.  
#the same 'print' subroutine is used to print both the address and value and hence in order to differentiate between the two a seperate #register is used.
#the display prints four register values in a line with an 8 space gap between the values.


.text
.align	4
.globl display_register

display_register:
Init:
 
	sw x0, 0(sp)				#store register values onto stack
	sw x1, -4(sp)
	sw x2, -8(sp)
	sw x3, -12(sp)
	sw x4, -16(sp)
	sw x5, -20(sp)
	sw x6, -24(sp)
	sw x7, -28(sp)
	sw x8, -32(sp)
	sw x9, -36(sp)
	sw x10, -40(sp)
	sw x11, -44(sp)
	sw x12, -48(sp)
	sw x13, -52(sp)
	sw x14, -56(sp)
	sw x15, -60(sp)
	sw x16, -64(sp)
	sw x17, -68(sp)
	sw x18, -72(sp)
	sw x19, -76(sp)
	sw x20, -80(sp)
	sw x21, -84(sp)
	sw x22, -88(sp)
	sw x23, -92(sp)
	sw x24, -96(sp)
	sw x25, -100(sp)
	sw x26, -104(sp)
	sw x27, -108(sp)
	sw x28, -112(sp)
	sw x29, -116(sp)
	sw x30, -120(sp)
	sw x31, -124(sp)

	

	li a3, 0x20        #store the no_of_register count = 32 into a3.
	addi a4,sp, 0
	li a0, 0x04        #byte threshold 
	li t2, 0x00	   #counter to keep track of the number of register locations
	li s1, 0x08        #nibble threshold
	li s5, 0x0a        #hex for 'a'
	li t4, 0x00        #line change counter
	li s9, 0x00
	li t6, 4
main_subr:
	li t6,  4   	   # Shift amount for register index display
	li s2, 0x3a	   # Ascii value for colon	
	addi t4, t4, 0x01  #increment the line change counter to indicate that a register value is going to be printed.
	li s3, 0x06        #nibble counter
	li s7, 0x00        #byte threshold
	addi a1, s9, 0x00  
	li t3, 0x00
	
loopr:
	srl s4, a1, t6       #shift a1 by the required value and store it in s4.
	addi t6, t6, -4      #decrement the shift value by 4.
	andi s4, s4, 0x0f    #to mask the first nibble alone
	bge s4, s5, alpha1r  #check if alphabet
	addi s4, s4, 0x30    #offset for numbers
	beq a0, a0, printr   #jump to print subroutine.


alpha1r:
	addi s4, s4, 0x57    #offset for alphabets
	beq a0, a0, printr   #jump the to print subroutine.
    	
printr:	
        jal t5, waitr       #call the wait loop
	csrrw x0,0x77f,s4   #output the value via 0x77f
	addi s3, s3, 0x01   #upgrade the nibble counter
	bne s3, s1, loopr   #check if all 8 nibbles are over
	beq t3, s5, nextr   #jump to next if the display was for the value
	addi s4, s2, 0x00   #store s2 value in s4 i.e. store ascii value for ':' in s4.

spacesr:
	jal t5, waitr               #call the wait loop.
	csrrw x0, 0x77f, s4         #output the value via reg_tx.
	lw t1, 0(a4)                #load the value stored in the memory address into t1.
	li t6, 28                   #reset the nibble offset to 28 for register value display.
	addi a1, t1, 0x00           #duplicate the value in t1 into a1.
	li t3, 0x0a                 #make t3 '0a' to indicate that the diplay is for register value and not register index.
	li s3, 0x00                 #reset nibble counter to 0.
	beq a0, a0, loopr           #go to subroutine 'loop' ti print the value.

nextr:	li s4, 0x9                  #store the ascii value of horizontal tab in to s2.
	jal t5, waitr               #call the wait loop.
	csrrw x0, 0x77f, s4         #output the value via reg_tx.
	beq t4, a0, line_changer    #if four register values have been displayed, print a line feed.
	
next1r:	
	addi a4, a4, -4             #increment the memory address by 4 to access the next value.
	addi t2, t2, 0x01           #increment the register index counter.
	addi s9, s9, 0x01           
	addi a6, s9, 0x00
	andi s9, s9, 0x0f
	blt s9, s5,herer
	andi a6, a6, 0xf0
	addi a6, a6, 0x10
herer:
	addi s9, a6, 0x00
	bne t2, a3, main_subr
	beq a0, a0, exit_DREG

line_changer:
	li t4, 0xa		     #store the ascii value of line feed in t4.
	jal t5, waitr                #call the wait loop.
	csrrw x0, 0x77f, t4          #output the value via reg_tx.
	li t4, 0x00                  #clear the value in t4 as it is also the line change counter.
	beq a0, a0, next1r           #go to subroutine 'next1' to get the next memory addrress.
waitr:
	csrrs s8,0x77f,x0   
	andi s6,s8,0x100     
	beq s6,x0,waitr
	jr t5

exit_DREG:  
  #lw ra, -8(sp) # since the stack pointer hasn't changed and ra was already stored in -8(sp) at the begining.
  
  	lw x0, 0(sp)
	lw x1, -4(sp)
	lw x2, -8(sp)
	lw x3, -12(sp)
	lw x4, -16(sp)
	lw x5, -20(sp)
	lw x6, -24(sp)
	lw x7, -28(sp)
	lw x8, -32(sp)
	lw x9, -36(sp)
	lw x10, -40(sp)
	lw x11, -44(sp)
	lw x12, -48(sp)
	lw x13, -52(sp)
	lw x14, -56(sp)
	lw x15, -60(sp)
	lw x16, -64(sp)
	lw x17, -68(sp)
	lw x18, -72(sp)
	lw x19, -76(sp)
	lw x20, -80(sp)
	lw x21, -84(sp)
	lw x22, -88(sp)
	lw x23, -92(sp)
	lw x24, -96(sp)
	lw x25, -100(sp)
	lw x26, -104(sp)
	lw x27, -108(sp)
	lw x28, -112(sp)
	lw x29, -116(sp)
	lw x30, -120(sp)
	lw x31, -124(sp)

  ret
