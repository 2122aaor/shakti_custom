#this function loads the data/code from an uploaded file into the memory space specified by the user.
#the syntax to call this function is LOAD <initial memory address in 8 bit hex format>.
#the format of the uploaded code should be as follows:(1) the beginning of the file should be indicated by a '#', this indicates the point #from where the code will be stored into the memory. (2) the end of line of the file should be denoted by carriage return. (3) the end of #file should by denoted by a '$' symbol, no more characters will be stored into the memory once a $ is encountered.

.text
.align	4
.globl load
load:
	addi sp, sp, -40	 #decrement stack for store initial values of registers.
	sw ra, 40(sp)		 #store the values of the registers being used into the memory.
	sw s2, 36(sp)
	sw s1, 32(sp)
	sw s0, 28(sp)
	sw t1, 24(sp)
	sw a5, 20(sp)
	sw a4, 16(sp)
	sw a3, 12(sp)
	sw a2, 8(sp)
	sw a1, 4(sp)	
  sw t2, 0(sp)
	
	li a1, 0x00             
	addi a1, x19, 0x00	#the starting address which was stored in x19 is copied to a1.
	li s0, 0x23		#s0 stores the ascii value of '#'.
	li s1, 0x24		#s1 stores the ascii value of '$'.
	li s2, 0x0d		#s2 stores the ascii value of 'carriage return'.
	li a2, 0x00		
	li a5, 0x00
	li a3, 0x00

	li a6, 0x0a		#store the ascii value of line feed in a4.
	
polling: csrrs a2, 0x77e, x0	#read reg_rx into a2.
	 andi a3, a2, 0x100	#get the ready_bit.
   beq a3, x0, polling	#unless the bit is 1 keep polling, a 1 bit indicates that the FIFO has some value.
	 andi a4, a2, 0xff	#get the lower 8 bits into a4.
  loopback2:  
    csrrs t2,0x77f,x0         # read rg_tx.
    andi t2,t2,0x100          # get the ready_bit.
    beq t2,x0,loopback2    # unless the bit is 1 keep polling. a 1 bit indicates the FIFO has value.  
    csrrw x0,0x77f,a4         # write lower 8 bits to rg_rtx. **This will only work if rd is x0**

	 bne a4, s0, polling	#if the first character isn't '#', continue polling till a '#' is received.
  
beg:	 
   csrrs a2, 0x77e, x0	#read reg_rx into a2.
	 andi a3, a2, 0x100	#get the ready_bit.
   beq a3, x0, beg	#unless the bit is 1 keep polling, a 1 bit indicates that the FIFO has some value.
   andi a4, a2, 0xff	#get the lower 8 bits into a4.

  loopback1:  
    csrrs t2,0x77f,x0         # read rg_tx.
    andi t2,t2,0x100          # get the ready_bit.
    beq t2,x0,loopback1    # unless the bit is 1 keep polling. a 1 bit indicates the FIFO has value.  
    csrrw x0,0x77f,a4         # write lower 8 bits to rg_rtx. **This will only work if rd is x0**

	 beq a4, s1, exits	#if the character is '$', go to end1 i.e. the end of file.
	 beq a4, s2, store_line	#if the character is 'carriage return', go to endl i.e. the end of line.
	 beq a4, a6, beg	#if the character is 'carriage return', go to endl i.e. the end of line.

   slli a5,a5,0x4 # shift previously read characters left by 4.
   addi a4,a4,-48 # normalize the current read character to it to acii 0x30 (represents decimal value)
   ori x29,x0,0xa
   bltu a4,x29,cont_beg1 # if the recieved ascii is not an alphabet then exit
   addi a4,a4,-7       # if the recieved ascii is alphabet then subtract 7 to get the real value. There are 7 other ascii values between 9 and A

   cont_beg1:
     or a5,a5,a4
     beq x0,x0,beg

   store_line:
	   sw a5, 0(a1) 		#if it is 'carriage return' store the character in the memory address specified by the user.
     lui a5,0x0
	   addi a1, a1, 0x04	#increment the memory address by 1.
	   beq x0, x0, beg	#jump to label beg to get the next character.

exits:  
	lw ra, 40(sp)		#store the values of the registers back from the memory.
	lw s2, 36(sp)
	lw s1, 32(sp)
	lw s0, 28(sp)
	lw t1, 24(sp)
	lw a5, 20(sp)
	lw a4, 16(sp)
	lw a3, 12(sp)
	lw a2, 8(sp)
	lw a1, 4(sp)	
  lw t2, 0(sp)

	addi sp,sp,40		# increment sp value.
	ret			#return to the main program.

	

