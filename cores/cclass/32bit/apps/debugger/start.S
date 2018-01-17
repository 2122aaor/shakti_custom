.text
.align	4
.globl	main
.type	main, @function

main:

CommandInit:
  la a3, welcome_str       # load pointer to welcome string in a3.
  jal print_string
  la a3, help_str       # load pointer to help string in a3.
  jal print_string


Save_Template_Commands:

	lui x6, 0x00001    # store all the command templates from 0x1000. this is where the scratch pad memory starts.
	lui x4, 0x44524    #DREG upper 20 bits
  ori x4,x4,0x547    #DREG upper 12 bits
	sw x4, 0(x6)       # Store the command template in the scratch memory
	lui x4, 0x444d4    #DMEM upper 20 bits
  ori x4,x4,0x54d    #DMEM upper 12 bits
	sw x4, 4(x6)       # Store the command template in the scratch memory
	lui x4, 0x4d4f4    #MODM upper 20 bits
  ori x4,x4,0x44d    #MODM upper 12 bits
	sw x4, 8(x6)       # Store the command template in the scratch memory
	lui x4, 0x4c4f4    #LOAD upper 20 bits
  ori x4,x4,0x144    #LOAD upper 12 bits
	sw x4, 16(x6)       # Store the command template in the scratch memory
	lui x4, 0x52534    #RSET upper 20 bits
  ori x4,x4,0x554    #RSET upper 12 bits
	sw x4, 20(x6)       # Store the command template in the scratch memory
	lui x4, 0x48454    #HELP upper 20 bits
  li x5,0xc5         #HELP Lower bits
  slli x5,x5,4       #HELP Lower bits with shift
  or x4,x5,x4        #HELP complete command
	sw x4, 24(x6)       # Store the command template in the scratch memory
	lui x4, 0x434C5    #CLRS upper 20 bits
  ori x4,x4,0x253    #CLRS upper 12 bits
	sw x4, 28(x6)       # Store the command template in the scratch memory
   
Init1:
 
   	li x14, 0x20  # space character.
   	li x15, 0x0d  # end of line character (eol).
   
   	li x17, 0x00
   	li x24, 0x00
   	li x29, 0x00
   
Start:
   	lui x11, 0x00 # Command
   	lui x19, 0x00 # Arg1
   	lui x18, 0x00 # Arg2
   	lui x20, 0x00 # Arg3
   	jal ReadCommand
   	beq x0,x0, Compare # compare the data with the command list.
   
ReadCommand:
    add sp,sp,-4                # decrement stack for store return address.
    sw ra, 4(sp)                # store return address.

  print_cursor:  
    li x4, 0x3e
    csrrs t2,0x77f,x0         # read rg_tx.
    andi t2,t2,0x100          # get the ready_bit.
    beq t2,x0,print_cursor    # unless the bit is 1 keep polling. a 1 bit indicates the FIFO has value.  
    csrrw x0,0x77f,x4         # write lower 8 bits to rg_rtx. **This will only work if rd is x0**

  read:
    csrrs x12, 0x77e, x0        # this loop polls over the uart unless a command is received.
   	andi x8, x12, 0x100
   	beq x8, x0, read
   	andi x12, x12, 0xff         # capture the character in x12.
  
  loopback:  
    csrrs t2,0x77f,x0         # read rg_tx.
    andi t2,t2,0x100          # get the ready_bit.
    beq t2,x0,loopback    # unless the bit is 1 keep polling. a 1 bit indicates the FIFO has value.  
    csrrw x0,0x77f,x12         # write lower 8 bits to rg_rtx. **This will only work if rd is x0**
   	
    beq x12, x14, Next_arg  # if recevied character is space then capture new argument.
    beq x12, x15, Check_for_end  # if recevied character is eol jump to exit
  
    li x29,0x3
    beq x24,x29,Arg3
    li x29,0x2
    beq x24,x29,Arg2
    li x29,0x1
    beq x24,x29,Arg1

    slli x11,x11,0x8 # command register being updated with new character. storing in ascii form itself without changes.
    or x11,x11,x12   
    beq x0, x0, read # go back to receive the new character.
    
    Arg1: # this will be given by user in hexadecimal format
      slli x19,x19,0x4 # shift previously read characters left by 4.
      addi x12,x12,-48 # normalize the current read character to it to acii 0x30 (represents decimal value)
      ori x29,x0,0xa
      blt x12,x29,cont_arg1 # if the recieved ascii is not an alphabet then exit
      addi x12,x12,-7       # if the recieved ascii is alphabet then subtract 7 to get the real value. There are 7 other ascii values between 9 and A

      cont_arg1:
        or x19,x19,x12
        beq x0, x0, read

    Arg2: # this will be provided as a decimal by the user. 
      li x29,0xa
      mul x18,x18,x29
      addi x12,x12,-48
      add x18,x18,x12
      beq x0, x0, read

    Arg3:
      slli x20,x20,0x4 # shift previously read characters left by 4.
      addi x12,x12,-48 # normalize the current read character to it to acii 0x30 (represents decimal value)
      ori x29,x0,0x9
      blt x12,x29,cont_arg3 # if the recieved ascii is not an alphabet then exit
      addi x12,x12,-7       # if the recieved ascii is alphabet then subtract 7 to get the real value. There are 7 other ascii values between 9 and A

      cont_arg3:
        or x20,x20,x12
        beq x0, x0, read
    
    Next_arg:
      addi x24,x24,0x1   # increment to the argument being recieved hencforth.
      beq x0,x0, read

    Check_for_end:
      lw ra, 4(sp)
      add sp,sp,4
      ret
   
   
Compare:

	lui x6, 0x00001
	
	lw x9, 0(x6)
  beq x11, x9, DREG
	lw x9, 4(x6)
  beq x11, x9, DMEM
	lw x9, 8(x6)
	beq x11, x9, MODM
	lw x9, 16(x6)
  beq x11, x9, LOAD
	lw x9, 20(x6)
  beq x11, x9, RSET
	lw x9, 24(x6)
  beq x11, x9, HELP
	lw x9, 28(x6)
 	beq x11, x9, CLRS
	#lw x9, 32(x6)
  #beq x11, x9, CRUN
	
  la a3, invalid_str       # load pointer to invalid string in a3.
  jal print_string    # print string
  beq x0,x0, Init1    # go back to reading the next command
   
DREG:
   	jal display_register
   	beq x0, x0, Init1
   
DMEM:
  	jal display_memory
   	beq x0,x0, Init1
   
MODM:
   	jal modmem
   	beq x0,x0, Init1
LOAD:
 
   	jal load
   	beq x0,x0,Init1
   
RSET:
    jal clearscreen
   	beq x0,x0,soft_reset
   
HELP:
  	
    la a3, help_str       # load pointer to string in a3.
  	jal print_string
   	beq x0,x0,Init1
   
CLRS:
    jal clearscreen
   	beq x0, x0, Init1
   
#CRUN:
#
#	addi a0, x0, 0x00
#
#	lw x5, 0(a0)
#
#	jr x5
#   	beq x0,x0,exit
# 
# 
#exit:
#   
#   	#beq x0, x0, Init1
   
print_string:
  addi sp,sp,-12
  sw a2, 12(sp)
  sw a1, 8(sp)
  sw t2, 4(sp)
  sw ra, 0(sp)
  li a1, 0x24
  rep:	
	  lb a2,0(a3)            # read the next byte of string into register x1
    beq a1,a2,exit_string        # if $ symbol read then end display

  rep1:  
    csrrs t2,0x77f,x0         # read rg_tx.
    andi t2,t2,0x100          # get the ready_bit.
    beq t2,x0,rep1            # unless the bit is 1 keep polling. a 1 bit indicates the FIFO has value.  
    csrrw x0,0x77f,a2         # write lower 8 bits to rg_rtx. **This will only work if rd is x0**
    addi a3,a3,1              # increment a1 to point to next byte
    beq a0,a0,rep             # keep doing this crap forever n ever
  
  exit_string: 
    lw ra,0(sp)
    lw t2, 4(sp)
    lw a1, 8(sp)
    lw a2, 12(sp)
    addi sp,sp,12
  ret
   
exit:
	  j exit
	  .size	main, .-main
    .ident "TRIAL"

.data
	  help_str :    .asciz " \n\n DREG: Display REGISTERS \n CLRS: CLEAR SCREEN \n DMEM: Hex-Address Dec-Location \n MODM: Number of locs - Starting Addr - Value - TO CHANGE MEMORY \n RSET: SOFTWARE RESET \n HELP: DISPLAY COMMANDS \n LOAD: Serial load \n $"
    welcome_str:  .asciz "\n\n Welcome to SHAKTI C-Class 32-bit controller \n $"
    invalid_str:  .asciz " Invalid cmd \n $"
