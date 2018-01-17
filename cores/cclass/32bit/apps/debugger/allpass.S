.text
.align	4
.globl allpass

allpass:

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

regtest_allones:
	
	lui x5, 0xfffff
	li x6, 0xff
	add x5, x6, x5
	srli x6, x5, 0x08
	or x5, x5, x6
	add x2, x5,x0
	add x3, x2,x0
	add x4, x3,x0
	add x5, x4,x0
	add x6, x5,x0
	add x7, x6,x0
	add x8, x7,x0
	add x9, x8,x0
	add x10, x9,x0
	add x11, x10,x0
	add x12, x11,x0
	add x13, x12,x0
	add x14, x13,x0
	add x15, x14,x0
	add x16, x15,x0
	add x17, x16,x0
	add x18, x17,x0
	add x19, x18,x0
	add x20, x19,x0
	add x21, x20,x0
	add x22, x21,x0
	add x23, x22,x0
	add x24, x23,x0
	add x25, x24,x0
	add x26, x25,x0
	add x28, x26,x0
	#add x28, x27,x0
	add x29, x28,x0
	add x30, x29,x0
	add x31, x30,x0
    
	xor x2, x5, x31
	bne x0, x2, error_allones

pass_allones:

	la a3, str3
	jal a4, Welcome_str
	beq x0, x0, regtest_allzeroes

error_allones:

	la a3, str1
	jal a4, Welcome_str

regtest_allzeroes:

	li x0, 0x00
	li x5, 0x000
	lui x5, 0x00000
	add x2, x5,x0
	add x3, x2,x0
	add x4, x3,x0
	add x5, x4,x0
	add x6, x5,x0
	add x7, x6,x0
	add x8, x7,x0
	add x9, x8,x0
	add x10, x9,x0
	add x11, x10,x0
	add x12, x11,x0
	add x13, x12,x0
	add x14, x13,x0
	add x15, x14,x0
	add x16, x15,x0
	add x17, x16,x0
	add x18, x17,x0
	add x19, x18,x0
	add x20, x19,x0
	add x21, x20,x0
	add x22, x21,x0
	add x23, x22,x0
	add x24, x23,x0
	add x25, x24,x0
	add x26, x25,x0
	add x28, x26,x0
	#add x28, x27,x0
	add x29, x28,x0
	add x30, x29,x0
	add x31, x30,x0
    
	xor x2, x5, x31
	bne x0, x2, error_allzeroes

pass_allzeroes:

	la a3, str4
	jal a4, Welcome_str
	beq x0, x0, exit2

error_allzeroes:

	la a3, str2
	jal a4, Welcome_str	
	beq x0, x0, exit2
	#li s6, 0x03

Welcome_str:
	   li a1, 0x24
	   li a0, 0x00
	   rep:	
	   lb a2,0(a3)            # read the next byte of string into register x1
           beq a1,a2,exitp         # if $ symbol read then end display
  rep1:  
     csrrs t2,0x77f,x0     # read rg_tx.
     andi t3,t2,0x100      # get the ready_bit.
     beq t3,a0,rep1       # unless the bit is 1 keep polling. a 1 bit indicates the FIFO has value.  
     csrrw x0,0x77f,a2       # write lower 8 bits to rg_rtx. **This will only work if rd is x0**
     addi a3,a3,1              # increment a1 to point to next byte
     beq a0,a0,rep   

exitp:

     jr a4

exit2:
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


.data

    str1 :    .asciz "error in all pass one test!\n$"
    str2 :    .asciz "error in all pass zero test!\n$"
    str3 :    .asciz "all ones test passed!\n$"
    str4 :    .asciz "all zeroes test passed!\n$"
    
