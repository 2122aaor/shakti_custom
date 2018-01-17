.globl _endProgram
.align 3

_endProgram:
  li a4,1
  auipc sp,0
  sw a4, 28(sp) # store 1 to tohost.
  loop: j loop

.section ".tohost","aw",@progbits
.align 4
.globl tohost
tohost: .word 0
.align 4
.globl fromhost
fromhost: .word 0 

