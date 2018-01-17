
OUTPUT_ARCH( "riscv" )

/*----------------------------------------------------------------------*/
/* Sections                                                             */
/*----------------------------------------------------------------------*/

SECTIONS
{

  /* text: test code section */
  . = 0x80000000;

  .text : { *(.text) }


  /* data segment */
  . = 0x80010000;
  .data : { *(.data) }

  .tohost ALIGN(0x100) : { *(.tohost) }

  /* End of uninitalized data segement */
  _end = .;
}

