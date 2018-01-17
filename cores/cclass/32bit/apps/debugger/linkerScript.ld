/*======================================================================*/
/* Proxy kernel linker script                                           */
/*======================================================================*/
/* This is the linker script used when building the proxy kernel. */

/*----------------------------------------------------------------------*/
/* Setup                                                                */
/*----------------------------------------------------------------------*/

/* The OUTPUT_ARCH command specifies the machine architecture where the
   argument is one of the names used in the BFD library. More
   specifically one of the entires in bfd/cpu-mips.c */

OUTPUT_ARCH( "riscv" )

/* The ENTRY command specifies the entry point (ie. first instruction
   to execute). The symbol _start should be defined in each test. */

ENTRY( _start )

/*----------------------------------------------------------------------*/
/* Sections                                                             */
/*----------------------------------------------------------------------*/

SECTIONS
{

  /* text: test code section */
  . = 0x80000000;


  .text : 
  {
    *crt.o(.text) 
    *lib_a*(.text)
    *(EXCLUDE_FILE (*endProgram.o)*.text*)
    *endProgram.o(.text)
 }
    .tohost : { *(.tohost) }

/*    . = 0x3000; */
  .rodata : {  * (*.rodata*);   }
  /* data segmemt */
  .data : { *(.data) }
  .bss : { *(.bss) }

  
  /* thread-local data segment */
  .tbss : { 
    *crt.o(.tbss) /* Make sure tls_start is the first TLS symbol */
    *(.tbss)
  }
  .tdata : { *(.tdata) }

   . = 0x8004000; 
  .print_data_sec : {  * (.print_data_sec);   }
  /* End of uninitalized data segement */
  _end = .;
}

