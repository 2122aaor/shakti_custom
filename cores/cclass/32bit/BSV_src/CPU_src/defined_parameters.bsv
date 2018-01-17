`define Reg_width 32 // the register data width of the processor.
`define RegFileSize 32 // describes the size of ht register file in the processor.
`define Addr_width 32 // the address width
`define Loop 8
`define BAUD_RATE 130
`define Addr_space 17	//since we are leaving off the lower 2 bits of address(byte addressable memory), we have to 
`ifdef simulate
  `define BAUD_RATE 5 //130 //
  `define Addr_space 31	//since we are leaving off the lower 2 bits of address(byte addressable memory), we have to 
`endif

// Branch_predictor_paramters
/////////////////////////// CACHE RELATED PARAMETERS ////////////////////////////////
`define DCACHE_ADDR 32
`define DCACHE_WAYS 2
`define DCACHE_BLOCK_SIZE 8
`define DCACHE_WORD_SIZE 4
`define DCACHE_SETS 32

`define ICACHE_ADDR 32
`define ICACHE_WAYS 2
`define ICACHE_BLOCK_SIZE 8
`define ICACHE_WORD_SIZE 4
`define ICACHE_SETS 32
/////////////////////////////////////////////////////////////////////////////////////

`define MISA_BITS   'h40141129 // 'h40101121 // A + F + I + M + U 
`define MTVEC_DEFUALT       30'h00000000
`define UTVEC_DEFAULT       30'h00000000
/////////////////////////// Register Mapping for Machine Mode Regs /////////////////
`define MVENDORID   'hF11 // Vendor ID                                                  
`define MARCHID     'hF12 // Architecture ID                                           
`define MIMPID      'hF13 // Implementation ID                                        
`define MHARTID     'hF14 // Hardware Thread ID                                      
`define MSTATUS     'h300 // Machine Status register                                
`define MISA        'h301 // ISA and extensions                                     
`define MEDELEG     'h302 // Machine exception delegation                               
`define MIDELEG     'h303 // Machine interrupt delegation                               
`define MIE         'h304 // Machine interrupt enable                                   
`define MTVEC       'h305 // Machine trap-handler base address                          
`define MSCRATCH    'h340 // Scratch rgister for machine trap hanglers                  
`define MEPC        'h341 // Machine exception program counter                          
`define MCAUSE      'h342 // Machine trap cause                                         
`define MBADADDR    'h343 // Machine bad address                                        
`define MIP         'h344 // Machine interrupt pending                                  
`define MBASE       'h380 // Base register                                              
`define MBOUND      'h381 // Bound register                                             
`define MIBASE      'h382 // Instruction Base register                                  
`define MIBOUND     'h383 // Instruction Bound register                                 
`define MDBASE      'h384 // Data Base Register                                         
`define MDBOUND     'h385 // Data Bound Register                                        
`define MCYCLE      'hB00 // Machine cycle counter                                      
`define MINSTRET    'hB02 // Machine instructions retired.                              
`define MCYCLEH     'hB80 // Upper 32 bits of mcycle                                   
`define MINSTRETH   'hB82 // Upper 32 bits of minstret.                                 
`define MUCOUNTEREN 'h320 // User-mode counter enable.                                  
`define UARTTX      'h77f // 
`define UARTRX      'h77e // 
`define MICACHEMISS	'hB03 // Captures the number of cache misses in the instruction cache.
`define MDCACHEMISS	'hB04	// Captures the number of cache misses in the data cache.

`define USTATUS     'h000 // User status register
`define UIE         'h004 // User interrupt enable register
`define UTVEC       'h005 // User trap handler base address
`define USCRATCH    'h040 // Scratch register for user trap handlers
`define UEPC        'h041 // User exception program counter
`define UCAUSE      'h042 // User trap cause
`define UBADADDR    'h043 // User bad address
`define UIP         'h044 // User interrupt pending
`define FFLAGS      'h001 // FP Accrued exceptions
`define FRM         'h002 // FP Dynamic rounding mode
`define FCSR        'h003 // FP Control and status register
`define UCYCLE      'hC00 // cycle counter for RDCYCLE instruction.
`define UTIME       'hC01 // Tiemr for RDTIME instruction
`define UINSTRET    'hC02 // Instruction retired counter for RDINSTRET
`define UCYCLEH     'hC80 // Upper 32bits of UCYCLE
`define UTIMEH      'hC81 // Upper 32bits of UTIME
`define UINSTRETH   'hC82 // Upper 32bits of UINSTRET
////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////// funct3 defintions for ISA ////////////////////
`define JALR_f3 'b000
`define BEQ_f3	'b000
`define BNE_f3 	'b001
`define BLT_f3	'b100
`define BGE_f3	'b101
`define BLTU_f3	'b110
`define BGEU_f3	'b111
`define LB_f3		'b000
`define Lh_f3		'b001
`define LW_f3		'b010
`define LBU_f3	'b100
`define LHU_f3	'b101
`define SB_f3		'b000
`define SH_f3		'b001
`define SW_f3		'b010
`define ADD_SUB_f3	'b000
`define SLT_SLTI_f3	'b010
`define SLTU_SLTIU_f3	'b011
`define XOR_XORI_f3	'b100
`define OR_ORI_f3	'b110
`define AND_ANDI_f3	'b111
`define SLL_SLLI_f3	'b001
`define SR_SRI_f3	'b101
`define ECALL_f3	'b000
`define EBREAK_f3	'b000
`define CSRRW_f3	'b001
`define CSRRS_f3	'b010
`define CSRRC_f3	'b011
`define CSRRWI_f3	'b101
`define CSRRSI_f3	'b110
`define CSRRCI_f3	'b111
`define MUL_f3		'b000
`define MULH_f3		'b001
`define MULHSU_f3	'b010
`define MULHU_f3	'b011
`define DIV_f3		'b100
`define DIVU_f3		'b101
`define REM_f3		'b110
`define REMU_f3		'b111
`define ATOMIC_f3	'b010
`define FENCE_f3	'b000
`define	FENCEI_f3	'b001
/////////////////////////////////////////////////////////////////////////
////////////////////// opcode definitions of ISA ////////////////////////
`define LUI_op				'b01101
`define AUIPC_op			'b00101
`define JAL_op				'b11011
`define JALR_op				'b11001
`define BRANCH_op			'b11000
`define LOAD_op				'b00000
`define FLOAD_op			'b00001
`define STORE_op			'b01000
`define FSTORE_op			'b01001
`define IMM_ARITH_op	'b00100
`define	ARITH_op			'b01100
`define CSR_op				'b11100
`define MULDIV_op			'b01100
`define ATOMIC_op			'b01011
`define FMADD_op  		'b10000
`define FMSUB_op			'b10001
`define FNMSUB_op			'b10010
`define FNMADD_op			'b10011
`define	SPFLOAT_op		'b10100
`define FENCE_op			'b00011
//////////////////////////////////////////////////////////////////////////
/////////////// funct7 deifnition of ISA /////////////////////////////////
`define	SLLI_f7		'b0000000
`define	LOGIC_SHIFT_f7		'b0000000
`define	ARITH_SHIFT_f7		'b0100000
`define	ARITH_f7		'b0000000
`define SUB_f7			'b0100000
`define MULDIV_f7		'b0000001
`define	LR_f5				'b00010
`define	SC_f5				'b00011
`define AMOSWAP_f5	'b00001
`define	AMOADD_f5		'b00000
`define	AMOXOR_f5		'b00100
`define	AMOAND_f5		'b01100
`define	AMOOR_f5		'b01000
`define	AMOMIN_f5		'b10000
`define	AMOMAX_f5		'b10100
`define	AMOMINU_f5	'b11000
`define	AMOMAXU_f5	'b11100
///////////////////////////////////////////////////////////////////////////
// TLM2 Request Response definitions for Processor to Bus connection
`define TLM_PRM_CPU_REQ 4, 32, 32, 5, Bit #(0)
`define TLM_PRM_CPU_RSP 4, 32, 32, 5, Bit #(0)

// TLM2 Request Response definitions for Memory to Bus connection
`define TLM_PRM_MEM_REQ 4, 32, 32, 5, Bit #(0)
`define TLM_PRM_MEM_RSP 4, 32, 32, 5, Bit #(0)

// Axi Request Response definitions for Processor as a Master
`define AXI_PRM_CPU	4, 32, 32, 5, Bit #(0)	// Fabric Interface
`define AXI_XTR_CPU TLMRequest #(`TLM_PRM_CPU_REQ), TLMResponse #(`TLM_PRM_CPU_RSP), `AXI_PRM_CPU // Transactor Interface

// Axi Request Response definitions for Memory as a Slave
`define AXI_PRM_MEM	4, 32, 32, 5, Bit #(0)	// Fabric Interface
`define AXI_XTR_MEM TLMRequest #(`TLM_PRM_MEM_REQ), TLMResponse #(`TLM_PRM_MEM_RSP), `AXI_PRM_MEM // Transactor Interface
///////////////////////////////////////////////////////////////////////////////
