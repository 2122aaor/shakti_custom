`define NUM_INSTRS 20200 
`define MEM_INIT_SIZE 1048576
`define STOP 100000


`define ENTRY_ROB_SIZE 16
`define ENTRY_ROB_INDEX_SIZE 4
`define IQ_SIZE 8
`define TOTAL_THREADS 4
`define REG_WIDTH 64
`define INSTR_WIDTH 32
`define SIMD_REG_WIDTH 256
`define REGFILE_SIZE 32
`define PRF_SIZE 64
`define ADDRESS_WIDTH 64
`define ADDRESS_SPACE 7 //?? (some MMU thing)
`define FETCH_WIDTH 2
`define ISSUE_WIDTH 4
`define MEMQ_SIZE 16
`define DCACHE_SIZE 64 
`define ICACHE_SIZE 64
`define IMM_BUF_SIZE 8
`define Addr_width 64
`define Word_size 8
`define CSR_ADDR 12
`define User_mode 0
`define Supervisor_mode 1
`define Hypervisor_mode 2
`define Machine_mode 3

`define RV64

`ifdef RV32
`define Multiplier32
`define Multiplier16
`define divider32
`endif

`ifdef RV64
`define Multiplier64
`define Multiplier32
`define Multiplier16
`define divider64
`endif

`define BTB_SIZE 16
`define BTB_BITS 4

`define FUN_UNITS 4
`define FPU_UNITS 4
`define ALU_UNITS 2
`define BRANCH_UNITS 4
`define MEM_UNITS 4

`define Ways 4
`define BLOCK_SIZE 2        // number of words in a block
`define Num_of_sets 512    // total number of sets
`define Num_of_tag_bits 19  // number of bits used to represent the tag.
`define Num_of_offset_bits 4 // number of bits used to represent the offset.
