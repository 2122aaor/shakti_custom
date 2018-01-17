package All_types_d;
  import riscv_types::*;
  typedef enum {Invld,FillReq,WrBack,FillResp} LdStatus_d deriving(Bits,Eq,FShow);

  typedef struct{
    Bit#(TMul#(8,_word_size)) data_word;
    Bit#(addr_width) address;
  }Recent_access_d#(numeric type addr_width, numeric type _word_size, numeric type _block_size) deriving(Eq,Bits);

  typedef struct{
    Bit#(addr_width) address;
    Bit#(TMul#(8,TMul#(word_size,block_size))) data_line;
    Bit#(2) transfer_size;
    Bit#(3) burst_mode;
  }WriteBack_structure_d#(numeric type addr_width,numeric type word_size, numeric type block_size) deriving(Bits,Eq);

  typedef struct{
    Bit#(addr_width) address; // 32 bit address.
    Bit#(2) transfer_size; // 0-8 bits. 1-16 bits. 2-32 bits 3-64 bits.
	Bit#(1) u_signed;      // 1-unsigned 0-signed
    Bit#(1) cache_enable; // 0 cache disabled 1 cache enabled.
    Access_type_d ld_st; // 0- read 1-write
    Bit#(TMul#(8,word_size)) write_data; // 32 bit data to be written 
    Bit#(TLog#(prf_size)) destination_tag;
  }From_Cpu_d#(numeric type addr_width,numeric type word_size,numeric type prf_size) deriving(Bits,Eq);

  typedef struct{
    Access_type_d ld_st;
    Bit#(TMul#(8,_word_size)) data_word;
    Bit#(TMul#(8,_word_size)) write_data;
    Bit#(TLog#(TMul#(8,_word_size))) upper_offset;
    Bit#(TLog#(TMul#(8,_word_size))) lower_offset;
    Bit#(TAdd#(TLog#(cbuff_size),1)) token;
    Bit#(2) transfer_size;
	Bit#(1) u_signed;      // 1-unsigned 0-signed
    Bit#(TLog#(ways)) replace_block;
    Bit#(addr_width) address;
    Bit#(TLog#(prf_size)) destination_tag;
  }Hit_structure_d#(numeric type addr_width,numeric type _word_size,numeric type _block_size, numeric type cbuff_size,numeric type ways,numeric type prf_size) deriving(Bits,Eq);
    
  typedef struct{
    Access_type_d ld_st;
    Bit#(addr_width) address;
    Bit#(2) transfer_size;
	Bit#(1) u_signed;      // 1-unsigned 0-signed
    Bit#(TMul#(8,_word_size)) write_data;
    Bit#(TLog#(TMul#(8,_word_size))) upper_offset;
    Bit#(TLog#(TMul#(8,_word_size))) lower_offset;
    Bit#(TAdd#(TLog#(cbuff_size),1)) token;
    Bit#(TLog#(ways)) replace_block;
    Bit#(TLog#(prf_size)) destination_tag;
  }WaitBuff_d#(numeric type addr_width, numeric type _word_size,numeric type _block_size,numeric type cbuff_size,numeric type ways,numeric type prf_size) deriving(Bits,Eq);
  
  typedef struct{
    Bit#(TMul#(word_size,8)) data_word;
    Bit#(1) bus_error;
    Bit#(1) mis_aligned_error;
    Bit#(TLog#(prf_size)) destination_tag;
	Bit#(2) transfer_size;
	Bit#(1) u_signed;
  }To_Cpu_d#(numeric type addr_width, numeric type word_size,numeric type prf_size) deriving(Bits,Eq);

  typedef struct{
    From_Cpu_d#(addr_width,word_size,prf_size) request;
    Bit#(TAdd#(TLog#(cbuff_size),1)) token;
  }Cpu_req_with_token_d#(numeric type addr_width,numeric type word_size,numeric type cbuff_size,numeric type prf_size) deriving(Bits,Eq);
  
  typedef struct{
    To_Cpu_d#(addr_width,word_size,prf_size) response;
    Bit#(TAdd#(TLog#(cbuff_size),1)) token;
    Access_type_d ld_st;
  }Cpu_resp_with_token_d#(numeric type addr_width, numeric type cbuff_size,numeric type word_size,numeric type prf_size) deriving(Bits,Eq);
  
  typedef struct{
    Bit#(addr_width) address;
    Bit#(2) transfer_size;
    Bit#(3) burst_mode;
    Access_type_d ld_st;
    Bit#(TMul#(8,_word_size)) write_data;
  } To_Memory_d#(numeric type addr_width,numeric type _word_size,numeric type _block_size) deriving(Bits,Eq);

  typedef struct{
    Bit#(TMul#(8,word_size)) data_word;
    Bit#(1) bus_error;
    Bit#(addr_width) address;
  }From_Memory_d#(numeric type addr_width, numeric type word_size, numeric type block_size) deriving(Bits,Eq);

  typedef struct {
    Bit#(TLog#(TMul#(8,_word_size))) upper_offset;
    Bit#(TLog#(TMul#(8,_word_size))) lower_offset;
    Bit#(TAdd#(TLog#(cbuff_size),1)) token;
    Bit#(TLog#(ways)) replace_block;
    Bit#(TLog#(prf_size)) destination_tag;
	Bit#(1) u_signed;      // 1-unsigned 0-signed
  }Metadata_d#(numeric type _word_size,numeric type _block_size,numeric type cbuff_size,numeric type ways,numeric type prf_size) deriving (Bits,Eq);

  typedef struct{
    To_Memory_d#(addr_width,_word_size,_block_size) request;
    Metadata_d#(_word_size,_block_size,cbuff_size,_ways,prf_size) metadata;
  }LoadBufferData_d#(numeric type addr_width,numeric type _word_size,numeric type _block_size,numeric type cbuff_size, numeric type _ways, numeric type prf_size) deriving (Bits,Eq);
endpackage
