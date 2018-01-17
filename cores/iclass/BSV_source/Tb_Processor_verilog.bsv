package  Tb_Processor;
import Processor::* ;
import riscv_types::* ;
`include "defined_parameters.bsv"
import RegFile::* ; 
import ConfigReg::* ;
import All_types::* ;
import All_types_d::* ;
import DReg::*;

(*preempts="rl_dcache_to_memory,rl_read_write_back_data"*)
(*preempts="rl_dcache_to_memory,rl_update_mem_with_write_back"*)
module mkTb_Processor();

Ifc_cache_to_memory processor <- mkProcessor();
RegFile#(UInt#(`ICACHE_SIZE),Bit#(`INSTR_WIDTH)) instr_memory <- mkRegFileLoad(
	  "input.hex",0,`NUM_INSTRS);
RegFile#(UInt#(`DCACHE_SIZE), Bit#(`REG_WIDTH)) data_memory <- mkRegFileLoad(
	  "rtl_mem_init.txt",0,`MEM_INIT_SIZE-1);
Reg#(Bit#(128))  rg_block <- mkReg(0); 
Reg#(Bit#(256))  rg_dblock <- mkReg(0); 
Reg#(Bit#(`ADDRESS_WIDTH)) rg_in_address <- mkReg(0);
Reg#(bit)				   rg_cnt		 <- mkDReg(0);
Reg#(Bit#(`ADDRESS_WIDTH)) rg_in_daddress <- mkReg(0);
Reg#(bit)				   rg_cnt_d		 <- mkDReg(0);
Reg#(Bit#(5)) rg_token<-mkReg(0);
Reg#(Bit#(3)) rg_write_back_loop <-mkReg(0);
Reg#(Bit#(256)) rg_write_back_data <-mkReg(0);
Reg#(Bit#(64)) rg_write_back_address <-mkReg(0);

rule rl_icache_to_memory(rg_cnt == 0 && !processor.flush);
	    let x <- processor.request_to_memory_from_icache; 
	   Bit#(128) lv_block = 0;
	   Bit#(`ADDRESS_WIDTH) icache_address = 0;
	    rg_in_address <= x.address;
		rg_token<=x.token;
	    icache_address = x.address >> 2;
	    $display("icache_request_to_memory data_block requested original address :%d with_shifted_address %d token :%d",x.address, icache_address,x.token);
	   	lv_block[31:0] = instr_memory.sub(unpack(icache_address));
	   	lv_block[63:32] = instr_memory.sub(unpack(icache_address+1));
	   	lv_block[95:64] = instr_memory.sub(unpack(icache_address+2));
	   	lv_block[127:96] = instr_memory.sub(unpack(icache_address+3));
	    rg_block <= lv_block;
		rg_cnt <= rg_cnt + 1;
endrule


rule rl_memory_to_icache(rg_cnt == 1 && !processor.flush);
		
	    $display("response_memory data_block obtained with_data %h Address :%h token : %d", rg_block,rg_in_address,rg_token);
	    processor.response_from_memory_to_icache(From_Memory{bus_error : 0, data_line : rg_block, address : rg_in_address, token:rg_token});
endrule

rule rl_dcache_to_memory(rg_cnt_d == 0 && !processor.flush);
	    let x <- processor.request_to_memory_from_dcache; 
	   Bit#(256) lv_block = 0;
	   Bit#(`ADDRESS_WIDTH) dcache_address = 0;
	    rg_in_daddress <= x.address;
	    dcache_address = x.address >> 3;
		if(x.ld_st == Load) begin
			if(dcache_address <= 'd1048576 && dcache_address >= 'd0) begin
	    		$display("dcache_request_to_memory data_block requested original address :%h with_shifted_address %h unpack_version %h",x.address, dcache_address, unpack(dcache_address));
	   			lv_block[63:0] = data_memory.sub(unpack(dcache_address));
	   			lv_block[127:64] = data_memory.sub(unpack(dcache_address+1));
	   			lv_block[191:128] = data_memory.sub(unpack(dcache_address+2));
	   			lv_block[255:192] = data_memory.sub(unpack(dcache_address+3));
			end
	    	rg_dblock <= lv_block;
		end
		else
		data_memory.upd(unpack(dcache_address), x.write_data);
		rg_cnt_d <= rg_cnt_d + 1;
endrule


rule rl_memory_to_dcache(rg_cnt_d == 1 && !processor.flush);
		
	    $display("response_memory_dcache data_block obtained with_data %h Address :%h", rg_dblock,rg_in_daddress);
	    processor.response_from_memory_to_dcache(From_Memory_d{bus_error : 0, data_line : rg_dblock, address : rg_in_daddress});
endrule

rule rl_read_write_back_data(rg_write_back_loop==0);
	let x <- processor.write_back_data;
	rg_write_back_loop<=1;
	rg_write_back_data<=x.data_line;
	rg_write_back_address<=(x.address)>>3;
	$display("Write Back Data reached memory and updated");
endrule

rule rl_update_mem_with_write_back(rg_write_back_loop!=0);
	data_memory.upd(unpack(rg_write_back_address),rg_write_back_data[63:0]);
	rg_write_back_data<=rg_write_back_data>>64;
	rg_write_back_address<=rg_write_back_address+1;
	if(rg_write_back_loop==4)
		rg_write_back_loop<=0;
	else 
		rg_write_back_loop<=rg_write_back_loop+1;
	$display("TB: Writing into memory for address: %h with data :%h:",rg_write_back_address,rg_write_back_data[63:0],$time);
endrule


endmodule
endpackage 
