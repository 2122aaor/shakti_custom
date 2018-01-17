package memory_unit_AHB;
  import defined_types::*;
  import DReg::*;
  `include "defined_parameters.bsv"
  import dcache_AHB::*;

  interface Ifc_memory_unit;
    method ActionValue#(Bool) input_from_memory(From_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE) mem_data);
    method ActionValue#(To_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)) data_to_memory;
    method ActionValue#(Maybe#(MemoryUnitResponse)) communicate_with_core (Memout execdata);
  endinterface

  (*synthesize*)
  module mkmemory_unit(Ifc_memory_unit);
    
    Ifc_dcache dcache <-mkdcache();
    Reg#(Bool) rg_state_wait_for_response <-mkReg(False);
    Wire#(Maybe#(MemoryUnitResponse)) wr_communicate_to_core<-mkDWire(tagged Invalid);
    Wire#(From_Cpu_D#(32,4)) wr_req_to_cache <-mkWire();
    Reg#(Memout) rg_execdata <-mkReg(?);

    rule read_response_from_cache(dcache.response_to_cpu matches tagged Valid .memdata);
      $display($time,"\tMEM_UNIT: got response from DCACHE: Data: %h Address: %h",memdata.data_word,memdata.address);
      MemoryUnitResponse info;
      info.destination_value=0;
      info.exception=tagged Invalid;
      info.badaddr=rg_execdata.address;
      if(memdata.misaligned_error==1)
        if(memdata.load_store==Load)
          info.exception=tagged Valid Load_addr_misaligned;
        else
          info.exception=tagged Valid Store_addr_misaligned;
      else if (memdata.bus_error==1)
        if(memdata.load_store==Load)
          info.exception=tagged Valid Load_access_fault;
        else
          info.exception=tagged Valid Store_access_fault;
			if(memdata.load_store==Load)
      	if(rg_execdata.signextend==1)begin
      	  if(rg_execdata.word_size=='d2)begin // word transfer
      	      info.destination_value = (zeroExtend(memdata.data_word[31:0]));
      	  end
      	  else if (rg_execdata.word_size=='d1)begin // half_word
      	    if(rg_execdata.address[1:0] ==0)
      	      info.destination_value = (zeroExtend(memdata.data_word[15:0]));
      	    else if(rg_execdata.address[1:0] ==2)
      	      info.destination_value = (zeroExtend(memdata.data_word[31:16]));
      	  end
      	  else if (rg_execdata.word_size=='d0) begin// one byte
      	    if(rg_execdata.address[1:0] ==0)
      	      info.destination_value = (zeroExtend(memdata.data_word[7:0]));
      	    else if(rg_execdata.address[1:0] ==1)
      	      info.destination_value = (zeroExtend(memdata.data_word[15:8]));
      	    else if(rg_execdata.address[1:0] ==2)
      	      info.destination_value = (zeroExtend(memdata.data_word[23:16]));
      	    else if(rg_execdata.address[1:0] ==3)
      	      info.destination_value = (zeroExtend(memdata.data_word[31:24]));
      	  end
      	end
      	else begin
      	  if(rg_execdata.word_size=='d2)begin // word transfer
      	      info.destination_value = (signExtend(memdata.data_word[31:0]));
      	  end
      	  else if (rg_execdata.word_size=='d1)begin // half_word
      	    if(rg_execdata.address[1:0] ==0)
      	      info.destination_value = (signExtend(memdata.data_word[15:0]));
      	    else if(rg_execdata.address[1:0] ==2)
      	      info.destination_value = (signExtend(memdata.data_word[31:16]));
      	  end
      	  else if (rg_execdata.word_size=='d0) begin// one byte
      	    if(rg_execdata.address[1:0] ==0)
      	      info.destination_value = (signExtend(memdata.data_word[7:0]));
      	    else if(rg_execdata.address[1:0] ==1)
      	      info.destination_value = (signExtend(memdata.data_word[15:8]));
      	    else if(rg_execdata.address[1:0] ==2)
      	      info.destination_value = (signExtend(memdata.data_word[23:16]));
      	    else if(rg_execdata.address[1:0] ==3)
      	      info.destination_value = (signExtend(memdata.data_word[31:24]));
      	  end
      	end
      wr_communicate_to_core<=tagged Valid info;
    endrule

    rule send_request_to_cache;
      dcache.request_from_cpu(wr_req_to_cache);
    endrule

    method ActionValue#(Bool) input_from_memory(From_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE) mem_data);
      let x<-dcache.response_from_memory(mem_data);
			return x;
    endmethod

    method ActionValue#(To_Memory_D#(`DCACHE_ADDR,`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)) data_to_memory=dcache.request_to_memory;

    method ActionValue#(Maybe#(MemoryUnitResponse)) communicate_with_core (Memout execdata);
        if(wr_communicate_to_core matches tagged Invalid)begin
								wr_req_to_cache<=From_Cpu_D{address:execdata.address,load_store:execdata.mem_type,data:execdata.memory_data,transfer_size:execdata.word_size 
								`ifdef atomic ,atomic_op:execdata.atomic_op `endif };
          $display($time,"\tMEM_UNIT: Address: %h Data: %h Access: ",execdata.address,execdata.memory_data,fshow(execdata.mem_type));
          rg_execdata<=execdata;
        end
      return wr_communicate_to_core;
    endmethod
  endmodule
endpackage
