package memory_unit;
  import defined_types::*;
  import DReg::*;
  `include "defined_parameters.bsv"

  interface Ifc_memory_unit;
    method Action input_from_memory(Data_from_mem mem_data);
    method Maybe#(Data_to_mem) data_to_memory;
    method ActionValue#(Maybe#(MemoryUnitResponse)) communicate_with_core (Execution_output execdata);
  endinterface

  (*synthesize*)
  module mkmemory_unit(Ifc_memory_unit);
    
    Wire#(Maybe#(Data_to_mem)) wr_data_to_mem <-mkDWire(tagged Invalid);
    Reg#(Maybe#(Data_from_mem)) wr_data_from_mem <-mkDReg(tagged Invalid);

    method Action input_from_memory(Data_from_mem mem_data);
      wr_data_from_mem<=tagged Valid mem_data;
    endmethod

    method Maybe#(Data_to_mem) data_to_memory;
      return wr_data_to_mem;
    endmethod

    method ActionValue#(Maybe#(MemoryUnitResponse)) communicate_with_core (Execution_output execdata);
      if(execdata.mem_type == LOAD)begin
        $display($time,"\tMEM_STAGE: LOAD Operation selected. Address: %h",execdata.aluresult);
        if(wr_data_from_mem matches tagged Valid .memdata)begin
          $display($time,"\tMEM_STAGE: Memory responded with data: %h",memdata.read_data);
          MemoryUnitResponse info;
          info.destination=execdata.destination;
          info.destination_value=0;

          if(execdata.signextend=='b1 && execdata.word_size=='b00) begin // LBU // unsigned byte load
            info.destination_value=zeroExtend(memdata.read_data[7:0]);
          end
          else if (execdata.signextend=='b1 && execdata.word_size=='b01) begin // LHU unsigned half-word load
            info.destination_value=zeroExtend(memdata.read_data[15:0]);
          end
          else if (execdata.signextend==0 && execdata.word_size=='b00) begin // LB signed byte load
            info.destination_value=signExtend(memdata.read_data[7:0]);
          end 
          else if (execdata.signextend=='b0 && execdata.word_size=='b01) begin // LH signed hlafword load
            info.destination_value=signExtend(memdata.read_data[15:0]);
          end	 
          else if (execdata.signextend=='b0 && execdata.word_size=='b10) // LW signed word
            info.destination_value=signExtend(memdata.read_data[31:0]);
          else if (execdata.signextend=='b1 && execdata.word_size=='b10) // LWU  unsigned word
            info.destination_value=zeroExtend(memdata.read_data[31:0]);
          else // LD signed word
            info.destination_value=memdata.read_data;

          info.exception=None;
          if(memdata.misaligned_error==1)
            info.exception=Instruction_misaligned;
          else if (memdata.bus_error==1)
            info.exception=Instruction_buserr;

          return tagged Valid info;
        end
        else begin
          $display($time,"\tMEM_STAGE: Waiting for Memory to respond"); 
          wr_data_to_mem<= tagged Valid Data_to_mem{address:execdata.aluresult,
                                      write_data:execdata.memory_data,
                                      read_write:0,
                                      word_size:execdata.word_size};
          return tagged Invalid;
        end

      end
      else begin // STORE OPERATION
        $display($time,"\tMEM_STAGE: STORE Operation selected. Address: %h Data: %h",execdata.aluresult,execdata.memory_data);
        if(wr_data_from_mem matches tagged Valid .memdata)begin
          Exception exec=None;
          if(memdata.misaligned_error==1)
            exec=Instruction_misaligned;
          else if (memdata.bus_error==1)
            exec=Instruction_buserr;

          MemoryUnitResponse info=MemoryUnitResponse{destination:0,destination_value:0,exception:exec};
          return tagged Valid info;
        end
        else begin
          $display($time,"\tMEM_STAGE: Waiting for Memory to respond"); 
          wr_data_to_mem<= tagged Valid Data_to_mem{address:execdata.aluresult,
                                      write_data:execdata.memory_data,
                                      read_write:1,
                                      word_size:execdata.word_size};
          return tagged Invalid;
        end
      end

    endmethod

  endmodule
endpackage
