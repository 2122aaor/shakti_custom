/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Names : Neel Gala, Arjun Menon
Email ID : neelgala@gmail.com, c.arjunmenon@gmail.com

Description : 

This module holds the registerFile of 32 registers. The operand forwarding is also done in this module.
For operand forwarding we  give priority to the Execution unit> Memory> Write-Back> RegisterFile.
When the data is forwarded by any of the stages, the stage will also indicate whether the valid data for the respective
rd is available or not. Suppose a LOAD memory instruction is in the execute stage, then the forwarded data will indicate False
on the valid structure but the destination address will be forwarded. Thus, the decoded stage will realise that there is an 
instruction in the pipe which will update the respective rd, but does not have updated value yet.

*/

package registerfile;

	import defined_types::*;
	`include "defined_parameters.bsv"
	import ConfigReg::*;
  import GetPut            ::*;
  import DReg::*;
  import csr::*;

	interface Ifc_registerfile;
		method Action _inputs_from_decode_stage(Bit#(5) rs1, Register_type rs1_type, Bit#(5) rs2, Register_type rs2_type, Bit#(5) rs3,Bit#(5) rd, Register_type rd_type, Bool firing, Bool privilege, Bit#(3) priv_funct, Bit#(12) priv_address);	// recives the input from the decode stage.
		method Action _inputs_from_writeback_stage (Bit#(5) destination, Register_type rd_type, Bit#(`Reg_width) destination_value, Bool firing); // recieves the input from the writeback stage.
		method Action _forwarding_from_memory (Operand_forwading_type data);	// forwarded data from the memory stage
		method Maybe#(Output_for_operand_fetch) output_to_decode_stage(); // return operands to the decodestage.
    method    Action      sin(Bit#(1) in);
    method    Bit#(1)     sout();
    `ifdef simulate
      method Action _print_all_rf(Bit#(`Reg_width) pc,Bit#(5) dest, Bit#(`Reg_width) dest_value, Register_type rd_type);
    `endif
	endinterface

	(*synthesize*)
	module mkregisterfile(Ifc_registerfile);

		Wire#(Bit#(5)) wr_rs1_decodestage <-mkDWire(0); // holds the address for operand1
		Wire#(Register_type) wr_rs1_type<-mkDWire(IntegerRF);
		Wire#(Bit#(5)) wr_rs2_decodestage <-mkDWire(0); // holds the address for operand2
		Wire#(Register_type) wr_rs2_type<-mkDWire(IntegerRF);
		Wire#(Bit#(5)) wr_rs3_decodestage <-mkDWire(0); // holds the address for operand2
		Wire#(Bit#(5)) wr_rd_decodestage <-mkDWire(0);  // holds the address for destination regsiter
		Wire#(Register_type) wr_rd_type<-mkDWire(IntegerRF);
		Wire#(Bool) wr_decode_firing <-mkDWire(False); // if true, indicates that the decode stage is firing;
    Wire#(Bool) wr_privilege <-mkDWire(False); // indicates whether its an AUIPC instruction or not.
    Wire#(Bit#(3)) wr_priv_funct <-mkDWire(0); // indicates whether its an AUIPC instruction or not.
    Wire#(Bit#(12)) wr_priv_addr <-mkDWire(0); // indicates whether its an AUIPC instruction or not.

		Wire#(Bit#(5)) wr_destination_wbstage <-mkDWire(0); // holds the address where the write needs to be poerformed by the WB stage
		Wire#(Register_type) wr_destination_type<-mkDWire(IntegerRF);
		Wire#(Bit#(`Reg_width)) wr_value_wbstage <-mkDWire(0); // holds the value to be commited.
		Wire#(Bool) wr_wb_firing <-mkDWire(False); // if true, indicates that the WB stage is firing;
		
		Wire#(Maybe#(Output_for_operand_fetch)) wr_output_to_decode <-mkDWire(tagged Invalid); // carries the output to the decode stage

		Wire#(Operand_forwading_type) wr_forward_from_FPU <-mkDWire(Operand_forwading_type{data_forward:0,rd_forward:0,valid:False,rd_type:IntegerRF});// holds the forwarded data from the execution stage
		Wire#(Operand_forwading_type) wr_forward_from_MEM <-mkDWire(Operand_forwading_type{data_forward:0,rd_forward:0,valid:False,rd_type:IntegerRF});// holds the forwarded data from the memory stage
		Wire#(Operand_forwading_type) wr_forward_from_WB  <-mkDWire(Operand_forwading_type{data_forward:0,rd_forward:0,valid:False,rd_type:IntegerRF});// holds the forwarded data from the writeback stage.

    Ifc_csr csr_rf <-mkcsr();
    ////////////////////////////////////////////// Integer Register File ////////////////////////////////////////////////////////
		ConfigReg#(Bit#(`Reg_width)) integer_rf[32]; // delcaring the integer register file.
		ConfigReg#(Bit#(`Reg_width)) floating_rf[32]; // delcaring the integer register file.
		for (Integer j = 0; j < 32; j = j + 1)begin
  			integer_rf [j] <- mkConfigReg(0);
				floating_rf[j]<-mkConfigReg(0);
		end

     ////////////////////////////////////////////// for Spike based debuggin only //////////////////////////////////////////////
    `ifdef simulate
      Reg#(Bit#(1)) rg_cnt <-mkReg(0);
      let reg_dump <- mkReg(InvalidFile) ;
      let freg_dump <- mkReg(InvalidFile) ;
      rule open_file(rg_cnt==0);
					String reg_dumpFile = "rtl_register_dump.txt" ;
          String freg_dumpFile = "rtl_fregister_dump.txt" ;
          File lfh <- $fopen( reg_dumpFile, "w" ) ;
          if ( lfh == InvalidFile )begin
              $display("cannot open %s", reg_dumpFile);
              $finish(0);
          end
          reg_dump <= lfh ;
          File lfh1 <- $fopen( freg_dumpFile, "w" ) ;
          if ( lfh1 == InvalidFile )begin
              $display("cannot open %s", freg_dumpFile);
              $finish(0);
          end
          freg_dump <= lfh1 ;
          rg_cnt <= 1 ;
      endrule
    `endif
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


		rule update_the_regfile;//(!wr_flush);
			if(wr_decode_firing) begin// decode stage is firing;
				Maybe#(Bit#(`Reg_width)) rs1=tagged Invalid;
				Maybe#(Bit#(`Reg_width)) rs2=tagged Invalid;
				Maybe#(Bit#(`Reg_width)) rs3=tagged Invalid;
        if(wr_forward_from_MEM.rd_forward==wr_rs1_decodestage && wr_rs1_decodestage!=0 && wr_forward_from_MEM.rd_type==wr_rs1_type)begin // if memory stage instruciton is going to update rd
          if(wr_forward_from_MEM.valid) // and the data is available
            rs1= tagged Valid wr_forward_from_MEM.data_forward;
        end
        else if(wr_forward_from_WB.rd_forward==wr_rs1_decodestage && wr_rs1_decodestage!=0 && wr_forward_from_WB.rd_type==wr_rs1_type)begin // if write-back stage instruciton is going to update rd
          if(wr_forward_from_WB.valid) // and the data is available
            rs1= tagged Valid wr_forward_from_WB.data_forward;
        end
        else if(wr_rs1_type==IntegerRF)begin// if the data is present in the regsiter file pick from there
          rs1= tagged Valid integer_rf[wr_rs1_decodestage];
        end
        else begin// if the data is present in the floating  regsiter file pick from there
          rs1= tagged Valid floating_rf[wr_rs1_decodestage];
        end

        if(wr_privilege)begin // if this is a privilege instruction
          if(wr_priv_funct[2]==1)begin // immediate value for CSR instruction
            let x<-csr_rf.inputs(wr_priv_funct,wr_priv_addr,zeroExtend(wr_rs1_decodestage));
            wr_output_to_decode<=tagged Valid (Output_for_operand_fetch{rs1:x,rs2:0,rs3:0});
          end
          else if(rs1 matches tagged Valid .op1)begin
            let x<-csr_rf.inputs(wr_priv_funct,wr_priv_addr,op1);
            wr_output_to_decode<=tagged Valid (Output_for_operand_fetch{rs1:x,rs2:0,rs3:0});
          end
        end
        else begin

          // if-else struct to find a valid operand2
          if(wr_forward_from_MEM.rd_forward==wr_rs2_decodestage && wr_rs2_decodestage!=0 && wr_forward_from_MEM.rd_type==wr_rs2_type)begin // if memory stage instruciton is going to update rd
            if(wr_forward_from_MEM.valid) // and the data is available
              rs2= tagged Valid wr_forward_from_MEM.data_forward;
          end
          else if(wr_forward_from_WB.rd_forward==wr_rs2_decodestage && wr_rs2_decodestage!=0 && wr_forward_from_WB.rd_type==wr_rs2_type)begin // if write-back stage instruciton is going to update rd
            if(wr_forward_from_WB.valid) // and the data is available
              rs2= tagged Valid wr_forward_from_WB.data_forward;
          end
          else if(wr_rs2_type==IntegerRF)begin// if the data is present in the regsiter file pick from there
            rs2= tagged Valid integer_rf[wr_rs2_decodestage];
          end
          else begin// if the data is present in the floating  regsiter file pick from there
            rs2= tagged Valid floating_rf[wr_rs2_decodestage];
          end

          if(wr_rs3_decodestage==0)
            rs3=tagged Valid 0;
          else if(wr_forward_from_MEM.rd_forward==wr_rs3_decodestage && wr_forward_from_MEM.rd_type==FloatingRF)begin // if memory stage instruciton is going to update rd
            if(wr_forward_from_MEM.valid) // and the data is available
              rs3= tagged Valid wr_forward_from_MEM.data_forward;
          end
          else if(wr_forward_from_WB.rd_forward==wr_rs3_decodestage && wr_forward_from_WB.rd_type==FloatingRF)begin // if write-back stage instruciton is going to update rd
            if(wr_forward_from_WB.valid) // and the data is available
              rs3= tagged Valid wr_forward_from_WB.data_forward;
          end
          else begin// if the data is present in the regsiter file pick from there
            rs3= tagged Valid floating_rf[wr_rs3_decodestage];
          end
          // if all the operands are available then send to the execution unit.
          if(rs1 matches tagged Valid .rs1data &&& rs2 matches tagged Valid .rs2data &&& rs3 matches tagged Valid .rs3data)begin // both the operands are available.
            $display($time,"\tRF: Reg1 :%d (%h) Reg2 :%d (%h) Reg3 :%d (%h) Rd :%d",wr_rs1_decodestage,rs1data,wr_rs2_decodestage,rs2data,wr_rs3_decodestage,rs3data,wr_rd_decodestage,$time);
            wr_output_to_decode<=tagged Valid (Output_for_operand_fetch{rs1:rs1data,rs2:rs2data,rs3:rs3data});
          end
        end
			end

			if(wr_wb_firing)begin 
        if(wr_destination_wbstage!=0 && wr_destination_type==IntegerRF)begin
					$display($time,"\tWriting into Integer Register : %d Value : %h ",wr_destination_wbstage,wr_value_wbstage,$time);
  				integer_rf[wr_destination_wbstage]<=wr_value_wbstage;
				end
				else if(wr_destination_type==FloatingRF)begin
					$display($time,"\tWriting into SP-Floating Register : %d Value : %h ",wr_destination_wbstage,wr_value_wbstage,$time);
					floating_rf[wr_destination_wbstage]<=wr_value_wbstage;
				end
			end
		endrule

		method Action _inputs_from_decode_stage(Bit#(5) rs1, Register_type rs1_type, Bit#(5) rs2, Register_type rs2_type, Bit#(5) rs3,Bit#(5) rd, Register_type rd_type, Bool firing, Bool privilege, Bit#(3) priv_funct, Bit#(12) priv_address);	// recives the input from the decode stage.
			wr_rs1_decodestage<=rs1;
			wr_rs2_decodestage<=rs2;
			wr_rs3_decodestage<=rs3;
			wr_rd_decodestage<=rd;
      wr_privilege<=privilege;
      wr_priv_funct<=priv_funct;
      wr_priv_addr<=priv_address;
			wr_decode_firing<=firing;
			wr_rs1_type<=rs1_type;
			wr_rs2_type<=rs2_type;
			wr_rd_type<=rd_type;
		endmethod

		method Action _inputs_from_writeback_stage (Bit#(5) destination, Register_type rd_type, Bit#(`Reg_width) destination_value, Bool firing); // recieves the input from the writeback stage.
			wr_destination_wbstage<=destination;
			wr_value_wbstage<=destination_value;
			wr_wb_firing<=firing;
			wr_forward_from_WB <= Operand_forwading_type {	data_forward	: destination_value,
															rd_forward 	:destination,
															valid: True,
															rd_type:rd_type};
			wr_destination_type<=rd_type;
		endmethod
		
		method Action _forwarding_from_memory (Operand_forwading_type data);
			wr_forward_from_MEM <= data;
		endmethod

		method Maybe#(Output_for_operand_fetch) output_to_decode_stage();
			return wr_output_to_decode;
		endmethod

    method    Action      sin(Bit#(1) in);
      csr_rf.sin(in);
    endmethod

    method    Bit#(1)     sout();
      return csr_rf.sout;
    endmethod
    
    `ifdef simulate
      method Action _print_all_rf(Bit#(`Reg_width) pc,Bit#(5) dest, Bit#(`Reg_width) dest_value, Register_type rd_type)if(rg_cnt==1);
          Bit#(64) pc_val=zeroExtend(pc);
          $fwrite(reg_dump,"PC=%h\n",pc_val);
          $fwrite(freg_dump,"PC=%h\n",pc_val);
          for(Bit#(6) i=1;i<32;i=i+1)begin
            Bit#(64) temp;
            Bit#(64) ftemp;
            if(i[4:0]==dest && rd_type==IntegerRF)
              temp=dest_value;
            else
              temp=integer_rf[i[4:0]];
          
						if(i[4:0]==dest && rd_type==FloatingRF)
              ftemp=dest_value;
					  else
              ftemp=floating_rf[i[4:0]];
            $fwrite(reg_dump,"REG %d %h\n",i,temp);
            $fwrite(freg_dump,"FREG %d %h\n",i,ftemp);
          end
          $fwrite(reg_dump,"\n");
          $fwrite(freg_dump,"\n");
      endmethod
    `endif

	endmodule
endpackage
