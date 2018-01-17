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
	import Vector::*;

	interface Ifc_registerfile;
    method ActionValue#(Maybe#(Output_for_operand_fetch)) _inputs_from_decode_stage(Bit#(5) rs1_addr, Register_type rs1_type, Bit#(5) rs2_addr, Register_type rs2_type `ifdef spfpu ,Bit#(5) rs3_addr `endif );	// recives the input from the decode stage.
		method Action _forwarding_from_memory (Maybe#(Operand_forwading_type) data);
		method Action _forwarding_from_execution (Maybe#(Operand_forwading_type) data);
    method    Action      sin(Bit#(1) in);
    method    Bit#(1)     sout();
    `ifdef simulate
      method Action _print_all_rf(Bit#(`Reg_width) pc);
    `endif
  	method ActionValue #(CSRResult) csr_access(CSRInsn inst,Bit#(5) destination, Register_type rd_type);
		method 	Action mtip(Bit#(1) mtip1);
	endinterface

	(*synthesize*)
	module mkregisterfile(Ifc_registerfile);

		Wire#(Maybe#(Output_for_operand_fetch)) wr_output_to_decode <-mkDWire(tagged Invalid); // carries the output to the decode stage

		Wire#(Maybe#(Operand_forwading_type)) wr_forward_from_MEM <-mkDWire(tagged Invalid);// holds the forwarded data from the memory stage
		Wire#(Maybe#(Operand_forwading_type)) wr_forward_from_EXE <-mkDWire(tagged Invalid);// holds the forwarded data from the memory stage

    Ifc_csr csr <-mkcsr(0,0);     
    ////////////////////////////////////////////// Integer Register File ////////////////////////////////////////////////////////
		Vector#(32,Array#(Reg#(Bit#(`Reg_width)))) integer_rf ; // delcaring the integer register file.
	`ifdef spfpu
		Vector#(32,Array#(Reg#(Bit#(`Reg_width)))) floating_rf;  // delcaring the integer register file.
	`endif
		for (Integer j = 0; j < 32; j = j + 1)begin
        `ifdef simulate
          if(j==5)
            integer_rf[5]<-mkCReg(3,'h80000000);
          else
        `endif
  			  integer_rf [j] <- mkCReg(3,0);
			`ifdef spfpu
				floating_rf[j]<-mkCReg(3,0);
			`endif
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



    method ActionValue#(Maybe#(Output_for_operand_fetch)) _inputs_from_decode_stage(Bit#(5) rs1_addr, Register_type rs1_type, Bit#(5) rs2_addr, Register_type rs2_type `ifdef spfpu ,Bit#(5) rs3_addr `endif );	// recives the input from the decode stage.
				Maybe#(Bit#(`Reg_width)) rs1=tagged Invalid;
				Maybe#(Bit#(`Reg_width)) rs2=tagged Invalid;
        `ifdef spfpu Maybe#(Bit#(`Reg_width)) rs3=tagged Invalid; `endif
        if(rs1_addr==0 && rs1_type==IntegerRF)
          rs1=tagged Valid 0;
        else if(wr_forward_from_EXE matches tagged Valid .data &&& data.rd_forward==rs1_addr  &&& data.rd_type==rs1_type)begin // if memory stage instruciton is going to update rd
					$display($time,"\tRF: rs1 waiting for EXE");
          if(data.valid) // and the data is available
            rs1= tagged Valid data.data_forward;
        end
        else if(wr_forward_from_MEM matches tagged Valid .data &&& data.rd_forward==rs1_addr  &&& data.rd_type==rs1_type)begin // if memory stage instruciton is going to update rd
					$display($time,"\tRF: rs1 waiting for MEM");
          if(data.valid) // and the data is available
            rs1= tagged Valid data.data_forward;
        end
        else if(rs1_type==IntegerRF)begin// if the data is present in the regsiter file pick from there
					$display($time,"\tRF: rs1 from Integer File");
          rs1= tagged Valid integer_rf[rs1_addr][1];
        end
				`ifdef spfpu
        else begin// if the data is present in the floating  regsiter file pick from there
					$display($time,"\tRF: rs1 from Floating File");
          rs1= tagged Valid floating_rf[rs1_addr][1];
        end
				`endif

        // if-else struct to find a valid operand2
        if(rs2_addr==0 && rs2_type==IntegerRF)
          rs2=tagged Valid 0;
        else if(wr_forward_from_EXE matches tagged Valid .data &&& data.rd_forward==rs2_addr  &&& data.rd_type==rs2_type)begin // if memory stage instruciton is going to update rd
					$display($time,"\tRF: rs2 waiting for EXE");
          if(data.valid) // and the data is available
            rs2= tagged Valid data.data_forward;
        end
        else if(wr_forward_from_MEM matches tagged Valid .data &&& data.rd_forward==rs2_addr  &&& data.rd_type==rs2_type)begin // if memory stage instruciton is going to update rd
					$display($time,"\tRF: rs2 waiting for MEM");
          if(data.valid) // and the data is available
            rs2= tagged Valid data.data_forward;
        end
        else if(rs2_type==IntegerRF)begin// if the data is present in the regsiter file pick from there
					$display($time,"\tRF: rs2 from Integer File");
          rs2= tagged Valid integer_rf[rs2_addr][1];
        end
				`ifdef spfpu
        else begin// if the data is present in the floating  regsiter file pick from there
					$display($time,"\tRF: rs2 from Floating File");
          rs2= tagged Valid floating_rf[rs2_addr][1];
        end
				`endif

        `ifdef spfpu
          if(wr_forward_from_EXE matches tagged Valid .data &&& data.rd_forward==rs3_addr &&& data.rd_type==FloatingRF)begin // if memory stage instruciton is going to update rd
            if(data.valid) // and the data is available
              rs3= tagged Valid data.data_forward;
          end
          else if(wr_forward_from_MEM matches tagged Valid .data &&& data.rd_forward==rs3_addr &&& data.rd_type==FloatingRF)begin // if memory stage instruciton is going to update rd
            if(data.valid) // and the data is available
              rs3= tagged Valid data.data_forward;
          end
          else begin// if the data is present in the regsiter file pick from there
            rs3= tagged Valid floating_rf[rs3_addr][1];
          end
        `endif
        // if all the operands are available then send to the execution unit.
         if(rs1 matches tagged Valid .rs1data &&& rs2 matches tagged Valid .rs2data `ifdef spfpu 
          &&& rs3 matches tagged Valid .rs3data `endif )begin // both the operands are available.
           $display($time,"\tRF: Reg1 :%d (%h) Reg2 :%d (%h) Reg3 :%d (%h)",rs1_addr,rs1data,rs2_addr,rs2data`ifdef spfpu ,rs3_addr, rs3data `endif );
           return tagged Valid (Output_for_operand_fetch{rs1:rs1data,rs2:rs2data `ifdef spfpu 
                                                                        ,rs3:rs3data `endif
                                                                        });
        end
				else return tagged Invalid;
		endmethod
  	method ActionValue #(CSRResult) csr_access(CSRInsn inst,Bit#(5) destination, Register_type rd_type);
    	inst.rs1_data=integer_rf[inst.rd_data[4:0]][0];
    	let x<-csr.csr_access(inst);
			if(!x.redirect)
	      if(destination!=0 && rd_type==IntegerRF)begin
						$display($time,"\tWriting into Integer Register : %d Value : %h ",destination,x.destination_value,$time);
  					integer_rf[destination][0]<=x.destination_value;
				end
				`ifdef spfpu
				else if(rd_type==FloatingRF)begin
						$display($time,"\tWriting into SP-Floating Register : %d Value : %h ",destination,x.destination_value,$time);
						floating_rf[destination][0]<=x.destination_value;
				end
				`endif
    	return x;
  	endmethod
		
		method Action _forwarding_from_memory (Maybe#(Operand_forwading_type) data);
			wr_forward_from_MEM <= data;
		endmethod
		method Action _forwarding_from_execution (Maybe#(Operand_forwading_type) data);
			wr_forward_from_EXE <= data;
		endmethod


    
    `ifdef simulate
      method Action _print_all_rf(Bit#(`Reg_width) pc)if(rg_cnt==1);
          Bit#(64) pc_val=zeroExtend(pc);
          $fwrite(reg_dump,"PC=%h\n",pc_val);
					`ifdef spfpu
          	$fwrite(freg_dump,"PC=%h\n",pc_val);
        	  Bit#(64) temp=zeroExtend(floating_rf[0][1]);
          	$fwrite(freg_dump,"FREG %d %h\n",6'd0,temp);
					`endif
          for(Bit#(6) i=1;i<32;i=i+1)begin
            Bit#(64) temp1;
            temp1=signExtend(integer_rf[i[4:0]][1]);
            $fwrite(reg_dump,"REG %d %h\n",i,temp1);
					`ifdef spfpu
            Bit#(64) ftemp;
            ftemp=zeroExtend(floating_rf[i[4:0]][1]);
            $fwrite(freg_dump,"FREG %d %h\n",i,ftemp);
					`endif
          end
          $fwrite(reg_dump,"\n");
					`ifdef spfpu
          	$fwrite(freg_dump,"\n");
					`endif
      endmethod
    `endif
  method    Action      sin(Bit#(1) in);
    csr.sin(in);
  endmethod
  method    Bit#(1)     sout() = csr.sout();
	method 	Action mtip(Bit#(1) mtip1);
		csr.mtip(mtip1);
	endmethod

	endmodule
endpackage
