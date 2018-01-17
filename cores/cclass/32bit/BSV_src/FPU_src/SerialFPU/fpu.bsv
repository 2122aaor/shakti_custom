/*
Copyright (c) 2013-2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name   	: Floating Point Unit
Author's Name 	: Arjun C. Menon, Vinod.G
e-mail id		: c.arjunmenon@gmail.com, g.vinod1993@gmail.com
Last updated on : 27th June 2016

	This unit performs all floating point operations. There are 7 functional units: 1. Add/Sub/Mul/Div, 2. Square root/Sign injection, 3. Single or Double FP to signed or unsigned to 32-bit or 64-bit integer conversion, 4. Signed or unsigned 32-bit or 64-bit integer to Single or Double FP conversion, 5. Single to double or vice-versa, 6. Compare instructions- Eq/Gt/Lt and Min/Max, 7. FSSR/FRSR/Move from SP or DP FP reg file to integer reg file, vice versa.

Here the output is obtained in different rules, as it is out-of-order execution. IN this case if one instruction takes less time for execution then, its result will be available and next process can work parallel, if its taking more time.
*/
package fpu;

`include "defined_parameters.bsv"	
import defined_types::*;
import fpu_compare_min_max::*;						
import fpu_int_to_fp::*;
import fpu_sign_injection::*;	
import fpu_divider::*;
import fpu_sqrt::*;
import fpu_fp_to_int::*;
import fpu_fm_add_sub::*;
import fpu_fclass::*;
import FIFO::*;
import SpecialFIFOs::*;
import RegFile::*;
import DReg::*;

interface Ifc_fpu;							//interface to module mk_fpu
	method ActionValue#(Maybe#(Floating_output#(`Reg_width))) _start(Bit#(`Reg_width) operand1, Bit#(`Reg_width) operand2, Bit#(`Reg_width) operand3, Bit#(5) opcode, Bit#(7) funct7, Bit#(3) funct3, Bit#(2) immediate_value,  Bit#(32) fsr); 			// input method to start the floating point operation
  method ActionValue#(Maybe#(Exception_cause)) exception;
endinterface


(*synthesize*)
module mkfpu(Ifc_fpu);

	Reg#(Maybe#(Floating_output#(`Reg_width))) wr_result			<- 					mkDReg(tagged Invalid);				// wire that holds the result
  	Wire#(Bit#(7))  wr_funct7 												<-						mkDWire(0);
  	Wire#(Bit#(3))  wr_funct3 												<-						mkDWire(0);
  	Wire#(Bit#(2))  wr_imm 													<-						mkDWire(0);
  	Wire#(Bit#(5))  wr_opcode 												<-						mkDWire(0);
  	Wire#(Bool)     wr_inputs_ready 										<-						mkDWire(False);         		 	// It is tagged Valid whenever the method Action _start is invoked
	Wire#(Bit#(`Reg_width))	wr_operand1									<-						mkDWire(0);								// wire that holds the value of operand1 of the current instruction which is to be executed
	Wire#(Bit#(`Reg_width))	wr_operand2									<-						mkDWire(0);								// wire that holds the value of operand2 of the current instruction which is to be executed
	Wire#(Bit#(`Reg_width)) wr_operand3									<-						mkDWire(0);       
  	Reg#(Bool) take_new_inputs 											<-						mkReg(True);

/***********************Single Precision Modules Instantiation*********************************************************************************************************************************/ 
  Ifc_fpu_compare_min_max#(32,23,8) inst_fpu_compare_min_max					<- 	mkfpu_compare_min_max();			// Instantiating compare or min/max module
  Ifc_fpu_int_to_fp#(32,32,23,8) 	inst_fpu_int_to_fp 							<- 	mkfpu_int_to_fp();		      	// Instantiating integer to floating point conversion module
  Ifc_fpu_sign_injection#(32,23,8)  inst_spfpu_sign_injection 					<- 	mkfpu_sign_injection(); 			//Instantiating SP sign injection Module
  Ifc_fpu_divider#(32,23,8) 			inst_spfpu_divider 							<- 	mkfpu_divider();
  Ifc_fpu_sqrt#(32,23,8) 				inst_spfpu_sqrt 								<- 	mkfpu_sqrt();
  Ifc_fpu_fp_to_int#(32,23,8,127) 	inst_spfp_to_int 								<- 	mkfpu_fp_to_int();            	//Instantiating SP Floating to Int Conversion Module
  Ifc_fpu_fm_add_sub#(32,23,8,64) 	inst_spfm_add_sub 							<- 	mkfpu_fm_add_sub();
  Ifc_fpu_fclass#(32,23,8) 			inst_spfpu_fclass 							<- 	mkfpu_fclass();               	//Instantiating SP Floating Point Classify Module
/************************************************************************************************************************************************************************************************/
	
	//rule that gives inputs to the integer to floating point converter
	rule rl_give_inputs_to_fpu_int_to_fp(wr_inputs_ready && (((wr_funct7=='b1101000)||(wr_funct7=='b1101001)) && wr_opcode == 'b10100));
		inst_fpu_int_to_fp._start(wr_operand1, wr_operand2[0], wr_funct3);
		$display($time,"\tGiving inputs to fpu_int_to_fp %h",wr_operand1);
	endrule
		
	//rule that takes output from integer to fo floating point converter
	rule rl_get_output_from_fpu_int_to_fp;
		$display($time,"\tGot output from fpu_int_to_fp");
		wr_result<= tagged Valid inst_fpu_int_to_fp.result_;
    take_new_inputs<=True;
	endrule

	//rule that gives inputs to compare or min/max module
	rule rl_give_inputs_to_fpu_compare_min_max(wr_inputs_ready && (((wr_funct7[6:2]=='b00101) || wr_funct7[6:2] == 'b10100) && wr_opcode == 'b10100));
			inst_fpu_compare_min_max._start(wr_operand1, wr_operand2, wr_funct3,wr_funct7[2]);
		$display($time,"\tGiving inputs to fpu_compare_min_max");
	endrule

	//rule that takes output from sp compare or min/max module
	rule rl_get_output_from_fpu_compare_min_max;
		$display($time,"\tGot output from fpu_compare_min_max");
		wr_result<= tagged Valid inst_fpu_compare_min_max.result_;
    take_new_inputs<=True;
	endrule

   //rule that gives inputs to the add_sub module
	rule rl_give_inputs_to_fpu_add_sub(wr_inputs_ready && (((wr_funct7[6:2] == 'b00000||wr_funct7[6:2]=='b00001) && wr_opcode == 'b10100)));
     		 inst_spfm_add_sub._start(32'h3f800000, wr_operand1,wr_operand2,wr_funct3,wr_funct7[2],1'b0,1'b0);    
		$display($time,"\tGiving inputs to the fpu add_sub");
	endrule

   //rule that gives inputs to fpu_sign_injection module
 	rule rl_give_inputs_to_fpu_sign_injection(wr_inputs_ready && (((wr_funct7[6:2] == 'b00100)||(wr_opcode == 'b00100)) && wr_opcode == 'b10100));
			inst_spfpu_sign_injection._start(wr_operand1, wr_operand2, wr_funct3);
		$display($time,"\tGiving inputs to the fpu sign injection");
  endrule
 
  //rule to get output from spfpu sign injection
  rule rl_get_output_from_spfpu_sign_injection;  
    $display($time,"\tGot output from spfpu sign injection unit");
    wr_result <= tagged Valid inst_spfpu_sign_injection.result_;
    take_new_inputs<=True;
	endrule
 
  //rule to give inputs to spfpu divider  //Changes in condition from here ~~~~~~~~~~
  rule rl_give_inputs_to_spfpu_divider(wr_inputs_ready && ((wr_funct7[6:2] == 'b00011) && wr_opcode == 'b10100));
		inst_spfpu_divider._start(wr_operand1,wr_operand2,wr_funct3);
		$display($time,"\tGiving inputs to the spfpu divider");
  endrule

  //rule to get output from spfpu divider
  rule rl_get_output_from_spfpu_divider;
    $display($time,"\tGot output from spfpu divider"); 
    wr_result <= tagged Valid inst_spfpu_divider.final_result_;
    take_new_inputs<=True;
  endrule
       
	//rule to give inputs to spfpu square root module
  rule rl_give_inputs_to_spfpu_sqrt(wr_inputs_ready && ((wr_funct7[6:2] == 'b01011) && wr_opcode == 'b10100));
		inst_spfpu_sqrt._start(wr_operand1, wr_funct3);
		$display($time,"\tGiving inputs to the spfpu sqrt");
  endrule

  //rule to get output spfpu square root module
  rule rl_get_output_from_spfpu_sqrt(inst_spfpu_sqrt.get_result matches tagged Valid .res); // TODO check for inexact and underflow
    $display($time,"\tGot output from spfpu sqrt"); 
    wr_result <= tagged Valid res;
    take_new_inputs<=True;
    inst_spfpu_sqrt.deque_buffer();
  endrule

  //rule to get inputs from fp to int module 
  rule rl_give_inputs_to_fp_to_int(wr_inputs_ready && (((wr_funct7 == 'b1100000) || (wr_funct7 == 'b1100001))  && wr_opcode == 'b10100)); // TODO check for flags.
		 inst_spfp_to_int._start(wr_operand1,wr_imm[0],wr_imm[1],wr_funct3);
		 $display($time,"\tGiving Inputs to fpu to int Conversion Module");  
   endrule

	 //rule to get output from fp to int
  rule rl_get_output_from_spfp_to_int;
		 $display($time,"\tGot output from fpu to int conversion Module");
		 wr_result <= tagged Valid inst_spfp_to_int.get_result();
    take_new_inputs<=True;
  endrule

	 //rule to get output from fused multiply add sub
	 rule rl_get_output_from_fm_add_sub;
		 $display($time,"\tGot output from sp fused multiple add conversion Module");
		 wr_result <= tagged Valid inst_spfm_add_sub.get_result;
     $display($time,"\tFMA Result : %h", inst_spfm_add_sub.get_result.final_result);
     take_new_inputs<=True;
	 endrule

	 //rule to give inputs to spfloating multiplier
	 rule rl_give_inputs_to_spfloating_multiplier(wr_inputs_ready && ((wr_funct7[6:2] == 'b00010) && wr_opcode == 'b10100));
		  $display("wr_funct3 : %h",wr_funct3);
         inst_spfm_add_sub._start(wr_operand1, wr_operand2,0,wr_funct3,1,0,1);
		 $display($time,"\tGiving inputs to the spfloating multiplier module");
	 endrule

	 rule rl_give_inputs_to_fm_add_sub(wr_inputs_ready && ((wr_opcode == 'b10000) || (wr_opcode == 'b10001) || (wr_opcode == 'b10010) || wr_opcode == 'b10011));
		 inst_spfm_add_sub._start(wr_operand1,wr_operand2,wr_operand3,wr_funct3,wr_opcode[0]^wr_opcode[1],wr_opcode[1],0);
		 $display($time,"\tGiving Inputs to sp fused multiply add Conversion Module");
       $display($time,"\tOperand 1: %h Operand 2: %h Operand 3: %h",wr_operand1[31:0], wr_operand2[31:0], wr_operand3[31:0]);
	 endrule

    //rule to give inputs to classify module
   rule rl_give_inputs_to_spfloating_classify(wr_inputs_ready && ((((wr_funct7 == 'b1110000) || (wr_funct7 == 'b1110001))&&(wr_funct3=='b001)) && wr_opcode == 'b10100));
	  inst_spfpu_fclass._start(wr_operand1);
		$display($time,"\tGiving inputs to floating classify module");
	 endrule

  //rule to get output from fp classify  
  rule rl_get_output_from_spfclass;
		$display($time,"\tGot output from spclassify module");
		wr_result <= tagged Valid inst_spfpu_fclass.result_;
    take_new_inputs<=True;
  endrule

  //rule for fmv.s.x and fmv.x.s instructions
  rule rl_give_inputs_get_outputs_fmv(wr_inputs_ready && (((wr_funct7 == 'b1110000 || wr_funct7 == 'b1111000) && wr_funct3 == 'b000) && wr_opcode == 'b10100));
        $display($time,"\tGiving inputs to FMV");
        wr_result <= tagged Valid Floating_output{
                                                  final_result : zeroExtend(wr_operand1),
                                                  fflags : 0};
        take_new_inputs<=True;
  endrule
	
  Reg#(Maybe#(Exception_cause)) wr_exception<-mkDReg(tagged Invalid);
	//method which starts performing a floating point operation
	method ActionValue#(Maybe#(Floating_output#(`Reg_width))) _start(Bit#(`Reg_width) operand1, Bit#(`Reg_width) operand2, Bit#(`Reg_width) operand3, Bit#(5) opcode, Bit#(7) funct7, Bit#(3) funct3, Bit#(2) immediate_value,  Bit#(32) fsr); // input method to start the floating point operation

	  Bit#(3) rounding_mode = (funct3 == 'b111) ? fsr[7:5] : funct3;
    if(funct3=='b111 && fsr[7:5]>4)begin
      wr_exception<=tagged Valid Illegal_inst;
      wr_result <= tagged Valid Floating_output{final_result :0,
                                                fflags : 0};
    end
    else if(take_new_inputs &&& wr_result matches tagged Invalid)begin
      wr_inputs_ready<= True;
      wr_opcode		<=	opcode;
      wr_funct7		<=	funct7;
      wr_funct3		<=	rounding_mode;
      wr_imm			<=	immediate_value;
      wr_operand1		<= operand1;                         // wire that holds the value of input 1
      wr_operand2		<= operand2;                         // wire that holds the value of input 2
      wr_operand3		<= operand3;                         // wire that holds the value of input 3, required for Fused Multiply add alone since it is a 3 input instruction,wire holds zero for others.
      take_new_inputs<= False;
    end

    return wr_result;
	endmethod

  method ActionValue#(Maybe#(Exception_cause)) exception;
    return wr_exception;
  endmethod
endmodule

/********************************************************* Testbench Module to test the FPU with IBM's FPgen Testcases**********************************************************************************/
//module mkTb_fpgen(Empty);
//
//Reg#(Bit#(32)) rg_clock <- mkReg(0);
//Reg#(int) cnt <- mkReg(0);
//Reg#(Bit#(16)) index <- mkReg(0);
//let file_handler <- mkReg(InvalidFile);
//
///****************************** Instantiating Register files of Single Precision Testcases *************************************************************************************************************/
//RegFile#(Bit#(16),Bit#(68)) add_nor_inp <- mkRegFileFullLoad("./testcases/fpgen_testcases/Add_normal_testcases.hex");   
//RegFile#(Bit#(16),Bit#(68)) add_denor_inp <- mkRegFileFullLoad("./testcases/fpgen_testcases/Add_denormal_testcases.hex");
//RegFile#(Bit#(16),Bit#(68)) sub_nor_inp <- mkRegFileFullLoad("./testcases/fpgen_testcases/Sub_normal_testcases.hex");
//RegFile#(Bit#(16),Bit#(68)) sub_denor_inp <- mkRegFileFullLoad("./testcases/fpgen_testcases/Sub_denormal_testcases.hex");
//RegFile#(Bit#(16),Bit#(68)) div_nor_inp <- mkRegFileFullLoad("./testcases/fpgen_testcases/Div_normal_testcases.hex");   
//RegFile#(Bit#(16),Bit#(68)) div_denor_inp <- mkRegFileFullLoad("./testcases/fpgen_testcases/Div_denormal_testcases.hex");   
//RegFile#(Bit#(16),Bit#(68)) mul_nor_inp <- mkRegFileFullLoad("./testcases/fpgen_testcases/Mul_normal_testcases.hex");   
//RegFile#(Bit#(16),Bit#(68)) mul_denor_inp <- mkRegFileFullLoad("./testcases/fpgen_testcases/Mul_denormal_testcases.hex");   
//RegFile#(Bit#(16),Bit#(100)) fma_inp <- mkRegFileFullLoad("./testcases/fpgen_testcases/FMA_testcases.hex");
//RegFile#(Bit#(16),Bit#(36)) sqrt_inp <- mkRegFileFullLoad("./testcases/fpgen_testcases/Sqrt_testcases.hex"); 
///*******************************************************************************************************************************************************************************************************/
//
//Ifc_fpu fpu <- mkfpu();  //Instantiating FPU module
//
//rule open_add_normal (cnt == 0);
//    File add_out_nor <- $fopen("./testcases/RTL_outputs/Add_normal_outputs.hex", "w+");
//    file_handler <= add_out_nor;
//    cnt <= cnt + 1;
//endrule
//
//rule execute_add_normal (cnt == 1);
//    if(index == 16561) 
//        cnt <= cnt + 2;
//    else begin
//        $display($time,"\tGoing to Add");
//        fpu._start(zeroExtend(add_nor_inp.sub(index)[67:36]),zeroExtend(add_nor_inp.sub(index)[35:4]),'b0,{7'b0,10'd0,add_nor_inp.sub(index)[2:0],5'd0,7'b1010011},32'h0);
//        index <= index + 1;
//        cnt <= cnt + 1;
//    end
//endrule
//
//rule open_add_denormal (cnt == 3);
//    File add_out_denor <- $fopen("./testcases/RTL_outputs/Add_denormal_outputs.hex","w+");
//    file_handler <= add_out_denor;
//    cnt <= cnt + 1;
//    index <= 0;
//endrule
//
//rule execute_add_denormal (cnt == 4);
//    if(index == 56)
//        cnt <= cnt + 2;
//    else begin
//        fpu._start(zeroExtend(add_denor_inp.sub(index)[67:36]),zeroExtend(add_denor_inp.sub(index)[35:4]),'b0,{7'b0,10'd0,add_denor_inp.sub(index)[2:0],5'd0,7'b1010011},32'h0);
//        index <= index + 1;
//        cnt <= cnt + 1;
//    end
//endrule
//
//rule open_sub_normal (cnt == 6);
//    File sub_out_nor <- $fopen("./testcases/RTL_outputs/Sub_normal_outputs.hex","w+");
//    file_handler <= sub_out_nor;
//    cnt <= cnt + 1;
//    index <= 0;
//endrule
//
//rule execute_sub_normal (cnt == 7);
//    if(index == 14999)
//        cnt <= cnt + 2;
//    else begin
//        fpu._start(zeroExtend(sub_nor_inp.sub(index)[67:36]),zeroExtend(sub_nor_inp.sub(index)[35:4]),'b0,{7'b0000100,10'd0,sub_nor_inp.sub(index)[2:0],5'd0,7'b1010011},32'h0);
//        index <= index + 1;
//        cnt <= cnt + 1;
//    end
//endrule
//
//rule open_sub_denormal (cnt == 9);
//    File sub_out_denor <- $fopen("./testcases/RTL_outputs/Sub_denormal_outputs.hex","w+");
//    file_handler <= sub_out_denor;
//    cnt <= cnt + 1;
//    index <= 0;
//endrule
//
//rule execute_sub_denormal (cnt == 10);
//    if(index == 56) begin
//        $display($time,"\tIndex : %d", index); 
//        cnt <= cnt + 2;
//    end
//   else begin
//        fpu._start(zeroExtend(sub_denor_inp.sub(index)[67:36]),zeroExtend(sub_denor_inp.sub(index)[35:4]),'b0,{7'b0000100,10'd0,sub_denor_inp.sub(index)[2:0],5'd0,7'b1010011},32'h0);
//        index <= index + 1;
//        cnt <= cnt + 1;
//    end
// 
//endrule
//
//rule open_div_normal (cnt == 12);
//    File div_out_nor <- $fopen("./testcases/RTL_outputs/Div_normal_outputs.hex","w+");
//    file_handler <= div_out_nor;
//    cnt <= cnt + 1;
//    index <= 0;
//endrule
//
//rule execute_div_normal (cnt == 13);
//    if(index == 788)
//        cnt <= cnt + 2;
//    else begin
//        fpu._start(zeroExtend(div_nor_inp.sub(index)[67:36]),zeroExtend(div_nor_inp.sub(index)[35:4]),'b0,{7'b0001100,10'd0,div_nor_inp.sub(index)[2:0],5'd0,7'b1010011},32'h0);
//        index <= index + 1;
//        cnt <= cnt + 1;
//    end
//endrule
//
//rule open_div_denormal (cnt == 15);
//    File div_out_denor <- $fopen("./testcases/RTL_outputs/Div_denormal_outputs.hex","w+");
//    file_handler <= div_out_denor;
//    cnt <= cnt + 1;
//    index <= 0;
//endrule
//
//rule execute_div_denormal (cnt == 16);
//    if(index == 406) begin
//         cnt <= cnt + 2;
//    end
//    else begin
//         fpu._start(zeroExtend(div_denor_inp.sub(index)[67:36]),zeroExtend(div_denor_inp.sub(index)[35:4]),'b0,{7'b0001100,10'd0,div_denor_inp.sub(index)[2:0],5'd0,7'b1010011},32'h0);
//         index <= index + 1;
//         cnt <= cnt + 1;
//     end
//
//endrule
//
//rule open_mul_normal (cnt == 18);
//    File mul_out_nor <- $fopen("./testcases/RTL_outputs/Mul_normal_outputs.hex","w+");
//    file_handler <= mul_out_nor;
//    cnt <= cnt + 1;
//    index <= 0;
//endrule
//
//rule execute_mul_normal (cnt == 19);
//    if(index == 931) begin
//         cnt <= cnt + 2;
//    end
//    else begin
//         fpu._start(zeroExtend(mul_nor_inp.sub(index)[67:36]),zeroExtend(mul_nor_inp.sub(index)[35:4]),'b0,{7'b0001000,10'd0,mul_nor_inp.sub(index)[2:0],5'd0,7'b1010011},32'h0);
//         index <= index + 1;
//         cnt <= cnt + 1;
//     end
//
//endrule
//
//rule open_mul_denormal (cnt == 21);
//    File mul_out_nor <- $fopen("./testcases/RTL_outputs/Mul_denormal_outputs.hex","w+");
//    file_handler <= mul_out_nor;
//    cnt <= cnt + 1;
//    index <= 0;
//endrule
//
//rule execute_mul_denormal (cnt == 22);
//    if(index == 593) begin
//         cnt <= cnt + 2;
//    end
//    else begin
//         fpu._start(zeroExtend(mul_denor_inp.sub(index)[67:36]),zeroExtend(mul_denor_inp.sub(index)[35:4]),'b0,{7'b0001000,10'd0,mul_denor_inp.sub(index)[2:0],5'd0,7'b1010011},32'h0);
//         index <= index + 1;
//         cnt <= cnt + 1;
//     end
//endrule
//
//rule open_fma (cnt == 24);
//    File fma_nor <- $fopen("./testcases/RTL_outputs/FMA_outputs.hex","w+");
//    file_handler <= fma_nor;
//    cnt <= cnt + 1;
//    index <= 0;
//endrule
//
//rule execute_fma (cnt == 25);
//    if(index == 18732) begin
//         cnt <= cnt + 2;
//    end
//    else begin
//         fpu._start(zeroExtend(fma_inp.sub(index)[99:68]),zeroExtend(fma_inp.sub(index)[67:36]),zeroExtend(fma_inp.sub(index)[35:4]),{7'b0000000,10'd0,fma_inp.sub(index)[2:0],5'd0,7'b1000011},32'h0);
//         index <= index + 1;
//         cnt <= cnt + 1;
//     end
//endrule
//
//rule open_sqrt (cnt == 27);
//    File sqrt_nor <- $fopen("./testcases/RTL_outputs/Sqrt_outputs.hex","w+");
//    file_handler <= sqrt_nor;
//    cnt <= cnt + 1;
//    index <= 0;
//endrule
//
//rule execute_sqrt (cnt == 28);
//    if(index == 64) begin
//         $finish();
//    end
//    else begin
//         fpu._start(zeroExtend(sqrt_inp.sub(index)[35:4]),'b0,'b0,{7'b0101100,10'd0,sqrt_inp.sub(index)[2:0],5'd0,7'b1010011},{'b0,sqrt_inp.sub(index)[2:0],5'b0});
//         index <= index + 1;
//         cnt <= cnt + 1;
//     end
//endrule
//
//rule get_result(fpu.fpu_result_ matches tagged Valid .res);
//    $fwrite(file_handler,"%h\n", res.final_result[31:0]);
//    cnt <= cnt - 1;
//endrule
//
//endmodule
///********************************************************************************************************************************************************************************************************/
//


endpackage
