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
e-mail id	: c.arjunmenon@gmail.com, g.vinod1993@gmail.com
Last updated on : 6th January 2016

	This unit performs all floating point operations. There are 7 functional units: 1. Add/Sub/Mul/Div, 2. Square root/Sign injection, 3. Single or Double FP to signed or unsigned to 32-bit or 64-bit integer conversion, 4. Signed or unsigned 32-bit or 64-bit integer to Single or Double FP conversion, 5. Single to double or vice-versa, 6. Compare instructions- Eq/Gt/Lt and Min/Max, 7. FSSR/FRSR/Move from SP or DP FP reg file to integer reg file, vice versa.

Here the output is obtained in different rules, as it is out-of-order execution. IN this case if one instruction takes less time for execution then, its result will be available and next process can work parallel, if its taking more time.

Tips and Tricks:
	-> Delay and area is max. when we use FIFO.
	   Delay and area is moderate when we use UGFIFOF1( Unguarded FIFO of depth=1 and with explicit notEmpty, notFull, etc. signals).
	   Delay and area are least when we used PipelinedFIFO.


*********Performance****************:
Using UMCIP 65nm library in SYNOPSYS
Critical Path Length     :    0.556 ns
Max. Operating Frequency :    1.8 GHz
Combinational Cell Count :    4405
Sequential Cell Count    :    434

Critical path is inst_int_to_fp_/ff_input_stage to inst_int_to_fp_/final_out.
*/
package fpu;

import riscv_types::*;					//contains various typedefs
import fpu_compare_min_max::*;						
import fpu_int_to_fp::*;
import fpu_convert_sp_dp::*; 
import fpu_add_sub::*;
import fpu_sign_injection::*;	
import fpu_spfloating_divider::*;
import fpu_dpfloating_divider::*;
import fpu_sqrt::*;
import fpu_fp_to_int::*;
import fpu_fm_add_sub::*;
import fpu_spfloating_multiplier::*;
import fpu_fclass::*;
interface Ifc_fpu;							//interface to module mk_fpu
	method Action _start(Bit#(64) operand1, Bit#(64) operand2, Bit#(64) operand3, Bit#(32) instruction, Bit#(5) destination,  Bit#(32) fsr, Bit#(4) rob_number, Bit#(32) pc); // input method to start the floating point operation
	method Maybe#(Output_type) fpu_result_();			// Output method
	method Action _set_flush(Bool f);				// Input method to initiate the flush routine
	method Bool inputs_taken_();					// Output method to indicate whether the input has been taken
endinterface

(*preempts= "rl_get_output_from_fpu_convert_sp_dp, rl_get_output_from_fpu_int_to_fp" *)
(*preempts= "rl_get_output_from_fpu_int_to_fp, rl_get_output_from_fpu_compare_min_max" *)// schedules the rule 'rl_get_output_from_fpu_int_to_fp' before 'rl_get_output_from_fpu_compare_min_max' if there is a conflict i.e. if both rules fire at the same time

(*synthesize*)
module mkfpu(Ifc_fpu);
	Wire#(Bool) wr_flush <-mkDWire(False); 				// wire to indicate that a flush has occured in the processor and all buffer need to be cleared.
	Wire#(Maybe#(Output_type)) wr_result<- mkDWire(tagged Invalid);	// wire that holds the result
	Wire#(Bool) wr_inputs_taken <-mkDWire(False);			// wire to indicate that the inputs have been taken

	Wire#(Bit#(5))  wr_destination<-mkDWire(0);			// wire that holds the destinaion address of the current instruction which is to be executed
	Wire#(Bit#(32)) wr_fsr<-mkDWire(0);				// wire that holds the fsr of the current instruction which is to be executed
	Wire#(Bit#(32)) wr_program_counter<-mkDWire(0);			// wire that holds the value of program counter of the current instruction which is to be executed
	Wire#(Bit#(4))  wr_rob_number<-mkDWire(0);			// wire that holds the rob number of the current instruction which is to be executed
	Wire#(Maybe#(Bit#(32))) wr_instruction<-mkDWire(tagged Invalid);// wire that holds the instruction of the current instruction which is to be executed
									// It is tagged Valid whenever the method Action _start is invoked and assigned the valueof current instruction
	Wire#(Bit#(64))	wr_operand1<-mkDWire(0);			// wire that holds the value of operand1 of the current instruction which is to be executed
	Wire#(Bit#(64))	wr_operand2<-mkDWire(0);			// wire that holds the value of operand2 of the current instruction which is to be executed
        Wire#(Bit#(64)) wr_operand3<-mkDWire(0);       
 
	Ifc_fpu_compare_min_max inst_fpu_compare_min_max<- mkfpu_compare_min_max();	// Instantiating compare or min/max module
	Ifc_fpu_int_to_fp inst_fpu_int_to_fp <- mkfpu_int_to_fp();			// Instantiating integer to floating point conversion module
	Ifc_fpu_convert_sp_dp inst_fpu_convert_sp_dp <- mkfpu_convert_sp_dp();		// Instantiating SP to DP/ DP to SP conversion module
        Ifc_fpu_add_sub#(32,23,8) inst_spfpu_add_sub <- mkfpu_add_sub();                // Instantiating SP Add_Sub Module
        Ifc_fpu_add_sub#(64,52,11) inst_dpfpu_add_sub <- mkfpu_add_sub();               // Instantiating DP Add_Sub Module
        Ifc_fpu_sign_injection#(32,23,8) inst_spfpu_sign_injection <- mkfpu_sign_injection(); //Instantiating SP sign injection Module
        Ifc_fpu_sign_injection#(64,52,11) inst_dpfpu_sign_injection <- mkfpu_sign_injection(); //Instantiating DP sign injection Module
        Ifc_spfloating_divider inst_spfpu_divider <- mkfpu_spfloating_divider();        //Instantiating SP Floating Divider Module
        Ifc_dpfloating_divider inst_dpfpu_divider <- mkfpu_dpfloating_divider();        //Instantiating DP Floating Divider Module
        Ifc_fpu_sqrt inst_spfpu_sqrt <- mkfpu_sqrt();                                   //Instantiating SP Floating Square root Module
        Ifc_fpu_fp_to_int inst_spfp_to_int <- mkfpu_fp_to_int();                        //Instantiating SP Floating to Int Conversion Module
        Ifc_fpu_fm_add_sub inst_spfm_add_sub <- mkfpu_fm_add_sub();                     //Instantiating Fused Multiply Add_Sub Module
        Ifc_fpu_spfloating_multiplier inst_spfloating_multiplier <- mkfpu_spfloating_multiplier(); //Instantiating SP floating Multiplier Module
        Ifc_fpu_fclass#(32,23,8) inst_spfpu_fclass <- mkfpu_fclass();                   //Instantiating SP Floating Point Classify Module
        Ifc_fpu_fclass#(64,52,11) inst_dpfpu_fclass <- mkfpu_fclass();                  // Instantiating DP Floating Point Classify Module
	//When flush is initiated, the rodules results are flushed
	rule rl_flush_all_fifos(wr_flush);
		inst_fpu_compare_min_max._set_flush(True);
		inst_fpu_int_to_fp._set_flush(True);
		inst_fpu_convert_sp_dp._set_flush(True);			
	endrule

	//rule that gives inputs to the integer to floating point converter
	rule rl_give_inputs_to_fpu_int_to_fp(!wr_flush &&& wr_instruction matches tagged Valid .instr &&& (instr[31:25]=='b1101000)||(instr[31:25]=='b1101001));
		inst_fpu_int_to_fp._start(wr_operand1, instr, wr_destination,  wr_fsr, wr_rob_number, wr_program_counter);
		wr_inputs_taken<= True;
		$display("Giving inputs to fpu_int_to_fp %h",wr_operand1);
	endrule
		
	//rule that takes output from integer to fo floating point converter
	rule rl_get_output_from_fpu_int_to_fp(!wr_flush);
		$display("Got output from fpu_int_to_fp");
		wr_result<= tagged Valid inst_fpu_int_to_fp.result_;
		inst_fpu_int_to_fp._deque_buffer();
	endrule

	//rule that gives inputs to compare or min/max module
	rule rl_give_inputs_to_fpu_compare_min_max(!wr_flush &&& wr_instruction matches tagged Valid .instr &&& ((instr[31:27]=='b00101) || instr[31:27] == 'b10100));
		//the following instance of the compare_min_max module is given with the input values.
		//instr[7]=0 => input is single precision, else input is double precision
		//instr[13:12] => tells which compare instr is it.
		//instr[14] is used to distinguish between compare and min max instructions. instr[14]=1 => compare instr, else min_max instr.
		inst_fpu_compare_min_max._start(wr_operand1, wr_operand2, instr[7], instr[14:12], instr[14], wr_destination,  wr_fsr, wr_rob_number, wr_program_counter);
		wr_inputs_taken<= True;
		$display("Giving inputs to fpu_compare_min_max");
	endrule

	//rule that takes output from compare or min/max module
	rule rl_get_output_from_fpu_compare_min_max(!wr_flush);
		$display("Got output from fpu_compare_min_max");
		wr_result<= tagged Valid inst_fpu_compare_min_max.result_;
		inst_fpu_compare_min_max._deque_buffer();
	endrule

	//rule that gives inputs to SP to DP/ DP to SP conversion module
	rule rl_give_inputs_to_fpu_convert_sp_dp(!wr_flush &&& wr_instruction matches tagged Valid .instr &&& (instr[31:25]=='b0100000)||(instr[31:25]=='b0100001));
		//the following instance of the compare_min_max module is given with the input values.
		//instr[7]=0 => conversion is from SP to DP; =1 => conversion is from DP to SP
		inst_fpu_convert_sp_dp._start(wr_operand1, instr[25], wr_destination,  wr_fsr, wr_rob_number, wr_program_counter);
		wr_inputs_taken<=True;
		$display("Give inputs to fpu_convert_sp_dp");
	endrule

	//rule that gives takes output from SP to DP/ DP to SP conversion module
	rule rl_get_output_from_fpu_convert_sp_dp(!wr_flush);
		wr_result <= tagged Valid inst_fpu_convert_sp_dp.result_;
		inst_fpu_convert_sp_dp._deque_buffer();
	$display("Got output from fpu_convert_sp_dp");
	endrule

        //rule that gives inputs to the add_sub module
        rule rl_give_inputs_to_fpu_add_sub(!wr_flush &&& wr_instruction matches tagged Valid .instr &&& (((instr[31:25] == 'b0000000))||(instr[31:25] == 'b0000001)||(instr[31:25]=='b0000100)||(instr[31:25]=='b0000101)));
        if(instr[25] == 1) 
        inst_dpfpu_add_sub._start(wr_operand1, wr_operand2, instr[2], instr[14:12]);
        else
        inst_spfpu_add_sub._start(truncate(wr_operand1), truncate(wr_operand2), instr[2], instr[14:12]);
        wr_inputs_taken <= True;
        $display("Giving inputs to the fpu add_sub");
        endrule

        //rule that gets output from the dp add_sub module
        rule rl_get_output_from_dpfpu_add_sub(!wr_flush);
            $display("Got output from dpfpu add_sub unit");
            wr_result <= tagged Valid inst_dpfpu_add_sub._result;
            inst_dpfpu_add_sub._deque_buffer_();
        endrule

        //rule that gets output from the sp add_sub module
        rule rl_get_output_from_spfpu_add_sub(!wr_flush);  
            $display("Got output from spfpu add_sub unit");
            wr_result <= tagged Valid inst_spfpu_add_sub._result;
            inst_spfpu_add_sub._deque_buffer_();
	endrule
 
        //rule that gives inputs to fpu_sign_injection module
 	rule rl_give_inputs_to_fpu_sign_injection(!wr_flush &&& wr_instruction matches tagged Valid .instr &&& ((instr[31:25] == 'b0010000))||(instr[6:2] == 'b0010001));
        if(instr[25] == 1) 
        inst_dpfpu_sign_injection._start(wr_operand1, wr_operand2, instr[14:12]);
        else
        inst_spfpu_sign_injection._start(truncate(wr_operand1), truncate(wr_operand2), instr[14:12]);
        wr_inputs_taken <= True;
        $display("Giving inputs to the fpu sign injection");
        endrule
 
        //rule to get output from dpfpu sign injection
        rule rl_get_output_from_dpfpu_sign_injection(!wr_flush);
            $display("Got output from dpfpu sign injection");
	    wr_result <= tagged Valid inst_dpfpu_sign_injection.result_;
            inst_dpfpu_sign_injection._deque_buffer_();
        endrule

        //rule to get output from spfpu sign injection
        rule rl_get_output_from_spfpu_sign_injection(!wr_flush);  
            $display("Got output from spfpu sign injection unit");
            wr_result <= tagged Valid inst_spfpu_sign_injection.result_;
            inst_spfpu_sign_injection._deque_buffer_();
	endrule
 
        //rule to give inputs to spfpu divider
        rule rl_give_inputs_to_spfpu_divider(!wr_flush &&& wr_instruction matches tagged Valid .instr &&& (instr[31:25] == 'b0001100));
        inst_spfpu_divider._start(truncate(wr_operand1),truncate(wr_operand2),wr_destination,wr_rob_number,wr_fsr,wr_program_counter);
        wr_inputs_taken <= True;
        $display("Giving inputs to the spfpu divider");
        endrule

        //rule to get output from spfpu divider
        rule rl_get_output_from_spfpu_divider(!wr_flush);
             $display("Got output from spfpu divider"); 
             wr_result <= tagged Valid inst_spfpu_divider.final_result_;
             inst_spfpu_divider._deque_buffer_reset_ready_signal();
        endrule

        //rule to give inputs to dpfpu divider
        rule rl_give_inputs_to_dpfpu_divider(!wr_flush &&& wr_instruction matches tagged Valid .instr &&& (instr[31:25] == 'b0001101));
        inst_dpfpu_divider._start(wr_operand1,wr_operand2,wr_destination,wr_rob_number,wr_fsr,wr_program_counter);
        wr_inputs_taken <= True;
        $display("Giving inputs to the dpfpu divider");
        endrule

        //rule to get output from dpfpu divider
        rule rl_get_output_from_dpfpu_divider(!wr_flush);
             $display("Got output from dpfpu divider"); 
             wr_result <= tagged Valid inst_dpfpu_divider.final_result_;
             inst_dpfpu_divider._deque_buffer_reset_ready_signal();
        endrule

        //rule to give inputs to spfpu square root module
        rule rl_give_inputs_to_spfpu_sqrt(!wr_flush &&& wr_instruction matches tagged Valid .instr &&& (instr[31:25] == 'b0101100));
        inst_spfpu_sqrt._start(truncate(wr_operand1),wr_destination,wr_fsr,wr_rob_number,wr_program_counter);
        wr_inputs_taken <= True;
        $display("Giving inputs to the spfpu sqrt");
        endrule

        //rule to get output spfpu square root module
        rule rl_get_output_from_spfpu_sqrt(!wr_flush);
             $display("Got output from spfpu sqrt"); 
             wr_result <= tagged Valid inst_spfpu_sqrt.get_result;
             inst_spfpu_sqrt.deque_buffer();
        endrule

       //rule to get inputs from fp to int module 
       rule rl_give_inputs_to_fp_to_int(!wr_flush &&& wr_instruction matches tagged Valid .instr &&& (instr[31:25] == 'b1100000));
       inst_spfp_to_int._start(truncate(wr_operand1),instr[20],wr_fsr);
       wr_inputs_taken <= True;
       $display("Giving Inputs to fpu to int Conversion Module");  
       endrule

       //rule to get output from fp to int
       rule rl_get_output_from_fp_to_int(!wr_flush);
         $display("Got output from fpu to int conversion Module");
         wr_result <= tagged Valid inst_spfp_to_int.get_result;
         inst_spfp_to_int.deque_buffer();
       endrule

       //rule to give inputs to fused multiply add sub
       rule rl_give_inputs_to_fm_add_sub(!wr_flush &&& wr_instruction matches tagged Valid .instr &&& ((instr[6:2] == 'b10000) || (instr[6:2] == 'b10001) || (instr[6:2] == 'b10010) || instr[6:2] == 'b10011));
       inst_spfm_add_sub._start(truncate(wr_operand1),truncate(wr_operand2),truncate(wr_operand3),instr[2]^instr[3],wr_fsr,instr[3]);
       wr_inputs_taken <= True;
       $display("Giving Inputs to sp fused multiply add Conversion Module");  
       endrule

       //rule to get output from fused multiply add sub
       rule rl_get_output_from_fm_add_sub(!wr_flush);
         $display("Got output from sp fused multiple add conversion Module");
         wr_result <= tagged Valid inst_spfm_add_sub.get_result;
         inst_spfm_add_sub.deque_buffer();
       endrule

       //rule to give inputs to spfloating multiplier
       rule rl_give_inputs_to_spfloating_multiplier(!wr_flush &&& wr_instruction matches tagged Valid .instr &&& (instr[31:25] == 'b0001000));
       inst_spfloating_multiplier._start(truncate(wr_operand1),truncate(wr_operand2),wr_destination,wr_rob_number,wr_fsr,wr_program_counter);
       wr_inputs_taken <= True;
       $display("Giving inputs to the spfloating multiplier module");
       endrule

       //rule to get output from spfloating multiplier
       rule rl_get_output_from_spfloating_multiplier(!wr_flush);
       $display("Got output from spfloating multiplier module");
       wr_result <= tagged Valid inst_spfloating_multiplier.result_multiplication_;
       inst_spfloating_multiplier._deque_buffer_reset_ready_signal();
       endrule

      rule rl_give_inputs_to_spfloating_classify(!wr_flush &&& wr_instruction matches tagged Valid .instr &&& (((instr[31:25] == 'b1110000) || (instr[31:25] == 'b1110001))&&(instr[14:12]=='b001)));
      if (instr[25] == 'b0)
      inst_spfpu_fclass._start(truncate(wr_operand1));
      else
      inst_dpfpu_fclass._start(wr_operand1);
      wr_inputs_taken <= True;
      $display("Giving inputs to floating classify module");
      endrule

      rule rl_get_output_from_spfclass(!wr_flush);
      $display("Got output from spclassify module");
      wr_result <= tagged Valid inst_spfpu_fclass.result_;
      inst_spfpu_fclass._deque_buffer_();
      endrule
      
      rule rl_get_output_from_dpfclass(!wr_flush);
      $display("Got output from dpclassify module");
      wr_result <= tagged Valid inst_dpfpu_fclass.result_;
      inst_dpfpu_fclass._deque_buffer_();
      endrule

		
	//method which starts performing a floating point operation
	method Action _start(Bit#(64) operand1, Bit#(64) operand2, Bit#(64) operand3, Bit#(32) instruction, Bit#(5) destination,  Bit#(32) fsr, Bit#(4) rob_number, Bit#(32) pc);
		
		wr_destination<= destination;                   //wire that holds rd register 
		wr_fsr<= fsr;                                   // wire that holds the values of floating point status register
		wr_program_counter<= pc;                        // wire that holds the program counter of the current instruction being executed
		wr_rob_number<= rob_number;                     // wire that holds the value of the numerical value of the re-order buffer entry
		wr_instruction<= tagged Valid instruction;	// instruction is tagged Valid and assigned the value of the current instruction.
								// the value of instruction decides which rule to fire.
		wr_operand1<= operand1;                         // wire that holds the value of input 1
		wr_operand2<= operand2;                         // wire that holds the value of input 2
                wr_operand3<= operand3;                         // wire that holds the value of input 3, required for Fused Multiply add alone since it is a 3 input instruction, for other instructions the wire holds zero.
		                                                //TODO operand 3 is a useless wire except for FMA instructions, should not be sent for every instruction.
	endmethod

	//Output method which returns the result of the operation
	method Maybe#(Output_type) fpu_result_();
		return wr_result;
	endmethod


	// when a flush is initiated in the processor this method will also be called.
	// it sets the flsuh wire to True
	method Action _set_flush(Bool f);
		wr_flush<=f;
	endmethod

	// This method tells whether the inputs have been taken by the corresponding execution unit or not
	method Bool inputs_taken_();
		return wr_inputs_taken;
	endmethod
endmodule

//testbench to give sample inputs to each module at each clock cycle and get the output when it arrives
//(*synthesize*)
module mkTb_fpu(Empty);

Reg#(Bit#(64)) input1<-mkReg(64'hff800010);	// operand 1
Reg#(Bit#(64)) input2<-mkReg(64'hbf000001);	// operand 2
Reg#(Bit#(64)) input3<-mkReg(64'hccf00002);
Reg#(Bit#(32)) clk<-mkReg(0);			// Clock
Ifc_fpu instance1_fpu <- mkfpu();		// An instance of the floating point unit


//rule that increments clock and keeps track of it
rule rl_clock_;
	clk <= clk+1;
	//$display("input 1= %h, input 2= %h", input1, input2);
	$display("%d",clk);
	
endrule

//rule which specifies when to stop simulation
rule rl_end_(clk == 'd45);
	$finish(0);
endrule

//The following rules call method Action _start and give inputs to the fpu.
//For other Opcodes to test - Please refer to RISCV-ISA Manual V 2.0

//sample input to fpu_int to fp module - Using FCVT.S.W instruction 
rule rl_input_1(clk == 'd2);
	instance1_fpu._start(input1,input2,'d0,{7'b1101000,5'b00000,5'b00001,3'b010,5'b00010,7'b1010011},2,32'h00300060,0,0);
endrule

//sample input to fpu_compare_min_max - using FMIN.S instruction
rule rl_input_2(clk == 'd4);
	instance1_fpu._start(input1,input2,'d0,{7'b0010100,5'b00001,5'b00010,3'b000,5'b00011,7'b1010011},2,32'h00300060,0,0);
endrule

//Sample input to sp/dp converter - using FCVT.S.D instruction 
rule rl_input_3(clk == 'd6);
	instance1_fpu._start(input1,input2,'d0,{7'b0100000,5'b00001,5'b00010,3'b011,5'b00011,7'b1010011},2,32'h00300060,0,0);
endrule

//Sample input to add sub module

rule rl_input_4(clk == 'd8);
        instance1_fpu._start(input1,input2,'d0,{7'b0000000,5'b00001,5'b00010,3'b010,5'b00011,7'b1010011},2,32'h00300060,0,0);
endrule


//sample input to sign injection module
rule rl_input_5(clk == 'd8);
        instance1_fpu._start(input1,input2,'d0,{7'b0010000,5'b00001,5'b00010,3'b000,5'b00011,7'b1010011},2,32'h00300060,0,0);
endrule

//sample input to spdiv module
rule rl_input_6(clk == 'd10);
        instance1_fpu._start(input1,input2,'d0,{7'b0001100,5'b00001,5'b00010,3'b000,5'b00011,7'b1010011},2,32'h00300060,0,0);
endrule

//sample input to dpdiv module
rule rl_input_7(clk == 'd12);
        instance1_fpu._start(input1,input2,'d0,{7'b0001101,5'b00001,5'b00010,3'b000,5'b00011,7'b1010011},2,32'h00300060,0,0);
endrule
//sample input to spsqrt module
rule rl_input_8(clk == 'd14);
        instance1_fpu._start(input1,input2,'d0,{7'b0101100,5'b00000,5'b00010,3'b000,5'b00011,7'b1010011},2,32'h00300060,0,0);
endrule
//sample input to fp-to-int module
rule rl_input_9(clk == 'd16);
        instance1_fpu._start(input1,input2,'d0,{7'b1100000,5'b00000,5'b00010,3'b000,5'b00011,7'b1010011},2,32'h00300060,0,0);
endrule

rule rl_input_10(clk== 'd18);
              instance1_fpu._start(input1,input2,'d0,{7'b0001000,5'b00000,5'b00010,3'b000,5'b00011,7'b1010011},2,32'h00300060,0,0);
endrule


//sample input to FMA module
rule rl_input_11(clk == 'd20);
        instance1_fpu._start(input1,input2,'d0,{5'b11111,2'b00,5'b00000,5'b00010,3'b000,5'b00011,7'b1000011},2,32'h00300060,0,0);
endrule

rule rl_input_12(clk == 'd26);
        instance1_fpu._start(input1,input2,'d0,{7'b1110000,5'b00000,5'b00010,3'b001,5'b00011,7'b100011},2,32'h00300060,0,0);
endrule

rule rl_input_13(clk == 'd30);
        instance1_fpu._start(input1,input2,'d0,{7'b1110001,5'b00000,5'b00010,3'b001,5'b00011,7'b100011},2,32'h00300060,0,0);
endrule









//This rule gets the output from the fpu whenever it is ready and displays the result and tells if any exception was generated.
rule rl_output_(instance1_fpu.fpu_result_ matches tagged Valid .abc);
	
	$display("result= %h",abc.final_result);

	/*$display("fsr= %h",abc.fsr);

	//instance1_fpu._set_flush(True);
	if(abc.exception matches tagged No_exception.*)
		$display("NO EXCEPTION");
	else if(abc.exception matches tagged Invalid .*)
		$display("INVALID EXCEPTION");
	else if(abc.exception matches tagged Inexact.*)
		$display("INEXACT EXCEPTION");
	else if(abc.exception matches tagged Overflow.*)
		$display("OVERFLOW EXCEPTION");*/
			
endrule 
	
endmodule:mkTb_fpu

endpackage
