/*Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name: Tb_integer_multiplier_riscv
Author Name: Rishi Naidu
Email id:    rishinaidu.rn@gmail.com
Last updated on : 29th October 2013

This is a parameterized bit integer divider that completes the division operation in 34 cycles for 64 by 64 division and 18 cycles for 32 by 32 division using "Non Restoring Division Algorithm". 
Here the divider is implemented in form of a state machine, which a CPI of 34 cycles. Using a state machine instead of Pipelining helps to decrease the area by huge amount.
Here SIZEDFIFO'S are used as mkFIFO gives a FIFO which can have two inputs and hence deq and enq in same cycles. So mkFIFO is good for debugging, but to have a rigid code its better to use mkSizedFIFO

Change the parameters in "defined_parameters.bsv" as per the requirement.
*****For RV64 Instructions*******
set Reg_width==64
and define divider64 (there should be no define divider32 when executing RV64)
*****For RV32 Instructions*******
set Reg_width==32
and define divider32 (there shouldb be no define divider64 when executing RV32)

Comparing with Intel's Haswell and Meron(65nm) which takes 32-96 and 29-61 cycles for execution of 64bit divider, we implemented a simple NON RESTORING DIVIDER which takes 34 cycles for RV64 division instructions and 18 cycles for RV32 instructions 

***** Special Cases******
Special cases as mentioned in riscv2.0 ISA for Division and remainder  and a case when the dividend<divisor is taken care of in the first stage(method _start) and hence takes only 1 cylce for execution

***** Performance*********
Using UMCIP 65nm library in SYNOPSYS
Critical Path Length    :    0.5679 ns
Frequency 	        :    1.724 GHz
Combinational Cell Count:    10169
Sequential Cell Count   :    559

Critical path : Stage3

The main 4 stage of divider of which stage3 undergoes recursive execution which helps to save the area

************Stage1**************
It's the start stage. Input is dividend, divisor, div_type and word_flag. div_type helps to detemine the type of division and remainder instruction. word_flag help to detect if its a word instruction. In this stage final dividend and divisor are determined to be carried out for unsigned division. This stage pass the dividend,divisor to be used in next stage and div_type, word_flag, dividend_sign_bit and divisor_sign_bit to be used in final stage to evaluate the final output

************Stage2**************
This stage is a buffer stage which gets the input from FIFO.A buffer stage is introduced to reset to inital stage once the division is completed and hence deq the FIFO.As stage 3 uses a register in place of FIFO (which has implicit condition) we need a buffer to avoid conflict from next instruction.
In a non restoring division of n bit number it takes n steps. In this stage we perform 2 steps and pass the output which is dividend, divisor, partial_remainder and divisor 2's complement to the next stage. Along with it we pass the div_type,word_flag, dividend_sign_bit and divisor_sign_bit to be used in last stage. All these are store in register

************Stage3**************
This is a recursive stage which is executed 31 times for 64 bit number and 15 times for 32 bit number and hence complete remaining steps of non restoring division. The output after each execution is stored in rg_stage. This rg_stage is again read in next cycle.
The final output dividend, divisor, divisor_2's_complement, div_type,word_flag,dividend_sign_bit,divisor_sign_bit,partial_rem are stored in rg_stage which is finally read in the stage4.

************Stage4**************
THis is the final stage.The output from unsigned division is obtained from the rg_state. This output is then changed based on the division or remainder type to get the final quotient and remainder.div_type, word_flag,dividend_sign_bit,divisor_sign_bit is used to get the final output. Final quotiend and remainder is stored in ff_quotien and ff_remainder respectively

*/
package integer_divider_riscv;
import FIFO::*;
import DReg::*;
import riscv_types::*;
`include "defined_parameters.bsv"

interface Ifc_integer_divider_riscv;

   /* Input Methods */
   
   method Action _start(Bit#(`REG_WIDTH) _dividend, Bit#(`REG_WIDTH) _divisor,ALU_func _div_type,Bit#(1) _word_flag, Bit#(TLog#(`TOTAL_THREADS)) thread_id, Bit#(TLog#(`PRF_SIZE)) _destination);// Start method
   method Action _release(); 																	      // Method to clear the result FIFO
   method Action _set_flush(Bool _flush);   														  // Method to flush all FIFOs	
	  /* Output Methods */
   method Bit#(`REG_WIDTH) result_();
   method Bit#(TLog#(`PRF_SIZE)) destination_address_();
   method Bit#(TLog#(`TOTAL_THREADS)) thread_id_();
   

endinterface:Ifc_integer_divider_riscv

typedef struct{
   Bit#(`REG_WIDTH) partial_rem;         	//Will store the partial remainder required in non restoring division
   Bit#(`REG_WIDTH) _dividend;	      		// Dividend which changes in every step
   Bit#(`REG_WIDTH) _divisor;	      		// Divisor 
   Bit#(`REG_WIDTH) _divisor_compl;    	//2's complement of divisor
   Bit#(1) div_or_rem;  			// 0 means div operation. otherwise remainder.
   Bit#(2)	_div_type ;	      	    //to determine the type of div or remainder instruction
   Bit#(1) _word_flag;	            //to determine if its a word flag
   Bit#(2) _dividend_sign_bit;     //dividend sign bit which contains bit MSB and 31(for word instruction)
   Bit#(2)  _divisor_sign_bit;     //divisor sign bit which contains bit MSB and 31(for word instruction)
   Bit#(TLog#(`TOTAL_THREADS)) thread_id;
   Bit#(TLog#(`PRF_SIZE)) destination;	// destination address where the output is to be written in the Regfile
   
   } Stage_data deriving(Eq,Bits);


typedef struct{
   Bit#(`REG_WIDTH) _dividend;	          //Dividend (stores the dividend for unsigned division)
   Bit#(`REG_WIDTH) _divisor;	     	  //Divisor (stores the divisor for unsigned division)
   Bit#(2)	_div_type ;	      	  //to determine the type of div or rem instruction
   Bit#(1) div_or_rem;  			// 0 means div operation. otherwise remainder.
   Bit#(1) _word_flag;	          //to determine the word instruction
   Bit#(2) _dividend_sign_bit;   //dividend sign bit which contains bit MSB and 31(for word instruction)
   Bit#(2)  _divisor_sign_bit;   //divisor sign bit which contains bit MSB and 31(for word instruction)
   Bit#(TLog#(`TOTAL_THREADS)) thread_id;
   Bit#(TLog#(`PRF_SIZE)) destination;	// destination address where the output is to be written in the Regfile
   
   } Stage_buffer_data deriving(Eq,Bits);

(*synthesize*)
module mkinteger_divider_riscv(Ifc_integer_divider_riscv);
   
   FIFO#(Bit#(`REG_WIDTH)) ff_final_result <- mkSizedFIFO(1); 					//FIFO to store the final quotient 

   FIFO#(Stage_buffer_data) ff_start_buffer <- mkSizedFIFO(1);				//Buffer FIFO for stage1. Needed so that there is no conflict in successive division because register is to be used for stage2.

   Reg#(Stage_data) rg_stage <- mkRegU();  								//Unassigned register for stage 3, which needs to be read and written in same stage. Could have used mkFIFO but it has more latency
   Wire#(Bool) wr_flush <-mkDWire(False);	 								//Wire for flush	
   Reg#(int) rg_state_counter <-mkDReg(0);  								//Register for state machine counter
   Reg#(Bit#(TLog#(`TOTAL_THREADS))) rg_thread_id <- mkReg(0);
   Reg#(Bit#(TLog#(`PRF_SIZE))) rg_destination_address<-mkReg(0);

   
   //Stage 2
   //Executed when state_counter==1	
   rule rl_initial_stage (rg_state_counter==1 && !wr_flush);
	  let lv_buffer_data = ff_start_buffer.first();  						//Get data from buffer FIFO.
	  
	  Bit#(`REG_WIDTH) lv_partial_rem = 'b0;           							//Partial remainder required in non restorind division
	  Bit#(`REG_WIDTH) lv_divisor_compl = ~lv_buffer_data._divisor +1; 			//Divisor 2's complement
	  

	  //*******************One step of non restoring division******************************
	  //(P,D) shift p-->partial remainder,D-->Dividend
	  lv_partial_rem[0] = lv_buffer_data._dividend[`REG_WIDTH-1];		
	  lv_buffer_data._dividend=lv_buffer_data._dividend <<1;
	  //Add/Subtract
	  lv_partial_rem = lv_partial_rem +lv_divisor_compl;
	  //Changing the LSB of Dividend based on sign bit of partial remainder
	  lv_buffer_data._dividend[0] = (lv_partial_rem[`REG_WIDTH-1]==1) ? 0 : 1;
	  
	  
	  //*******************Second step of non restoring division******************************
	  //(P,D) shift p-->partial remainder,D-->Dividend
	  lv_partial_rem =lv_partial_rem <<1;
	  lv_partial_rem[0] =lv_buffer_data._dividend[`REG_WIDTH-1];
	  lv_buffer_data._dividend=lv_buffer_data._dividend <<1;
	  //Add/Subtract
	  lv_partial_rem =lv_partial_rem + (lv_partial_rem[`REG_WIDTH-1]==0? lv_divisor_compl:lv_buffer_data._divisor);
	  //Changing the LSB of Dividend based on sign bit of partial remainder
	  lv_buffer_data._dividend[0] =lv_partial_rem[`REG_WIDTH-1]==0 ? 1'b1:1'b0;


	  //Storing the required data in register
	  rg_stage <= (Stage_data {partial_rem:lv_partial_rem,
							   _dividend: lv_buffer_data._dividend,
							   _divisor : lv_buffer_data._divisor,
							   _divisor_compl:lv_divisor_compl,
							   _div_type : lv_buffer_data._div_type,
							   div_or_rem: lv_buffer_data.div_or_rem,
							   _word_flag : lv_buffer_data._word_flag,
							   _dividend_sign_bit:lv_buffer_data._dividend_sign_bit,
							   thread_id:lv_buffer_data.thread_id,
							   destination:lv_buffer_data.destination,
							   _divisor_sign_bit:lv_buffer_data._divisor_sign_bit});
	  rg_state_counter <=rg_state_counter +1; 								//Incrementing state counter
   endrule

   //Stage3
   //Executed when state_counter is from 2 to 32(64bit)
   `ifdef divider64
   rule rl_interstage(rg_state_counter>1 && rg_state_counter<33 && !wr_flush);
	  `endif
	  //Executed when state_counter is from 2 to 16(32bit)	
	  `ifdef divider32
	  rule rl_interstage(rg_state_counter>1 && rg_state_counter<17 && !wr_flush);
		 `endif
		 
		 let lv_data_stage = rg_stage;		//Getting data from previous stage which does 2 steps of the 64 steps required in  non restorind division

		 //Avoiding FOR loop as it has more delay	
		 //*******************One step of non restoring division******************************
		 //(P,D) shift p-->partial remainder,D-->Dividend	
		 lv_data_stage.partial_rem = {(lv_data_stage.partial_rem)[`REG_WIDTH-2:0],lv_data_stage._dividend[`REG_WIDTH-1]};
		 //Add/Subtract
		 let y1=  lv_data_stage.partial_rem +lv_data_stage._divisor;
		 let y2 = lv_data_stage.partial_rem +lv_data_stage._divisor_compl;
		 if(lv_data_stage.partial_rem[`REG_WIDTH-1]==0)
			lv_data_stage.partial_rem=y2;
		 else
			lv_data_stage.partial_rem=y1;
		 //Changing the LSB of dividend based on sign bit of partial remainder
		 lv_data_stage._dividend ={lv_data_stage._dividend[`REG_WIDTH-2:0],~lv_data_stage.partial_rem[`REG_WIDTH-1]};
		 
		 
		 //*******************Second step of non restoring division******************************
		 //(P,D) shift p-->partial remainder,D-->Dividend	
		 lv_data_stage.partial_rem = {(lv_data_stage.partial_rem)[`REG_WIDTH-2:0],lv_data_stage._dividend[`REG_WIDTH-1]};
		 //Add/Subtract
		 let x1=  lv_data_stage.partial_rem +lv_data_stage._divisor;
		 let x2 = lv_data_stage.partial_rem +lv_data_stage._divisor_compl;
		 if(lv_data_stage.partial_rem[`REG_WIDTH-1]==0)
			lv_data_stage.partial_rem=x2;
		 else
			lv_data_stage.partial_rem=x1;
		 //Changing the LSB of dividend based on sign bit of partial remainder
		 lv_data_stage._dividend ={lv_data_stage._dividend[`REG_WIDTH-2:0],~lv_data_stage.partial_rem[`REG_WIDTH-1]};
		 
		 rg_state_counter <= rg_state_counter +1;   //Incrementing state counter
		 
		 //Storing the required data in register for next state
		 rg_stage <= (Stage_data {partial_rem:lv_data_stage.partial_rem,
			_dividend:lv_data_stage._dividend ,
			_divisor :lv_data_stage._divisor,
			_divisor_compl:lv_data_stage._divisor_compl,
			_div_type : lv_data_stage._div_type,
			div_or_rem: lv_data_stage.div_or_rem,
			_word_flag :lv_data_stage._word_flag,
			_dividend_sign_bit:lv_data_stage._dividend_sign_bit,
			thread_id:lv_data_stage.thread_id,
			destination:lv_data_stage.destination,
			_divisor_sign_bit:lv_data_stage._divisor_sign_bit});

	  endrule : rl_interstage
	  //Stage4	
	  //Executed when state_counter==33(64bit) 
	  `ifdef divider64
	  rule rl_final_output (rg_state_counter==33 && !wr_flush);
		 `endif
		 //Executed when state_counter==17(32 bit)
		 `ifdef divider32
		 rule rl_final_output (rg_state_counter==17 && !wr_flush);
			`endif
			let lv_final_data = rg_stage; //Get the final data from unsigned division

			rg_thread_id<=lv_final_data.thread_id;
			//rg_destination_address<=lv_final_data.destination;
			if (lv_final_data.partial_rem[`REG_WIDTH-1]==1) lv_final_data.partial_rem=lv_final_data.partial_rem +lv_final_data._divisor; 		//Getting the remainder which needs to be positive. This is the final restore step
			   
			   
			   //	$display ("Quo_final_stage= %d", lv_final_data._dividend);
			   Bit#(`REG_WIDTH) _final_quo = lv_final_data._dividend;  																		//Set default value of final_quotient as the quotient after unsigned division
			   Bit#(`REG_WIDTH) _final_rem = lv_final_data.partial_rem;																    	//Set default value of reminder_quotient as the remainder after unsigned division
			   
			   if (lv_final_data._word_flag==1) begin //DIVW,DIVUW,REMW,REMUW (Word Instrucitons occur in RV64 instructions)
				  `ifdef divider64
				  //DIVW, DIVUW	
				  if ((lv_final_data._dividend_sign_bit[0] ^ lv_final_data._divisor_sign_bit[0])==1 && lv_final_data._div_type[0]==0) //Changing the final quotient based on sign of dividend and divisor
					 
					 _final_quo = signExtend(~lv_final_data._dividend[31:0] +1); 												    //when one of dividend or divisor is negative
				  else 
					 _final_quo = signExtend(lv_final_data._dividend[31:0]); 													    //when the output is positive
					 
					 //REMW,REMUW
					 if (lv_final_data._dividend_sign_bit[1]==1 && lv_final_data._div_type[0]==0) 										//Changing the final remainder based on dividend and divisor sign
						
						_final_rem = signExtend(~lv_final_data.partial_rem[31:0] +1); 													//Remainder is negative when dividend is negative and divisor is positive
					 else 
						_final_rem = signExtend(lv_final_data.partial_rem[31:0]);   													//Else remainder is always positive	
						`endif              
			   end
			   else begin  //DIV,DIVU,REM,REMU (No word Instruction)
				  if ((lv_final_data._dividend_sign_bit[1] ^ lv_final_data._divisor_sign_bit[1])==1 && lv_final_data._div_type[0]==0) //Changing final quo based on dividend and divisor
					 _final_quo = ~lv_final_data._dividend +1;
				  if (lv_final_data._dividend_sign_bit[1]==1 && lv_final_data._div_type[0]==0)										//Changing final remainder based on dividend and divisor
					 _final_rem = ~lv_final_data.partial_rem +1;
			   end
			//	$display ("Quotient= %d , Remainder= %d", _final_quo,_final_rem);
			if(lv_final_data.div_or_rem == 0)
			   ff_final_result.enq(_final_quo);
			else
			   ff_final_result.enq(_final_rem);
			rg_state_counter <= 0;																								    //resetting the counter to 0	
			$display("Divider remainder is %h Quotient is %h", _final_quo, _final_rem); 
			
		 endrule : rl_final_output	
		 
		 
		 //Word_flag is 1 only for the word RV64 instructions else it is 0.
		 //Non restoring division algorithm
		 //Stage1 (start method)
		 method Action _start(Bit#(`REG_WIDTH) _dividend, Bit#(`REG_WIDTH) _divisor,ALU_func _div_name,Bit#(1) _word_flag, Bit#(TLog#(`TOTAL_THREADS)) thread_id, Bit#(TLog#(`PRF_SIZE)) _destination)if (!wr_flush);
			Bit#(`REG_WIDTH) _input_dividend='b0;		//creating a dividend variable			
			Bit#(`REG_WIDTH) _input_divisor='b0;		//creating a divisor variable
		 	Bit#(2)          _div_type=0;
			Bit#(1)			 _div_or_rem=0;
			rg_destination_address <= _destination;
			case(_div_name)
		//		DIVW: _div_type = 'b10;
				DIV: begin 
					_div_type = 'b00;
					_div_or_rem = 0;
				end	
				DIVU: begin 
					_div_type = 'b01;
					_div_or_rem = 0;
				end
				REM : begin
					_div_type = 'b00;
					_div_or_rem = 1;
				end
				REMU : begin
					_div_type = 'b01;
					_div_or_rem = 1;
				end
		//		DIVWU: _div_type = 'b11;	
		 	endcase
			
			if(_word_flag==1 && _div_type[0]==0) begin
				_divisor = signExtend(_divisor[31:0]);
				_dividend = signExtend(_dividend[31:0]);
			end
			else if(_word_flag==1 && _div_type[0]==1) begin
				_divisor = zeroExtend(_divisor[31:0]);
				_dividend = zeroExtend(_dividend[31:0]);
			end

			$display("Divisor %h and Dividend %h", _divisor, _dividend);
		 
			if (_divisor=='d0 && _div_or_rem==0)					//Special case(divisor==0)
			   ff_final_result.enq('d-1);
			else if (_divisor=='d0 && _div_or_rem==1)					//Special case(divisor==0)
			   ff_final_result.enq(_dividend);
			`ifdef divider64 //**********For RV64***************
			else if (_dividend==64'h8000000000000000 && _divisor== 'd-1 && (_div_type[0]==0) && _div_or_rem==0)  //Special case (dividend=-2^(64-1) and divisor=-1)
			   ff_final_result.enq(64'h8000000000000000);
			else if((_dividend == _divisor) && _div_or_rem==0)
				ff_final_result.enq(1);
			else if((_dividend == _divisor) && _div_or_rem==1)
				ff_final_result.enq(0);

			`ifdef divider32//**********For RV32****************
			else if (_dividend==32'h80000000 && _divisor== 'd-1 && (_div_type[0]==0) && _div_or_rem==0)  //Special case (dividend=-2^(32-1) and divisor=-1)
			   ff_final_result.enq(32'h80000000);
			else if (_dividend==32'h80000000 && _divisor== 'd-1 && (_div_type[0]==0) && _div_or_rem==1)  //Special case (dividend=-2^(32-1) and divisor=-1)
			   ff_final_result.enq(0);

			else if((_dividend == _divisor) && _div_or_rem==0)
				ff_final_result.enq(1);
			else if((_dividend == _divisor) && _div_or_rem==1)
				ff_final_result.enq(0);


			`endif
			else begin
			   if(_div_type[0]==0) begin //DIV,REM
				  _input_dividend = _dividend[`REG_WIDTH-1]==1 ? (~_dividend+1) : _dividend;
				  _input_divisor = _divisor[`REG_WIDTH-1]==1 ? (~_divisor+1) : _divisor;
				  end
				  else begin //DIVU, REMU
					 _input_dividend = _dividend;
					 _input_divisor = _divisor;
				  end
			end
			   
			if (_input_dividend<_input_divisor && _div_or_rem==0)			//special case when dividend is less than divisor
			   ff_final_result.enq(0);					//Quotient is 0
			else if (_input_dividend<_input_divisor && _div_or_rem==1)			//special case when dividend is less than divisor
			   ff_final_result.enq(_input_dividend);	//Remainder is the dividend
			   //Enquing data in start buffer
			   ff_start_buffer.enq(Stage_buffer_data{_dividend:_input_dividend,
				  div_or_rem:_div_or_rem,
													 _divisor:_input_divisor,
													 _div_type : _div_type,
													 _word_flag : _word_flag,
													 thread_id:thread_id,
				  									 destination:_destination,
													 _dividend_sign_bit:{_dividend[`REG_WIDTH-1],_dividend[31]},
													 _divisor_sign_bit:{_divisor[`REG_WIDTH-1],_divisor[31]}});
			
			rg_state_counter <= rg_state_counter + 1;
		 
		 
		 endmethod					
		 
		 /*
		 method Bit#(`REG_WIDTH) get_quotient_();   //Method to get the result
		 return ff_final_quotient.first();
		 endmethod

		 method Bit#(`REG_WIDTH) get_remainder_();   //Method to get the result
		 return ff_final_remainder.first();
		 endmethod
		 */
		 method Bit#(`REG_WIDTH) result_();
			return ff_final_result.first();
		 endmethod

		 method Action _release()if (!wr_flush); // Method to release all FIFOs and reset the counter
			ff_final_result.deq();
		 //		ff_final_quotient.deq();
			ff_start_buffer.deq();
		 //ff_final_remainder.deq();
			rg_state_counter <=0;
		 endmethod

		 method Action _set_flush(Bool _flush); // Method to flush the pipe
			wr_flush<=_flush;
			ff_final_result.clear();
			ff_start_buffer.clear();
		 endmethod

		 
		 method Bit#(TLog#(`TOTAL_THREADS)) thread_id_();
			return rg_thread_id;
		 endmethod

		 method Bit#(TLog#(`PRF_SIZE)) destination_address_();
			return rg_destination_address;
		 endmethod
endmodule : mkinteger_divider_riscv

endpackage: integer_divider_riscv
