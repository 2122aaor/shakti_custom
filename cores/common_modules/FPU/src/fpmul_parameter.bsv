package fpmul_parameter;
import FIFO::*;
	import FIFOF::*;
	import SpecialFIFOs::*;
	import anupama_types_1::*;
        import integer_multiplier_for_fmul::*;

typedef struct{
		Bit#(TAdd#(x,1)) mantissa1; 	// mantissa of operand1
		Bit#(TAdd#(x,1)) mantissa2; 	// mantissa of operand2
		bit sign; 		// sign bit of the result
		Bit#(y) summed_exponent;// exponent of the resultant
		Bit#(4) rob;		// holds the rob_number alloted to the operations during issue stage.
		Bit#(6) destination;	// holds the destination address of the operation in the registerfile
		Bit#(32) program_counter;// the program_counter value of the instruction.
		Bit#(1) infinity;	// indicating that the ff_output is infinity.
		Bit#(1) invalid;	// indicating that the ff_output is NaN.
		Bit#(1) denormal;	// indicating that atleast 1 of the inputs is denormal.
		Bit#(1) zero;		// indicating that the ff_output is zero.
		Bit#(32) psw;		// the floating-point status register containing all the flags and control bits for rounding.
		}Input_data_type#(numeric type x,numeric type y) deriving (Bits,Eq);//x=fpman,y=fpexp2

typedef struct{
		bit sign;			// sign bit of the result
		Bit#(TAdd#(v,2)) exponent;		// exponent of the resultant
		Bit#(TAdd#(u,2)) mantissa;	// manstissa of the result
		Bit#(4) rob;			//holds the rob_number alloted to the operations during issue stage.
		Bit#(32) program_counter;	// the program_counter value of the instruction.
		Bit#(32) psw;			// the floating-point status register containing all the flags and control bits for rounding.
		Bit#(1) infinity;		// indicating that the ff_output is infinity.
		Bit#(1) invalid;		// indicating that the ff_output is NaN.
		Bit#(1) zero;			// indicating that the ff_output is zero.
		Bit#(1) underflow;		// indicating that the ff_output has an underflow.
		Bit#(1) denormal;	// indicating that atleast 1 of the inputs is denormal.
		Bit#(6) destination;		// holds the destination address of the operation in the registerfile
		bit inexact;
		}Stage3_data_type#(numeric type u,numeric type v) deriving (Bits,Eq);//u=fpman;v=fpexp;	
	
module mkfpmul_parameter(Ifc_FPU#(fpexp, fpman, fpbias));
	let fPINP= valueOf(fpinp);
	let fPMAN= valueOf(fpman);
	let fPEXP= valueOf(fpexp);
        let fPBIAS= valueOf(fpbias);

    FIFOF#(Output_type#(fpinp)) output_ <-mkFIFOF();			// output FIFO

    Ifc_integer_multiplier_for_fmul#(fpinp, fpexp, 'd4) integer_multiplier<-mkinteger_multiplier_for_fmul;
Wire#(Bool) wr_flush <-mkDWire(False); // wire to indicate that a wr_flush has occured in the processor and all buffer need to be cleared.
FIFO#(Stage3_data_type#(fpman,fpexp)) ff_stage3 <-mkLFIFO();	// intermediate FIFO
		FIFO#(Input_data_type#(fpman,fpexp2)) ff_input_register<-mkLFIFO();	// input FIFO
		
		Reg#(Maybe#(Bit#(fpexp2))) rg_exponent_a <-mkReg(tagged Invalid); // intermediate register to hold the exponent value
		Reg#(Bool) rg_ready_signal <-mkReg(False); 	// register to indicate that the output fifo now holds valid result.   
 

// this rule is used to clear all the buffers/signals when the top module initiates a FLUSH 
		rule rl_flush_all_fifos(wr_flush);
			ff_stage3.clear();
			ff_input_register.clear();
			output_.clear();
			rg_exponent_a<=tagged Invalid;
		        integer_multiplier._set_flush(True);
			
		endrule:rl_flush_all_fifos
		
		/*
		this rule will transfer data to the integer multiplier from the data recieved from the input register.
		since the integer multiplier is a 32 bit multiplier and the mantissas are only 24 bits wide, 8 zeroz
		are appended at the MSB of each mantissa and then sent into the integer multiplier.
		since the input of the integer mulitplier is a FIFO, this rule can fire only when the input FIFO of the
		integer multiplier is empty. else it will not fire.
		all data like exponent, excpetion falgs etc. provided by the input register are also transfered to the integer
		multiplier. these datas are simply buffered along the stages of the integer multiplier and are not used for any 
		computation.
		*/

rule rl_stage1_after_input_stage(!wr_flush);
			//$display("Executing FMUL stage 2");
			//$display("exp= %b", ff_input_register.first().summed_exponent);
			$display("Sending inputs to multiplier.. inp1: %h inp2: %h",ff_input_register.first().mantissa1,ff_input_register.first().mantissa2);
			Bit#(TSub#(TSub#(fpinp,fpman),1)) lv_zeros_to_append= 'd0;
			
        integer_multiplier._start({lv_zeros_to_append,ff_input_register.first().mantissa1}, {lv_zeros_to_append,ff_input_register.first().mantissa2},ff_input_register.first().destination,ff_input_register.first().program_counter, ff_input_register.first().psw,ff_input_register.first().rob, ff_input_register.first().sign, ff_input_register.first().summed_exponent, ff_input_register.first().invalid,ff_input_register.first().infinity,ff_input_register.first().zero,ff_input_register.first().denormal);
		
			
ff_input_register.deq();	
		endrule:rl_stage1_after_input_stage
		
		/*
		in this rule the number of leading zeros are counted and stored in the register rg_leftzeros.
		also the final exponent is incremented by 1 if the result from the integer mulitplier has set the carry bit.
		
		
		this is the final stage of the operation where the ff_output is decided based on the exception flags
		which where generate in the previous stages.
		also the new psw is generated based no the new flags that are set.
		in case no exception is generated and all the flags are set to 0 then the ff_output is the same as that generated
		in the previous stage and the normal flag is set to 1.
		the ready signal is also made true to indicate that the ff_output buffer now holds valid data.
		*/

rule rl_stage_3(!wr_flush);
			$display("Executing FMUL stage 3");


let lv_result= integer_multiplier.result_();

Bit#(fpexp2) lv_exponent=0;	
lv_exponent= lv_result.summed_exponent;

			//Size of lv_new_mantissa is [(FPMAN+1)*2 ] + 1= 2*FPMAN + 3
let lv_new_mantissa=0;

lv_new_mantissa= {lv_result.final_result,1'b0};

			$display("new mantissa: %h",lv_new_mantissa);

	let lv_result1= integer_multiplier.result_;
	
		
bit lv_sign=0;
lv_sign= lv_result1.sign;

			Bit#(TAdd#(fpman,2)) lv_rounded_mantissa= 0;
			bit lv_sticky= 0;
			bit lv_underflow= 0;
			bit lv_overflow= 0;
			bit lv_roundup= 0;
			bit lv_guard;
			bit lv_round;
			Exception e = tagged No_exception;
let lv_psw=0;							
lv_psw= lv_result.psw;


			//$display("mantissa is %b",integer_multiplier.result_().final_result[47:0]);
			$display("exp: %b",lv_exponent);

			let lv_index= (2*fPMAN)+2;
			if(lv_new_mantissa[lv_index]==1)		//If the MSB is 1, mantissa is right shifted by 1 and the new exponent is obtained by adding 1 to the old exponent
			begin
				$display("The mantissa is 1x.xx..");
				lv_exponent= lv_exponent+1;
				lv_new_mantissa=lv_new_mantissa>>1;		//TODO verify if this needs to be there or not
			end
		
			if(lv_exponent[fPEXP+1]==1 || lv_exponent[fPEXP:0]==0) begin
				$display("Underflow...");
				lv_underflow= 1;
			end
			lv_guard= lv_new_mantissa[fPMAN];
			lv_round= lv_new_mantissa[fPMAN-1];
			Bit#(TSub#(fpman,1)) lv_v_man_all_zeros= 'd0;
			if(lv_new_mantissa[fPMAN-2:0]!=lv_v_man_all_zeros)		
				lv_sticky= 1;
		
			bit lv_inexact= lv_guard | lv_round | lv_sticky;
			Bit#(2) rounding_mode = lv_psw[21:20];  				//Assigning the rounding mode from psw input //TODO verify
			$display("G=%d R=%d S=%d Rounding_mode: ",lv_guard, lv_round,lv_sticky,rounding_mode);
		
			if(rounding_mode== 'b00)			// round to nearest, ties to even
				 lv_roundup = lv_guard & (lv_new_mantissa[fPMAN+1] | lv_round | lv_sticky);
			/*else if(rounding_mode == 'b100)		// round to nearest, ties to max magnitude	
				lv_roundup = lv_guard & (lv_round | lv_sticky | lv_sign);*/
			else if(rounding_mode == 'b10)		// round up		                  		
				 lv_roundup = (lv_guard | lv_round | lv_sticky) & (~lv_sign);
			else if(rounding_mode == 'b11 )	// round down                               			 
				 lv_roundup = (lv_guard | lv_round | lv_sticky) & (lv_sign);
			// else if the rounding mode is round_to_zero, roundup should be zero. Since the default value is zero, we needn't have an else if statement for that.
		
			if( lv_roundup==1)begin
				lv_rounded_mantissa = {1'b0,lv_new_mantissa[(2*fPMAN)+1:fPMAN+1]}+1;
				//$display("Roundup is made 1.");
			end
			else
				lv_rounded_mantissa = {1'b0,lv_new_mantissa[(2*fPMAN)+1:fPMAN+1]};
		
			if(lv_rounded_mantissa[fPMAN+1]==1)
				lv_exponent= lv_exponent+1;
		
bit lv_zero=0;			bit lv_normal=0;			
lv_zero= lv_result1.zero;


bit lv_infinity=0;
lv_infinity= lv_result1.infinity;


			Bit#(fpinp) lv_final_output= 'd0;
			Bit#(fpexp) exp_all_ones= 'd-1;
		
             
                if(integer_multiplier.result_().invalid == 1) begin
				$display("Invalid");
				lv_final_output = {1'b0,exp_all_ones,1'b1,'d0};		
				e= tagged Invalid True;
				lv_underflow=0;
				lv_inexact=0;
				lv_sign=0;
                 end
                else if(lv_zero==1 || integer_multiplier.result_.denormal==1) begin
				$display("Zero or Denormal");
				lv_final_output= {lv_sign,'d0};
				lv_underflow=0;
				lv_inexact=0;
				lv_zero=1;
			end
			
                 
                 
			else if(lv_infinity==1) begin
				$display("Infinity");
				lv_final_output={lv_sign,exp_all_ones,'d0};
			end
			else if(lv_underflow==1) begin
				$display("Underflow");
				lv_final_output= {lv_sign,'d0};
				lv_zero=1;
				lv_inexact=1;
				e=tagged Underflow True;
			end
			else if(lv_exponent[fPEXP]==1'b1 || (lv_exponent[fPEXP-1:0]==exp_all_ones)) begin
				$display("Overflow");
				lv_overflow= 1;
				lv_inexact= 1;
				e= tagged Overflow True;
				Bit#(TSub#(fpinp,1)) lv_intermediate_result;		
				if((rounding_mode == 'b10 && lv_sign==0) || (rounding_mode == 'b11 && lv_sign==1) || (rounding_mode=='b00))		// (round up and +ve result) or (round down and -ve result) or (round to even)
				begin
					lv_intermediate_result= {exp_all_ones,'d0};
					lv_infinity= 1;
				end
				else begin
					Bit#(fpexp) max_finite_exp= 'd-2;
					lv_intermediate_result= {max_finite_exp,'d-1};
					lv_normal= 1;
				end
				lv_final_output= {lv_sign, lv_intermediate_result};
		    end
			else begin
				$display("Normal");
				Bit#(fpexp) lv_v_exp= lv_exponent[fPEXP-1:0];
				Bit#(fpman) lv_v_man= lv_rounded_mantissa[fPMAN-1:0];
		 		lv_final_output= {lv_sign, lv_v_exp, lv_v_man};
				lv_normal=1;
				//if(|(lv_final_output[31:0])==0)		//TODO verify if this is not needed
				//	lv_zero= 1;
				if(lv_inexact==1)
		   			e= tagged Inexact True;
			end
		
			
                        
                    lv_psw= {lv_psw[31:16], lv_normal, integer_multiplier.result_.denormal, integer_multiplier.result_.invalid, integer_multiplier.result_.invalid, 1'b0, lv_underflow, lv_infinity, lv_inexact, lv_psw[7:4], lv_zero, lv_sign, lv_overflow, 1'b0};
		
			
				output_.enq(Output_type{destination		: integer_multiplier.result_().destination,
									psw				: lv_psw,
									program_counter	: integer_multiplier.result_().program_counter,
									rob_number		: integer_multiplier.result_().rob_number,
									final_result	: lv_final_output,
									except			: e
									});
			
			rg_ready_signal <= True;
				 
			integer_multiplier._dequeue();// dequing the output buffer of the integer multiplier.
                        
                        
		endrule:rl_stage_3
		
		
		/*
		this is input stage.
		Here the flag values are decided based on the inputs.
		Once all the flag variables are set, using the 32 bit operands, various fields of the stage1 buffer are filled.
		the new exponent is calcualted by adding the exponents and subtracting the bias from it.
		the sign bit of the result is nothing but the xor of the sign bits of the two operands.
		*/
method Action _start(Bit#(fpinp) _operand1,Bit#(fpinp) _operand2,Bit#(32) _instruction,Bit#(4) _rob_number, Bit#(32) _psw, Bit#(32) _program_counter)if(!wr_flush);

				bit lv_inf=0;
				bit lv_inv=0;
				Bit#(1) lv_zero =0;
				Bit#(1) lv_exp1=0;	//1 if all the bits of exponent are set; used to check if op1 is infinity or NaN
				Bit#(1) lv_exp2=0;	//1 if all the bits of exponent are set; used to check if op2 is infinity or NaN
				Bit#(1) lv_man1_is_zero=0;
		       	Bit#(1) lv_man2_is_zero=0;
		       	Bit#(1) lv_exp1_is_zero=0;
		       	Bit#(1) lv_exp2_is_zero=0;
		       	Bit#(1) lv_op1_is_zero=0;
		       	Bit#(1) lv_op2_is_zero=0;
				bit lv_op1_denormal=0;
		        bit lv_op2_denormal=0;



				Bit#(fpexp) lv_exp_all_ones= 'd-1;
				if(_operand1[fPINP-2:fPMAN]== lv_exp_all_ones)
					lv_exp1=1;
				if(_operand2[fPINP-2:fPMAN]== lv_exp_all_ones)
					lv_exp2=1;
		
				Bit#(fpman) lv_man_all_zeros= 'd0;
				if(_operand1[fPMAN-1:0]== lv_man_all_zeros)
					lv_man1_is_zero=1;
				if(_operand2[fPMAN-1:0]== lv_man_all_zeros)
					lv_man2_is_zero=1;
		
				Bit#(fpexp) lv_exp_all_zeros= 'd0;
				if(_operand1[fPINP-2:fPMAN]== lv_exp_all_zeros)
					lv_exp1_is_zero=1;
				if(_operand2[fPINP-2:fPMAN]== lv_exp_all_zeros)
					lv_exp2_is_zero=1;
		
				lv_op1_is_zero= lv_man1_is_zero & lv_exp1_is_zero;
				lv_op2_is_zero= lv_man2_is_zero & lv_exp2_is_zero;
		
				if(lv_exp1_is_zero==1 && lv_man1_is_zero==0)
					lv_op1_denormal=1;
				if(lv_exp2_is_zero==1 && lv_man2_is_zero==0)
					lv_op2_denormal=1;
		
				if((lv_exp1==1 && lv_man1_is_zero==0) || (lv_exp2==1 && lv_man2_is_zero==0))		// either of the operands are NaN
					lv_inv=1;
				else if((lv_exp1==1 && lv_man1_is_zero==1) || (lv_exp2==1 && lv_man2_is_zero==1))	// checks if op1 or op2 are infinite
				begin
					if(lv_op2_is_zero==1 || lv_op1_is_zero==1 || lv_op1_denormal==1 || lv_op2_denormal==1)					// if either op1 or op2 are zero, or either op1 or op2 are denormal then 0*infinity results in NaN
						lv_inv=1;
					else 								//if both are infinite, result is infinite
						lv_inf=1;
				end
				else if(lv_op1_is_zero==1 || lv_op2_is_zero==1)
					lv_zero=1;
				Bit#(fpexp2) yy=fromInteger(fPBIAS); 
				Bit#(1) lv_sign= _operand1[fPINP-1]^_operand2[fPINP-1];
				Bit#(fpexp2) lv_exponent= {2'b0,_operand1[fPINP-2:fPMAN]}+{2'b0,_operand2[fPINP-2:fPMAN]} - yy;
		
				ff_input_register.enq(Input_data_type{summed_exponent: lv_exponent,
								sign:lv_sign,
								mantissa1:{~lv_op1_denormal,_operand1[fPMAN-1:0]},
								mantissa2:{~lv_op2_denormal,_operand2[fPMAN-1:0]},
								rob:_rob_number,
								destination: _instruction[21:16],
								psw:_psw,
								zero:lv_zero,
								infinity:lv_inf,
								invalid:lv_inv,
								denormal: lv_op1_denormal | lv_op2_denormal,
								program_counter:_program_counter});
			
		endmethod
		
		// this method sends out the valid result after performing the valid operation.
		// this method will only fire as long as the output_ FIFO is not empty.
		// if empty then the rule calling this method will also not fire.

			method Output_type#(fpinp) result_();
		       return output_.first();
			endmethod
		
		
		// this method is called once the data from the output_ FIFO has been read in the top module(i.e reservatin station).
		// this method will hence empty the output_ FIFO and also if the next add/sub operation is not in the consecutive cycle
		// then the rg_ready_signal is also reset to False.
		method Action _deque_buffer_reset_ready_signal();
			output_.deq();
			if(output_.notFull())
				rg_ready_signal<=False;
		endmethod
		
		// this me returns the status of the rg_ready_signal. When true idicates that the ff_output FIFO not holds valid 
		// result data.
		method Bool ready_();
			return rg_ready_signal;
		endmethod
		
		
		// when a wr_flush is initiated in the processor this method will also be called.
		// it sets the flsuh wire to True, hence no rule other flush_all_fifos will
		// fire and hence clear all the buffer and intermediate register/ wires ...
		// including rg_ready_signal.
		method Action _set_flush(Bool _flush);
				wr_flush<=_flush;
		endmethod
	endmodule




/*		Test Bench 		*/

(*synthesize*)
module mkTest();
Ifc_FPU#(8, 23, 128) ff<-mkfpmul_parameter();

rule getinput;
ff._start(32'h00800001, 32'h00800001,32'd6788,4'd12,32'd6500,32'd124);

endrule
rule putoutput;
let lv_resultfinal = ff.result_();
		ff._deque_buffer_reset_ready_signal();
		$display("Result is: %h",lv_resultfinal.final_result);
		$display("Sign=%b Exponent=%b Mantissa=%b",lv_resultfinal.final_result[31],lv_resultfinal.final_result[30:23],lv_resultfinal.final_result[22:0]);
        $display("psw : %h\n\n",lv_resultfinal.psw);
endrule
endmodule

endpackage

