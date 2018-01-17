package integer_multiplier_for_fmul;

`include "defined_parameters.bsv"
import anupama_types_1::*;
import RB_NB::*;      //contain function to convert redundant binary number to normal binary number (Stage5)
import RBA::*;	      //contain functions for wallace tree redudant binary addition (Stage2,3,4)
import Booth2_pp_gen::*;//contain function to get partial products based on booth encoding (Stage1)
import Vector::*;
import FIFO::*;

// The interface for the Integer Multiplier which accepts two 64 bit operands and produce 128 bit result
interface Ifc_integer_multiplier_for_fmul#(numeric type regwidth,numeric type fpexp,numeric type tokensize);

		/* Input Methods */
	method Action _start(Bit#(regwidth) _in1, Bit#(regwidth) _in2, Bit#(6) _destination, Bit#(32) _program_counter, Bit#(32) _psw, Bit#(tokensize) _token, bit _sign, Bit#(TAdd#(fpexp,2)) _summed_exponent, bit _invalid, Bit#(1) _infinity, Bit#(1) _zero, Bit#(1) _denormal);// Start method to get inputs.
	method Action _set_flush(Bool _flush);   // Method to flush all FIFOs	
	method Action _dequeue(); 		// Method to clear the result FIFO

		/* Output Methods */
	method Stage5_data#(regwidth,tokensize,fpexp) result_();	//Method to get result

endinterface


/*
Following are the types of the structures used in each stage of the pipeline
*/
typedef struct{
	Vector#(TDiv#(x,2),Bit#(TMul#(x,2))) stage1; //Will store the 32 partial products generated in stage1
	Bit#(TMul#(x,2)) stage1_extra_pp;   //Generated due to Unsigned multiplication in booth                   
	Bit#(y) token; // token assigned to each instruction
	Bit#(6) destination;	// destination address where the output is to be written in the Regfile
	Bit#(32) psw;				// the input PSW
	Bit#(32) program_counter;	// the Program counter of the Instruction
	Bit#(TAdd#(z,2)) summed_exponent;	// the sum of the exponents
	Bit#(1) infinity;		// the infinity Flag
	Bit#(1) invalid;		// the Invalid Flag
	Bit#(1) denormal;		// the Denormal Flag
	Bit#(1) zero;			// The Zero Flag
	bit sign;			// the sign bit	
}Stage1_data#(numeric type x,numeric type y,numeric type z) deriving(Eq,Bits);//x=regwidth,y=tokensize,z=fpexp

typedef struct{
	Bit#(TMul#(x1,4)) data_computed_stage2; //Will store result of addition of 16 partial products computed in stage2
	Vector#(TDiv#(x1,4),Bit#(TMul#(x1,2))) vector_pp; // Will again store the 32 partial products passed from stage1
	Bit#(TMul#(x1,2)) stage2_extra_pp;      //Generated due to Unsigned multiplication in booth                   
	Bit#(y1) token; // token assigned to each instruction
	Bit#(6) destination;	// destination address where the output is to be written in the Regfile
	Bit#(32) psw;				// the input PSW
	Bit#(32) program_counter;	// the Program counter of the Instruction
	Bit#(TAdd#(z1,2)) summed_exponent;	// the sum of the exponents
	Bit#(1) infinity;		// the infinity Flag
	Bit#(1) invalid;		// the Invalid Flag
	Bit#(1) denormal;		// the Denormal Flag
	Bit#(1) zero;			// The Zero Flag
	bit sign;			// the sign bit	
}Stage2_data#(numeric type x1,numeric type y1,numeric type z1) deriving(Eq,Bits);//x1=regwidth,y1=tokensize,z1=fpexp

typedef struct{
	Bit#(TMul#(x2,4)) data_from_stage2; //Will store result of addition of 1st set of 16 partial products
	Bit#(TMul#(x2,4)) data_from_stage3; //Will store result of addition of 2nd set of 16 partial products
	Bit#(TMul#(x2,2)) stage3_extra_pp;       //Generated due to Unsigned multiplication in booth                   
	Bit#(y2) token; // token assigned to each instruction
	Bit#(6) destination;	// destination address where the output is to be written in the Regfile
	Bit#(32) psw;				// the input PSW
	Bit#(32) program_counter;	// the Program counter of the Instruction
	Bit#(TAdd#(z2,2)) summed_exponent;	// the sum of the exponents
	Bit#(1) infinity;		// the infinity Flag
	Bit#(1) invalid;		// the Invalid Flag
	Bit#(1) denormal;		// the Denormal Flag
	Bit#(1) zero;			// The Zero Flag
	bit sign;			// the sign bit	
}Stage3_data#(numeric type x2,numeric type y2,numeric type z2) deriving(Eq,Bits);//x2=regwidth,y2=tokensize,z2=fpexp

typedef struct{
	Bit#(TMul#(x3,4)) final_rb_number; //Will store the final RB number computed in stage4
	Bit#(TMul#(x3,2)) stage4_extra_pp;        //Generated due to Unsigned multiplication in booth                   
	Bit#(y3) token; // token assigned to each instruction
	Bit#(6) destination;	// destination address where the output is to be written in the Regfile
	Bit#(32) psw;				// the input PSW
	Bit#(32) program_counter;	// the Program counter of the Instruction
	Bit#(TAdd#(z3,2)) summed_exponent;	// the sum of the exponents
	Bit#(1) infinity;		// the infinity Flag
	Bit#(1) invalid;		// the Invalid Flag
	Bit#(1) denormal;		// the Denormal Flag
	Bit#(1) zero;			// The Zero Flag
	bit sign;			// the sign bit	

}Stage4_data#(numeric type x3,numeric type y3,numeric type z3) deriving(Eq,Bits);//x3=regwidth,y3=tokensize,z3=fpexp


typedef struct{ 
	Bit#(TMul#(x4,2)) final_result; // Unsigned multiplication after Rb to Nb conversion
	Bit#(y4) rob_number; // token assigned to each instruction
	Bit#(6) destination;	// destination address where the output is to be written in the Regfile
	Bit#(32) psw;				// the input PSW
	Bit#(32) program_counter;	// the Program counter of the Instruction
	Bit#(TAdd#(z4,2)) summed_exponent;	// the sum of the exponents
	Bit#(1) infinity;		// the infinity Flag
	Bit#(1) invalid;		// the Invalid Flag
	Bit#(1) denormal;		// the Denormal Flag
	Bit#(1) zero;			// The Zero Flag
	bit sign;			// the sign bit	
}Stage5_data#(numeric type x4,numeric type y4,numeric type z4) deriving(Eq,Bits);//x4=regwidth,y4=tokensize,z4=fpexp


//(*synthesize*)
module mkinteger_multiplier_for_fmul(Ifc_integer_multiplier_for_fmul#(regwidth, fpexp, tokensize));

	
let rEGWIDTH= valueOf(regwidth);	
let fPEXP = valueOf(fpexp);
let tOKENSIZE= valueOf(tokensize);

Wire#(Bool) wr_flush <-mkDWire(False);	 //Wire for flush
	FIFO#(Stage1_data#(regwidth,tokensize,fpexp)) ff_stage1 <- mkFIFO;  //This FIFO stores the 32 128 bit partial products produced in stage 1
	FIFO#(Stage2_data#(regwidth,tokensize,fpexp)) ff_stage2 <- mkFIFO;	 //This FIFO stores the result of addition of 1st set of 16 partial products and the 32 128 bit partial products computed in stage1
	FIFO#(Stage3_data#(regwidth,tokensize,fpexp)) ff_stage3 <- mkFIFO;	 //This FIFO stores the result of the 1st set and 2nd set of 16 partial products computed in stage 2 and 3 respectively
	FIFO#(Stage4_data#(regwidth,tokensize,fpexp)) ff_stage4 <- mkFIFO;	 //This FIFO stores the result of the Redundant Binary Addition of the Redundant Binary Partial Products
	FIFO#(Stage5_data#(regwidth,tokensize,fpexp)) ff_final_result <- mkFIFO;	 //This FIFO stores the final 64 bit result of MUL instructions
	Reg#(Bit#(tokensize)) rg_token <-mkReg(0); // Token for output instruction
	Reg#(Bit#(5)) rg_destination_address<-mkReg(0);
	/* This rule is fired only when the wr_flush signal is invoked.
	Here all the fifos are cleared and thus any comutation is abandoned.
	*/
        
	rule rl_flush_data(wr_flush);
		ff_stage1.clear();
		ff_stage2.clear();
		ff_stage3.clear();
		ff_stage4.clear();
		ff_final_result.clear();
		rg_token<=0;
	endrule:rl_flush_data 
	

	//Stage 2
	rule rl_wallace_tree_part1(!wr_flush);
		//$display("Executing Integer MUL stage 2"); 
		let lv_data_stage1=ff_stage1.first;    //Get the 32 partial products from FIFO stage1
		ff_stage1.deq;	
		Vector#(TDiv#(regwidth,4),Bit#(TMul#(regwidth,2))) upper_half = take(lv_data_stage1.stage1);
		let lv_result_stage2=wallace_rba(upper_half);
		
		ff_stage2.enq(Stage2_data{	data_computed_stage2:lv_result_stage2, //Forward result to next stage
									vector_pp:takeTail(lv_data_stage1.stage1), //Forward the 32 partial product for addition of next 16 partial products in stage3
									stage2_extra_pp: lv_data_stage1.stage1_extra_pp,
									token:lv_data_stage1.token,
									destination:lv_data_stage1.destination,
									summed_exponent:lv_data_stage1.summed_exponent,
									sign :lv_data_stage1.sign,
									zero:lv_data_stage1.zero,
									infinity:lv_data_stage1.infinity,
									invalid:lv_data_stage1.invalid,
									denormal:lv_data_stage1.denormal,
									psw: lv_data_stage1.psw,
									program_counter:lv_data_stage1.program_counter					
					});

	endrule:rl_wallace_tree_part1

	//Stage 3
	rule rl_wallace_tree_part2(!wr_flush);
		//$display("Executing Integer MUL stage 3");
		let lv_data_stage2=ff_stage2.first;	//Get result of additon of 16 pp and the 32 pp's for next step of addition
//		Vector#(TDiv#(rEGWIDTH,4),Bit#(TMul#(rEGWIDTH,2))) lower_half = takeTail(lv_data_stage2.vector_pp);
		let lv_result_stage3= wallace_rba(lv_data_stage2.vector_pp);
		ff_stage2.deq;

		ff_stage3.enq(Stage3_data{	data_from_stage2: lv_data_stage2.data_computed_stage2, //Result of additon of 1st 16 partial products
									data_from_stage3: lv_result_stage3,      //Result of addition of next 16 partial products
									stage3_extra_pp: lv_data_stage2.stage2_extra_pp,
									token:lv_data_stage2.token,
									destination:lv_data_stage2.destination,
									summed_exponent:lv_data_stage2.summed_exponent,
									sign :lv_data_stage2.sign,
									zero:lv_data_stage2.zero,
									infinity:lv_data_stage2.infinity,
									invalid:lv_data_stage2.invalid,
									denormal:lv_data_stage2.denormal,
									psw: lv_data_stage2.psw,
									program_counter:lv_data_stage2.program_counter					
					});
	endrule:rl_wallace_tree_part2
	//Stage 4
	rule rl_wallace_tree_final(!wr_flush);
		//$display("Executing Integer MUL stage 4");
		let lv_data_stage3=ff_stage3.first;		// Get result of additon of both 1st 16 pp and next 16 pp
		let lv_result_stage4=wallace_rba_final(lv_data_stage3.data_from_stage2,lv_data_stage3.data_from_stage3); // Final RB number. An extra RB number is added in function due to logic in redundant binary addition
		ff_stage3.deq;

		ff_stage4.enq(Stage4_data{	final_rb_number:lv_result_stage4,
									stage4_extra_pp: lv_data_stage3.stage3_extra_pp,		
									token:lv_data_stage3.token,
									destination:lv_data_stage3.destination,
									summed_exponent:lv_data_stage3.summed_exponent,
									sign :lv_data_stage3.sign,
									zero:lv_data_stage3.zero,
									infinity:lv_data_stage3.infinity,
									invalid:lv_data_stage3.invalid,
									denormal:lv_data_stage3.denormal,
									psw: lv_data_stage3.psw,
									program_counter:lv_data_stage3.program_counter					
					  });  //FInal RB number in Stage 4 FIFO
	endrule:rl_wallace_tree_final



	//Stage5

	rule rl_rb_nb(!wr_flush);
		// The Redundant Binary product generated in the previous stage is converted into the normal binary form.
		
		let lv_data_stage4= ff_stage4.first(); // Get RB number generated in stage 4
//		let normal_binary_product=rb_nb(lv_data_stage4.final_rb_number) + lv_data_stage4.stage4_extra_pp; // Converted RB number is added to extra pp to get the final result of unsigned multiplication
//		let normal_binary_product = lv_data_stage4.final_rb_number[255:128] + (~lv_data_stage4.final_rb_number[127:0] +1)+lv_data_stage4.stage4_extra_pp;
		let normal_binary_product = lv_data_stage4.final_rb_number[(rEGWIDTH*4)-1:(rEGWIDTH*2)] + (~lv_data_stage4.final_rb_number[(rEGWIDTH*2)-1:0] +1)+lv_data_stage4.stage4_extra_pp;
		ff_stage4.deq();
		
		ff_final_result.enq(Stage5_data{	final_result : normal_binary_product,		
									rob_number:lv_data_stage4.token,
									destination:lv_data_stage4.destination,
									summed_exponent:lv_data_stage4.summed_exponent,
									sign :lv_data_stage4.sign,
									zero:lv_data_stage4.zero,
									infinity:lv_data_stage4.infinity,
									invalid:lv_data_stage4.invalid,
									denormal:lv_data_stage4.denormal,
									psw: lv_data_stage4.psw,
									program_counter:lv_data_stage4.program_counter					
					  });
	
	endrule:rl_rb_nb
	

	method Action _start(Bit#(regwidth) _in1, Bit#(regwidth) _in2, Bit#(6) _destination, Bit#(32) _program_counter, Bit#(32) _psw, Bit#(tokensize) _rob_number, bit _sign, Bit#(TAdd#(fpexp,2)) _summed_exponent, bit _invalid, Bit#(1) _infinity, Bit#(1) _zero, Bit#(1) _denormal);// Start method to get inputs.
		Bit#(regwidth) _operand1 = _in1;
		Bit#(regwidth) _operand2 = _in2;
		Bit#(1) _sign_bit_op2 = _in2[rEGWIDTH-1];  // Sign bit operand1 for propogation across different stages to be used in the final stage
		Bit#(1) _sign_bit_op1 = _in1[rEGWIDTH-1];  //Sign bit operand1 for propogation across different stages to be used in the final stage
			
		Bit#(TAdd#(regwidth,3)) lv_mult={2'b00,_operand1,0};		// lv_mult     = multiplier appended with a zero and extended with zeros, a requirement of booth's unsigned multiplication of second order algorithm
		Bit#(TMul#(regwidth,2)) lv_multp=zeroExtend(_operand2);	// lv_multp    = multiplicand
                
	
		Bit#(TMul#(regwidth,2)) lv_lmultp = lv_multp<<1;	// lv_lmultp    = multiplicand left shifted one bit position/ twice of multiplicand requied for booth encoding
		/*	
		 Generation of the 32 partial products using Booth's second order algorithm. The i+1 i i-1 th bits are used as the select bits
		 These 32 partial products are converted into redundant binary partial products by taking two at a time.i.e every adjacent partial product.
		 To form a RB number, the two's complement of the second number is needed. So, every odd numbered partial product is inverted. To avoid an                   addition of 1
		 which will involve a a carry propagation, we take these 16 1's along with a 128'b0 to form the 9th partial product.
		 The last two bit positions is due to the left shifting of two bits for adjacent partial products. Since, the odd numbered partial products
		 are inverted, we append one's. For even numbered partial products, we append zeroes.
		*/
		Vector#(TDiv#(regwidth,2),Bit#(TMul#(regwidth,2))) lv_pp = replicate(0); // Creating a vector for 32 128 bit pp's
		Bit#(TMul#(regwidth,2)) lv_extra_pp = 0; //Extra partial product generated due to the 2 zero bit extended in multiplier for unsigned multiplication 

		lv_pp[0]=gen_pp64 (lv_multp,lv_lmultp,lv_mult[2:0],0);
		lv_pp[1]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[4:2],1)<<2 | 'b11);
		lv_pp[2]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[6:4],0)<<4);
		lv_pp[3]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[8:6],1)<<2 | 'b11)<<4;
		lv_pp[4]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[10:8],0)<<8);
		lv_pp[5]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[12:10],1)<<2 | 'b11)<<8;
		lv_pp[6]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[14:12],0)<<12);
		lv_pp[7]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[16:14],1)<<2 | 'b11)<<12;
		
                 if(rEGWIDTH==32)
                 begin
		
			lv_pp[8]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[18:16],0)<<16);
			lv_pp[9]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[20:18],1)<<2 | 'b11)<<16;
			lv_pp[10]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[22:20],0)<<20);
			lv_pp[11]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[24:22],1)<<2 | 'b11)<<20;
			lv_pp[12]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[26:24],0)<<24);
			lv_pp[13]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[28:26],1)<<2 | 'b11)<<24;
			lv_pp[14]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[30:28],0)<<28);
			lv_pp[15]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[32:30],1)<<2 | 'b11)<<28;
		
                 
                 if(rEGWIDTH==64)
                 begin
		
			lv_pp[16]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[34:32],0)<<32);
			lv_pp[17]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[36:34],1)<<2 | 'b11)<<32;
			lv_pp[18]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[38:36],0)<<36);
			lv_pp[19]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[40:38],1)<<2 | 'b11)<<36;
			lv_pp[20]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[42:40],0)<<40);
			lv_pp[21]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[44:42],1)<<2 | 'b11)<<40;
			lv_pp[22]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[46:44],0)<<44);
			lv_pp[23]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[48:46],1)<<2 | 'b11)<<44;
			lv_pp[24]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[50:48],0)<<48);
			lv_pp[25]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[52:50],1)<<2 | 'b11)<<48;
			lv_pp[26]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[54:52],0)<<52);
			lv_pp[27]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[56:54],1)<<2 | 'b11)<<52;
			lv_pp[28]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[58:56],0)<<56);
			lv_pp[29]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[60:58],1)<<2 | 'b11)<<56;
			lv_pp[30]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[62:60],0)<<60);
			lv_pp[31]=(gen_pp64(lv_multp,lv_lmultp,lv_mult[64:62],1)<<2 | 'b11)<<60;
			lv_extra_pp[127:64] = gen_pp64(lv_multp,lv_lmultp,lv_mult[66:64],0)[63:0];
		
                 end
		 end   
		// The 32 partial products are stored in the stage1 FIFO along with parameters to determine MUL type and the extra pp
		ff_stage1.enq(Stage1_data{	stage1:	lv_pp,                                          // Storing the partial products in the final FIFO
									stage1_extra_pp : lv_extra_pp,
									token:_rob_number,
									destination: _destination,
									summed_exponent:_summed_exponent,
									sign :_sign,
									zero:_zero,
									infinity:_infinity,
									invalid:_invalid,
									denormal: _denormal,
									psw: _psw,
									program_counter:_program_counter					
								}); 

	endmethod


	method Stage5_data#(regwidth,tokensize,fpexp) result_();	//Method to get result
		return ff_final_result.first();
	endmethod
	
	method Action _set_flush(Bool _flush); // Method to flush the pipe
		wr_flush<=_flush;
	endmethod

	method Action _dequeue(); // Method to release the final result FIFO after obtaining result
		ff_final_result.deq();
	endmethod 

endmodule

endpackage
