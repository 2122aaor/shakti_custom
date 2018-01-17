/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Names : Rahul Bodduna 
Email ID : rahulbodduna@gmail.com
*/
/*
The algorithm is implemented from Atkins.pdf. An example for radix 2 can be found in example.pdf. The efficient way to 
select quotients is implemented from quotient_digit.pdf. To convert the quotients from redundant binary(generated as part of algorithm
to normal binary on-the-fly conversion is used - implemented from on_the_fly.pdf

This is variable latency algorithm. The number of cycles this division takes is the difference in lead zeros of dividend and divisor.
*/
package divider;
import lead_zero_detect64 ::*; 
import types:: *;
`include "defined_parameters.bsv"

interface IFC_sdivider#(numeric type div_width);
	method Action input_operands(Bit#(div_width) _dividend, Bit#(div_width) _input_divisor, ALU_func _div_name, Bit#(1) _word_flag, Bit#(TLog#(`PRF_SIZE)) _destination);
	method Bit#(div_width) result_;
	method Bit#(1) _release;
	method Bit#(TLog#(`PRF_SIZE)) destination_address_;
endinterface

	
module mksdivider(IFC_sdivider#(div_width))
	provisos(
		Add#(1, div_width, abs_div_width),
		Add#(1, msb_div_width, div_width),
		Add#(a__, 1, div_width),
		Add#(b__, 32, div_width),
		Add#(c__, 1, div_width_bits),
		Mul#(2, div_width, d_div_width),
		Log#(div_width, div_width_bits));

	let v_div_width = valueOf(div_width);
	let v_msb_div_width = valueOf(msb_div_width);
	let v_abs_div_width = valueOf(abs_div_width);
	let v_div_width_bits = valueOf(div_width_bits);
	let v_d_div_width = valueOf(d_div_width);

function Bit#(abs_div_width) compliment2(Bit#(abs_div_width) _input);
	Bit#(abs_div_width) _result = signExtend(1'b1);
	bit carry = 1;
	bit new_carry = 1;
	_result = _input^_result;
	for(Integer i = 0; i < v_div_width; i = i+1) begin
		new_carry = carry;
		carry = _result[i]&carry;
		_result[i] = _result[i]^new_carry;
	end
	return _result;
endfunction

	Reg#(Bit#(abs_div_width)) rg_divisor[2] <- mkCReg(2,0);
	Reg#(Bit#(abs_div_width)) rg_remainder[2] <- mkCReg(2,0);
	Reg#(Bit#(div_width)) rg_quotient_a[2] <- mkCReg(2,0);
	Reg#(Bit#(div_width)) rg_quotient_b[2] <- mkCReg(2,0);
	Reg#(Bit#(TLog#(`PRF_SIZE))) rg_destination <- mkReg(0);
	Reg#(Bit#(div_width_bits)) rg_shift_divisor <- mkReg(0);
	Reg#(bit) rg_negative_quotient <- mkReg(0);
	Reg#(bit) rg_div_rem <- mkReg(0);
	Reg#(bit) rg_release <- mkReg(0);
	Reg#(bit) rg_en_divider <- mkReg(0);
	Reg#(Bit#(div_width_bits)) rg_cycle_counter <- mkReg(0);
	Reg#(Bool) rg_sign <- mkReg(False);
	Reg#(Bool) rg_final_cycle <- mkReg(False);

rule rl_divide(rg_en_divider == 1);
	$display("Into SRT algorithm with cycle counter %d", rg_cycle_counter);
	Bit#(div_width) bit_mask_a = rg_quotient_a[1];
	$display("The quotient at this step %h", rg_quotient_a[1]);
	Bit#(div_width) bit_mask_b = rg_quotient_b[1];
	Bit#(div_width_bits) cycle_counter = rg_cycle_counter;
	cycle_counter = cycle_counter + 1;
	bit_mask_b[fromInteger(v_msb_div_width)-rg_cycle_counter] = 1;
	Bit#(abs_div_width) divider = 0;
	Bit#(abs_div_width) shifted_remainder = rg_remainder[1];
	if(rg_remainder[1][v_abs_div_width-1] == 0) begin
		if(rg_remainder[1][v_abs_div_width-2] == 1) begin
			$display("The quotient is 1");
			divider = compliment2(rg_divisor[1]);
			bit_mask_b = bit_mask_a;
			bit_mask_a[fromInteger(v_msb_div_width)-rg_cycle_counter] = 1;
		end
	end
	else if(rg_remainder[1][v_abs_div_width-1] == 1) begin
		if(rg_remainder[1][v_abs_div_width-2] == 0) begin
			$display("The quotient is -1");
			divider = rg_divisor[1];
			bit_mask_a = bit_mask_b;
			bit_mask_b[fromInteger(v_msb_div_width) - rg_cycle_counter] = 0;
		end
	end
	$display("The divider at this step %h", divider);
	if(rg_final_cycle) begin
		if(shifted_remainder[v_div_width]==1) begin
			divider = rg_divisor[1];
		end
		else
			divider = 0;
	$display("last step divider is %h", divider);
	end
	$display("The remainder at this step %h", shifted_remainder);
	shifted_remainder = shifted_remainder + divider;
	$display("The remainder at this step %h", shifted_remainder);
	$display("The the total quotient at this stage is %h", bit_mask_a);
	if(rg_final_cycle) begin 
		$display("\n \n***********************final cycle********************* \n \n");
		rg_final_cycle <= False;
		rg_remainder[1] <= shifted_remainder >> rg_shift_divisor;
		rg_quotient_a[1] <= rg_quotient_b[1];
		rg_en_divider <= 0;
		rg_release <= 1;
		$display("The shifted remainder at this step %h", shifted_remainder);
	end
	else if(rg_cycle_counter == 63) begin
		//rg_remainder[1] <= shifted_remainder;
		rg_remainder[1] <= shifted_remainder;
		rg_final_cycle <= True;
	end
	else begin
		rg_remainder[1] <= shifted_remainder << 1;
	end
	if(!rg_final_cycle) begin
		$display("The shifted remainder at this step %h", shifted_remainder);
		rg_quotient_a[1] <= bit_mask_a;
		rg_quotient_b[1] <= bit_mask_b;
		rg_cycle_counter <= cycle_counter;
		$display("***********************cycle********************* \n \n");
		$display("final quotient : %h, other_quotient : %h, final remainder %h", bit_mask_a, bit_mask_b, shifted_remainder<<1);
	end
endrule

method Action input_operands(Bit#(div_width) _dividend, Bit#(div_width) _divisor, ALU_func _div_name, Bit#(1) _word_flag, Bit#(TLog#(`PRF_SIZE)) _destination) if(rg_en_divider==0);
		Bit#(1)          _div_type=0;
		Bit#(1)			 _div_or_rem=0;
		rg_destination <= _destination;
		Bit#(1) lv_release = 1;
		Bit#(1) lv_en_divider = 0;
	//_div = 0 , _rem = 1
	case(_div_name)
//		DIVW: _div_type = 'b10;
		DIV: begin 
			_div_type   = 0;
			_div_or_rem = 0;
		end	
		DIVU: begin 
			_div_type   = 1;
			_div_or_rem = 0;
		end
		REM : begin
			_div_type   = 0;
			_div_or_rem = 1;
		end
		REMU : begin
			_div_type   = 1;
			_div_or_rem = 1;
		end
//		DIVWU: _div_type = 'b11;	
 	endcase
	rg_div_rem <= _div_or_rem;
	if(_word_flag==1 && _div_type==0) begin
		_divisor = signExtend(_divisor[31:0]);
		_dividend = signExtend(_dividend[31:0]);
	end
	else if(_word_flag==1 && _div_type==1) begin
		_divisor = zeroExtend(_divisor[31:0]);
		_dividend = zeroExtend(_dividend[31:0]);
	end
	$display("Divisor %h and Dividend %h", _divisor, _dividend);
	
	Bit#(div_width) caseK = 0;
	caseK[v_msb_div_width] = 1;
	if (_divisor=='d0 && _div_or_rem==0)					//Special case(divisor==0)
	   rg_quotient_a[1] <= 'd-1;
	else if (_divisor=='d0 && _div_or_rem==1)					//Special case(divisor==0)
	   rg_remainder[1] <= {1'b0,_dividend};
	//`ifdef divider64 //**********For RV64***************
	else if (_dividend==caseK && _divisor== 'd-1 && (_div_type[0]==0) && _div_or_rem==0)  //Special case (dividend=-2^(64-1) and divisor=-1)
	   rg_quotient_a[1] <= _dividend;//64'h8000000000000000;
	else if((_dividend == _divisor) && _div_or_rem==0)
		rg_quotient_a[1] <= 1;
	else if((_dividend == _divisor) && _div_or_rem==1)
		rg_remainder[1] <= 0;
	//`endif

	//`ifdef divider32//**********For RV32****************
	//else if (_dividend==32'h80000000 && _divisor== 'd-1 && (_div_type[0]==0) && _div_or_rem==0)  //Special case (dividend=-2^(32-1) and divisor=-1)
	//   	rg_quotient_a[1] <= 32'h80000000;
	//else if (_dividend==32'h80000000 && _divisor== 'd-1 && (_div_type[0]==0) && _div_or_rem==1)  //Special case (dividend=-2^(32-1) and divisor=-1)
	//   	rg_remainder[1] <= 0;
	//else if((_dividend == _divisor) && _div_or_rem==1)
	//	rg_remainder[1] <= 1;
	//`endif
	else begin
		lv_en_divider = 1;
		lv_release = 0;
		if(_div_type[0]==0) begin //DIV,REM
			$display("Divider enabled");
			if(_dividend[v_msb_div_width-1]==1) begin
		  		_dividend = compliment2({1'b1,_dividend})[v_msb_div_width:0];
				if(_divisor[v_msb_div_width-1]==1) begin
		  			_divisor = compliment2({1'b1,_divisor})[v_msb_div_width:0];
					rg_sign <= False;
				end
				else begin 
					_divisor = _divisor;
					rg_sign <= True;
				end
			end	
			else begin
		  		_dividend = _dividend;
				rg_sign <= True;
			end
		end
		else begin //DIVU, REMU
			rg_sign <= False;
			_dividend = _dividend;
			_divisor = _divisor;
		end
		if(_dividend < _divisor) begin
			lv_en_divider = 0;
			lv_release = 1;
		 	rg_remainder[1] <= {1'b0,_dividend};
		 	rg_quotient_a[1] <= 0;
		end
		else begin
			Bit#(div_width) lv_zeros = 0;
			Bit#(div_width_bits) lv_ones = signExtend(1'b1);
			let shift_dividend = countZerosMSB(_dividend);
			let shift_divisor = countZerosMSB(_divisor);
			rg_shift_divisor <= pack(shift_divisor)[v_div_width_bits-1:0];
			//Bit#(d_div_width) append_divisor = {_divisor,lv_zeros};
			//let append_divisor = lv_ones - pack(shift_divisor)[valueOf(div_width_bits)-1:0];
			$display("shift divisor is %d for divisor %h", shift_divisor, _divisor);
			rg_divisor[1] <= {1'b0,_divisor << shift_divisor}; 
				//{1'b0,append_divisor[fromInteger(v_d_div_width-1)-shift_divisor:fromInteger(v_div_width-1)-shift_divisor]}
			rg_remainder[1] <= {1'b0,_dividend << shift_dividend}; 
			rg_cycle_counter <= pack(shift_dividend-shift_divisor-1)[v_div_width_bits-1:0];
		end
	end
	rg_en_divider <= lv_en_divider;
	rg_release <= lv_release;
endmethod
	
method Bit#(div_width) result_() if(rg_release == 1);

	Bit#(abs_div_width) quotient;
	if(rg_sign && rg_quotient_a[1][v_msb_div_width]==1)
		quotient = compliment2({1'b1,rg_quotient_a[1]});
	else quotient = {1'b0,rg_quotient_a[1]};
	if(rg_div_rem == 0)
		return quotient[v_msb_div_width:0];
	else 
		return rg_remainder[1][v_msb_div_width:0];
endmethod

method Bit#(1) _release;
	return rg_release;
endmethod

method Bit#(TLog#(`PRF_SIZE)) destination_address_;
	return rg_destination;
endmethod

endmodule

interface IFC_divider;
	method Action input_operands(Bit#(`REG_WIDTH) _dividend, Bit#(`REG_WIDTH) _input_divisor, ALU_func _div_name, Bit#(1) _word_flag, Bit#(TLog#(`PRF_SIZE)) _destination);
	method Bit#(`REG_WIDTH) result_;
	method Bit#(1) _release;
	method Bit#(TLog#(`PRF_SIZE)) destination_address_;
endinterface

interface Ifc_divider;
	method Action input_operands(Bit#(`REG_WIDTH) _dividend, Bit#(`REG_WIDTH) _input_divisor, ALU_func _div_name, Bit#(1) _word_flag, Bit#(TLog#(`PRF_SIZE)) _destination);
	method Bit#(`REG_WIDTH) result_;
	method Bit#(1) _release;
	method Bit#(TLog#(`PRF_SIZE)) destination_address_;
endinterface

(*synthesize*)
module mkdivider(Ifc_divider); 
	IFC_sdivider#(`REG_WIDTH) divider <-mksdivider();

	
	method Action input_operands(Bit#(`REG_WIDTH) _dividend, Bit#(`REG_WIDTH) _input_divisor, ALU_func _div_name, Bit#(1) _word_flag, Bit#(TLog#(`PRF_SIZE)) _destination);
			divider.input_operands(_dividend, _input_divisor, _div_name, _word_flag, _destination);
	endmethod
	
	method Bit#(`REG_WIDTH) result_;
		return divider.result_;
	endmethod
	
	method Bit#(1) _release;
		return divider._release;
	endmethod

	method Bit#(TLog#(`PRF_SIZE)) destination_address_;
		return divider.destination_address_;
	endmethod
endmodule

endpackage
