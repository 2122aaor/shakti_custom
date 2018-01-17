/*
Copyright (c) 2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name     : Handcoded test vectors for ROCC Unit
Author Name     : Sumanth Sridhar, Vinod G
e-mail Id       : sumanthsridhar.009@gmail.com, g.vinod1993@gmail.com
Last updated on : 30th June 2016
*/

VectorReq t[16];

t[0] =	VectorReq {	
	ctrlInstruction	: {f7_VMCA, 5'd0, 5'd24, 3'b010, 5'd4, custom1}, 
	value64: '1
};


t[1] = 	VectorReq {	
	ctrlInstruction	: {7'd0, 5'd0, 5'd24, 3'b010, rd_vsetcfg, custom0}, 
	value64: '1
};	// vsetcfg

t[2] =	VectorReq {	
	ctrlInstruction	: {f7_VMCA, 5'd0, 5'd24, 3'b010, 5'd4, custom1}, 
	value64: '1
};

t[3] = 	VectorReq {	
	ctrlInstruction	: {f7_VUNCFG, 5'd0, 5'd0, 3'b0, 5'd0, 7'd0}, 
	value64: '1
};	// vuncfg error

t[4] =	VectorReq {
	ctrlInstruction	: {7'd63, 5'b10000, 5'd15, 3'b010, 5'b11111, custom1},
	value64: 64'hdecaf
};	// VF

t[5] =	VectorReq {
	ctrlInstruction	: {7'd63, 5'b01000, 5'd15, 3'b010, 5'b11111, custom1},
	value64: 64'hdecaf
};	// VF error

t[6] =	VectorReq {
	ctrlInstruction	: {7'd63, 5'b10000, 5'd15, 3'b110, 5'b11111, custom0},
	value64: 64'h3
};	// Vsetvl

t[7] =	VectorReq {
	ctrlInstruction	: {7'd63, 5'b10000, 5'd15, 3'b011, 5'b11111, custom1},
	value64: 64'hdecaf
};	// VF error

t[8] =	VectorReq {
	ctrlInstruction	: {f7_VMCS, 5'b01100, 5'd15, 3'b010, 5'b11111, custom1},
	value64: 64'hdeadcaf
};	// VMCS

t[9] = VectorReq {
	ctrlInstruction	: {f7_VGETCFG, 5'b0,5'b0,3'b100,5'd1,custom0},
	value64	: 64'hbaddecaf
};	// VGETCFG

t[10] = VectorReq {
	ctrlInstruction	: {f7_VSETVL, 5'b0,5'h1f,3'b10,5'h1f,custom0},
	value64	: 64'hdead
};	// vsetvl error 

t[11] = VectorReq {	
	ctrlInstruction	: {f7_VUNCFG, 5'd0, 5'd0, 3'b0, 5'd0, custom0}, 
	value64: '0
};

t[12] = VectorReq {
	ctrlInstruction	: {f7_VSETVL, 5'b0,5'h1f,3'b110,5'h1f,custom0},
	value64	: 64'hbac2decaf
};

t[13] = VectorReq {
	ctrlInstruction	: {f7_VSETVL, 5'b0,5'h1f,3'b110,5'h1f,custom0},
	value64	: 64'hbac2decaf
};
