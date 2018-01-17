/*

Copyright (c) 2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name     : Vector ISA Defs 
Author Name     : Sumanth Sridhar, Vinod.G
e-mail Id       : sumanthsridhar.009@gmail.com, g.vinod1993@gmail.com
Last updated on : 30th June 2016

This Module contains the ISA Definitions for the Vector Fetch Architecture.
Ref: http://www.eecs.berkeley.edu/Pubs/TechRpts/2015/EECS-2015-262.pdf

*/

package VectorAccelDefs;

`include "defined_parameters.bsv"
`include "vxu_defs.bsv"

// Exceptions 
// todo assign correct values
Bit#(64)    accelDisabled   = '1;
Bit#(64)    illegalInstr    = '1;

// max h/w Vector Length --> used vector lt = min {requested VL, max h/w VL}
Bit#(64)    maxVecLt        = 'd8;

//////////////////////////////////////////   Front_End   //////////////////////////////////////////

typedef Bit#(32)    Instr32;
typedef Bit#(7)     Opcode;
typedef Bit#(5)     RegName;

typedef struct {
	Instr32     ctrlInstruction;
	Bit#(64)    value64;
} VectorReq deriving (Eq, Bits, FShow);

typedef struct {
	RegName     destReg; 
	Bit#(64)    value64;
} VectorResp deriving (Eq, Bits, FShow);

// decoded Instr32
typedef enum {
	Vsetcfg , Vsetvl, Vgetcfg, Vgetvl, Vuncfg, Vmcs, Vmca, Vf, ERROR
} DInstr32 deriving (Eq, Bits, FShow);

typedef struct {
	DInstr32    instrName;
	Instr32     ctrlInstruction;
	Bit#(64)    value64;
} CMDQReq deriving (Eq, Bits, FShow);


typedef Bit#(64)    Vcfg;
typedef Bit#(64)    Vlen;
typedef struct {
	Vlen    vlen;
	Bool    setVL_response;
} VlenStruct deriving (Eq, Bits, FShow);

// vcfg field extract functions
function Bit#(9)    vcfg_v64    (Bit#(64) x) = x[8:0];      //todo change Bit#(64) if reqd
function Bit#(5)    vcfg_pred   (Bit#(64) x) = x[13:9];
function Bit#(9)    vcfg_v32    (Bit#(64) x) = x[22:14];
function Bit#(9)    vcfg_v16    (Bit#(64) x) = x[31:23];


/////////////////////////////////////   Control Instructions   ////////////////////////////////////

// from RISC-V base 32-bit opcode map for inst[6:0]
Opcode  custom0 = 'b_00_010_11;
Opcode  custom1 = 'b_01_010_11;

// funct7 field values for decoding control-thread instructions
// todo assign correct values
Bit#(7) f7_VSETVL   =   'b_000_0001;
Bit#(7) f7_VGETVL   =   'b_000_0100;
Bit#(7) f7_VGETCFG  =   'b_000_1000;
Bit#(7) f7_VUNCFG   =   'b_001_0000;

Bit#(7) f7_VMCS     =   'b_010_0000;
Bit#(7) f7_VMCA     =   'b_100_0000;

// VSETCFG is decoded by the value of the rd field
// todo determine what exactly the rd field should hold : vcfg addr, or some other spl vsetcfg value?
Bit#(5) rd_vsetcfg  =   'b01010;    // NOTE this is a random value

// functions to extract reqd fields from ctrl instructions
function RegName    i32_rs1           (Instr32 x) = x [19:15];
function RegName    i32_rs2           (Instr32 x) = x [24:20];
function RegName    i32_rd            (Instr32 x) = x [11:7];
function Opcode     i32_opcode        (Instr32 x) = x [6:0];
function Bit#(7)    i32_funct7        (Instr32 x) = x [31:25];
function Bit#(1)    i32_xd_bit        (Instr32 x) = x [14];
function Bit#(1)    i32_xs1_bit       (Instr32 x) = x [13];
function Bit#(1)    i32_xs2_bit       (Instr32 x) = x [12];
function Bit#(2)    i32_xd_xs1        (Instr32 x) = x [14:13];      // only these 2 are reqd as no ctrl thread ops use src2
function Bool       i32_xd_bool       (Instr32 x) = (x [14] == '1);
function Bool       i32_xs1_bool      (Instr32 x) = (x [13] == '1);
function Bool       i32_xs2_bool      (Instr32 x) = (x [12] == '1);

/////////////////////////////////////   Vector Instructions   /////////////////////////////////////


typedef Bit#(64)    Instr64;
typedef Bit#(12)    I64_Opcode;

/* defined opcodes from manual
 the lowest 7 bits of a Hwacha worker thread instruction (inst[6:0]) are tied
 to 0_111_111 in order to follow the 64-bit RISC-V instruction encoding */

I64_Opcode op_OPIMM             = 'b_00_100_0111111;        // scalar arithmetic ops
I64_Opcode op_AUIPC             = 'b_00_101_0111111;        // add upper immediate to pc (scalar)
I64_Opcode op_OPIMM32           = 'b_00_110_0111111;        // 32-bit scalar arithmetic ops

I64_Opcode op_AMO               = 'b_01_011_0111111;        // Vector Atomic Ops
I64_Opcode op_OP                = 'b_01_100_0111111;        // vector/scalar ops (integer, compare & predicate arith)
I64_Opcode op_LUI               = 'b_01_101_0111111;        // load upper immediate (scalar)
I64_Opcode op_OP32              = 'b_01_110_0111111;        // vector/scalar 32-bit integer arithmetic

I64_Opcode op_MADD              = 'b_10_000_0111111;        // vector/scalar floating-point fused-multiply-add
I64_Opcode op_MSUB              = 'b_10_001_0111111;        // vector/scalar floating-point fused-multiply-sub
I64_Opcode op_NMSUB             = 'b_10_010_0111111;        // vector/scalar floating-point neg(fused-multiply-sub)
I64_Opcode op_NMADD             = 'b_10_011_0111111;        // vector/scalar floating-point neg(fused-multiply-add)
I64_Opcode op_OP_FP             = 'b_10_100_0111111;        // vector/scalar floating-point instructions
I64_Opcode op_VLOAD             = 'b_10_110_0111111;        // vector/scalar loads (custom-2)

I64_Opcode op_CTRL              = 'b_11_000_0111111;        // VSTOP, VFENCE
I64_Opcode op_JALR              = 'b_11_001_0111111;        // VCJALR
I64_Opcode op_JAL               = 'b_11_011_0111111;        // VCJAL
I64_Opcode op_VSTORE            = 'b_11_110_0111111;        // vector/scalar stores (custom-3)


// functions to extract reqd fields from worker instructions
function Bit#(4)    i64_flags   (Instr64 x) = x[63:60];
function Bit#(7)    i64_funct7  (Instr64 x) = x[59:53];
function Bit#(3)    i64_funct3  (Instr64 x) = x[52:50];
function Bit#(9)    i64_funct9  (Instr64 x) = x[49:41];
function Bit#(8)    i64_rs3     (Instr64 x) = x[48:41];
function Bit#(8)    i64_rs2     (Instr64 x) = x[40:33];
function Bit#(1)    i64_n       (Instr64 x) = x[32];
function Bit#(8)    i64_rs1     (Instr64 x) = x[31:24];
function Bit#(8)    i64_rd      (Instr64 x) = x[23:16];
function Bit#(4)    i64_p       (Instr64 x) = x[15:12];
function I64_Opcode i64_opcode  (Instr64 x) = x[11:0];

//////////////////////////////////////// WorkerThread pipe ////////////////////////////////////////

/*
typedef struct {
	Bit#(`VS_AddrWidth) addr;
	Bit#(64)            data;
} VSAddr_Data deriving (Eq, Bits, FShow);
*/

`ifdef USE_ROB
	typedef union tagged {
		Bit#(TLog#(`ROB_BUFFER_SIZE))   RobAddr;
		Bit#(64)                        Data;
	} AddrOrData deriving (Eq, Bits, FShow);
`else
	typedef union tagged {
		Bit#(64)                        Data;
	} AddrOrData deriving (Eq, Bits, FShow);
`endif

typedef struct {
	Bit#(4)    flags;
	Bit#(7)    f7;
	Bit#(3)    f3;
	Bit#(9)    f9;
	Bit#(8)    rs2;
	Bit#(1)    n;
	Bit#(8)    rs1;
	Bit#(8)    rd;
	Bit#(4)    p;
	I64_Opcode opcode;
} I64_VR deriving (Eq, Bits, FShow);

typedef struct {
	Bit#(32)   imm32;
	Bit#(8)    rs1;
	Bit#(8)    rd;
	Bit#(4)    f4;
	I64_Opcode opcode;
} I64_VI deriving (Eq, Bits, FShow);

typedef enum {
	ADD_SUB, SLL, SLT, SLTU, XOR, SRL_SRA, OR, AND
} AluOp deriving (Eq, Bits, FShow);

typedef struct {
	Bit#(4)    _flags;
	Bit#(7)    _f7;
	AluOp      _f3;
	Bit#(9)    _f9;
	Bit#(8)    _rs2;
	Bit#(1)    _n;
	Bit#(8)    _rs1;
	Bit#(8)    _rd;
	Bit#(4)    _p;
	I64_Opcode _opcode;
} Disp_instr64 deriving (Eq, Bits, FShow);

typedef struct {
	Bit#(32)   _imm32;
	Bit#(8)    _rs1;
	Bit#(8)    _rd;
	Bit#(1)    _zero;
	AluOp      _f3_imm;
	I64_Opcode _opcode;
} Disp_instr64_imm deriving (Eq, Bits, FShow);

`ifdef USE_ROB
	// when ROB is used, RdToken is the address of the reserved address line (renamed register address)
	`ifdef Scalar_WriteBack_Priority
		typedef Bit#(TLog#(`ROB_BUFFER_SIZE))           RdToken;
	`else // ifdef Scalar_WriteBack_OldestFirst
		typedef Bit#(TAdd#(TLog#(`ROB_BUFFER_SIZE),1))  RdToken;
	`endif
`else
	// when ROB is not used, RdToken = rd field of the instruction (since no reg renaming)
		typedef Bit#(`VS_AddrWidth)                     RdToken;
`endif

typedef struct {
	RdToken             rdToken;
	Bit#(64)            data;
} RdToken_Data deriving (Eq, Bits, FShow);

typedef enum {
	// 0,      1,   2,    3,       4
	Exec, MulDiv, Mem, FPop, VecRedn, NONE
} FU_Code deriving (Eq, Bits, FShow);

typedef struct {
	Instr64             instr64;
	RdToken             rdAddr;
	Maybe#(Bit#(64))    operand1;
} DecodedDataQ1 deriving (Eq, Bits, FShow);

typedef struct {
	Instr64             instr64;
	RdToken             rdAddr;
	Maybe#(Bit#(64))    operand1;
	Maybe#(Bit#(64))    operand2;
} DecodedDataQ2 deriving (Eq, Bits, FShow);

typedef struct {
	Instr64             instr64;
	RdToken             rdAddr;
	Maybe#(Bit#(64))    operand1;
	Maybe#(Bit#(64))    operand2;
	Maybe#(Bit#(64))    operand3;
} DecodedDataQ3 deriving (Eq, Bits, FShow);

typedef struct {
	Instr64     instr64;
	Bit#(64)    storeAddr;
} StoreBuffer deriving (Eq, Bits, FShow);


////////////////////////////////////////////   Lane   /////////////////////////////////////////////

typedef enum {
	Zero, One, Two, NOP
} LatchOperandID deriving (Eq, Bits, FShow);

typedef enum {
	B, H, W, D, Q, Predicate	// byte (8b), half-word (16b), word (32b), double-word (64b), quad-word (128b)
} BitWidth deriving (Eq, Bits, FShow);

typedef struct {
	BitWidth	dataType;
	d_type		data;
} DataAndType#(type d_type) deriving (Eq, Bits, FShow);

typedef struct {
	Bit#(TLog#(`VRF_DEPTH))				addr;
	Bit#(TSub#(TLog#(`BANK_WIDTH),3))	byteOffset;
	// BitWidth							operandWidsth;
} VRFRead deriving (Eq, Bits, FShow);

typedef enum {
	FromALU, FromWriteBuffer
} VRFWriteSource deriving (Eq, Bits, FShow);

typedef struct {
	Bit#(TLog#(`VRF_DEPTH))				addr;
	Bit#(TLog#(`BANK_PRED_WIDTH))		predBitOffset;
	Bit#(`BANK_PRED_WIDTH)				pred;
	VRFWriteSource						sourceSel;
	Bit#(TSub#(TLog#(`BANK_WIDTH),3))	byteOffset;
} VRFWrite deriving (Eq, Bits, FShow);

typedef struct {
	Bit#(TLog#(`PRF_DEPTH))			addr;
	Bit#(TLog#(`BANK_PRED_WIDTH))	bitOffset;
	Bit#(1)							n;
} PredReadSel deriving (Eq, Bits, FShow);

typedef enum {
	FromALU, FromPLU, FromWriteBuffer
} PRFWriteSource deriving (Eq, Bits, FShow);

typedef struct {
	Bit#(TLog#(`PRF_DEPTH))			addr;
	PRFWriteSource					sourceSel;
	Bit#(TLog#(`BANK_PRED_WIDTH))	bitOffset;
} PredWrite deriving (Eq, Bits, FShow);

typedef struct {
	Bit#(3)		funct3;
	Bool		isOP32;					// for ops like VADDW, VSLLW, etc. that operate on 32-bit words
	Bool		doAlternateOperation;	// adder will do SUB, right-shifter will do arithmetic shift
	Bool		isVectorCompare;
} FopALU deriving (Eq, Bits, FShow);

typedef struct {
	VRFRead		vrf_read;						// Read from VRF
	PredReadSel pred_read;						// Read from PRF
	Maybe#(VRFWrite) vrf_write;					// Select VRF writeback mux and write to VRF
	Maybe#(PredWrite) pred_write;				// Select PRF writeback mux and write to PRF
	LatchOperandID opl;							// Write to operand latch
	Bool pdl;									// Write to predicate latch
	Maybe#(BitWidth) xbar;						// Drive crossbar with operands
	Bool sreg;									// Use given scalar operand instead
	FopALU fop_alu;					// Use ALU functional unit local to the bank
	// fop_plu;					// Use PLU functional unit local to the bank
	Bool fop_brq;								// Write to local BRQ
	Bool fop_bpq;								// Write to local BPQ
	// fop_vfu0_fma0;			// Use FMA0 functional unit on VFU0
	// fop_vfu0_imul;			// Use IMul functional unit on VFU0
	// fop_vfu0_fconv;			// Use FConv functional unit on VFU0
	// fop_vfu1_fma1;			// Use FMA1 functional unit on VFU1
	// fop_vfu1_fcmp;			// Use FCmp functional unit on VFU1
	// fop_vfu2;				// Write to LPQ/LRQ for VFU2
	// fop_vgu;					// Write to LPQ/LRQ for VGU

} MicroOps deriving (Eq, Bits, FShow);

// assuming predXBar is not independant of operandXBar
typedef struct {
	// NOTE change fifo type to "DataAndType#(Bit#(`W))" if reqd
	Bit#(`W) op0;
	Bit#(`W) op1;
	Bit#(`W) op2;
	Bit#(`BANK_PRED_WIDTH) pred;
} XBar deriving (Eq, Bits, FShow);

typedef struct {
	MicroOps uops;
	DataAndType#(Bit#(`W)) sreg0;
	DataAndType#(Bit#(`W)) sreg1;
	DataAndType#(Bit#(`W)) sreg2;
} LaneSequencerPacket deriving (Eq, Bits, FShow);

/*
typedef enum {
	Fop_alu, Fop_plu,								// Use ALU/PLU functional unit local to the bank
	Fop_brq, Fop_bpq,								// Write to local BRQ (bank operand read queue)/BPQ (bank predicate read queue)
	Fop_vfu0_fma0, Fop_vfu0_imul, Fop_vfu0_fconv,	// Use FMA0/FCONV/IMUL functional unit on VFU0
	Fop_vfu1_fma1, Fop_vfu1_fcmp,					// Use FMA1/FCMP functional unit on VFU1
	Fop_vfu2,										// Write to LRQ (lane operand read queue)/LPQ (lane predicate read queue) for VFU2
	Fop_vgu											// Write to LPQ/LRQ for VGU (vector address generation unit)
} BankMicroOps deriving (Eq, Bits, FShow);

// todo Change these enum based on interface of each VFU and info reqd for VFU operations
typedef enum {
	Vfu0_fma0_fmadd, Vfu0_fma0_fmsub, 
	Vfu0_fma0_fnmadd, Vfu0_fma0_fnmsub, 
	Vfu0_imul, Vfu0_fconv
} VFU0MicroOps deriving (Eq, Bits, FShow);

typedef struct {
	Bit#(`W) op0;
	Bit#(`W) op1;
	Bit#(`W) op2;
	VFU0MicroOps fop_vfu0;
} VFU0packet deriving (Eq, Bits, FShow);


typedef enum {
	Vfu1_fma1_fmadd, Vfu1_fma1_fmsub,
	Vfu1_fma1_fnmadd, Vfu1_fma1_fnmsub,
	Vfu1_fcmp
} VFU1MicroOps deriving (Eq, Bits, FShow);

typedef struct {
	Bit#(`W) op0;
	Bit#(`W) op1;
	Bit#(`W) op2;
	VFU1MicroOps fop_vfu1;
} VFU1packet deriving (Eq, Bits, FShow);


typedef enum {
	// todo
} VFU2MicroOps deriving (Eq, Bits, FShow);

typedef struct {
	Bit#(`W) op0;
	Bit#(`W) op1;
	VFU2MicroOps fop_vfu2;
} VFU2packet deriving (Eq, Bits, FShow);

*/

///////////////////////////////////////////////////////////////////////////////////////////////////

endpackage: VectorAccelDefs
