//Standard basic libraries
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <boost/multiprecision/cpp_int.hpp>

using namespace boost::multiprecision;
#include "math.h"

//Mapping of canonical names to registers
#include "program_structure/Rregdict.cpp"
int getRreg(char *reg);

//Utility programs which facilitate in computing the outputs
#include "utility programs/str2bin.cpp"
void str2bin(int *bin,char *str); //Used directly in ISS code!
#include "utility programs/bindec.cpp"
int64_t bin2dec(int r[64]);
void dec2bin(int r[64],int64_t num);
void dec2bin128(char opt, int r[64], int128_t num);
int bin2dec32(int r[64]);
void dec2binsg32(int r[64],int num);
#include "utility programs/usgnbindec.cpp"
uint64_t ubin2dec(int r[64]);
void udec2bin(int r[64],uint64_t num);
void udec2bin128(char opt, int r[64], uint128_t num);
#include "utility programs/bitlogic.cpp"
int bit_xor(int x, int y);
int bit_and(int x, int y);
int bit_or(int x, int y);
#include "utility programs/printreg.cpp"
void printreg(int Rindex);
#include "utility programs/Mem.cpp"
#include "utility programs/printmem.cpp"
void printmem(uint64_t Mindex);
//#include "utility programs/parseline.cpp"
//void parseline(char ** parseline,char * line);
#include "utility programs/binhex.cpp"
char binhex(int n);
#include "utility programs/fileprintmem.cpp"
void fileprintmem(int reg, uint64_t mloc, bool* mval);
#include "utility programs/fileprintreg.cpp"
void fileprintreg();


//Programs associated with each opcode
#include "arithmetic instructions/lui.cpp"
void lui(int rd, int *imm);
#include "arithmetic instructions/auipc.cpp"
void auipc(int rd, int *imm);
#include "arithmetic instructions/addi.cpp"
void addi(int rd,int rs1, int *imm);
#include "arithmetic instructions/slti.cpp"
void slti(int rd,int rs1, int *imm);
#include "arithmetic instructions/sltiu.cpp"
void sltiu(int rd,int rs1, int *imm);
#include "arithmetic instructions/xori.cpp"
void xori(int rd,int rs1, int *imm);
#include "arithmetic instructions/ori.cpp"
void ori(int rd,int rs1, int *imm);
#include "arithmetic instructions/andi.cpp"
void andi(int rd,int rs1, int *imm);
#include "arithmetic instructions/slli.cpp"
void slli(int rd,int rs1, int *shamt);
#include "arithmetic instructions/srli.cpp"
void srli(int rd,int rs1, int *shamt);
#include "arithmetic instructions/srai.cpp"
void srai(int rd,int rs1, int *shamt);
#include "arithmetic instructions/add.cpp"
void Add(int rd, int rs1, int rs2);
#include "arithmetic instructions/sub.cpp"
void sub(int rd, int rs1, int rs2);
#include "arithmetic instructions/slt.cpp"
void slt(int rd, int rs1, int rs2);
#include "arithmetic instructions/sltu.cpp"
void sltu(int rd, int rs1, int rs2);
#include "arithmetic instructions/sll.cpp"
void sll(int rd, int rs1, int rs2);
#include "arithmetic instructions/srl.cpp"
void srl(int rd, int rs1, int rs2);
#include "arithmetic instructions/sra.cpp"
void sra(int rd, int rs1, int rs2);
#include "arithmetic instructions/xor.cpp"
void Xor(int rd, int rs1, int rs2);
#include "arithmetic instructions/and.cpp"
void And(int rd, int rs1, int rs2);
#include "arithmetic instructions/or.cpp"
void Or(int rd, int rs1, int rs2);
#include "arithmetic instructions/addiw.cpp"
void addiw(int rd,int rs1, int *imm);
#include "arithmetic instructions/slliw.cpp"
void slliw(int rd,int rs1, int *shamt);
#include "arithmetic instructions/srliw.cpp"
void srliw(int rd,int rs1, int *shamt);
#include "arithmetic instructions/sraiw.cpp"
void sraiw(int rd,int rs1, int *shamt);
#include "arithmetic instructions/sllw.cpp"
void sllw(int rd, int rs1, int rs2);
#include "arithmetic instructions/srlw.cpp"
void srlw(int rd, int rs1, int rs2);
#include "arithmetic instructions/sraw.cpp"
void sraw(int rd, int rs1, int rs2);
#include "arithmetic instructions/addw.cpp"
void addw(int rd, int rs1, int rs2);
#include "arithmetic instructions/subw.cpp"
void subw(int rd, int rs1, int rs2);
#include "arithmetic instructions/divs.cpp"
void divs(int rd,int rs1,int rs2);
#include "arithmetic instructions/divu.cpp"
void divu(int rd,int rs1,int rs2);
#include "arithmetic instructions/divw.cpp"
void divw(int rd,int rs1,int rs2);
#include "arithmetic instructions/rem.cpp"
void rem(int rd,int rs1, int rs2);
#include "arithmetic instructions/remu.cpp"
void remu(int rd,int rs1,int rs2);
#include "mul instructions/mul.cpp"
void mul(int rd, int rs1, int rs2);
#include "mul instructions/mulh.cpp"
void mulh(int rd, int rs1, int rs2);
#include "mul instructions/mulhu.cpp"
void mulhu(int rd, int rs1, int rs2);
#include "mul instructions/mulhsu.cpp"
void mulhsu(int rd, int rs1, int rs2);
#include "mul instructions/mulw.cpp"
void mulw(int rd, int rs1, int rs2);
#include "memory instructions/lb.cpp"
void lb(int rd , int rs1 , int * imm);
#include "memory instructions/lh.cpp"
void lh(int rd , int rs1 , int * imm);
#include "memory instructions/lhu.cpp"
void lhu(int rs2 , int rs1 , int * imm);
#include "memory instructions/lbu.cpp"
void lbu(int rd , int rs1 , int * imm);
#include "memory instructions/lwu.cpp"
void lwu(int rs2 , int rs1 , int * imm);
#include "memory instructions/lw.cpp"
void lw(int rs2 , int rs1 , int * imm);
#include "memory instructions/ld.cpp"
void ld(int rs2 , int rs1 , int * imm);
#include "memory instructions/sb.cpp"
void sb(int rs1 , int rs2 , int * imm);
#include "memory instructions/sh.cpp"
void sh(int rs1 , int rs2 , int * imm);
#include "memory instructions/sw.cpp"
void sw(int rs1 , int rs2 , int * imm);
#include "memory instructions/sd.cpp"
void sd(int rs1 , int rs2 , int * imm);
#include "branch instructions/jal.cpp"
void jal(int rd , int *imm);
#include "branch instructions/jalr.cpp"
void jalr(int rd , int rs1 , int * imm);
void execute(char **parsline);
#include "branch instructions/beq.cpp"
void beq(int rs1 , int rs2 , int * imm);
#include "branch instructions/blt.cpp"
void blt(int rs1 , int rs2 , int * imm);
#include "branch instructions/bge.cpp"
void bge(int rs1 , int rs2 , int * imm);
#include "branch instructions/bltu.cpp"
void bltu(int rs1 , int rs2 , int * imm);
#include "branch instructions/bgeu.cpp"
void bgeu(int rs1 , int rs2 , int * imm);
#include "branch instructions/bne.cpp"
void bne(int rs1 , int rs2 , int * imm);



//Program execute.cpp calls other functions in above programs based on the opcode
//#include "program_structure/execute.cpp" 
