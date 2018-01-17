//execute.c calls other functions in programs based on the opcode
#include <stdio.h>
#include <string.h>

void execute(char **parsline)
{
	int rd,rs1,rs2;
	if(strcmp(parsline[0],"LUI") == 0)
	{
		int imm[20];
		str2bin(imm,parsline[2]);
		rd = getRreg(parsline[1]);
		lui(rd,imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"ADDI") == 0)
	{
		int imm[12];
		str2bin(imm,parsline[3]);
		rd = getRreg(parsline[1]);
		//printf("rd=%d\n",rd);
		rs1 = getRreg(parsline[2]);
		//printf("rs1=%d\n",rs1);
		/*int i;
		for (i = 0; i < 14; ++i)
		{
			printf("%d", imm[i]);
		fileprintreg();	
		}*/
		addi(rd,rs1,imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SLTI") == 0)
	{
		int imm[12];
		str2bin(imm,parsline[3]);
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		slti(rd,rs1,imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SLTIU") == 0)
	{
		int imm[12];
		str2bin(imm,parsline[3]);
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		sltiu(rd,rs1,imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"XORI") == 0)
	{
		int imm[12];
		str2bin(imm,parsline[3]);
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		xori(rd,rs1,imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"ORI") == 0)
	{
		int imm[12];
		str2bin(imm,parsline[3]);
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		ori(rd,rs1,imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"ANDI") == 0)
	{
		int imm[12];
		str2bin(imm,parsline[3]);
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		andi(rd,rs1,imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SLLI") == 0)
	{
		int shamt[5];
		str2bin(shamt,parsline[3]);
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		slli(rd,rs1,shamt);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SRLI") == 0)
	{
		int shamt[5];
		str2bin(shamt,parsline[3]);
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		srli(rd,rs1,shamt);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SRAI") == 0)
	{
		int shamt[5];
		str2bin(shamt,parsline[3]);
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		srai(rd,rs1,shamt);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"ADD") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		Add(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SUB") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		sub(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SLT") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		slt(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SLTU") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		sltu(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SLL") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		sll(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SRL") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		srl(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SRA") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		sra(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"XOR") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		Xor(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"AND") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		And(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"OR") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		Or(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"ADDIW") == 0)
	{
		int imm[12];
		str2bin(imm,parsline[3]);
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		addiw(rd,rs1,imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SLLIW") == 0)
	{
		int shamt[5];
		str2bin(shamt,parsline[3]);
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		slliw(rd,rs1,shamt);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SRLIW") == 0)
	{
		int shamt[5];
		str2bin(shamt,parsline[3]);
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		srliw(rd,rs1,shamt);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SRAIW") == 0)
	{
		int shamt[5];
		str2bin(shamt,parsline[3]);
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		sraiw(rd,rs1,shamt);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SLLW") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		sllw(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SRLW") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		srlw(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SRAW") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		sraw(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"ADDW") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		addw(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SUBW") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		subw(rd,rs1,rs2);
		fileprintreg();
	}
	/*else if(strcmp(parsline[0],"MUL") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		mul(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"MULH") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		mulh(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"MULHU") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		mulhu(rd,rs1,rs2);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"MULHSU") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		mulhsu(rd,rs1,rs2);
		fileprintreg();
	}*/
	else if(strcmp(parsline[0],"LB")==0)
	{
		int imm[12];
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		lb(rd , rs1 , imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"LH")==0)
	{
		int imm[12];
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		lh(rd , rs1 , imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"LW")==0)
	{
		int imm[12];
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		lw(rd , rs1 , imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"LBU")==0)
	{
		int imm[12];
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		lbu(rd , rs1 , imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"LHU")==0)
	{
		int imm[12];
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		lhu(rd , rs1 , imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"LWU")==0)
	{
		int imm[12];
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		lwu(rd , rs1 , imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"LD")==0)
	{
		int imm[12];
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		ld(rd , rs1 , imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SB")==0)
	{
		int imm[12];
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		sb(rs1 , rs2 , imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SH")==0)
	{
		int imm[12];
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		sh(rs1 , rs2 , imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SW")==0)
	{
		int imm[12];
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		sw(rs1 , rs2 , imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"SD")==0)
	{
		int imm[12];
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		sd(rs1 , rs2 , imm);
		fileprintreg();
	}
	else if(strcmp(parsline[0],"JAL")==0)
	{
		int imm[20];
		rd = getRreg(parsline[1]);
		str2bin(imm,parsline[2]);
		jal(rd ,imm);
	}
	else if(strcmp(parsline[0],"JALR")==0)
	{
		int imm[12];
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		jalr(rd , rs1, imm);
	}
	else if(strcmp(parsline[0],"BEQ")==0)
	{
		fileprintreg();
		int imm[12];
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		beq(rs1,rs2, imm);
	}
	else if(strcmp(parsline[0],"BLT")==0)
	{
		fileprintreg();
		//printf("found\n" );
		int imm[12];
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		blt(rs1,rs2, imm);
	}
	else if(strcmp(parsline[0],"BGE")==0)
	{
		fileprintreg();
		int imm[12];
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		bge(rs1, rs2, imm);
	}
	else if(strcmp(parsline[0],"BLTU")==0)
	{
		fileprintreg();
		int imm[12];
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		bltu(rs1,rs2, imm);
	}
	else if(strcmp(parsline[0],"BGEU")==0)
	{
		fileprintreg();
		int imm[12];
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		bgeu(rs1,rs2, imm);
	}
	else if(strcmp(parsline[0],"BNE")==0)
	{
		fileprintreg();
		int imm[12];
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);
		str2bin(imm,parsline[3]);
		bne(rs1,rs2, imm);
	}
	else if(strcmp(parsline[0],"AUIPC") == 0)
	{
		int imm[20];
		str2bin(imm,parsline[2]);
		rd = getRreg(parsline[1]);
		auipc(rd,imm);
		fileprintreg();
	}

}
