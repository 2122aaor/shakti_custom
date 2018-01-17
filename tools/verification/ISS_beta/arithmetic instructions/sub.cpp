//Program for the SUB opcode
//Program to subtract one 32 bit signed number from another
#include <stdio.h>

void sub(int rd, int rs1, int rs2)
{
	int64_t numrs1;
	int64_t numrs2;
	int64_t numrd;
	
	numrs1 = bin2dec(Rreg[rs1]);
	numrs2 = bin2dec(Rreg[rs2]);
	numrd = numrs1 - numrs2;
	if(rd!=0)
	dec2bin(Rreg[rd],numrd);
	std::cout << "SUB TAKEN\n";
	/*printf("rs1 = %ld", numrs1);
	printf("rs2 = %ld", numrs2);
	printf("rd = %ld", numrd);
	
	printf("SUB\n");
	printreg(rs1);
	printreg(rs2);
	printreg(rd);*/
}
