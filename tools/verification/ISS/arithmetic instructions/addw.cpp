//Program for the ADDW opcode
//Program to add two 64 bit signed numbers
//Works on lower 32 bit
#include <stdio.h>

void addw(int rd, int rs1, int rs2)
{
	int numrs1;
	int numrs2;
	int numrd;

	numrs1 = bin2dec32(Rreg[rs1]);
	numrs2 = bin2dec32(Rreg[rs2]);
	numrd = numrs2 + numrs1;
	if(rd!=0)
	dec2binsg32(Rreg[rd],numrd);
	std::cout << "ADDW TAKEN\n";
}
