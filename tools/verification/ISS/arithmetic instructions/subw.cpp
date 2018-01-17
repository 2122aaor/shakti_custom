//Program for the SUBW opcode
//Program to subtract a 64 bit signed number from another
//Works on lower 32 bit
#include <stdio.h>

void subw(int rd, int rs1, int rs2)
{
	int numrs1;
	int numrs2;
	int numrd;

	numrs1 = bin2dec32(Rreg[rs1]);
	numrs2 = bin2dec32(Rreg[rs2]);
	numrd = numrs1 - numrs2;
	if(rd!=0)
	dec2binsg32(Rreg[rd],numrd);
	std::cout << "SUBW TAKEN\n";
}
