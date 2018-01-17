//Program for the SLT opcode
//SLT performs signed compare of two 64 bit registers, writing 1 to rd if rs1 < rs2, 0 otherwise.
#include <stdio.h>

void slt(int rd, int rs1, int rs2)
{
	int64_t numrs1;
	int64_t numrs2;

	numrs1 = bin2dec(Rreg[rs1]);
	numrs2 = bin2dec(Rreg[rs2]);
	if(rd!=0)
	{
	if(numrs1 < numrs2)
		dec2bin(Rreg[rd],1);
	else
		dec2bin(Rreg[rd],0);
	}
	std::cout << "SLT TAKEN\n";
	/*printf("SLT\n");
	printreg(rs1);
	printreg(rs2);
	printreg(rd);*/
}
