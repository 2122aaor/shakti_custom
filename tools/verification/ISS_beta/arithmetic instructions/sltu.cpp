//Program for the SLTU opcode
//SLTU performs unsigned compare of two 32 bit registers, writing 1 to rd if rs1 < rs2, 0 otherwise.

#include <stdio.h>

void sltu(int rd, int rs1, int rs2)
{
	
	uint64_t numrs1;
	uint64_t numrs2;
	
	numrs1 = ubin2dec(Rreg[rs1]);
	numrs2 = ubin2dec(Rreg[rs2]);
	if(rd!=0)
	{
	if(numrs1 < numrs2)
		dec2bin(Rreg[rd],1);
	else
		dec2bin(Rreg[rd],0);
	}
	std::cout << "SLTU TAKEN\n";
	/*printf("SLTU\n");
	printreg(rs1);
	printreg(rs2);
	printreg(rd);*/
}
