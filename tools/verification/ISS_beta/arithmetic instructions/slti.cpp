//Program for SLTI opcode
/*SLTI (set less than immediate) places the value 1 in register rd if register rs1 is less than the
sign-extended immediate when both are treated as signed numbers, else 0 is written to rd.*/

#include <stdio.h>

void slti(int rd,int rs1, int *imm)
{
	//int rd[32];
	//int rs1[32] = {0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,1,1,1,0,1,1,1,0,1,1,0};
	//int imm[12] = {1,1,1,0,1,0,1,0,0,1,1,1};
	int imm64[64];
	int64_t numrs1;
	int64_t numimm64;
	int64_t numrd;
	int i = 0;
	for (i= 0; i < 12; ++i)
	{
		imm64[63-i] = imm[11-i];
	}
	i = 0;
	while(i < 52)
	{
		imm64[i] = imm[0];
		i++;
	}
	numrs1 = bin2dec(Rreg[rs1]);
	numimm64 = bin2dec(imm64);
	if(numrs1 < numimm64)
		numrd = 1;
	else
		numrd = 0;
	if(rd!=0)
	{
	Rreg[rd][63] = numrd;
	for(i = 0; i < 63; ++i)
	    Rreg[rd][i] = 0;
	}
	std::cout << "SLTI TAKEN\n";
	//dec2bin(Rreg[rd],numrd);
	//printreg(rd);
}
