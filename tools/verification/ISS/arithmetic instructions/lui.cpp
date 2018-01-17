//Program for LUI opcode

/*LUI (load upper immediate) is used to build 64-bit constants and uses the U-type format. LUI
places the U-immediate value in the top 20 bits of the destination register rd, filling in the lowest
12 bits with zeros.*/

#include <stdio.h>

void lui(int rd,int *imm)
{
	int i = 0;
	if(rd!=0)
	{
	for (i= 0; i < 20; ++i)
	{
		Rreg[rd][32+i] = imm[i];
	}
	i = 0;
	while(i < 12)
	{
		Rreg[rd][63-i] = 0;
		i++;
	}

	for(i = 0; i < 32; i++)
	{
		Rreg[rd][i] = Rreg[rd][32];
	}
	}
	std::cout << "LUI TAKEN" <<"\n";
	fileprintreg();
	//printreg(rd);
    
}
