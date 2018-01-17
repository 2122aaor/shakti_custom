//Program for the SRA opcode
//SRA is an arithmetic right shift (the original sign bit is copied into the vacated upper bits).
#include <stdio.h>

void sra(int rd, int rs1, int rs2)
{
	int shift=0, temp[64];
	int i=0;
	
	for(i=0; i<6; i++)
	{
		shift += pow(2,i)*Rreg[rs2][63-i];
	}
	
	for (i = 0; i < shift; ++i)
	{
		temp[i] = Rreg[rs1][0];
	}
	i = 0;
	while(i < 64-shift)
	{
		temp[i+shift] = Rreg[rs1][i];
		i++;
	}
	if(rd!=0)
	{
	for( i = 0; i < 64; ++i)
	    Rreg[rd][i] = temp[i];
	}
	std::cout << "SRA TAKEN\n";
	/*printf("SRA\n");
	printreg(rs1);
	printreg(rs2);
	printreg(rd);*/
}
