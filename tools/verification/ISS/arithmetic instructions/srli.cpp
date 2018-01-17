//Program for SRLI opcode 
//SRLI is a logical right shift (zeros are shifted into the upper bits)
#include <stdio.h>

void srli(int rd,int rs1, int *shamt)
{
	int shamt64[64], temp[64];
	int shift;
	int i = 0;
	for (i= 0; i < 6; ++i)
	{
		shamt64[63-i] = shamt[5-i];
	}
	i = 0;
	while(i < 58)
	{
		shamt64[i] = 0;
		i++;
	}
	shift = (int)bin2dec(shamt64);
	for (i = 0; i < shift; ++i)
	{
		temp[i] = 0;
	}
	i = 0;
	while(i < 64-shift)
	{
		temp[i+shift] = Rreg[rs1][i];
		i++;
	}
	if(rd!=0)
	{
	for (i = 0; i < 64; ++i)
		Rreg[rd][i] = temp[i];
	}
	std::cout << "SRLI TAKEN\n";
	/*printf("\nshift = %d\n", shift);
	for (i = 0; i < 32; ++i)
	{
		printf("%d",Rreg[rs1][i]);
	}
	printf("\n");
	for (i = 0; i < 32; ++i)
	{
		printf("%d",Rreg[rd][i]);
	}*/
}
