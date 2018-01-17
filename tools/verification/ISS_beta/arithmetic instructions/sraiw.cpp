//Program for SRAIW opcode 
//SRAIW is a logical right shift (the original sign bit is copied into the vacated upper bits)
//Works only on lower 32 bit values
#include <stdio.h>

void sraiw(int rd,int rs1, int *shamt)
{
	int shamt64[64], temp[64];
	int shift;
	int i = 0;
	for (i= 0; i < 5; ++i)
	{
		shamt64[63-i] = shamt[4-i];
	}
	i = 0;
	while(i < 59)
	{
		shamt64[i] = 0;
		i++;
	}
	shift = (int)bin2dec(shamt64);
	for (i = 0; i < shift; ++i)
	{
		temp[32+i] = Rreg[rs1][32];
	}
	i = 32;
	while(i < 64-shift)
	{
		temp[i+shift] = Rreg[rs1][i];
		i++;
	}

	for(i = 0; i < 32; i++)
	{
		temp[i] = temp[32];
	}
	if(rd!=0)
	{
	for( i = 0; i < 64; ++i)
	    Rreg[rd][i] = temp[i];
	}
	std::cout << "SRAIW TAKEN\n";
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
