//Program for SRLIW opcode 
//SRLIW is a logical right shift (zeros are shifted into the upper bits)
//Works only on lower 32 bit values
#include <stdio.h>

void srliw(int rd,int rs1, int *shamt)
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
	std::cout << "SRLIW TAKEN\n";
	printf("shift: %d",shift);
	for (i = 0; i < shift; ++i)
	{
		temp[32+i] = 0;
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
