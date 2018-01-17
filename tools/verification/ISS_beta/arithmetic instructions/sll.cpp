//Program for the SLL opcode
//SLL is a logical left shift (zeros are shifted into the lower bits)
#include <stdio.h>

void sll(int rd, int rs1, int rs2)
{
	int shift=0, temp[64];
	int i=0;
	
	for(i=0; i<6; i++)
	{
		shift += pow(2,i)*Rreg[rs2][63-i];
	}
	
	for (i = 0; i < shift; ++i)
	{
		temp[63-i] = 0;
	}
	i = 0;
	while(i < 64-shift)
	{
		temp[i] = Rreg[rs1][i+shift];
		i++;
	}
	if(rd!=0)
	{
	for( i = 0; i < 64; ++i)
	    Rreg[rd][i] = temp[i];
	}
	std::cout << "SLL TAKEN\n";
	/*printf("SLL\n");
	printreg(rs1);
	printreg(rs2);
	printreg(rd);*/
}
