//Program for ORI opcode
/*ANDI, ORI, XORI are logical operations that perform bitwise AND, OR, and XOR on register rs1
and the sign-extended 12-bit immediate and place the result in rd.*/

#include <stdio.h>

void ori(int rd,int rs1, int *imm)
{
	//int rd[32];
	//int rs1[32] = {0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,1,1,1,0,1,1,1,0,1,1,0};
	//int imm[12] = {1,1,1,0,1,0,1,0,0,1,1,1};
	int i = 0;
	if(rd!=0)
	{
	for (i= 0; i < 12; ++i)
	{
		Rreg[rd][63-i] = bit_or(Rreg[rs1][63-i],imm[11-i]);
	}
	i = 0;
	while(i < 52)
	{
		Rreg[rd][i] = bit_or(Rreg[rs1][i],imm[0]);
		i++;
	}
	}
	std::cout << "ORI TAKEN\n";
	/*printf("\n");
	for (i = 0; i < 20; ++i)
	{
		printf("%d",imm[0]);
	}
	for (i = 0; i < 12; ++i)
	{
		printf("%d",imm[i]);
	}
	printf("\n");
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
