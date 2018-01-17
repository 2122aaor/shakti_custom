//Program for and opcode
//AND, OR, and XOR perform bitwise logical operations.
#include <stdio.h>

void And(int rd, int rs1, int rs2)
{
	int i = 0;
	if(rd!=0)
	{
	for(i=0; i<64; i++)
	{
		Rreg[rd][i] = bit_and(Rreg[rs2][i],Rreg[rs1][i]);
	}
	}
	std::cout << "AND TAKEN\n";
	/*printf("And\n");
	printreg(rs1);
	printreg(rs2);
	printreg(rd);*/
}
