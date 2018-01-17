//Program for ADDI opcode
//ADDI adds the sign-extended 12-bit immediate to register rs1 and places the value in rd.
#include <stdio.h>

void addi(int rd,int rs1, int *imm)
{
	//int rd[32];
	//int rs1[32] = {0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,1,1,1,0,1,1,1,0,1,1,0};
	//int imm[12] = {1,1,1,0,1,0,1,0,0,1,1,1};
	int imm64[64];
	int64_t numrs1;
	int64_t numimm64;
	int64_t numrd;
	std::cout << "RS1 : " << rs1 << " RD : " << rd  << "\n";
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
	//std::cout << "ADDI TAKEN, PC: " << PC << "\n";
	numrd = numimm64 + numrs1;
	std::cout << "NUMRD: " << numrd;
	dec2bin(Rreg[rd],numrd);
	std::cout << "ADDI TAKEN\n";
	/*printf("Addi\n");
	printreg(rs1);*/
	//printf("%d\n", numrd);
}
