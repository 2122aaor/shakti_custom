//Program for ADDIW opcode
//ADDIW adds the sign-extended 12-bit immediate to register rs1 and places the lower 32 bits sign-extended to 64 in rd.
#include <stdio.h>

void addiw(int rd,int rs1, int *imm)
{
	//int rd[32];
	//int rs1[32] = {0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,1,1,1,0,1,1,1,0,1,1,0};
	//int imm[12] = {1,1,1,0,1,0,1,0,0,1,1,1};
	int imm64[64];
	int numrs1;
	int numimm64;
	int numrd;
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
	numrs1 = bin2dec32(Rreg[rs1]);
	numimm64 = bin2dec32(imm64);
	numrd = numimm64 + numrs1;
	if(rd!=0)
	dec2binsg32(Rreg[rd],numrd);
	std::cout << "ADDIW TAKEN\n";
	/*printf("Addi\n");
	printreg(rs1);*/
	//printf("%d\n", numrd);
}
