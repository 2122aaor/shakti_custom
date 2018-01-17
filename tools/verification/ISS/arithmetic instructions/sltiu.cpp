//Program for SLTIU opcode 
/*SLTIU (set less than immediate unsigned) places the value 1 in register rd if register rs1 is less than the
sign-extended immediate when both are treated as unsigned numbers, else 0 is written to rd.
Note, SLTIU rd, rs1, 1 sets rd to 1 if rs1 equals zero, otherwise sets rd to 0 (assembler psuedo-op SEQZ rd, rs).
*/

#include <stdio.h>

void sltiu(int rd,int rs1, int *imm)
{
	//int rd[32];
	//int rs1[32] = {0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,1,1,1,0,1,1,1,0,1,1,0};
	//int imm[12] = {1,1,1,0,1,0,1,0,0,1,1,1};
	int imm64[64];
	uint64_t numrs1;
	uint64_t numimm64;
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
	
	numrs1 = ubin2dec(Rreg[rs1]);
	numimm64 = ubin2dec(imm64);
	
	if(numrs1 < numimm64)
		numrd = 1;
	else
		numrd = 0;
	if(rd!=0)
	dec2bin(Rreg[rd],numrd);
	std::cout << "SLTIU TAKEN\n";
	//dec2bin(Rreg[rd],numrd);
	//printreg(rd);
}
