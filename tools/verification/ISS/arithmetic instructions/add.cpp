//Program for the ADD opcode
//Program to add two 64 bit signed numbers
#include <stdio.h>

void Add(int rd, int rs1, int rs2)
{
	int64_t numrs1;
	int64_t numrs2;
	int64_t numrd;
	
	numrs1 = bin2dec(Rreg[rs1]);
	numrs2 = bin2dec(Rreg[rs2]);
	numrd = numrs2 + numrs1;

	if(rd!=0)
	dec2bin(Rreg[rd],numrd);
	std::cout << "ADD TAKEN\n";
	//printf("%ld \n", numrd);
	//printf("%ld \n", numrs1);
	//printf("%ld \n", numrs2);
	//printf("%d \n", Rreg[rs2][0]);

	/*printf("ADD\n");
	printreg(rs1);
	printreg(rs2);
	printreg(rd);*/
}
