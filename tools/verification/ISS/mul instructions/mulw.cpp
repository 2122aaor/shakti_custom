//Program for the MULW opcode
//Program to multiply lower 32 bits of two 64 bit signed numbers and store the lower 64 bits of the result in destination.
#include <stdio.h>

void mulw(int rd, int rs1, int rs2)
{
	int32_t numrs1;
	int32_t numrs2;
	int32_t numrd;

	numrs1 = bin2dec32(Rreg[rs1]);
	numrs2 = bin2dec32(Rreg[rs2]);
	numrd = numrs2 * numrs1;

	/*printf("%d\n",numrs1 );
	printf("%d\n",numrs2 );
	printf("%ld\n",numrd );*/

	dec2binsg32(Rreg[rd],numrd);
	
	/*printf("MUL\n");
	printreg(rs1);
	printreg(rs2);
	printreg(rd);*/
}