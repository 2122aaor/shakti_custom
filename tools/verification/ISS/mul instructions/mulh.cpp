//Program for the MULH opcode
//Program to multiply two 64 bit signed numbers and store the higher 64 bits of the result in destination.
#include <stdio.h>

void mulh(int rd, int rs1, int rs2)
{
	int64_t numrs1;
	int64_t numrs2;
	int128_t numrd;

	numrs1 = bin2dec(Rreg[rs1]);
	numrs2 = bin2dec(Rreg[rs2]);
	numrd = numrs2 * numrs1;

	/*printf("%d\n",numrs1 );
	printf("%d\n",numrs2 );
	printf("%ld\n",numrd );*/

	dec2bin128('H',Rreg[rd],numrd);

	/*printf("MULH\n");
	printreg(rs1);
	printreg(rs2);
	printreg(rd);*/
}