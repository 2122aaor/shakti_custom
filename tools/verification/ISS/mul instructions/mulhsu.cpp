//Program for the MULHSU opcode
//Program to multiply one 64 bit signed number to a 64 bit unsigned number and store the higher 64 bits of the result in destination.
#include <stdio.h>
#include <stdint.h>

void mulhsu(int rd, int rs1, int rs2)
{
	int64_t numrs1;
	uint64_t numrs2;
	int128_t numrd;

	numrs1 = bin2dec(Rreg[rs1]);
	numrs2 = ubin2dec(Rreg[rs2]);
	numrd = numrs2 * numrs1;

	/*printf("%ld\n",numrs1 );
	printf("%lu\n",numrs2 );
	printf("%ld\n",numrd );*/

	dec2bin128('H',Rreg[rd],numrd);
	
	/*printf("MULHSU\n");
	printreg(rs1);
	printreg(rs2);
	printreg(rd);*/
}