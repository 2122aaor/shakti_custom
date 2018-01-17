//Program for the MULHU opcode
//Program to multiply two 64 bit unsigned numbers and store the higher 64 bits of the result in destination.
#include <stdio.h>
#include <stdint.h>

void mulhu(int rd, int rs1, int rs2)
{
	uint64_t numrs1;
	uint64_t numrs2;
	uint128_t numrd;

	numrs1 = ubin2dec(Rreg[rs1]);
	numrs2 = ubin2dec(Rreg[rs2]);
	numrd = numrs2 * numrs1;

	/*printf("%lu\n",numrs1 );
	printf("%lu\n",numrs2 );
	printf("%lu\n",numrd );*/

	udec2bin128('H',Rreg[rd],numrd);
	
	/*printf("MULHU\n");
	printreg(rs1);
	printreg(rs2);
	printreg(rd);*/
}