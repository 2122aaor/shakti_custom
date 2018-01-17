//Program for the MULH opcode
//Program to multiply two 32 bit signed numbers and store the higher 32 bits of the result in destination.
#include <stdio.h>

void mulh(int rd, int rs1, int rs2)
{
	long int numrs1;
	long int numrs2;
	long int numrd;

	numrs1 = bin2dec(Rreg[rs1]);
	numrs2 = bin2dec(Rreg[rs2]);
	numrd = numrs2 * numrs1;

	/*printf("%d\n",numrs1 );
	printf("%d\n",numrs2 );
	printf("%ld\n",numrd );*/
	if(rd!=0)
	dec2bin64('H',Rreg[rd],numrd);

	/*printf("MULH\n");
	printreg(rs1);
	printreg(rs2);
	printreg(rd);*/
}
