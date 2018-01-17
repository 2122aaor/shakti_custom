/*This program is used to print the value in a register to the command line
by taking the integer assigned to it in Rregdict.c as the input */

#include <stdio.h>

void printreg(int Rindex)
{
	int i;
	for (i = 0; i < 64; ++i)
	{
		printf("%d", Rreg[Rindex][i]);
	}
	printf("\n");
}