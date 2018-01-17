#include <stdio.h>

void printmem(uint64_t Mindex)
{
	int i;
	for (i = 0; i < 64; ++i)
	{
		printf("%d",Mem(Mindex)[i]);
	}
	printf("\n");
}