#include <stdio.h>

void fileprintmem(int reg, uint64_t mloc, bool* mval)
{
	int j=0;
	int n;
	char a[17];
	a[16] = '\0';

	fprintf(memout, "%d\t\t0x",reg);

	for (int i =15; i >= 0; --i)
	{
		n = mloc%16;
		mloc = mloc/16;
		a[i] = binhex(n);
	}
	fprintf(memout, "%s\t0x", a);

	while(j<64)
	{
		n = 0;
		for (int k = 3; k >= 0; k--)
		{
			n += mval[j] * pow(2,k);
			j++;
		}
		fprintf(memout,"%c",binhex(n));
	}

	fprintf(memout, "\tPC=%ld\n", PC);
}