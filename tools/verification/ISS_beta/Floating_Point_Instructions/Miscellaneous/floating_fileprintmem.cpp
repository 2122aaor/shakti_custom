#include <stdio.h>

void floating_fileprintmem(int reg, uint64_t mloc, bool* mval)
{
	int j=0;
	int n;
	char a[17];
	a[16] = '\0';

	fprintf(fmemout, "%d\t\t0x",reg);

	for (int i =15; i >= 0; --i)
	{
		n = mloc%16;
		mloc = mloc/16;
		a[i] = binhex(n);
	}
	fprintf(fmemout, "%s\t0x", a);

	while(j<64)
	{
		n = 0;
		for (int k = 3; k >= 0; k--)
		{
			n += mval[j] * pow(2,k);
			j++;
		}
		fprintf(fmemout,"%c",binhex(n));
	}

	fprintf(fmemout, "\tPC=%ld\n", PC);
}
