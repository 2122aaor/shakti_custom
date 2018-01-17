#include <stdio.h>


void fileprintreg()
{
	int i =0;
	int n=0;
	fprintf(output, "PC = %ld\n", (PC-1)*4 );
	while(i<32)
	{
		fprintf(output,"REG %d\t", i);
		int j=0;
		while(j<64)
		{
			n = 0;
			for (int k = 3; k >= 0; k--)
			{
				n += Rreg[i][j] * pow(2,k);
				j++;
				
			}
			fprintf(output,"%c",binhex(n));
		}
		fprintf(output,"\n");
		i++;
	}
	fprintf(output,"\n");
}
