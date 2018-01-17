#include <stdio.h>

void ldmem2reg8bitsigned(int64_t memloc , int rd, int wh_byte[]);

void lb (int rd , int rs1 , int * imm){

	//printf("check1\n");
	int imm64[64], memloc_addr[64];
	int byte_addr[3];
	int i = 0;
	for (i= 0; i < 12; i=i+1)
	{
		imm64[63-i] = imm[11-i];
	}
	i = 0;
	while(i < 52)
	{
		imm64[i] = imm[0];
		i++;
	}
	
	int immvalue = bin2dec(imm64);
	long rs1value = ubin2dec(Rreg[rs1]);

	int64_t memlocation = immvalue + rs1value ;

	udec2binwr(memlocation, memloc_addr);

	for(i = 2; i >= 0; i--) 
	    byte_addr[i] = memloc_addr[i+61];

	for(i = 60; i >= 0; i--)
	    memloc_addr[i+3] = memloc_addr[i]; 

	for(i = 2; i >= 0; i--)
	    memloc_addr[i] = 0;
	int64_t finalmemlocation = memlocation/8;

	printf("%ld\n",finalmemlocation );
	ldmem2reg8bitsigned(finalmemlocation, rd, byte_addr);
	
}

void ldmem2reg8bitsigned(int64_t memloc , int rd, int wh_byte[])
{
	int i =0 ;
/*while (i<32){
		printf("%d",Rreg[rd][i]);
		i++;
	}
	printf("\n");*/

	//printf ("%ld\n",memloc);
	//printf("%d\n",rd );
	bool* currmem;
	currmem = Mem(memloc);
	std::cout << "MemLoc : " << memloc << "\n";
	int j = (4*wh_byte[0] + 2*wh_byte[1] + wh_byte[2])*8; 
	std::cout << "LB TAKEN\n";	
	for(int m = 0 ; m < 64 ; ++m)
	{
	 std::cout << currmem[m];
	}
	std::cout << "\n";

	for (i= 0; i < 8; ++i)
	{
		Rreg[rd][63-i] = currmem[63-j-i];
	}
	i = 0;
	//printf("checkp\n");
	while(i < 56)
	{
		Rreg[rd][i] = Rreg[rd][56];
		i++;
	}

	fprintf(memout, "LB\t\t");
	fileprintmem(rd, memloc, currmem);
	/*fprintf(memout, "PC = %ld\n", PC);
	fprintf(memout, "LB\t\t%d\t\t%ld\t\t\t",rd, memloc);
	for(i=0; i<32; i++)
	{
		fprintf(memout, "%d", currmem[i]);
	}
	fprintf(memout, "\n");*/
	/*while (i<32){
		printf("%d",Rreg[rd][i]);
		i++;
	}
	printf("\n");*/

}
