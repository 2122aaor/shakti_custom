#include <stdio.h>

void ldmem2reg64bit(int64_t memloc , int rd, int wh_byte[]);

void ld (int rd , int rs1 , int * imm){

	//printf("check1\n");
	int imm64[64], memloc_addr[64];
	int byte_addr[3];
	int i = 0;
	for (i= 0; i < 12; ++i)
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
	int64_t rs1value = ubin2dec(Rreg[rs1]);

	int64_t memlocation = immvalue + rs1value ;

	udec2binwr(memlocation, memloc_addr);
	std::cout << "LD TAKEN\n";
	for(i = 2; i >= 0; i--) 
	    byte_addr[i] = memloc_addr[i+61];

	for(i = 60; i >= 0; i--)
	    memloc_addr[i+3] = memloc_addr[i]; 

	for(i = 2; i >= 0; i--)
	    memloc_addr[i] = 0;

	int64_t finalmemlocation = memlocation/8;


	ldmem2reg64bit(finalmemlocation, rd, byte_addr);

}

void ldmem2reg64bit(int64_t memloc , int rd, int wh_byte[]){
	int i =0 ;
	/*while (i<32){
		printf("%d",Rreg[rd][i]);
		i++;
	}
	printf("\n");*/
	bool* currmem;
	currmem = Mem(memloc);
	std::cout << "MemLoc : " << memloc << "\n";
	for(int m = 0 ; m < 64 ; ++m)
	{
	 std::cout << currmem[m];
	}
	std::cout << "\n";

	for (i= 0; i < 64; ++i)
	{
		Rreg[rd][63-i] = currmem[63-i];
	}

	fprintf(memout, "LD\t\t");
	fileprintmem(rd, memloc, currmem);
	/*while (i<32){
		printf("%d",Rreg[rd][i]);
		i++;
	}
	printf("\n");*/
}
