#include <stdio.h>
//TODO Change Function name 

void ldmem2reg32bitsigned(int64_t memloc , int rd, int wh_byte[]);

void flw (int rd , int rs1 , int * imm){

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
	int64_t rs1value = ubin2dec(Freg[rs1]);

	int64_t memlocation = immvalue + rs1value ;

	udec2binwr(memlocation, memloc_addr);

	for(i = 2; i >= 0; i--) 
	    byte_addr[i] = memloc_addr[i+61];

	for(i = 60; i >= 0; i--)
	    memloc_addr[i+3] = memloc_addr[i]; 

	for(i = 2; i >= 0; i--)
	    memloc_addr[i] = 0;

	int64_t finalmemlocation = memlocation/8;

	ldmem2reg32bitsigned(finalmemlocation, rd, byte_addr);

}

void ldmem2reg32bitsigned(int64_t memloc , int rd, int wh_byte[]){
	int i =0 ;
	bool* currmem;
	currmem = Mem(memloc);
	std::cout << "MemLoc : " << memloc << "\n";
	int j = (4*wh_byte[0])*8; 
	for (i= 0; i < 32; ++i)
	{
		Freg[rd][63-i] = currmem[63-j-i];
	}
	i = 0;
	while(i < 32)
	{
		Freg[rd][i] = Freg[rd][32];
		i++;
	}

	fprintf(fmemout, "FLW\t\t");
	floating_fileprintmem(rd, memloc, currmem);
	std::cout << "FLW TAKEN\n";
}
