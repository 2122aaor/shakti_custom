#include <stdio.h>

void auipc(int rd , int* imm)
{
	int imm64[64];
	long numrd;
	long numimm;
	int newPC;
	int i = 0;
	memset(imm64,0,sizeof(imm64));
        for(i=0;i<=31;++i)
	imm64[i]=imm[0];
	for(i=32;i<=51;++i)
	imm64[i]=imm[i-32];
	numimm = ubin2dec(imm64);
	std::cout << "AUIPC TAKEN\n";
	//printf("PC = %ld ",PC);
	newPC = (PC-1)*4;
	numrd = newPC + numimm;
	if(rd!=0)
	udec2bin(Rreg[rd],numrd);
	fileprintreg();
}
