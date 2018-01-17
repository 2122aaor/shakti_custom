#include <stdio.h>
#include<iostream>
#include<string>

void bne(int rs1 , int rs2 , int * imm)
{

	int imm64[64];
	int64_t numrs1,numrs2,numimm64;
	int i = 0;
	int PC1 = PC;
	
	numrs1 = bin2dec(Rreg[rs1]);
	numrs2 = bin2dec(Rreg[rs2]);
	//printf("%d  %d\n", numrs1, numrs2);
	fileprintreg();
	if(numrs1 != numrs2)
	{
		imm64[63] = 0;
		for (i= 0; i < 12; ++i)
		{
			imm64[62-i] = imm[11-i];
		}
		i = 0;
		while(i < 51)
		{
			imm64[i] = imm[0];
			i++;
		}
		std::cout << "BNE TAKEN : " << "NUMrs1: " << numrs1 << " NUMrs2: " << numrs2 << "\n";
		int64_t decre = bin2dec(imm64)/4 ;
		i=1;
		if(decre < 0)
		{
			if(decre*(-1) < (PC))
			{
			PC += decre-1;
			std::cout << "REVERSE BRANCH TAKEN" << "\n";
			int j = 1;
			fclose(assins);
			assins = fopen("input.hex","r");
			while(j <= PC) {
			fgets(line,sizeof(line),assins);
			j++;
			    }
			}
			else 
			{
			std::cout << "ILLEGAL BNE JUMP - CHECK CODE";
			exit(1);
			} 
		}
		else if(decre > 0)
		{
			PC += decre-1;	
			std::cout << "FORWARD BRANCH TAKEN \n";
			std::cout << PC << "\n";
			while(i<decre)
			{
				PC1++;
				//std::cout << PC1 << "\n";
				fgets(line,sizeof(line),assins);
				addq(line,PC1,data);
				if(PC1 > 1024)
				{
					delq(data);
				}
				i++;
			}
		}
		
	}
	else 
	std::cout << "BNE NOT TAKEN ";
}
