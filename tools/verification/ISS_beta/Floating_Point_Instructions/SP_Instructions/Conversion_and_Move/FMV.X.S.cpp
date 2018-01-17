#include<stdio.h>
#include<sstream>
#include<iostream>
#include<string>


void fmvxs(int rd, int rs1,int rs2, std::string rm)
{
for(int i= 0; i < 64 ; ++i)
Rreg[rd][i] = Freg[rs1][i];	
}
