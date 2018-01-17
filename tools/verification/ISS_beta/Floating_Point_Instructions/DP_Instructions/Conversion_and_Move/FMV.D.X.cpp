#include<stdio.h>
#include<sstream>
#include<iostream>
#include<string>


void fmvdx(int rd, int rs1, std::string rm)
{
getrounding(rm);
for(int i= 0; i < 64 ; ++i)
Rreg[rs1][i] = Freg[rd][i];	
}
