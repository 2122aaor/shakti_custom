#include<stdio.h>
#include<sstream>
#include<iostream>
#include<string>

void fsgnjns(int rd, int rs1, int rs2,std::string rm)
{
  for(int i = 31; i < 64 ; ++i)
    {
    if(Freg[rs2][31] == 0)
    Freg[rs1][i]=1;
    else
    Freg[rs1][i]=0;
    }
  for(int i = 0 ; i < 64 ; ++i)
    Freg[rd][i] = Freg[rs1][i];
}
