#include<stdio.h>
#include<sstream>
#include<iostream>
#include<string>

void fsgnjs(int rd, int rs1, int rs2,std::string rm)
{
  for(int i = 31; i < 64 ; ++i)
    Freg[rs1][i]=Freg[rs2][31];
  for(int i = 0 ; i < 64 ; ++i)
    Freg[rd][i] = Freg[rs1][i];
}
