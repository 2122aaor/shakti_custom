#include<iostream>
#include<stdio.h>
#include <sstream>
#include <fenv.h>
#include <string>
#pragma STDC FENV_ACCESS ON



void fcvtdul(int rd,int rs1, std::string rm)
{
const int Round = fegetround();
getrounding(rm);
std::stringstream out1;
for(int i=0; i < 64 ; ++i)
out1 << Rreg[rs1][i];
uint64_t a1;
out1 >> a1; 
double a2 = a1; 
std::string operand = GetBinary64(a2);
std::string::size_type sz;
unsigned int dec = stoi(operand,&sz);
dec2bin(Freg[rd],dec);
fesetround(Round);
}
