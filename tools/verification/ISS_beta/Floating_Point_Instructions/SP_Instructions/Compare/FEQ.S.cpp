#include<iostream>
#include<cmath>

void feqs(int rd,int rs1,int rs2, std::string rm)
{
std::stringstream out1,out2;
   for(int i=32;i<64;++i)
	{
	out1 << Freg[rs1][i];
	out2 << Freg[rs2][i];
	}
   std::string operand1(out1.str());
   std::string operand2(out2.str());
   float a1 = GetFloat32(operand1);
   float a2 = GetFloat32(operand2);
   int a3;
   if(isnan(a1)||isnan(a2))
	a3 = 0;
   else if(a1 == a2)
	a3 = 1;
   else 
	a3 = 0;

   dec2bin(Rreg[rd],a3);
 }
