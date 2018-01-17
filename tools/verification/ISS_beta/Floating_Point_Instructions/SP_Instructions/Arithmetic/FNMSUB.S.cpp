#include<stdio.h>
#include <sstream>
#include<iostream>

void fnmsubs(int rd, int rs1 , int rs2,int rs3,std::string rm)
{
  std::stringstream out1,out2,out3,out4;
   for(int i=32;i<64;++i)
	{
	out1 << Freg[rs1][i];
	out2 << Freg[rs2][i];
	out3 << Freg[rs3][i];
	}
   std::string operand1(out1.str());
   std::string operand2(out2.str());
   std::string operand3(out3.str());
   float a1 = GetFloat32(operand1);
   float a2 = GetFloat32(operand2);
   float a3 = GetFloat32(operand3);
   getrounding(rm);
   float a4 = -(a1*a2-a3);
   std::string operand4 = GetBinary32(a4);
	for(int i = 0 ; i < 32 ; ++i)
	  Freg[rd][i+32] = operand3[i]-48;
	for(int i = 0 ; i < 32 ; ++i)
	  Freg[rd][i] = Freg[rd][32];
    	
}
