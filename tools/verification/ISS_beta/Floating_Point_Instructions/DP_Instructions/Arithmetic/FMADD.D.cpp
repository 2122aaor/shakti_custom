#include<stdio.h>
#include <sstream>
#include<iostream>

void fmaddd(int rd, int rs1 , int rs2,int rs3,std::string rm)
{
  std::stringstream out1,out2,out3,out4;
   for(int i=0;i<64;++i)
	{
	out1 << Freg[rs1][i];
	out2 << Freg[rs2][i];
	out3 << Freg[rs3][i];
	}
   std::string operand1(out1.str());
   std::string operand2(out2.str());
   std::string operand3(out3.str());
   double a1 = GetFloat64(operand1);
   double a2 = GetFloat64(operand2);
   double a3 = GetFloat64(operand3);
   getrounding(rm);
   double a4 = a1*a2+a3;
   std::string operand4 = GetBinary64(a4);
	for(int i = 0 ; i < 64 ; ++i)
	  Freg[rd][i] = operand4[i]-48;
    	
}

