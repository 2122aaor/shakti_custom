#include<stdio.h>
#include <sstream>
#include<iostream>
#include<math.h>

void fsqrtd(int rd, int rs1 , int rs2, std::string rm)
{
  std::stringstream out1,out2,out3;
   for(int i=0;i<64;++i)
	{
	out1 << Freg[rs1][i];
	//out2 << Freg[rs2][i];
	}
   std::string operand1(out1.str());
   //std::string operand2(out2.str());
   double a1 = GetFloat64(operand1);
   //float a2 = GetFloat32(operand2);
   getrounding(rm);
   double a2 = sqrt(a1);
   
   std::string operand3 = GetBinary64(a2);
	for(int i = 0 ; i < 64 ; ++i)
         Freg[rd][i] = operand3[i]-48;
    	
}
