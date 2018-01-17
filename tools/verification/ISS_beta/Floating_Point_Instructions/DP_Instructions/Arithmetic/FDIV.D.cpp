#include<stdio.h>
#include <sstream>
#include<iostream>

// TODO Incorporate Overflow Exceptions and Divide by Zero Conditions.

void fdivd(int rd, int rs1 , int rs2, std::string rm)
{
  std::stringstream out1,out2,out3;
   for(int i=0;i<64;++i)
	{
	out1 << Freg[rs1][i];
	out2 << Freg[rs2][i];
	}
   std::string operand1(out1.str());
   std::string operand2(out2.str());
   double a1 = GetFloat64(operand1);
   double a2 = GetFloat64(operand2);
   getrounding(rm);
   double a3 = a1 / a2 ; 
   std::cout << a3;
   std::string operand3 = GetBinary64(a3);
	for(int i = 0 ; i < 64 ; ++i)
	  Freg[rd][i] = operand3[i]-48;
    	
}

