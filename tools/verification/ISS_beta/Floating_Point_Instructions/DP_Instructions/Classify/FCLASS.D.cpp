#include<iostream>
#include<cmath>

void fclassd(int rd, int rs1,int rs2, std::string rm)
{
std::stringstream out1;
   for(int i=0;i<64;++i)
	out1 << Freg[rs1][i];
   std::string operand1(out1.str());
   double a1 = GetFloat64(operand1);
   double a2;
   if(std::fpclassify(a1)==FP_INFINITE && std::signbit(a1))
      a2 = 1;
   else if(std::fpclassify(a1)==FP_NORMAL && std::signbit(a1))
      a2 = 2;
   else if(std::fpclassify(a1)==FP_SUBNORMAL && std::signbit(a1))
      a2 = 4;
   else if(std::fpclassify(a1)==FP_ZERO && std::signbit(a1))
      a2 = 8;
   else if(std::fpclassify(a1)==FP_ZERO && !std::signbit(a1))
      a2 = 16;
   else if(std::fpclassify(a1)==FP_SUBNORMAL && !std::signbit(a1))
      a2 = 32;
   else if(std::fpclassify(a1)==FP_NORMAL && !std::signbit(a1))
      a2 = 64;
   else if(std::fpclassify(a1)==FP_INFINITE && !std::signbit(a1))
      a2 = 128;
   else if(std::isnan(a1))
      a2 = 256;
   else
      a2 = 512;
   dec2bin(Rreg[rd],a2);
} 
