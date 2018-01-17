#include<stdio.h>
#include <sstream>
#include<iostream>

/*int Freg[3][64] = {{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}};
*/

void fsubs(int rd, int rs1 , int rs2, std::string rm)
{
  std::stringstream out1,out2,out3;
   for(int i=32;i<64;++i)
	{
	out1 << Freg[rs1][i];
	out2 << Freg[rs2][i];
	}
   std::string operand1(out1.str());
   std::string operand2(out2.str());
   float a1 = GetFloat32(operand1);
   float a2 = GetFloat32(operand2);
   float a3;
   getrounding(rm);
   a3 = a1 - a2 ;
   std::cout << a3;
   std::string operand3 = GetBinary32(a3);
	for(int i = 0 ; i < 32 ; ++i)
	  Freg[rd][i+32] = operand3[i]-48;
	for(int i = 0 ; i < 32 ; ++i)
	  Freg[rd][i] = Freg[rd][32];
	for(int i = 0 ; i < 64; ++i)
	  std:: cout << Freg[rd][i] ;
    	
}
/*
int main()
{
int rd = 0, rs1 = 1, rs2 = 2;
FSUB(rd,rs1,rs2,3);
return 0;
} */

