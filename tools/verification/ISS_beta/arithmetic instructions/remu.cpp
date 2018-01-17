#include<stdio.h>
#include<math.h>

void remu(int rd, int rs1, int rs2)
{
uint64_t numrs1;
uint64_t numrs2;
uint64_t numrd;
numrs1 = bin2dec(Rreg[rs1]);
numrs2 = bin2dec(Rreg[rs2]);

std::cout << "REMU TAKEN\n";

if(numrs2 == 0) {
numrd = numrs1;
std::cout << "DIVISION BY ZERO EXCEPTION\n";
}
else
numrd = numrs1 % numrs2;

dec2bin(Rreg[rd],numrd);
}
