#include<stdio.h>
#include<math.h>

void divu(int rd, int rs1, int rs2)
{
uint64_t numrs1;
uint64_t numrs2;
uint64_t numrd;
numrs1 = bin2dec(Rreg[rs1]);
numrs2 = bin2dec(Rreg[rs2]);

if(numrs2 == 0) {
numrd = pow(2,64)-1;
std::cout << "DIVISION BY ZERO EXCEPTION\n";
}
else
numrd = numrs1 / numrs2;

std::cout << "DIVU TAKEN\n";
dec2bin(Rreg[rd],numrd);
}
