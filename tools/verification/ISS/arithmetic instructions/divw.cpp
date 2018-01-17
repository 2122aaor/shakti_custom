#include<stdio.h>
#include<math.h>

void divw(int rd, int rs1, int rs2)
{
int32_t numrs1;
int32_t numrs2;
int32_t numrd;
numrs1 = bin2dec32(Rreg[rs1]);
numrs2 = bin2dec32(Rreg[rs2]);

if(numrs2 == 0) {
numrd = -1;
std::cout << "DIVISION BY ZERO EXCEPTION\n";
}
else if (numrs2 == -1 && numrs1 == -pow(2,63)) {
numrd = numrs1;
std::cout << "OVERFLOW EXCEPTION\n";
}
else
numrd = numrs1 / numrs2;

std::cout << "DIVW TAKEN\n";
dec2binsg32(Rreg[rd],numrd);
}
