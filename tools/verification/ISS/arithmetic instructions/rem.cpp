#include<stdio.h>
#include<math.h>

void rem(int rd, int rs1, int rs2)
{
int64_t numrs1;
int64_t numrs2;
int64_t numrd;
numrs1 = bin2dec(Rreg[rs1]);
numrs2 = bin2dec(Rreg[rs2]);

std::cout << "REM TAKEN\n";

if(numrs2 == 0) {
numrd = numrs1;
std::cout << "DIVISION BY ZERO EXCEPTION\n";
}
else if (numrs2 == -1 && numrs1 == -pow(2,63)) {
numrd = 0;
std::cout << "OVERFLOW EXCEPTION\n";
}
else
numrd = numrs1 % numrs2;

dec2bin(Rreg[rd],numrd);
}
