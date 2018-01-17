#include<stdio.h>
#include<math.h>

void div(int rd, int rs1, int rs2)
{
int64_t numrs1;
int64_t numrs2;
int64_t numrd;
numrs1 = bin2dec(Rreg[rs1]);
numrs2 = bin2dec(Rreg[rs2]);

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

std::cout << "DIV TAKEN\n";
dec2bin(Rreg[rd],numrd);
}
