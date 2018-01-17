/*Program which associates each register to a number.
This is used in the execute program which sends the numbers as parameters to the functions
so that they can directly access the registers they need to operate with.*/
#include <stdio.h>
#include <string.h>

char* getRreg(char *reg)
{
	if(strcmp(reg,"zero") == 0)
		return "00000";
	else if(strcmp(reg,"ra") == 0)
		return "00001";
	else if(strcmp(reg,"v0") == 0)
		return "00010";
	else if(strcmp(reg,"v1") == 0)
		return "00011";
	else if(strcmp(reg,"a0") == 0)
		return "00100";
	else if(strcmp(reg,"a1") == 0)
		return "00101";
	else if(strcmp(reg,"a2") == 0)
		return "00110";
	else if(strcmp(reg,"a3") == 0)
		return "00111";
	else if(strcmp(reg,"a4") == 0)
		return "01000";
	else if(strcmp(reg,"a5") == 0)
		return "01001";
	else if(strcmp(reg,"a6") == 0)
		return "01010";
	else if(strcmp(reg,"a7") == 0)
		return "01011";
	else if(strcmp(reg,"t0") == 0)
		return "01100";
	else if(strcmp(reg,"t1") == 0)
		return "01101";
	else if(strcmp(reg,"t2") == 0)
		return "01110";
	else if(strcmp(reg,"t3") == 0)
		return "01111";
	else if(strcmp(reg,"t4") == 0)
		return "10000";
	else if(strcmp(reg,"t5") == 0)
		return "10001";
	else if(strcmp(reg,"t6") == 0)
		return "10010";
	else if(strcmp(reg,"t7") == 0)
		return "10011";
	else if(strcmp(reg,"s0") == 0)
		return "10100";
	else if(strcmp(reg,"s1") == 0)
		return "10101";
	else if(strcmp(reg,"s2") == 0)
		return "10110";
	else if(strcmp(reg,"s3") == 0)
		return "10111";
	else if(strcmp(reg,"s4") == 0)
		return "11000";
	else if(strcmp(reg,"s5") == 0)
		return "11001";
	else if(strcmp(reg,"s6") == 0)
		return "11010";
	else if(strcmp(reg,"s7") == 0)
		return "11011";
	else if(strcmp(reg,"s8") == 0)
		return "11100";
	else if(strcmp(reg,"s9") == 0)
		return "11101";
	else if(strcmp(reg,"sp") == 0)
		return "11110";
	else if(strcmp(reg,"tp") == 0)
		return "11111";
}