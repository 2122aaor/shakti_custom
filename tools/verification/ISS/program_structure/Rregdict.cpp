/*Program which associates each register to a number.
This is used in the execute program which sends the numbers as parameters to the functions
so that they can directly access the registers they need to operate with.*/
#include <stdio.h>
#include <string.h>

int getRreg(char *reg)
{
	if(strcmp(reg,"zero") == 0)
		return 0;
	else if(strcmp(reg,"ra") == 0)
		return 1;
	else if(strcmp(reg,"v0") == 0)
		return 2;
	else if(strcmp(reg,"v1") == 0)
		return 3;
	else if(strcmp(reg,"a0") == 0)
		return 4;
	else if(strcmp(reg,"a1") == 0)
		return 5;
	else if(strcmp(reg,"a2") == 0)
		return 6;
	else if(strcmp(reg,"a3") == 0)
		return 7;
	else if(strcmp(reg,"a4") == 0)
		return 8;
	else if(strcmp(reg,"a5") == 0)
		return 9;
	else if(strcmp(reg,"a6") == 0)
		return 10;
	else if(strcmp(reg,"a7") == 0)
		return 11;
	else if(strcmp(reg,"t0") == 0)
		return 12;
	else if(strcmp(reg,"t1") == 0)
		return 13;
	else if(strcmp(reg,"t2") == 0)
		return 14;
	else if(strcmp(reg,"t3") == 0)
		return 15;
	else if(strcmp(reg,"t4") == 0)
		return 16;
	else if(strcmp(reg,"t5") == 0)
		return 17;
	else if(strcmp(reg,"t6") == 0)
		return 18;
	else if(strcmp(reg,"t7") == 0)
		return 19;
	else if(strcmp(reg,"s0") == 0)
		return 20;
	else if(strcmp(reg,"s1") == 0)
		return 21;
	else if(strcmp(reg,"s2") == 0)
		return 22;
	else if(strcmp(reg,"s3") == 0)
		return 23;
	else if(strcmp(reg,"s4") == 0)
		return 24;
	else if(strcmp(reg,"s5") == 0)
		return 25;
	else if(strcmp(reg,"s6") == 0)
		return 26;
	else if(strcmp(reg,"s7") == 0)
		return 27;
	else if(strcmp(reg,"s8") == 0)
		return 28;
	else if(strcmp(reg,"s9") == 0)
		return 29;
	else if(strcmp(reg,"sp") == 0)
		return 30;
	else if(strcmp(reg,"tp") == 0)
		return 31;
}