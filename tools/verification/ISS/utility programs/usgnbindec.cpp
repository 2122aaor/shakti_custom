//Program contains functions to convert binary to decimal and vice versa using unsigned notation
//Note: For unsigned numbers the value must be stored in long int datatype

#include <stdio.h>
#include <math.h>
#include <stdint.h>

uint64_t ubin2dec(int r[64])
{
	//int r[32] = {0,0,0,0,0,0,1,0,0,1,0,0,1,0,1,1,0,1,0,1,0,0,0,0,0,1,1,1,1,0,0,0};
	uint64_t num = 0;
	int i;
	for(i = 0; i<64; i++)
	{
		num = num + r[i]*(uint64_t)pow(2,63-i);
	}
	//printf("%ld", num);
	return num;
}

void udec2bin(int r[64],uint64_t num)
{
	/*int r[32];
	long int num = 38902903;*/
	int i=0;
	for (i = 63; i >= 0; --i)
	{
		r[i] = num % 2;
		num = num / 2;
	}
	/*for (i = 0; i < 32; ++i)
	{
		printf("%d",r[i]);
	}*/
}

/*This function takes a decimal unsigned integer and converts into binary of 64bits unsigned and stores the lower or higher 32 bits
into a 32 bit register depending on the opt char variable is 'L' or 'H' respectively. By default its low*/
void udec2bin128(char opt, int r[64], uint128_t num)
{
	int i=0;
	for (i = 63; i >= 0; --i)
	{
		if(num%2 == 0)
			r[i] = 0;
		else
			r[i] = 1;
		num = num / 2;
	}
	if(opt == 'H')
	{
		for (i = 63; i >= 0; --i)
		{
			if(num%2 == 0)
				r[i] = 0;
			else
				r[i] = 1;
			num = num / 2;
		}
	}
}