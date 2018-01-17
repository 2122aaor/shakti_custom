//Program contains functions to convert binary to decimal and vice versa using signed notation
#include <stdio.h>
#include <math.h>

int64_t bin2dec(int r[64])
{
    //int r[32] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,0,0,0};
    int64_t num = 0;
    int i = 0;
    int temp = 0;
    int64_t temppow = 0;
    if(r[0] == 0)
    {
        for(i = 1; i<64; i++)
        {
            temppow = r[i]*(int64_t)pow(2,63-i);
            num = num + temppow;
        }
    }
    else
    {
        for(i = 0; i<64; i++)
        {
            if(r[i] == 0)
                temp = 1;
            else
                temp = 0;
            num = num + temp*(int64_t)pow(2,63-i);
        }
        num = -1*(num + 1);
    }
    return num;
    //printf("%d", num);     
}
//This functions stores the binary format of the number in the register passed as the argument
void dec2bin(int r[64],int64_t num)
{
	int i=0;
	if (num >= 0)
	{
		r[0] = 0;
		for (i = 63; i > 0; --i)
		{
			r[i] = num % 2;
			num = num / 2;
		}
	}
	else
	{
		r[0] = 1;
		num++;
		for (i = 63; i > 0; --i)
		{
			if(num%2 == 0)
				r[i] = 1;
			else
				r[i] = 0;
			num = num/2;
		}
	}
	/*for (i = 0; i < 32; ++i)
	{
		printf("%d",r[i]);
	}*/	
}

//This functions stores sign extension of 32bit number to 64bit into a register
void dec2binsg32(int r[64],int num)
{
	int i=0;
	if (num >= 0)
	{
		r[32] = 0;
		for (i = 63; i > 32; --i)
		{
			r[i] = num % 2;
			num = num / 2;
		}
	}
	else
	{
		r[32] = 1;
		num++;
		for (i = 63; i > 32; --i)
		{
			if(num%2 == 0)
				r[i] = 1;
			else
				r[i] = 0;
			num = num/2;
		}
	}
	for(i = 0; i < 32; i ++)
	{
		r[i] = r[32];
	}
	/*for (i = 0; i < 32; ++i)
	{
		printf("%d",r[i]);
	}*/	
}

/*returns the signed value of lower 32 bits of a 64bit register*/
int bin2dec32(int r[64])
{
	//int r[32] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,0,0,0};
	int num = 0;
	int i = 0;
	int temp = 0;
	if(r[32] == 0)
	{
		for(i = 32; i<64; i++)
		{
			num = num + r[i]*pow(2,63-i);
		}
	}
	else
	{
		for(i = 32; i<64; i++)
		{
			if(r[i] == 0)
				temp = 1;
			else
				temp = 0;
			num = num + temp*pow(2,63-i);
		}
		num = -1*(num + 1);
	}
	return num;
	//printf("%d", num);	
}

/*This function takes a decimal integer and converts into binary of 128bits and stores the lower or higher 64 bits
into a 64 bit register depending on the opt char variable is 'L' or 'H' respectively. By default its low*/
void dec2bin128(char opt, int r[64], int128_t num)
{
	int i=0;
	if(num >= 0)
	{
		for(i = 63; i>=0; --i)
		{
			if(num%2 == 0)
				r[i] = 0;
			else
				r[i] = 1;
			num = num / 2;
		}
		if(opt == 'H')
		{
			r[0] = 0;
			for (i = 63; i > 0; --i)
			{
				if(num%2 == 0)
					r[i] = 0;
				else
					r[i] = 1;
				num = num / 2;
			}
		}
	}
	else
	{
		num++;
		for (i = 63; i >=0; --i)
		{
			if(num%2 == 0)
				r[i] = 1;
			else
				r[i] = 0;
			num = num/2;
		}
		if(opt == 'H')
		{
			r[0] = 1;
			for (i = 63; i > 0; --i)
			{
				if(num%2 == 0)
					r[i] = 1;
				else
					r[i] = 0;
				num = num/2;
			}
		}
	}
}

void udec2binwr(int64_t num, int *r)
{
	int i=0;
	{
		for (i = 63; i >= 0; i--)
		{
			if(num % 2 == 0)
				r[i] = 0;
			else
				r[i] = 1;
			num = num / 2;
		}
	}
	
	//for (i = 0; i < 64; ++i)
	//{
	//	printf("%d",r[i]);
	//}	
}
