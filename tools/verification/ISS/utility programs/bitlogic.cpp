//Program contains functions to perform bitwise logical operations
#include <stdio.h>
#include <cstdlib>
#include "math.h"


int bit_xor(int x, int y)
{
	int z;
	if (x == y)
		z = 0;
	else
		z = 1;
	return z;
}

int bit_and(int x, int y)
{
	int z;
	if (x == 1 && y == 1)
		z = 1;
	else
		z = 0;
	return z;
}

int bit_or(int x, int y)
{
	int z;
	if (x == 1 || y == 1)
		z = 1;
	else
		z = 0;
	return z;
}