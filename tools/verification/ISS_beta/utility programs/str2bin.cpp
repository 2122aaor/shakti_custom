//Converts the immediates string input to Binary (int array) for variable lengths
#include <stdio.h>
#include <string.h>

void str2bin(int *bin,char *str)
{
	//int bin[12];
	//char str[12] = "000111000110";
	int l = strlen(str);
	//printf("str=%s", str);
	//printf("l=%d\n", l);
	int i;
	for (i = 0; i < l; ++i)
	{
		bin[i] = (int)str[i] - 48;
		//printf("%d",bin[i] );
	}
}