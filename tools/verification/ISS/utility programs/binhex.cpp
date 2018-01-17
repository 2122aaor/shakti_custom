#include <stdio.h>

char binhex(int n)
{
	switch(n)
	{
		case 0:
			return '0';
			break;
		case 1:
			return '1';
			break;
		case 2:
			return '2';
			break;
		case 3:
			return '3';
			break;
		case 4:
			return '4';
			break;
		case 5:
			return '5';
			break;
		case 6:
			return '6';
			break;
		case 7:
			return '7';
			break;
		case 8:
			return '8';
			break;
		case 9:
			return '9';
			break;
		case 10:
			return 'a';
			break;
		case 11:
			return 'b';
			break;
		case 12:
			return 'c';
			break;
		case 13:
			return 'd';
			break;
		case 14:
			return 'e';
			break;
		case 15:
			return 'f';
			break;
	}
}
