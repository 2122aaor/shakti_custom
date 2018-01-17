#include<iostream>

using namespace std;

void str2bin(int *bin,string str)
{
	int l = str.size();
	int i;
	for (i = 0; i < l; ++i)
	{
		bin[i] = (int)str[i] - 48;		
	}
}

int main()
{
string a = "00101000";
int bin[20];
str2bin(bin,a);
for(int i=0;i<a.size();++i)
cout << bin[i];
return 0;
}
