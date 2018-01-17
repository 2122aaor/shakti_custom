#include<iostream>
#include<string>
#include<fstream>
#include<cstdio>
#include<bitset>

using namespace std;

int main()
	{

		string a, b, c, d, e,f,g,line;
		string::size_type size = 0;
		ofstream resultfile,resultfile1,resultfile2,resultfile3,resultfile4,resultfile5,resultfile6,resultfile7,resultfile8,resultfile9,resultfile10,resultfile11;
		ifstream infile("Add-Shift.txt");
		ifstream infile1("Add-Cancellation.txt");		
		ifstream infile2("Add-Cancellation-And-Subnorm-Result.txt");
		ifstream infile3("Add-Shift-And-Special-Significands.txt");
		ifstream infile4("Basic-Types-Inputs.txt");
		ifstream infile5("Basic-Types-Intermediate.txt");
		ifstream infile6("Compare-Different-Input-Field-Relations.txt");
		ifstream infile7("Corner-Rounding.txt");
		ifstream infile8("Divide-Divide-By-Zero-Exception.txt");		
		ifstream infile9("Hamming-Distance.txt");
		ifstream infile10("Input-Special-Significand.txt");
		ifstream infile11("MultiplyAdd-Cancellation.txt");
		ifstream infile12("MultiplyAdd-Cancellation-And-Subnorm-Result.txt");
		ifstream infile13("MultiplyAdd-Shift.txt");
		ifstream infile14("MultiplyAdd-Shift-And-Special-Significands.txt");
		ifstream infile15("MultiplyAdd-Special-Events-Inexact.txt");
		ifstream infile16("MultiplyAdd-Special-Events-Overflow.txt");
		ifstream infile17("MultiplyAdd-Special-Events-Underflow.txt");
		ifstream infile18("Overflow.txt");
		ifstream infile19("Rounding.txt");		
		ifstream infile20("Underflow.txt");
		ifstream infile21("Vicinity-Of-Rounding-Boundaries.txt");		
		resultfile.open("FAdd.txt");
		resultfile1.open("FSub.txt");
		resultfile2.open("FMul.txt");
		resultfile3.open("FDiv.txt");
		resultfile4.open("FSqrt.txt");
		resultfile5.open("FMAdd.txt");
		resultfile6.open("FMax.txt"); //= >c : maxnum
		resultfile7.open("FMin.txt"); //= <c : minnum
		resultfile8.open("FMaxmag.txt"); //=>a : maxmug
 
	while(getline(infile,line))	
	{
		string a = line.substr(0,4);
		if(a == "b32+")	
		resultfile << line << endl;
		else if(a == "b32-")
		resultfile1 << line << endl;
	}
	while(getline(infile1,line))	
	{
		string a = line.substr(0,4);
		if(a == "b32+")	
		resultfile << line << endl;		
		else if(a == "b32-")
		resultfile1 << line << endl;
	}
	while(getline(infile2,line))
	{
		string a = line.substr(0,4);
		if(a == "b32+")
		resultfile << line << endl;
		else if(a == "b32-")
		resultfile1 << line << endl;
	}
	while(getline(infile3,line))
	{
		string a = line.substr(0,4);
		if(a == "b32+")
		resultfile << line << endl;
		else if(a == "b32-")
		resultfile1 << line << endl;
	}
	while(getline(infile5,line))
	{
		string a = line.substr(0,5);
		if(a == "b32+ ")
		resultfile << line << endl;
		else if(a == "b32- ")
		resultfile1 << line << endl;		
		else if(a == "b32/ ")
		resultfile3 << line << endl;
		else if(a == "b32V ")
		resultfile4 << line << endl;	
		else if(a == "b32* ")
		resultfile2 << line << endl;
		else if(a == "b32*+")
		resultfile5 << line << endl;		
	}

	while(getline(infile6,line))	
	{
		string a = line.substr(0,5);
		if(a == "b32>C")	
		resultfile6 << line << endl;		
		if(a == "b32<C")	
		resultfile7 << line << endl;		
		else if(a == "b32>A")
		resultfile8 << line << endl;
	}
	while(getline(infile7,line))	
	{
		string a = line.substr(0,5);
		if(a == "b32/ ")
		resultfile3 << line << endl;
		else if(a == "b32V ")
		resultfile4 << line << endl;	
		else if(a == "b32* ")
		resultfile2 << line << endl;
		else if(a == "b32*+")
		resultfile5 << line << endl;
	}
	while(getline(infile8,line))	
	{
		string a = line.substr(0,4);
		if(a == "b32/ ")
		resultfile3 << line << endl;
	}
	while(getline(infile9,line))
	{
		string a = line.substr(0,5);
		if(a == "b32+ ")
		resultfile << line << endl;
		else if(a == "b32- ")
		resultfile1 << line << endl;		
		else if(a == "b32/ ")
		resultfile3 << line << endl;
		else if(a == "b32* ")
		resultfile2 << line << endl;
		else if(a == "b32*+")
		resultfile5 << line << endl;		
	}
	while(getline(infile10,line))
	{
		string a = line.substr(0,5);
		if(a == "b32/ ")
		resultfile3 << line << endl;
		else if(a == "b32* ")
		resultfile2 << line << endl;
		else if(a == "b32V ")
		resultfile4 << line << endl;		
	}

	while(getline(infile11,line))	
	{
		string a = line.substr(0,5);
		if(a == "b32*+")
		resultfile5 << line << endl;
	}
	
	while(getline(infile12,line))	
	{
		string a = line.substr(0,5);
		if(a == "b32*+")
		resultfile5 << line << endl;
	}	
	while(getline(infile13,line))	
	{
		string a = line.substr(0,5);
		if(a == "b32*+")
		resultfile5 << line << endl;
	}	
	while(getline(infile14,line))	
	{
		string a = line.substr(0,5);
		if(a == "b32*+")
		resultfile5 << line << endl;
	}		
	while(getline(infile15,line))	
	{
		string a = line.substr(0,5);
		if(a == "b32*+")
		resultfile5 << line << endl;
	}		
	while(getline(infile16,line))	
	{
		string a = line.substr(0,5);
		if(a == "b32*+")
		resultfile5 << line << endl;
	}		
	while(getline(infile17,line))	
	{
		string a = line.substr(0,5);
		if(a == "b32*+")
		resultfile5 << line << endl;
	}	
	
	while(getline(infile18,line))
	{
		string a = line.substr(0,5);
		if(a == "b32+ ")
		resultfile << line << endl;
		else if(a == "b32- ")
		resultfile1 << line << endl;		
		else if(a == "b32/ ")
		resultfile3 << line << endl;
		else if(a == "b32* ")
		resultfile2 << line << endl;
		else if(a == "b32*+")
		resultfile5 << line << endl;		
	}
	while(getline(infile19,line))
	{
		string a = line.substr(0,5);
		if(a == "b32+ ")
		resultfile << line << endl;
		else if(a == "b32- ")
		resultfile1 << line << endl;		
		else if(a == "b32/ ")
		resultfile3 << line << endl;
		else if(a == "b32V ")
		resultfile4 << line << endl;	
		else if(a == "b32* ")
		resultfile2 << line << endl;
		else if(a == "b32*+")
		resultfile5 << line << endl;		
	}
	while(getline(infile20,line))
	{
		string a = line.substr(0,5);
		if(a == "b32+ ")
		resultfile << line << endl;
		else if(a == "b32- ")
		resultfile1 << line << endl;		
		else if(a == "b32/ ")
		resultfile3 << line << endl;
		else if(a == "b32* ")
		resultfile2 << line << endl;
		else if(a == "b32*+")
		resultfile5 << line << endl;		
	}
	while(getline(infile21,line))
	{
		string a = line.substr(0,5);
		if(a == "b32+ ")
		resultfile << line << endl;
		else if(a == "b32- ")
		resultfile1 << line << endl;		
		else if(a == "b32/ ")
		resultfile3 << line << endl;
		else if(a == "b32* ")
		resultfile2 << line << endl;
		else if(a == "b32*+")
		resultfile5 << line << endl;		
	}

	return 0;
	}

