#include<iostream>
#include<fstream>
#include<string>

using namespace std;

int main()
{
ifstream infile("dhrystone.riscv.hex");
ofstream outfile("input.hex");
string get_line;
while(getline(infile,get_line)) {
outfile << get_line.substr(24,8) << "\n" << get_line.substr(16,8) << "\n" << get_line.substr(8,8) << "\n" << get_line.substr(0,8) << "\n" ;
}

return 0;
}
