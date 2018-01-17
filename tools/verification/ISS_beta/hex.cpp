#include<iostream>
#include <boost/multiprecision/cpp_int.hpp>
#include<sstream>
#include<iomanip>
#include<string>
#include<fstream>
using namespace std;
using namespace boost::multiprecision;

int main()
{
ifstream infile("store_data.txt");
ofstream outfile("store_data.hex");
int128_t a;
while(infile >> a){
stringstream s;
s << hex << a;
string b(s.str());
outfile << b << endl;
}

return 0;
}
