#include <limits.h>
#include <iostream>
#include <math.h>
#include <bitset>
 
int Binary2Hex32( std::string Binary )
{
    std::bitset<32> set(Binary);      
    int hex = set.to_ulong();
     
    return hex;
}

int64_t Binary2Hex64( std::string Binary )
{
    std::bitset<64> set(Binary);      
    int64_t hex = set.to_ulong();
     
    return hex;
}
 
float GetFloat32( std::string Binary )
{
    int HexNumber = Binary2Hex32( Binary );
 
    bool negative  =   HexNumber & 0x80000000;
    int  exponent  =   (HexNumber & 0x7f800000) >> 23;    
    int sign = negative ? -1 : 1;
 
    exponent -= 127;
    int power = -1;
    float total = 0.0;
    for ( int i = 0; i < 23; i++ )
    {
        int c = Binary[ i + 9 ] - '0';
        total += (float) c * (float) pow( 2.0, power );
        power--;
    }
    total += 1.0;
 
    float value = sign * (float) pow( 2.0, exponent ) * total;
 
    return value;
}


double GetFloat64( std::string Binary )
{
    int64_t HexNumber = Binary2Hex64( Binary );
 
    bool negative  =   HexNumber & 0x8000000000000000;
    int64_t  exponent  =   (HexNumber & 0x7ff0000000000000) >> 52;    
    int sign = negative ? -1 : 1;
 
    exponent -= 1023;
    int power = -1;
    double total = 0.0;
    for ( int i = 0; i < 52; i++ )
    {
        int c = Binary[ i + 12 ] - '0';
        total += (double) c * (double) pow( 2.0, power );
        power--;
    }
    total += 1.0;
 
    double value = sign * (double) pow( 2.0, exponent ) * total;
 
    return value;
}

std::string GetBinary32( float value )
{
    union
    {
         float input;  
         int   output;
    }    data;
 
    data.input = value;
 
    std::bitset<sizeof(float) * CHAR_BIT>   bits(data.output);
 
    std::string mystring = bits.to_string<char, 
                                          std::char_traits<char>,
                                          std::allocator<char> >();
 
    return mystring;
}

std::string GetBinary64( double value )
{
    union
    {
         double input;  
         int64_t output;
    }    data;
 
    data.input = value;
 
    std::bitset<sizeof(double) * CHAR_BIT>   bits(data.output);
 
    std::string mystring = bits.to_string<char, 
                                          std::char_traits<char>,
                                          std::allocator<char> >();
 
    return mystring;
}

/* 
int main()
{
    std::string str = GetBinary32( (float) 19.566656565654 );
    std::string str1 = GetBinary64( (double) 19.566656565654);
    std::cout << "Binary equivalent of 19.5:" << std::endl;
    std::cout << str << std::endl << std::endl;
    std::cout << "Binary equivalent of Double 19.5:" << std::endl;
    std::cout << str1 << std::endl << std::endl;
    float f = GetFloat32(str);
    double g = GetFloat64(str1);
    std::cout << "Decimal equivalent of " << str << ":" << std::endl;
    std::cout << f << std::endl;
    std::cout << "Decimal equivalent of" << str1 << ":" << std::endl;
    std::cout << g << std::endl;
    return 0;
} 
*/
