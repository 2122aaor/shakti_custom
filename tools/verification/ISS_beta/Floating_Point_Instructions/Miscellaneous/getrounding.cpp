#include<cstdio>
#include<fenv.h>
#pragma STDC FENV_ACCESS ON


void getrounding(std::string rm)
{
if(rm == "000")
fesetround(FE_TONEAREST);
else if(rm == "001")
fesetround(FE_TOWARDZERO);
else if(rm == "010")
fesetround(FE_DOWNWARD);
else if(rm == "011")
fesetround(FE_UPWARD);

}

