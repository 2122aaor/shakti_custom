#include<iostream>
#include<cstdio>

typedef struct				 
{
	int nan;
	int inf;
	int norm;
	int deno;
	int zero;
	int sign;
}Operand_type;

typedef struct
{
	int dz;
	int ine;
	int inv;
	int ovf;
	int unf;
	int none;
}Exception_type;

Exception_type get_FP_exceptions(void)
{
	Exception_type exceptions_generated = {.dz=0, .ine=0, .inv=0, .ovf=0, .unf=0, .none=1};
    printf("exceptions raised:");
    if(fetestexcept(FE_INEXACT))
	{
		printf(" FE_INEXACT");
		exceptions_generated.ine= 1;
		exceptions_generated.none= 0;
	}
    if(fetestexcept(FE_DIVBYZERO))
	{
		printf(" FE_DIVBYZERO");
		exceptions_generated.dz= 1;
		exceptions_generated.none= 0;
	}
    if(fetestexcept(FE_INVALID))
	{
		printf(" FE_INVALID");
		exceptions_generated.inv= 1;
		exceptions_generated.none= 0;
	}
    if(fetestexcept(FE_OVERFLOW))
	{
		printf(" FE_OVERFLOW");
		exceptions_generated.ovf= 1;
		exceptions_generated.none= 0;
	}
    if(fetestexcept(FE_UNDERFLOW))
	{
		printf(" FE_UNDERFLOW");
		exceptions_generated.unf= 1;
		exceptions_generated.none= 0;
	}
	if(exceptions_generated.none==1)
		printf(" none");
    feclearexcept(FE_ALL_EXCEPT);
    printf("\n");
	return exceptions_generated;
}

Operand_type check_sp_operand_type(int in_fp)
{
	Operand_type op_type= { .nan=0, .inf=0, .norm=0, .deno=0, .zero=0, .sign= 0};
	int exp= (in_fp & 0x7f800000) >> 23;
	int mantissa= (in_fp & 0x007fffff);
	op_type.sign= (in_fp>>31)&1;
	
	if(exp==0xFF)
	{
		if(mantissa==0)
			op_type.inf=1;
		else
		{
			op_type.nan=1;
			op_type.sign= 0;					//For NaN, sign is always 0
		}
	}
	else if(exp==0)
	{
		if(mantissa==0)
			op_type.zero=1;
		else
			op_type.deno=1;
	}
	else
	{
		op_type.norm=1;
	}
	return op_type;
}
int check_denormal_sp(int rs1, int rs2)
{
	int op1= GPR[rs1];
	int op2= GPR[rs2];

	int exp1= op1 & 0x7f800000;
	int man1= op1 & 0x007fffff;
	int exp2= op2 & 0x7f800000;
	int man2= op2 & 0x007fffff;
	
	if(exp1==0 && man1!=0)
		return 1;
	else if(exp2==0 && man2!=0)
		return 1;
	else return 0;
}

