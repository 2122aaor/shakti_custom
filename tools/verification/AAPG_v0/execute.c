//execute.c calls other functions in programs based on the opcode
#include <stdio.h>
#include <string.h>

void execute(char **parsline)
{
	char *rs1;
	char *rs2;
	char *rd;

	if(strcmp(parsline[0],"LUI") == 0)
	{
		rd = getRreg(parsline[1]);
		fprintf(output,"%.20s%s0110111\n",parsline[2],rd);
	}
	else if(strcmp(parsline[0],"AUIPC") == 0)
	{
		rd = getRreg(parsline[1]);
		fprintf(output,"%.20s%s0010111\n",parsline[2],rd);
	}
	else if(strcmp(parsline[0],"ADDI") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "%.12s%s000%s0010011\n", parsline[3], rs1, rd);
	}
	else if(strcmp(parsline[0],"SLTI") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "%.12s%s010%s0010011\n", parsline[3], rs1, rd);
	}
	else if(strcmp(parsline[0],"SLTIU") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "%.12s%s011%s0010011\n", parsline[3], rs1, rd);
	}
	else if(strcmp(parsline[0],"XORI") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "%.12s%s100%s0010011\n", parsline[3], rs1, rd);
	}
	else if(strcmp(parsline[0],"ORI") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "%.12s%s110%s0010011\n", parsline[3], rs1, rd);
	}
	else if(strcmp(parsline[0],"ANDI") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "%.12s%s111%s0010011\n", parsline[3], rs1, rd);
	}
	else if(strcmp(parsline[0],"SLLI") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "000000%.6s%s001%s0010011\n", parsline[3], rs1, rd);
	}
	else if(strcmp(parsline[0],"SRLI") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "000000%.6s%s101%s0010011\n", parsline[3], rs1, rd);
	}
	else if(strcmp(parsline[0],"SRAI") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "010000%.6s%s101%s0010011\n", parsline[3], rs1, rd);
	}
	else if(strcmp(parsline[0],"ADD") == 0)
	{		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0000000%s%s000%s0110011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"SUB") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0100000%.5s%s000%s0110011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"SLT") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0000000%s%s010%s0110011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"SLTU") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0000000%s%s011%s0110011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"SLL") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0000000%s%s001%s0110011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"SRL") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0000000%s%s101%s0110011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"SRA") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0100000%s%s101%s0110011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"XOR") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0000000%s%s100%s0110011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"AND") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0000000%s%s111%s0110011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"OR") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0000000%s%s110%s0110011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"ADDIW") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "%.12s%s000%s0011011\n", parsline[3], rs1, rd);
	}
	else if(strcmp(parsline[0],"SLLIW") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "000000%.6s%s001%s0011011\n", parsline[3], rs1, rd);
	}
	else if(strcmp(parsline[0],"SRLIW") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "000000%.6s%s101%s0011011\n", parsline[3], rs1, rd);
	}
	else if(strcmp(parsline[0],"SRAIW") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "010000%.6s%s101%s0011011\n", parsline[3], rs1, rd);
	}
	else if(strcmp(parsline[0],"SLLW") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0000000%s%s001%s0111011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"SRLW") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0000000%s%s101%s0111011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"SRAW") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0100000%s%s101%s0111011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"ADDW") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0000000%s%s000%s0111011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"SUBW") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0100000%s%s000%s0111011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"MUL") == 0)
	{
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0000001%s%s000%s0110011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"MULH") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0000001%s%s001%s0110011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"MULHU") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0000001%s%s011%s0110011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"MULHSU") == 0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		rs2 = getRreg(parsline[3]);
		fprintf(output, "0000001%s%s010%s0110011\n", rs2,rs1,rd);
	}
	else if(strcmp(parsline[0],"LB")==0){
		
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "%.12s%s000%s0000011\n", parsline[3],rs1,rd);
	}
	else if(strcmp(parsline[0],"LH")==0){
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "%.12s%s001%s0000011\n", parsline[3],rs1,rd);
	}
	else if(strcmp(parsline[0],"LW")==0){
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "%.12s%s010%s0000011\n", parsline[3],rs1,rd);
	}
	else if(strcmp(parsline[0],"LBU")==0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "%.12s%s100%s0000011\n", parsline[3],rs1,rd);
	}
	else if(strcmp(parsline[0],"LHU")==0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "%.12s%s101%s0000011\n", parsline[3],rs1,rd);
	}
	else if(strcmp(parsline[0],"LWU")==0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "%.12s%s110%s0000011\n", parsline[3], rs1, rd);
	}
	else if(strcmp(parsline[0],"LD")==0)
	{
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);
		fprintf(output, "%.12s%s011%s0000011\n", parsline[3], rs1, rd);
	}
	else if(strcmp(parsline[0],"SB")==0){
		
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);

		fprintf(output, "%.7s%s%s000%.5s0100011\n", parsline[3],rs2,rs1,&parsline[3][7]);
	}
	else if(strcmp(parsline[0],"SH")==0){
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);

		fprintf(output, "%.7s%s%s001%.5s0100011\n", parsline[3],rs2,rs1,&parsline[3][7]);
	}
	else if(strcmp(parsline[0],"SW")==0){
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);
		fprintf(output, "%.7s%s%s010%.5s0100011\n", parsline[3],rs2,rs1,&parsline[3][7]);
	}
	else if(strcmp(parsline[0],"SD")==0)
	{
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);
		fprintf(output, "%.7s%s%s011%.5s0100011\n", parsline[3],rs2,rs1,&parsline[3][7]);
	}
	else if(strcmp(parsline[0],"JAL")==0){
		rd = getRreg(parsline[1]);

		fprintf(output, "%c%.10s%c%.8s%s1101111\n",parsline[2][0],&parsline[2][10],parsline[2][9],&parsline[2][1],rd);
	}
	else if(strcmp(parsline[0],"JALR")==0){
		rd = getRreg(parsline[1]);
		rs1 = getRreg(parsline[2]);

		fprintf(output, "%s%s000%s1100111\n", parsline[3],rs1,rd);
	}
	else if(strcmp(parsline[0],"BEQ")==0){
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);

		fprintf(output, "%c%.6s%s%s000%.4s%c1100011\n", parsline[3][0],&parsline[3][2],rs2,rs1,&parsline[3][8],parsline[3][1]);
	}
	else if(strcmp(parsline[0],"BNE")==0){
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);

		fprintf(output, "%c%.6s%s%s001%.4s%c1100011\n", parsline[3][0],&parsline[3][2],rs2,rs1,&parsline[3][8],parsline[3][1]);
	}
	else if(strcmp(parsline[0],"BLT")==0){
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);

		fprintf(output, "%c%.6s%s%s100%.4s%c1100011\n", parsline[3][0],&parsline[3][2],rs2,rs1,&parsline[3][8],parsline[3][1]);
	}
	else if(strcmp(parsline[0],"BGE")==0){
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);

		fprintf(output, "%c%.6s%s%s101%.4s%c1100011\n", parsline[3][0],&parsline[3][2],rs2,rs1,&parsline[3][8],parsline[3][1]);
	}
	else if(strcmp(parsline[0],"BLTU")==0){
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);

		fprintf(output, "%c%.6s%s%s110%.4s%c1100011\n", parsline[3][0],&parsline[3][2],rs2,rs1,&parsline[3][8],parsline[3][1]);
	}
	else if(strcmp(parsline[0],"BGEU")==0){
		rs1 = getRreg(parsline[1]);
		rs2 = getRreg(parsline[2]);

		fprintf(output, "%c%.6s%s%s111%.4s%c1100011\n", parsline[3][0],&parsline[3][2],rs2,rs1,&parsline[3][8],parsline[3][1]);
	}
}
