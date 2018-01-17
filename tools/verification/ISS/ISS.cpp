#include<iostream>
#include<string> 
#include<map>
#include<functional>
#include<cstring>
#include<inttypes.h>
#include "program_structure/queue.cpp"
#include "program_structure/MMU.cpp"


//Function Declarations
const char* hexchar2bin(char);
std::string hexstr2binstr(std::string);
void opcodemapper(std::string);
int registers(std::string);
void str2bin1(int*,std::string);
//Global variables
int Rreg[32][64]; // Register Initialization
bool outofbound[64]; 
bool Memo[1048576][64];
long int PC ,BC;
char line[70];
struct queue * data= (struct queue *)malloc(sizeof(struct queue));//queue intialisation	
struct memqueue *memex = (struct memqueue *)malloc(sizeof(struct memqueue));//extra memory queue 
FILE *assins , *output, *memout;


#include "main.h"


std::map<std::string,std::function< void(int,int,int*)> > itype = {{"00011001",jalr},{"00000000",lb},{"00100000",lh},{"01000000",lw},{"10000000",lbu},{"10100000",lhu},{"00000100",addi},{"01000100",slti},{"01100100",sltiu},{"10000100",xori},{"11000100",ori},{"11100100",andi},{"11000000",lwu},{"01100000",ld},{"00000110",addiw}};  //(17,3) + (25,5) (Starting index,size)

std::map<std::string,std::function< void(int,int,int)> > rtype_rs2 = {{"000000000001100",Add},{"010000000001100",sub},{"000000000101100",sll},{"000000001001100",slt},{"000000001101100",sltu},{"000000010001100",Xor},{"000000010101100",srl},{"010000010101100",sra},{"000000011001100",Or},{"000000011101100",And},{"000000000001110",addw},{"010000000001110",subw},{"000000000101110",sllw},{"000000010101110",srlw},{"010000010101110",sraw},{"000000100001100",mul},{"000000100101100",mulh},{"000000101001100",mulhsu},{"000000101101100",mulhu},{"000000100001110",mulw},{"000000110001100",divs},{"000000110101100",divu},{"000000111001100",rem},{"000000111101100",remu},{"000000110001110",divw}}; //(0,7) + (17,3) + (25,5) (Starting index,size)

std::map<std::string,std::function< void(int,int,int*)> > rtype_shamt64 = {{"00000000100100",slli},{"00000010100100",srli},{"01000010100100",srai}};

std::map<std::string,std::function< void(int,int,int*)> > rtype_shamt32 = {{"0000000001",slliw},{"0000000101",srliw},{"0100000101",sraiw}};

std::map<std::string,std::function< void(int,int,int*)> > stype = {{"000",sb},{"001",sh},{"010",sw},{"011",sd}};

std::map<std::string,std::function< void(int,int,int*)> > sbtype = {{"000",beq},{"001",bne},{"100",blt},{"101",bge},{"110",bltu},{"111",bgeu}};

std::map<std::string,std::function< void(int,int*)> > uujtype = {{"01101",lui},{"00101",auipc},{"11011",jal}};

int main()
{
std::memset(Rreg, 0, sizeof(Rreg[0][0])*32*64);
FILE *fp;
  int i=0;
	int m = 1;
     int j=0;
     char k[16];
     int l;
     if((fp = fopen("ISS/memory_dcache.txt","r")) == NULL)
     {     fprintf(stderr,"Unable to open file\n");
           exit(1);
     }
     for(i = 0; i <1048576; i++)
     {
	  fscanf(fp, "%s", k);
          for(j = 0; j < 64; j++)
          {
	 	switch(k[j])
		{
			case '0' : Memo[i][4*j]=0; Memo[i][4*j+1]=0; Memo[i][4*j+2]=0; Memo[i][4*j+3]=0; break; 
			case '1' : Memo[i][4*j]=0; Memo[i][4*j+1]=0; Memo[i][4*j+2]=0; Memo[i][4*j+3]=1; break; 
			case '2' : Memo[i][4*j]=0; Memo[i][4*j+1]=0; Memo[i][4*j+2]=1; Memo[i][4*j+3]=0; break; 
			case '3' : Memo[i][4*j]=0; Memo[i][4*j+1]=0; Memo[i][4*j+2]=1; Memo[i][4*j+3]=1; break; 
			case '4' : Memo[i][4*j]=0; Memo[i][4*j+1]=1; Memo[i][4*j+2]=0; Memo[i][4*j+3]=0; break; 
			case '5' : Memo[i][4*j]=0; Memo[i][4*j+1]=1; Memo[i][4*j+2]=0; Memo[i][4*j+3]=1; break; 
			case '6' : Memo[i][4*j]=0; Memo[i][4*j+1]=1; Memo[i][4*j+2]=1; Memo[i][4*j+3]=0; break; 
			case '7' : Memo[i][4*j]=0; Memo[i][4*j+1]=1; Memo[i][4*j+2]=1; Memo[i][4*j+3]=1; break; 
			case '8' : Memo[i][4*j]=1; Memo[i][4*j+1]=0; Memo[i][4*j+2]=0; Memo[i][4*j+3]=0; break; 
			case '9' : Memo[i][4*j]=1; Memo[i][4*j+1]=0; Memo[i][4*j+2]=0; Memo[i][4*j+3]=1; break; 
			case 'a' : Memo[i][4*j]=1; Memo[i][4*j+1]=0; Memo[i][4*j+2]=1; Memo[i][4*j+3]=0; break; 
			case 'b' : Memo[i][4*j]=1; Memo[i][4*j+1]=0; Memo[i][4*j+2]=1; Memo[i][4*j+3]=1; break; 
			case 'c' : Memo[i][4*j]=1; Memo[i][4*j+1]=1; Memo[i][4*j+2]=0; Memo[i][4*j+3]=0; break; 
			case 'd' : Memo[i][4*j]=1; Memo[i][4*j+1]=1; Memo[i][4*j+2]=0; Memo[i][4*j+3]=1; break; 
			case 'e' : Memo[i][4*j]=1; Memo[i][4*j+1]=1; Memo[i][4*j+2]=1; Memo[i][4*j+3]=0; break; 
			case 'f' : Memo[i][4*j]=1; Memo[i][4*j+1]=1; Memo[i][4*j+2]=1; Memo[i][4*j+3]=1; break; 
		}
          }
     }

     fclose(fp);
	createqueue(data);//queue creation
	creatememq(memex);//extra memory queue creation
	PC = 0; BC=0; // PC and BC intialization
	std::string get_line;
	std::string opcode_str;
	assins = fopen("ISS/input.hex","r");
	output = fopen("regdump.txt","w");
	memout = fopen("instdump.txt","w");
	fprintf(memout, "Type\tReg\t\tMemloc\t\t\t\tValue\t\t\t\tPC\n" );
	while(fgets(line,sizeof(line),assins)!=NULL)
	{
		PC ++ ; //Program Counter Incrementation
		addq(line,PC,data);
		std::cout << PC << "\n";
		if(PC > 1024)
		{
		delq(data);
		}
		get_line = line;
		std::cout << "HEX LINE: " << get_line << "\n";
		opcode_str = hexstr2binstr(get_line);	
		opcodemapper(opcode_str);
	}
	memex_close(memex);
	destroyqueue(data);
	fclose(output);
	fclose(assins);	 
	return 0;
}

const char* hexchar2bin(char c)
{
    switch(toupper(c))
    {
        case '0': return "0000";case '1': return "0001";case '2': return "0010";case '3': return "0011";case '4': return "0100";
        case '5': return "0101";case '6': return "0110";case '7': return "0111";case '8': return "1000";case '9': return "1001";
        case 'A': return "1010";case 'B': return "1011";case 'C': return "1100";case 'D': return "1101";case 'E': return "1110";
        case 'F': return "1111";
    }
}
std::string hexstr2binstr(std::string hex)
{
    std::string bin;
    for(unsigned i = 0; i < hex.length()-1; ++i)
       bin += hexchar2bin(hex[i]);
    return bin;
}
int registers(std::string in)
{
        if(in.compare("00000") == 0) return 0;else if(in.compare("00001") == 0) return 1;
        else if(in.compare("00010") == 0) return 2;else if(in.compare("00011") == 0) return 3;
        else if(in.compare("00100") == 0) return 4;else if(in.compare("00101") == 0) return 5;
        else if(in.compare("00110") == 0) return 6;else if(in.compare("00111") == 0) return 7;
        else if(in.compare("01000") == 0) return 8;else if(in.compare("01001") == 0) return 9;
        else if(in.compare("01010") == 0) return 10;else if(in.compare("01011") == 0) return 11;
        else if(in.compare("01100") == 0) return 12;else if(in.compare("01101") == 0) return 13;
	else if(in.compare("01110") == 0) return 14;else if(in.compare("01111") == 0) return 15;
	else if(in.compare("10000") == 0) return 16;else if(in.compare("10001") == 0) return 17;
	else if(in.compare("10010") == 0) return 18;else if(in.compare("10011") == 0) return 19;
	else if(in.compare("10100") == 0) return 20;else if(in.compare("10101") == 0) return 21;
	else if(in.compare("10110") == 0) return 22;else if(in.compare("10111") == 0) return 23;
	else if(in.compare("11000") == 0) return 24;else if(in.compare("11001") == 0) return 25;
	else if(in.compare("11010") == 0) return 26;else if(in.compare("11011") == 0) return 27;
	else if(in.compare("11100") == 0) return 28;else if(in.compare("11101") == 0) return 29;
	else if(in.compare("11110") == 0) return 30;else if(in.compare("11111") == 0) return 31;
}

void str2bin1(int *bin,std::string str)
{
	int l = str.size();
	int i;
	for (i = 0; i < l; ++i)
	{
		bin[i] = (int)str[i] - 48;		
	}
}


void opcodemapper(std::string op_str)
{
 std::string op_sub = op_str.substr(25,5);
 std::string op_func3 = op_str.substr(17,3);
if(op_sub=="11001"||op_sub=="00000"||(op_sub=="00100"&&op_func3!="001"&&op_func3!="101")||(op_sub=="00110"&& op_func3=="000"))
 {
	int imm[12];
        int op_rd = registers(op_str.substr(20,5));
        int op_rs1 = registers(op_str.substr(12,5));
        std::string imm_str = op_str.substr(0,12);
	str2bin1(imm,imm_str);
	itype[op_func3+op_sub](op_rd,op_rs1,imm);
	if(op_sub!="11001")
	fileprintreg();
 }
else if(op_sub=="01100"||op_sub=="01110")
{
      int op_rd = registers(op_str.substr(20,5));
      int op_rs1 = registers(op_str.substr(12,5));
      int op_rs2 = registers(op_str.substr(7,5));
      rtype_rs2[op_str.substr(0,7)+op_func3+op_sub](op_rd,op_rs1,op_rs2);
      fileprintreg();
}

else if ((op_sub=="00100"&&op_func3=="001")||(op_sub=="00100"&&op_func3=="101"))
{
   
      int shamt[6];
      int op_rd = registers(op_str.substr(20,5));
      int op_rs1 = registers(op_str.substr(12,5));
      std::string imm_str = op_str.substr(6,6);
      str2bin1(shamt,imm_str);
      std::string op_temp = op_str.substr(0,6) + op_func3 + op_sub;
      rtype_shamt64[op_temp](op_rd,op_rs1,shamt);
      fileprintreg();      
}
else if (op_sub=="00110" && op_func3!="000")
{
      int shamt[5];
      int op_rd = registers(op_str.substr(20,5));
      int op_rs1 = registers(op_str.substr(12,5));
      std::string imm_str = op_str.substr(7,5);
      str2bin1(shamt,imm_str);
      std::string op_temp = op_str.substr(0,7) + op_func3;
      rtype_shamt32[op_temp](op_rd,op_rs1,shamt);
      fileprintreg();      

}
else if(op_sub == "01000")
{
     int imm[12];
     int op_rs1 = registers(op_str.substr(12,5));
     int op_rs2 = registers(op_str.substr(7,5));
     std::string imm_str = op_str.substr(0,7)+op_str.substr(20,5);
     str2bin1(imm,imm_str);
     stype[op_func3](op_rs1,op_rs2,imm);
     fileprintreg();
} 
else if(op_sub == "11000")
{
     int imm[12];
     int op_rs1 = registers(op_str.substr(12,5));
     int op_rs2 = registers(op_str.substr(7,5));
     std::string imm_str = op_str.substr(0,1)+op_str.substr(24,1)+op_str.substr(1,6)+op_str.substr(20,4);
     str2bin1(imm,imm_str);
     sbtype[op_func3](op_rs1,op_rs2,imm);
}
else if(op_sub == "01101"||op_sub=="00101"||op_sub=="11011")
{
    int imm[20];
    std::string imm_str;
    int op_rd = registers(op_str.substr(20,5));
    if(op_sub == "11011")
    imm_str = op_str.substr(0,1) + op_str.substr(12,8) + op_str.substr(11,1) + op_str.substr(1,10);
    else
    imm_str = op_str.substr(0,20);
    str2bin1(imm,imm_str);
    uujtype[op_sub](op_rd,imm);
}
 
}
