#include <string>
#include <stdio.h>
std::ofstream file_raw2,file_raw,file_disassembled;
volatile int dumpMode;
extern const char* xpr_name[];

extern volatile unsigned long int programCounter;

void sim_t:: risc_dump()
{
	file_raw.open("spike_register_dump.txt");
	file_raw2.open("spike_fregister_dump.txt");
	file_disassembled.open("dump_disassembled.txt");
  while (!done())
  {
    std::cerr <<std::flush;
    std::string cmd, tmp;
    std::vector<std::string> args;

    set_procs_debug(true);
    step(1);
    
    typedef void (sim_t::*interactive_func)(const std::string&, const std::vector<std::string>&);
    std::map<std::string,interactive_func> funcs;
	  cmd="fregs_dump";
    funcs["fregs_dump"] = &sim_t::interactive_fregs_dump;
    try
    {
      if(funcs.count(cmd))
        (this->*funcs[cmd])(cmd, args);
    }
    catch(trap_t t) {}
	cmd="regs_dump";
    funcs["regs_dump"] = &sim_t::interactive_regs_dump;
    try
    {
      if(funcs.count(cmd))
        (this->*funcs[cmd])(cmd, args);
    }
    catch(trap_t t) {}
  }
  ctrlc_pressed = false;
}



extern  std::string current_instruction;

void sim_t::interactive_fregs_dump(const std::string& cmd, const std::vector<std::string>& args)
{
    //const char* registers[]={"zero","ra","s0","s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","sp","tp","v0","v1","a0","a1","a2","a3","a4","a5","a6","a7","t0","t1","t2","t3","t4","gp"};
    std::vector<std::string> args_temp={"0","r0"};
    unsigned long int reg_value;
	static unsigned long int previous_reg_values[32]={0};
	static unsigned long int previous_pc=0;
	if (previous_pc==programCounter) 
    return;
	else 
    previous_pc=programCounter;
	file_raw2<<"PC = "<<std::setfill('0')<<std::setw(16)<<std::hex<<(programCounter & 0xFFFFFFFF)<<"\n";
	for(int nndu=0;nndu<32; nndu++)
	{   
		args_temp[1] = fpr_name[nndu];
		reg_value=get_freg(args_temp);
		//if (reg_value==previous_reg_values[nndu]) continue;
		
		previous_reg_values[nndu]=reg_value;
		file_raw2<<"FREG "<<std::setfill(' ')<<std::setw(2)<<std::dec<<nndu<<" "<<std::setfill('0')<<std::setw(16)<<std::hex<<reg_value<<"\n";;
	
	}
	file_raw2<<'\n';
}

void sim_t::interactive_regs_dump(const std::string& cmd, const std::vector<std::string>& args)
{
    //const char* registers[]={"zero","ra","s0","s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","sp","tp","v0","v1","a0","a1","a2","a3","a4","a5","a6","a7","t0","t1","t2","t3","t4","gp"};
    std::vector<std::string> args_temp={"0","r0"};
    unsigned long int reg_value;
	static unsigned long int previous_reg_values[32]={0};
	static unsigned long int previous_pc=0;
	if (previous_pc==programCounter) return;
	else previous_pc=programCounter;
	file_disassembled<<std::setfill(' ')<<std::setw(16)<<std::hex<<programCounter<<"  "<<std::setfill(' ')<<std::setw(36)<<current_instruction.c_str()<<"  ";
	file_raw<<"PC = "<<std::setfill('0')<<std::setw(16)<<std::hex<<(programCounter & 0xFFFFFFFF)<<"\n";
	for(int nndu=1;nndu<32; nndu++)
	{   
		args_temp[1] = xpr_name[nndu];
		reg_value=get_reg(args_temp);
		//if (reg_value==previous_reg_values[nndu]) continue;
		
		previous_reg_values[nndu]=reg_value;
		file_raw<<"REG "<<std::setfill(' ')<<std::setw(2)<<std::dec<<nndu<<" "<<std::setfill('0')<<std::setw(16)<<std::hex<<reg_value<<"\n";;
		file_disassembled<<"x"<<std::dec<<nndu<<"/"<<xpr_name[nndu]<<" = "<<std::setfill('0')<<std::setw(16)<<std::hex<<reg_value<<"  ";
	
	}
	file_raw<<'\n';
	file_disassembled<<'\n';
}

