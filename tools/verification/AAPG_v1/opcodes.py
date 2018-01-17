#Author: Nandu Raj P ( nndurj@gmail.com )	

from commonStuff import *

base_r_x=('add', 'sub', 'sll', 'slt', 'sltu', 'xor', 'srl', 'sra', 'or', 'and', 'mul', 'mulh', 'mulhsu', 'mulhu', 'div', 'divu', 'rem', 'remu')
base_i_x=('jalr', 'lb', 'lh', 'lw', 'lbu', 'lhu', 'addi', 'slti', 'sltiu', 'xori', 'ori', 'andi', 'slli', 'srli', 'srai')
base_s_x=('sb', 'sh', 'sw')
base_sb=('beq', 'bne', 'blt', 'bge', 'bltu', 'bgeu')
base_u=('lui', 'auipc')
base_uj=('jal',)

base_r_64=('addw', 'subw', 'sllw', 'srlw', 'sraw', 'mulw', 'divw', 'divuw', 'remw', 'remuw')
base_i_64=('ld', 'lwu', 'addiw', 'slliw', 'srliw', 'sraiw')
base_s_64=('sd',)

if bitwidth==64:
	base_r=base_r_x+base_r_64
	base_i=base_i_x+base_i_64
	base_s=base_s_x+base_s_64
else:
	base_r=base_r_x
	base_i=base_i_x
	base_s=base_s_x


system_1=('scall', 'sbreak', 'nop')
system_2_x=('rdcycle', 'rdtime', 'rdinstret')
system_2_32=('rdcycleh', 'rdtimeh', 'rdinstreth')

if bitwidth==64:
	system_2=system_2_x
else:
	system_2=system_2_x+system_2_32

base_integer_computational_x=('nop', 'lui', 'auipc', 'addi', 'slti', 'sltiu', 'xori', 'ori', 'andi', 'slli', 'srli', 'srai', 'add', 'sub', 'sll', 'slt', \
		 'sltu', 'xor', 'srl', 'sra', 'or', 'and', 'nop', 'add', 'mul', 'mulh', 'mulhsu', 'mulhu', 'div', 'divu', 'rem', 'remu')
base_integer_computational_64=('addiw', 'slliw', 'srliw', 'sraiw', 'subw', 'sllw', 'srlw', 'sraw', 'mulw' , 'divw', 'divuw', 'remw', 'remuw')
base_control_transfer=('beq', 'bne', 'blt', 'bge', 'bltu', 'bgeu', 'jal', 'jalr')
base_load_store_x=('sb', 'sh', 'sw', 'lb', 'lh', 'lw', 'lbu', 'lhu')
base_load_store_64=('sd', 'ld', 'lwu')
base_system_x=('scall', 'sbreak', 'rdcycle', 'rdtime', 'rdinstret','nop')
base_system_32=('rdcycleh', 'rdtimeh', 'rdinstreth')

if bitwidth==64:
	base_load_store= base_load_store_x + base_load_store_64
	base_system = base_system_x
	base_integer_computational = base_integer_computational_x + base_integer_computational_64
else:
	base_load_store= base_load_store_x
	base_system = base_system_x + base_system_32
	base_integer_computational = base_integer_computational_x
	
# Single Precision Floating Point

sp_fp_load_store=('flw','fsw')
sp_fp_computational=('fadd.s','fsub.s','fmul.s','fdiv.s','fmin.s','fmax.s','fsqrt.s', 'fmadd.s','fmsub.s','fnmsub.s','fnmadd.s')
sp_fp_conversion_mov_x=('fcvt.w.s', 'fcvt.s.w', 'fcvt.wu.s', 'fcvt.s.wu', 'fsgnj.s', 'fsgnjn.s', 'fsgnjx.s', 'fmv.x.s', 'fmv.s.x')
sp_fp_conversion_mov_64=('fcvt.l.s', 'fcvt.lu.s', 'fcvt.s.l', 'fcvt.s.lu')
sp_fp_compare=('feq.s', 'flt.s', 'fle.s')
sp_fp_classify=('fclass.s',)

if bitwidth==64:
	sp_fp_conversion_mov = sp_fp_conversion_mov_x + sp_fp_conversion_mov_64
else:
	sp_fp_conversion_mov = sp_fp_conversion_mov_x


sp_fp_i=('flw',)
sp_fp_s=('fsw',)
sp_fp_r4=('fmadd.s', 'fmsub.s', 'fnmsub.s', 'fnmadd.s')
sp_fp_r=('fadd.s', 'fsub.s', 'fmul.s', 'fdiv.s', 'fsqrt.s', 'fsgnj.s', 'fsgnjn.s', 'fsgnjx.s', 'fmin.s', 'fmax.s', 'fcvt.w.s', \
		'fcvt.wu.s', 'fmv.x.s', 'feq.s', 'flt.s', 'fle.s', 'fclass.s', 'fcvt.s.w', 'fcvt.s.wu', 'fmv.s.x')
		
sp_fp_other_x=('frcsr', 'frrm', 'frflags', 'fscsr', 'fsrm', 'fsflags', 'fsrmi', 'fsflagsi')
sp_fp_other_64=('fcvt.l.s', 'fcvt.lu.s', 'fcvt.s.l', 'fcvt.s.lu')

if bitwidth==64:
	sp_fp_other = sp_fp_other_x+ sp_fp_other_64
else:
	sp_fp_other = sp_fp_other_x

# Double Precision Floating Point


dp_fp_load_store=('fld', 'fsd')
dp_fp_computational=('fadd','fsub','fmul','fdiv','fmin','fmax','fsqrt', 'fmadd','fmsub','fnmsub','fnmadd')
dp_fp_conversion_mov_x=('fcvt.w.d', 'fcvt.d.w', 'fcvt.wu.d', 'fcvt.d.wu', 'fcvt.s.d', 'fcvt.d.s', 'fsgnj.d', 'fsgnjn.d', 'fsgnjx.d')
dp_fp_conversion_mov_64=('fcvt.l.d', 'fcvt.lu.d', 'fcvt.d.l', 'fcvt.d.lu', 'fmv.x.d', 'fmv.d.x')
dp_fp_compare=('feq.d', 'flt.d', 'fle.d')
dp_fp_classify=('fclass.d',)

dp_fp_r4=('fmadd.d', 'fmsub.d', 'fnmsub.d', 'fnmadd.d')

if bitwidth==64:
	dp_fp_conversion_mov = dp_fp_conversion_mov_x + dp_fp_conversion_mov_64
else:
	dp_fp_conversion_mov = dp_fp_conversion_mov_x



atomicInstr_x=('lr.w', 'sc.w', 'amoswap.w', 'amoadd.w', 'amoxor.w',  'amoand.w', 'amoor.w', 'amomin.w', 'amomax.w', 'amominu.w', 'amomaxu.w')
atomicInstr_64=('lr.d', 'sc.d', 'amoswap.d', 'amoadd.d', 'amoxor.d',  'amoand.d', 'amoor.d', 'amomin.d', 'amomax.d', 'amominu.d', 'amomaxu.d')

if bitwidth==64:
	atomicInstr=atomicInstr_x+ atomicInstr_64
else:
	atomicInstr=atomicInstr_x

privilegedInstr=('csrrw', 'csrrs', 'csrrc', 'csrrwi', 'csrrsi', 'csrrci')  
changePrivilegeInstr = ('ecall', 'ebreak', 'eret') 
trapRedirectionInstr = ('mrts', 'mrth')
interruptManagementInstr =('wfi',)
memoryManagementInstr =('sfence.vm', )

userLevelCSR=('fflags', 'frm', 'fcsr', 'cycle', 'time', 'instret')
if bitwidth==32:
	userLevelCSR+=('cycleh', 'timeh', 'instreth')


userLevelCSR_RO=('cycle', 'time', 'instret', 'cycleh', 'timeh', 'instreth')
userLevelCSR_RW=('fflags', 'frm', 'fcsr')	
machineLevelCSR=('mcpuid', 'mimpid' ,'mhartid', 'mstatus', 'mtvec', 'mtdeleg', 'mie',\
				 'mtimecmp', 'mtime', 'mscratch', 'mepc', 'mcause', 'mbadaddr', 'mip',\
				 'mbase', 'mbound', 'mibase', 'mibound', 'mdbase', 'mdbound', 'htime')
machineLevelCSR32=('mtimeh', 'htimew')
if bitwidth==32:
	machineLevelCSR+=machineLevelCSR32

if __name__ == "__main__":
	print("Please Run aapg.py")
