#Author: Nandu Raj P ( nndurj@gmail.com )	

from commonStuff import *
import commonVar as cv
import random

def gen_sp_fp_instr(opcode):
	"""Generates a random SPFP instruction using the provided opcode"""
	instruction=None
	reg_f_1=find_rand_float_reg_destination()
	reg_f_2=find_rand_float_reg_source()
	reg_f_3=find_rand_float_reg_source()
	reg_f_4=find_rand_float_reg_source()

	if opcode in op.sp_fp_i+op.sp_fp_s:
		#fsw	fa5,-28(s0)      flw	fa4,-20(s0)
		memoryLimit_by_4=initialMemorySize*(512-24)//4
		immediate=random.randint(-memoryLimit_by_4, memoryLimit_by_4)*4
		instruction= '{0} {1}, {2}(x{3})'.format(opcode,reg_f_1,immediate,memoryBaseAddressRegister)
	
	elif opcode in op.sp_fp_r4:
		#FMADD.S rd,rs1,rs2,rs3
		instruction= '{0} {1}, {2}, {3}, {4}'.format(opcode, reg_f_1, reg_f_2, reg_f_3, reg_f_4)
		
	elif opcode in ('fadd.s', 'fsub.s', 'fmul.s', 'fdiv.s', 'fsgnj.s', 'fsgnjn.s', 'fsgnjx.s', 'fmin.s', 'fmax.s'):
		instruction = '{0} {1}, {2}, {3}'.format(opcode, reg_f_1, reg_f_2, reg_f_3)
	
	elif opcode in('fsqrt.s',):
		instruction = '{0} {1}, {2}'.format(opcode, reg_f_1, reg_f_2)
		
	elif opcode in ('fcvt.w.s', 'fcvt.wu.s', 'fcvt.l.s', 'fcvt.lu.s', 'fmv.x.s', 'fclass.s') :
		reg_int=find_rand_destination_reg()
		instruction= '{0} x{1}, {2}'.format(opcode,reg_int,reg_f_1)
	
	elif opcode in ('feq.s', 'flt.s', 'fle.s'):
		reg_int=find_rand_destination_reg()
		instruction= '{0} x{1}, {2}, {3}'.format(opcode,reg_int,reg_f_1,reg_f_2)
	
	elif opcode in ('fcvt.s.w', 'fcvt.s.wu', 'fcvt.s.l', 'fcvt.s.lu', 'fmv.s.x'):
		reg_int=find_rand_destination_reg()
		instruction= '{0} {1}, x{2}'.format(opcode, reg_f_1, reg_int)		
	
	elif opcode in ('frcsr', 'frrm', 'frflags'):
		reg_int=find_rand_destination_reg()
		instruction = '{0} x{1}'.format(opcode,reg_int)
	
	elif opcode in ('fscsr', 'fsrm', 'fsflags'):
		reg_int1=find_rand_destination_reg()
		reg_int2=find_rand_destination_reg()
		instruction= '{0}, x{1}, x{2}'.format(opcode, reg_int1, reg_int2)
	
	elif opcode in ('fsrmi', 'fsflagsi'):
		return
		reg_int=find_rand_destination_reg()
		rand_immediate=random.randint(0,1000)
		instruction= '{0} x{1}, {2}'.format(opcode, reg_int, rand_immediate)
	if instruction is not None:
		if type(instruction)==tuple:
			return instruction
		else:
			return (instruction,)
		
def gen_dp_fp_instr(opcode):
	"""Generates a random DPFP instruction using the provided opcode"""
	instruction=None
	reg_f_1=find_rand_float_reg_destination()
	reg_f_2=find_rand_float_reg_source()
	reg_f_3=find_rand_float_reg_source()
	reg_f_4=find_rand_float_reg_source()

	if opcode in ('fld', 'fsd'):
		memoryLimit_by_4=initialMemorySize*(512-24)//8
		immediate=random.randint(-memoryLimit_by_4, memoryLimit_by_4)*8
		instruction= '{0} {1}, {2}(x{3})'.format(opcode,reg_f_1,immediate,memoryBaseAddressRegister)
	
	elif opcode in op.dp_fp_r4:
		instruction= '{0} {1}, {2}, {3}, {4}'.format(opcode, reg_f_1, reg_f_2, reg_f_3, reg_f_4)
		
	elif opcode in ('fadd.d', 'fsub.d', 'fmul.d', 'fdiv.d', 'fsgnj.d', 'fsgnjn.d', 'fsgnjx.d', 'fmin.d', 'fmax.d'):
		instruction = '{0} {1}, {2}, {3}'.format(opcode, reg_f_1, reg_f_2, reg_f_3)
	
	elif opcode in('fsqrt.d',):
		instruction = '{0} {1}, {2}'.format(opcode, reg_f_1, reg_f_2)
		
	elif opcode in ('fcvt.w.d', 'fcvt.wu.d', 'fcvt.l.d', 'fcvt.lu.d', 'fmv.x.d', 'fclass.d') :
		reg_int=find_rand_destination_reg()
		instruction= '{0} x{1}, {2}'.format(opcode,reg_int,reg_f_1)
	
	elif opcode in ('feq.d', 'flt.d', 'fle.d'):
		reg_int=find_rand_destination_reg()
		instruction= '{0} x{1}, {2}, {3}'.format(opcode,reg_int,reg_f_1,reg_f_2)
	
	elif opcode in ('fcvt.d.w', 'fcvt.d.wu', 'fcvt.d.l', 'fcvt.d.lu', 'fmv.d.x'):
		reg_int=find_rand_destination_reg()
		instruction= '{0} {1}, x{2}'.format(opcode, reg_f_1, reg_int)		
	
	elif opcode in ('frcsr', 'frrm', 'frflags'):
		reg_int=find_rand_destination_reg()
		instruction = '{0} x{1}'.format(opcode,reg_int)
	
	elif opcode in ('fscsr', 'fsrm', 'fsflags'):
		reg_int1=find_rand_destination_reg()
		reg_int2=find_rand_destination_reg()
		instruction= '{0}, x{1}, x{2}'.format(opcode, reg_int1, reg_int2)
	
	elif opcode in ('fsrmi', 'fsflagsi'):
		reg_int=find_rand_destination_reg()
		rand_immediate=random.randint(0,1000)
		instruction= '{0} x{1}, {2}'.format(opcode, reg_int, rand_immediate)
	if instruction is not None:
		if type(instruction)==tuple:
			return instruction
		else:
			return (instruction,)

loadReserveAddressHistory=[]
def gen_atomic_instr(opcode):
	global loadReserveAddressHistory
	if 'amo' in opcode:
		rd=find_rand_destination_reg()
		rs1=find_rand_destination_reg()
		rs2=find_rand_source_reg()
		mem_limit=initialMemorySize*64-2
		rand_mem_location=random.randint(-mem_limit,mem_limit)*8
		instr1='addi x{0}, x{1}, {2}'.format(rs1,memoryBaseAddressRegister,rand_mem_location)
		instr2='{0} x{1}, x{2}, (x{3})'.format(opcode, rd, rs2, rs1)
		return (instr1, instr2)
	elif 'lr.' in opcode:
		rd=find_rand_destination_reg()
		rs1=find_rand_destination_reg()
		mem_limit=initialMemorySize*64-2
		rand_mem_location=random.randint(-mem_limit,mem_limit)*8
		loadReserveAddressHistory.append(rand_mem_location)
		instr1='addi x{0}, x{1}, {2}'.format(rs1,memoryBaseAddressRegister,rand_mem_location)
		instr2='{0} x{1}, (x{2})'.format(opcode,rd, rs1)
		return (instr1, instr2)
	elif 'sc.' in opcode:
		rd=find_rand_destination_reg()
		rs1=find_rand_destination_reg()
		rs2=find_rand_source_reg()
		try:
			mem_location=loadReserveAddressHistory.pop(0)
		except:
			return
		instr1='addi x{0}, x{1}, {2}'.format(rs1,memoryBaseAddressRegister,mem_location)
		instr2='{0} x{1}, x{2}, (x{3})'.format(opcode,rd,rs2,rs1)
		return(instr1, instr2)
		
		
customPrivilegedInstructionsFile=open('customPrivilegedInstr.txt')
customPrivilegedInstructions=customPrivilegedInstructionsFile.readlines()
def genPrivilegedInstr(opcode):
		if opcode=='csrrw':
			rd=find_rand_destination_reg()
			rs1=find_rand_destination_reg()
			csr=random.choice(op.userLevelCSR_RW)
			instr1='csrrs x{0}, {1}, x0'.format(rs1,csr)
			instr2='csrrw x{0}, {1}, x{2}'.format(rd, csr, rs1)
			return(instr1, instr2)
		elif opcode=='csrrs':
			rd=find_rand_destination_reg()
			csr=random.choice(op.userLevelCSR)
			instr1='csrrs x{0}, {1}, x0'.format(rd,csr)
			return (instr1, )
		elif opcode=='csrrc':
			rd=find_rand_destination_reg()
			csr=random.choice(op.userLevelCSR_RW)
			instr1='csrrc x{0}, {1}, x0'.format(rd,csr)
			return (instr1, )
		elif opcode=='csrrwi':
			return
			rd=find_rand_destination_reg()
			imm=0
			csr=random.choice(op.userLevelCSR_RW)
			instr1='csrrwi x{0}, {1}, {2}'.format(rd, csr, imm)
			return (instr1, )
		elif opcode=='csrrsi':
			rd=find_rand_destination_reg()
			imm=0
			csr=random.choice(op.userLevelCSR_RW)
			instr1='csrrsi x{0}, {1}, {2}'.format(rd, csr, imm)
			return (instr1, )
		elif opcode=='csrrci':
			rd=find_rand_destination_reg()
			imm=0
			csr=random.choice(op.userLevelCSR_RW)
			instr1='csrrci x{0}, {1}, {2}'.format(rd, csr, imm)
			return (instr1, )
		elif opcode=='customPrivileged':
			if len(customPrivilegedInstructions)>0:
				return (random.choice(customPrivilegedInstructions), )
			else:
				return
if __name__ == "__main__":
	print("Please Run aapg.py")
