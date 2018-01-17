#!/usr/bin/env python3

#Author: Nandu Raj P ( nndurj@gmail.com )		


import sys
from commonStuff import *
import baseInstructions as bI
import commonVar as cv
import random
import standardExtensions as sE
import time
import os
asmFile = 0


def initialCode():
	asmFile.write('''
	.text
	.align	4
	.globl	main
	.type	main, @function
main:
	csrwi minstret,1
	csrwi mcycle,1
  lui x2, 0x40000   ## x2<- 0x40000000
  slli x2,x2,1      ## x2<- 0x80000000
  lui x3, 0x10      ## x2<- 0x00010000   
  add x2,x2,x3      ## x2<- 0x80010000
  lui	t0,0x3					# enable FPU
  csrs	mstatus,t0			# enable FPU
'''.format(memoryBaseAddressRegister, initialMemorySize*512-8))
  #	fssr	zero
	
	
	# Loading all registers with the value in initial memory, which is random
	for regNo in range(1,32):
		if regNo in cv.unusedRegs:
			continue
		if bitwidth==64:
			asmFile.write("\tld x{0}, {1}(x{2})\n".format(regNo,regNo*8,memoryBaseAddressRegister))
		else:
			asmFile.write("\tlw x{0}, {1}(x{2})\n".format(regNo,regNo*4,memoryBaseAddressRegister))
	
	for regNo in range(0,32):
		if percentDPFloat>0:
			asmFile.write("\tfld f{0}, {1}(x{2})\n".format(regNo, regNo*8, memoryBaseAddressRegister))
		elif percentSPFloat>0:
			asmFile.write("\tflw f{0}, {1}(x{2})\n".format(regNo, regNo*4, memoryBaseAddressRegister))


def writeInstructions(instructions):
	for val in instructions:
		asmFile.write("i_%d:\n" % cv.instructionNumber)
		asmFile.write("\t%s\n" % val)
		cv.instructionNumber+=1

def opcode_checker(opcode):
	returnValue = bI.setBranchInstr()
	if returnValue is not None:
		asmFile.write("i_%d:\n" % cv.instructionNumber)
		asmFile.write("\t%s\n" % returnValue[0])
		asmFile.write("\t%s\n" % returnValue[1])
		cv.instructionNumber+=1
		return
	returnValue = bI.initializeLoop()
	if returnValue is not None:
		writeInstructions(returnValue)
		return		
	returnValue=bI.gen_base_instr(opcode)
	if returnValue is not None:
		writeInstructions(returnValue)
		return
	returnValue=sE.gen_sp_fp_instr(opcode)
	if returnValue is  not None:
		writeInstructions(returnValue)
		return
	returnValue=sE.gen_dp_fp_instr(opcode)
	if returnValue is  not None:
		writeInstructions(returnValue)
		return
	returnValue=sE.gen_atomic_instr(opcode)
	if returnValue is  not None:
		asmFile.write("i_%d:\n" % cv.instructionNumber)
		asmFile.write("\t%s\n" % returnValue[0])
		asmFile.write("\t%s\n" % returnValue[1])
		cv.instructionNumber+=1
		return
	returnValue=sE.genPrivilegedInstr(opcode)
	if returnValue is not None:
		asmFile.write("i_{0}:\n".format(cv.instructionNumber))
		for instr in returnValue:
			asmFile.write('\t{0}\n'.format(instr))
		cv.instructionNumber+=1
		return
	
#Main Program Starts Here
def random_ASM_generator():
	global totalInstructions, percentBaseInstr, perIntegerComputation, perControlTransfer, perLoadStore, perSystemInstr
	global percentSPFloat, percentSPLoadStore, percentSPComputational, percentSPConversionMov, percentSPCompare, PercentSPClassify
	global percentDPFloat, percentDPLoadStore, percentDPComputational, percentDPConversionMov, percentDPCompare, PercentDPClassify
	

	progressRange=range(0,totalInstructions,totalInstructions//100)
	while totalInstructions> cv.instructionNumber:
		extn_prob=random.uniform(0,100)
		typ_prob=random.uniform(0,100)
		if cv.instructionNumber in progressRange:
			sys.stdout.write('\rRunning AAPG {0}%'.format(progressRange.index(cv.instructionNumber)+1))
			sys.stdout.flush()
		if extn_prob<percentBaseInstr:
			if typ_prob<perIntegerComputation:
				opcode=random.choice(op.base_integer_computational)
			elif typ_prob<perIntegerComputation+perControlTransfer:
				opcode=random.choice(op.base_control_transfer)
			elif typ_prob<perIntegerComputation+perControlTransfer+perLoadStore:
				opcode=random.choice(op.base_load_store)
			else:
				opcode=random.choice(op.base_system)
		elif extn_prob<percentBaseInstr+percentSPFloat:
			if typ_prob<percentSPLoadStore:
				opcode=random.choice(op.sp_fp_load_store)
			elif typ_prob<percentSPLoadStore+percentSPComputational:
				opcode=random.choice(op.sp_fp_computational)
			elif typ_prob<percentSPLoadStore+percentSPComputational+percentSPConversionMov:
				opcode=random.choice(op.sp_fp_conversion_mov)
			elif typ_prob<percentSPLoadStore+percentSPComputational+percentSPConversionMov+percentSPCompare:
				opcode=random.choice(op.sp_fp_compare)
			else:
				opcode=random.choice(op.sp_fp_classify)
		elif extn_prob<percentBaseInstr+percentSPFloat+percentDPFloat:
			if typ_prob<percentDPLoadStore:
				opcode=random.choice(op.dp_fp_load_store)
			elif typ_prob<percentDPLoadStore+percentDPComputational:
				opcode=random.choice(op.dp_fp_computational)
			elif typ_prob<percentDPLoadStore+percentDPComputational+percentDPConversionMov:
				opcode=random.choice(op.dp_fp_conversion_mov)
			elif typ_prob<percentDPLoadStore+percentDPComputational+percentDPConversionMov+percentDPCompare:
				opcode=random.choice(op.dp_fp_compare)
			else:
				opcode=random.choice(op.dp_fp_classify)
		elif extn_prob<percentBaseInstr+percentSPFloat+percentDPFloat+percentAtomicInstructions:
			opcode=random.choice(op.atomicInstr)
			
		elif extn_prob<percentBaseInstr+percentSPFloat+percentDPFloat+percentAtomicInstructions+percentPrivilegedInstructions:
			if typ_prob<percentPrivilegedBaseInstr:
				opcode=random.choice(op.privilegedInstr)
			elif typ_prob<percentPrivilegedBaseInstr+percentChangePrivilegeInstr:
				opcode=random.choice(op.changePrivilegeInstr)
			elif typ_prob<percentPrivilegedBaseInstr+percentChangePrivilegeInstr+percentTrapRedirectionInstr:
				opcode=random.choice(op.trapRedirectionInstr)
			elif typ_prob<percentPrivilegedBaseInstr+percentChangePrivilegeInstr+percentTrapRedirectionInstr+percentInterruptManagementInstr:
				opcode=random.choice(op.interruptManagementInstr)
			elif typ_prob<percentPrivilegedBaseInstr+percentChangePrivilegeInstr+percentTrapRedirectionInstr+percentInterruptManagementInstr+percentMemoryManagementInstr:
				opcode=random.choice(op.memoryManagementInstr)
			elif typ_prob<percentPrivilegedBaseInstr+percentChangePrivilegeInstr+percentTrapRedirectionInstr+percentInterruptManagementInstr+percentMemoryManagementInstr+percentCustomInstr:
				opcode='customPrivileged'
		opcode_checker(opcode)
	for i in range(forwardBranchRange):
		asmFile.write("i_%d:\n" % cv.instructionNumber)
		Instruction = "\tnop"
		asmFile.write("%s\n" % Instruction)
		cv.instructionNumber+=1

	return

def aapgMain(fileName):
	global asmFile
	asmFile=open(fileName , "w")
	start_time=time.time()
	bI.initialize_vars()
	cv.instructionNumber=0
	initialCode()
	random_ASM_generator()
	asmFile.write('''
 j tohost_exit

 tohost_exit:
  addi x2, x0,0x1
  slli x2, x2, 0x1f # 0x80000000
  lui x3, 0x10  # 
  add x2,x2,x3
  li a4, 1
  sw a4, %lo(tohost)(x2) # store 1 to tohost.
  loop: j loop
  
.size	main, .-main

.data
.align 4
.globl data
data: 
''')
	if bitwidth==64:
		for _ in range(0, initialMemorySize*1024//8):
			asmFile.write('\n\t.dword {0}'.format(str(hex(random.getrandbits(64))[:-1])))
	else:
		for _ in range(0, initialMemorySize*1024//4):
			asmFile.write('\n\t.word {0}'.format(str(hex(random.getrandbits(32))[:-1])))

	asmFile.write('''
.size	data, .-data
.section ".tohost","aw",@progbits
.align 4
.globl tohost
tohost: .word 0
.align 4
.globl fromhost
fromhost: .word 0
''')
	asmFile.close()
	print( '\nNumber of loops =' , cv.numberOfLoops, '   Elapsed time =', time.time()-start_time)


if __name__ == '__main__':
	if len(sys.argv)<3:
		print('aapg.py <seed> <filename>')
		exit(1)
	os.makedirs('build',exist_ok=True)
	choice = sys.argv[1]
	random.seed(choice)
	aapgMain('build/'+sys.argv[2])
	print('done! check build/{}'.format(sys.argv[2]))
