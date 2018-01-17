#Author: Nandu Raj P ( nndurj@gmail.com )	

from commonStuff import *
from collections import deque
import random
import commonVar as cv
import opcodes as op


branchLock=None
previousBranches=None
loopPositions=None
initializeLoopQueue=None

def initialize_vars():
	global branchLock, previousBranches, loopPositions, initializeLoopQueue

	branchLock=0
	previousBranches=deque([ 0 for i in range(100)])
	loopPositions=[{'lValue':0,'targetInstr':0,'instrPosition':0,'setBranch':True,'opcode':'NULL' ,'r1':0,'r2':0} for indx in range(maxNestedLoops)]
	initializeLoopQueue=[{'r1':0,'r2':0,'initialized':True,'instrPosition':0, 'loopCount':0} for indx in range(maxNestedLoops)]


def initializeLoop():
	global initializeLoopQueue, branchLock
	
	for index, memb in enumerate(initializeLoopQueue):
		if memb['initialized'] == False and memb['instrPosition'] < cv.instructionNumber+1:
			if random.randint(0,1)==0:
				randImmediate=random.randint(-2048,-1834)
			else:
				randImmediate=random.randint(1834,2047-memb['loopCount'])
			instr1 = 'addi x{0}, x0, {1}'.format(memb['r1'],randImmediate)
			instr2 = 'addi x{0}, x0, {1}'.format(memb['r2'],randImmediate+memb['loopCount'])
			branchLock=branchLock-1
			initializeLoopQueue[index]['initialized']=True
			return (instr1, instr2)
	return

def setBranchInstr():
	global loopPositions
	for indx, memb in enumerate(loopPositions):
		if memb['setBranch'] is False and memb['instrPosition'] <= cv.instructionNumber:
			instr1 = 'addi x'+str(memb['r1'])+' , x'+str(memb['r1']) + ' , 1'
			if memb['opcode'] in ('bne' , 'blt' , 'bltu'):
				instr2 = '{0} x{1}, x{2}, i_{3}'.format(memb['opcode'],memb['r1'],memb['r2'],memb['targetInstr'])
			else:
				instr2 = '{0} x{1}, x{2}, i_{3}'.format(memb['opcode'],memb['r2'],memb['r1'],memb['targetInstr'])
			loopPositions[indx]['setBranch']=True
			cv.branchRegisters.remove(memb['r1'])
			cv.branchRegisters.remove(memb['r2'])
			return (instr1, instr2)
	return


def gen_base_instr(opcode):
	global loopPositions, initializeLoopQueue, branchLock, initialMemorySize
	if opcode in op.base_i:
	# addi rd,rs1,imm
		j = find_rand_destination_reg()
		k = find_rand_source_reg()
		if opcode in ("slli" ,"srli" ,"srai"):
			if bitwidth==64:
				l=random.randint(1,4) #63
			else:
				l=random.randint(1,4)	#31
		elif opcode in ('slliw' ,'srliw' ,'sraiw'):
			l=random.randint(1,4) #31
		else:
			l=random.randint(-2048,2047)
		if opcode in ("jalr"):
			return
		if opcode in ("ld" ,"lb" ,"lh" ,"lw" ,"lbu" ,"lhu" ,'lwu'):
			memoryLimit=initialMemorySize*512-24
			l=random.randint(-memoryLimit,memoryLimit)
			if opcode in ('lh' ,'lhu'):
				l=l+l%2
			elif opcode in ('lw' ,'lwu'):
				if l > 0:
					l=l-(l%4)
				elif l < 0:
					l=-(abs(l)-(abs(l)%4))
			elif opcode=='ld':
				if l > 0:
					l=l-(l%8)
				elif l < 0:
					l=-(abs(l)-(abs(l)%8))

			Instruction ="{0} x{1}, {2}(x{3})".format(opcode, j, l, memoryBaseAddressRegister)
		else:
			Instruction = '{0} x{1}, x{2}, {3}'.format(opcode, j, k, l)
			
		return (Instruction,)

	elif opcode in op.base_u:
	#LUI rd,imm
		j=find_rand_destination_reg()
		l=random.randint(0,1048575)
		Instruction = '{0} x{1}, {2}'.format(opcode, j, l)
		return (Instruction,)


	elif opcode in op.base_uj:
	#JAL rd,imm
	#nandu
		return
		
		j=find_rand_destination_reg()
		#l=random.randint(0,1048575)
		l=random.randint(0,20)
		Instruction = opcode + " x" + str(j) + " , " + str(l)
		return (Instruction,)

	elif opcode in op.base_r:
	#ADD rd, rs1,rs2
		j = find_rand_destination_reg()
		k = find_rand_source_reg()
		l = find_rand_source_reg()
		if opcode in ('srl' ,'sra' ,'sll' ,'sllw' ,'srlw' ,'sraw'):
			l=find_rand_destination_reg()
			if bitwidth==64 and opcode not in ('sllw' ,'srlw' ,'sraw'):
				instructionTmp='addi x{0}, x0, {1}'.format(l,random.randint(1,63))
			else:
				instructionTmp='addi x{0}, x0, {1}'.format(l,random.randint(1,31))
			Instruction = '{0} x{1}, x{2}, x{3}'.format(opcode,j,k,l)
			return (instructionTmp,Instruction)
		else:
			Instruction = '{0} x{1}, x{2}, x{3}'.format(opcode,j,k,l)
			return (Instruction,)

	elif opcode in op.base_s:
	#SB rs1,rs2,imm
		memoryLimit=initialMemorySize*512-24
		j = find_rand_source_reg()
		l = random.randint(-memoryLimit,memoryLimit)
		if opcode == 'sh':
			l=l+l%2
		elif opcode =='sw':
			if l > 0:
				l=l-(l%4)
			elif l < 0:
				l=-(abs(l)-(abs(l)%4))
		elif opcode =='sd':
			if l > 0:
				l=l-(l%8)
			elif l < 0:
				l=-(abs(l)-(abs(l)%8))
		Instruction ="{0} x{1}, {2}(x{3})".format(opcode, j, l, memoryBaseAddressRegister) 
		return (Instruction,)

	elif opcode in op.base_sb:
	#BEQ s1,rs2,imm
		
		bType=random.random()
		if branchBackwardProbability>bType:
			freeSlot=False
			for memb in loopPositions:
				if memb['setBranch']==True:
					freeSlot=True
			if not freeSlot:
				return
			maxBranchPosition=loopRange+cv.instructionNumber
			for memb in loopPositions:
				if memb['instrPosition']-2<maxBranchPosition and memb['setBranch'] == False:
					maxBranchPosition=memb['instrPosition']-2
			
			if maxBranchPosition< cv.instructionNumber+6:
				return
			minBranchPosition=max((max(previousBranches)+4,cv.instructionNumber+4))
			
			if minBranchPosition >= maxBranchPosition:
				return
			branchInstrPosition = random.randint(minBranchPosition,maxBranchPosition)
			
			j = find_rand_destination_reg()
			cv.branchRegisters.append(j)
			k = find_rand_destination_reg()
			cv.branchRegisters.append(k)
			loopCount=random.randint(2,maxLoopIterations)
			branchTarget=minBranchPosition
			previousBranches.append(branchTarget)
			
			for index, memb in enumerate(initializeLoopQueue):
				if memb['initialized']==True:
					initializeLoopQueue[index]={'r1':j,'r2':k,'initialized':False,'instrPosition':branchTarget-2, 'loopCount':loopCount}
					branchLock+=1
					break
					
			branchInstructions=('bne ' ,'blt' ,'bge' ,'bltu' ,'bgeu')
			if opcode in ('beq'):
				opcode = branchInstructions[random.randint(0,4)]
			for indx, member in enumerate(loopPositions):
				if member['setBranch']==True:
					loopPositions[indx]={'lValue':branchInstrPosition-branchTarget,'targetInstr':branchTarget,'instrPosition':branchInstrPosition,
										'r1':j,'r2':k, 'opcode':opcode, 'setBranch':False}
					cv.numberOfLoops+=1
					break
		else:
			j = find_rand_source_reg()
			k = find_rand_source_reg()
			l = random.randint(1,forwardBranchRange)  #-2048 to 2047
			for branch in loopPositions:
				if branch['instrPosition'] in [l+cv.instructionNumber+indx for indx in range(-2,2)] or branchLock>0:
					return ('nop',)
			
			previousBranches.append(l+cv.instructionNumber)
			previousBranches.popleft()
			Instruction = '{0} x{1}, x{2}, i_{3}'.format(opcode, j, k, l+cv.instructionNumber)
			return (Instruction,)

	elif opcode in op.system_1:
	#FENCE
		return
		Instruction=opcode
		return (Instruction,)

	elif opcode in op.system_2:
	#RDCYCLE rd
		return
		k = find_rand_destination_reg()
		Instruction=opcode + " x" + str(k)
		return (Instruction,)

	
	else: 
		return
		
if __name__ == "__main__":
	print("Please Run aapg.py")
