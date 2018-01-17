#Author: Nandu Raj P ( nndurj@gmail.com )	

from config import *
import opcodes as op
import commonVar as cv
import random
from collections import deque

cv.unusedRegs+=[memoryBaseAddressRegister]

previousIntegerDestinations=deque()
previousIntegerSources=deque()
def find_rand_destination_reg():
	"""Find a random integer register that is writable"""
	
	global previousIntegerDestinations
	prob=random.uniform(0,1)
	
	# Write after write
	if previousIntegerDestinations and prob<writeAfterWrite:
		suitableNums = [ n for n in previousIntegerDestinations if n not in cv.unusedRegs+cv.branchRegisters]
		if suitableNums:
			num=random.choice(suitableNums)
			return num
	
	# Write after read
	elif previousIntegerSources and prob<(writeAfterWrite+writeAfterRead):
		suitableNums = [ n for n in previousIntegerSources if n not in cv.unusedRegs+cv.branchRegisters]
		if suitableNums:
			num=random.choice(suitableNums)
			previousIntegerDestinations.append(num)
			while len(previousIntegerDestinations)>numberOfPreviousRegistersToConsider:
				previousIntegerDestinations.popleft()
			return num
	reg_int=random.randint(1,31)
	while reg_int in cv.unusedRegs+cv.branchRegisters:
		reg_int=random.randint(1,31)
	previousIntegerDestinations.append(reg_int)
	return reg_int
	
def find_rand_source_reg():
	"""Find random source register based on readAfterWrite probability"""
	prob=random.uniform(0,1)
	while len(previousIntegerSources)>numberOfPreviousRegistersToConsider:
		previousIntegerSources.popleft()
	if prob<readAfterWrite and previousIntegerDestinations:
		num=random.choice(previousIntegerDestinations)
	else:
		num=random.randint(1,31)
	previousIntegerSources.append(num)
	return num
	
	
'''
float_regs=( "fs0", "fs1",  "fs2",  "fs3",  "fs4",  "fs5",  "fs6",  "fs7",\
  "fs8", "fs9", "fs10", "fs11", "fs12", "fs13", "fs14", "fs15",\
  "fv0", "fv1", "fa0",   "fa1",  "fa2",  "fa3",  "fa4",  "fa5",\
  "fa6", "fa7", "ft0",   "ft1",  "ft2",  "ft3",  "ft4",  "ft5")
'''

float_regs=( "f0", "f1",  "f2",  "f3",  "f4",  "f5",  "f6",  "f7",\
  "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15",\
  "f16", "f17", "f18",   "f19",  "f20",  "f21",  "f22",  "f23",\
  "f24", "f25", "f26",   "f27",  "f28",  "f29",  "f30",  "f31")
  
int_regs=("x0", "x1",  "x2",  "x3",  "x4",  "x5",  "x6",  "x7",\
  "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",\
  "x16", "x17", "x18",   "x19",  "x20",  "x21",  "x22",  "x23",\
  "x24", "x25", "x26",   "x27",  "x28",  "x29",  "x30",  "x31")
  
previousFloatDestinationRegisters=deque()
previousFloatSourceRegisters=deque()
 
def find_rand_float_reg_destination():
	'''return a random writable FP register based on data hazard probabilities'''
	
	global previousFloatDestinationRegisters
	prob=random.uniform(0,1)
	
	while len(previousFloatDestinationRegisters)>numberOfPreviousRegistersToConsider:
		previousFloatDestinationRegisters.popleft()
	# Write after write
	if previousFloatDestinationRegisters and prob<writeAfterWrite:
		freg=random.choice(previousFloatDestinationRegisters)
		return freg
	
	
	# Write after read
	if previousFloatSourceRegisters and writeAfterWrite<prob<writeAfterWrite+writeAfterRead:
		freg=random.choice(previousFloatSourceRegisters)
		previousFloatDestinationRegisters.append(freg)
		return freg
	
	freg=random.choice(float_regs)
	previousFloatDestinationRegisters.append(freg)
	return freg

def find_rand_float_reg_source():
	'''find a random FP source register based on readAfterWrite probability'''
	
	global previousFloatSourceRegisters
	prob=random.uniform(0,1)
	
	while len(previousFloatSourceRegisters)>numberOfPreviousRegistersToConsider:
		previousFloatSourceRegisters.popleft()
	if prob<readAfterWrite and previousFloatDestinationRegisters:
		freg=random.choice(previousFloatDestinationRegisters)
		previousFloatSourceRegisters.append(freg)
		return freg
	
	freg=random.choice(float_regs)
	previousFloatSourceRegisters.append(freg)
	return freg
		

if __name__ == "__main__":
	print("Please Run aapg.py")

