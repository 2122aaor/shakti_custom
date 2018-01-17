#!/usr/bin/env python3

#Author: Nandu Raj P ( nndurj@gmail.com )	

import re

def parseObjectFile(fileName,bitwidth,codeFileName='code.hex',memoryFileName='memory.hex'):
	
	textStart=0
	memoryStart=0
	codeOut=open(codeFileName,'w')
	memoryOut=open(memoryFileName,'w')
	codeInitialSection=False
	memoryFillZeros=False
	currentMemoryAddress=0
	memoryStartAddress=0
	memoryEndAddress=0
	memoryPreviousAddress=0
	lineSplits=[]
	with open(fileName) as inFile:
		for line in inFile:
			if 'Contents of section' in line:
				if '.text' in line:
					textStart=1
				elif 'debug' not in line and 'comment' not in line:
					memoryStart=1
					textStart=2
					memoryFillZeros=True
					if currentMemoryAddress>0:
						currentMemoryAddress=int(lineSplits[0],16)+(len(lineSplits)-2)*4
				else:
					memoryStart=2
					memoryEndAddress=int(lineSplits[0],16)+(len(lineSplits)-2)*4
					break
				continue
			
			if textStart==1:
				lineSplits=line.split()
				if codeInitialSection == False:
					for cnt in range(int(lineSplits[0],16)//4):
						codeOut.write('00000013\n')
					codeInitialSection=True
				data=lineSplits[1:5]
				for instr in data:
					correctInstr=re.sub(r'(..)(..)(..)(..)',r'\4\3\2\1',instr)+'\n'
					if '0000006f' in correctInstr:
						codeOut.write(correctInstr)
						codeOut.close()
						textStart=2
						break
					codeOut.write(correctInstr)
			
			if memoryStart==1:
				lineSplits=line.split()
				if memoryFillZeros:
					currentLineAddress=int(lineSplits[0],16)
					if bitwidth==64:
						for cnt in range((currentLineAddress-currentMemoryAddress)//8):
							memoryOut.write('0000000000000000\n')
					else:
						for cnt in range((currentLineAddress-currentMemoryAddress)//4):
							memoryOut.write('00000000\n')
					memoryFillZeros=False
					if currentMemoryAddress==0:
						memoryStartAddress=currentLineAddress
					currentMemoryAddress=currentLineAddress
				if len(lineSplits)>4:
					data = [ lineSplits[1]+lineSplits[2],lineSplits[3]+lineSplits[4]]
				else:
					data = [ lineSplits[1]+lineSplits[2]]
				for dat in data:
					correctData=re.sub(r'(..)(..)(..)(..)(..)(..)(..)(..)',r'\8\7\6\5\4\3\2\1',dat)
					if bitwidth==32:
						memoryOut.write(correctData[8:]+'\n')
						memoryOut.write(correctData[:8]+'\n')
					else:
						memoryOut.write(correctData+'\n')
	memoryOut.close()
	return {'memoryStartAddress':memoryStartAddress,'memoryEndAddress':memoryEndAddress}
				
					
if __name__ == "__main__":
	import sys
	if len(sys.argv)<2:
		print('Format : parseObjdump.py <filename>  <bitwidth>')
		exit(1)
	print (parseObjectFile(sys.argv[1],int(sys.argv[2])))
				
			
			
