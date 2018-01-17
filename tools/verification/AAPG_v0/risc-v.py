#!/usr/bin/python
import sys
import random
ps = 40
pagesize = ps*256
PC = 0
LJump = 0
file1 = open("./AAPG/output/assembly_newlib.s", "w")
file2 = open("assembly.s", "w")
iss_memory_init_file = open("./AAPG/output/memory_iss.txt","w")
rtl_memory_init_file = open("./AAPG/output/memory_dcache.txt","w")

for i in range(0,1048576):
    val = random.getrandbits(64)
    rtl_memory_init_file.write("%016x"%(val)+"\n")
    val1='{0:064b}'.format(val)
    j=0
    while(j<32):
        iss_memory_init_file.write(val1[j]+" ")
        j=j+1
    iss_memory_init_file.write("\n")


canregr = {'r0' : 'zero', 'r1' : 'ra', 'r2' : 'v0', 'r3' : 'v1', 'r4' : 'a0', 'r5' : 'a1', 'r6' : 'a2' , 'r7' : 'a3' , 'r8' : 'a4' , 'r9' : 'a5' , 'r10' : 'a6' ,'r11' : 'a7' , 'r12' : 't0', 'r13' : 't1', 'r14' : 't2', 'r15' : 't3', 'r16' : 't4', 'r17' : 't5', 'r18' : 't6', 'r19' : 't7', 'r20' : 's0', 'r21' : 's1', 'r22' : 's2', 'r23' : 's3', 'r24' : 's4', 'r25' : 's5', 'r26' : 's6', 'r27' : 's7', 'r28' : 's8', 'r29' : 's9', 'r30' : 'sp', 'r31' : 'tp' }

#Register 30 and 31 are reserved for branch instructions
#Functional definition of Opcode Checker 
def opcode_checker(opcode):

	global PC
	global LJump

	I_f 		= [line.strip() for line in open("./AAPG/R-I-S-U-grouping/I-f.txt", 'r')]
	I_r			= [line.strip() for line in open("./AAPG/R-I-S-U-grouping/I-r.txt", 'r')]	    
	O_f 		= [line.strip() for line in open("./AAPG/R-I-S-U-grouping/O-f.txt", 'r')]
	OI_f 		= [line.strip() for line in open("./AAPG/R-I-S-U-grouping/OI-f.txt", 'r')]
	OI_r 		= [line.strip() for line in open("./AAPG/R-I-S-U-grouping/OI-r.txt", 'r')]	    
	OI_rm_f    	= [line.strip() for line in open("./AAPG/R-I-S-U-grouping/OI-rm-f.txt", 'r')]
	O_r 		= [line.strip() for line in open("./AAPG/R-I-S-U-grouping/O-r.txt", 'r')]
	R4_rm_f		= [line.strip() for line in open("./AAPG/R-I-S-U-grouping/R4-rm-f.txt", 'r')]
	R_f 		= [line.strip() for line in open("./AAPG/R-I-S-U-grouping/R-f.txt", 'r')]	    
	R_r         = [line.strip() for line in open("./AAPG/R-I-S-U-grouping/R-r.txt", 'r')]
	R_rm_f 		= [line.strip() for line in open("./AAPG/R-I-S-U-grouping/R-rm-f.txt", 'r')]
	SB_r		= [line.strip() for line in open("./AAPG/R-I-S-U-grouping/SB-r.txt", 'r')]
	shamt_r		= [line.strip() for line in open("./AAPG/R-I-S-U-grouping/shamt-r.txt", 'r')]	    
	Solo        = [line.strip() for line in open("./AAPG/R-I-S-U-grouping/Solo.txt", 'r')]
	S_r 		= [line.strip() for line in open("./AAPG/R-I-S-U-grouping/S-r.txt", 'r')]
	UJ_r 		= [line.strip() for line in open("./AAPG/R-I-S-U-grouping/UJ-r.txt", 'r')]
	U_r         = [line.strip() for line in open("./AAPG/R-I-S-U-grouping/U-r.txt", 'r')]
	
	if opcode in R_r:
    		j = random.randint(1,29)
    		k = random.randint(1,29)
    		l = random.randint(1,29)
    		Instruction =   opcode + " "+canregr['r' + str(j)] +  ' ' + canregr['r' + str(k)] + ' '+canregr['r' + str(l)]
    		Instruction2 =   opcode + " "+'r'+ str(j) +  ' ' +'r' + str(k) + ' '+'r' + str(l)
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
    		return
	elif opcode in R_f:
    		j = random.randint(1,29)
    		k = random.randint(1,29)
    		l = random.randint(1,29)
    		Instruction =   opcode +" f"+ str(j) +" f"+ str(k)+" f"+ l
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction)
    		return
	elif opcode in I_r and opcode == "JALR" and pagesize-PC >128 and PC-LJump >216:
			LJump = PC
			j = random.randint(1,29)
			k = random.randint(1,29)
			l = getbits(7)
			if l == 0: 
				return	
			PC = PC + 2 
			PCbin = bin(PC*4)
			PCbinnew = '{0:032b}'.format(PC*4) 
			if len(PCbin)>13 and PCbin[-12]=='1' :
				PC = PC - 1; 
				return 
			elif len(PCbin)>13 and PCbin[-12]!='1' :
				Instruction = "AUIPC"+" "+canregr["r"+str(k)]+" "+" 00000000000000000000" 
				Instruction2 = "AUIPC"+" "+"r"+str(k)+" "+" 00000000000000000000" 
				file1.write("%s\n" % Instruction)
				file2.write("%s\n" % Instruction2)
				Instruction = "ADDI " + canregr['r' + str(k)] +" " + canregr['r' + str(k)] + " " + PCbinnew[-12:]
				Instruction2 = "ADDI " + 'r' + str(k) +" " + 'r' + str(k) + " " + PCbinnew[-12:]
				file1.write("%s\n" % Instruction)
				file2.write("%s\n" % Instruction2)
			else : 
				Instruction = "AUIPC"+" "+canregr["r"+str(k)]+" 00000000000000000000"
				Instruction2 = "AUIPC"+" "+"r"+str(k)+" 00000000000000000000"
				file1.write("%s\n" % Instruction)
				file2.write("%s\n" % Instruction2)
				i = len(PCbin) 
				Instruction = "ADDI " + canregr['r' + str(k)] +" " + canregr['r' + str(k)] + " " + PCbinnew[-12:]
				Instruction2 = "ADDI " + 'r' + str(k) +" " + 'r' + str(k) + " " + PCbinnew[-12:]
				file1.write("%s\n" % Instruction)
				file2.write("%s\n" % Instruction2)
			Instruction =   opcode +' '+canregr['r' + str(j)] + ' '+canregr['r' + str(k)] +" 000"+str(l)+"00"
			Instruction2 =   opcode +' '+'r' + str(j) + ' '+'r' + str(k) +" 000"+str(l)+"00"
			file1.write("%s\n" % Instruction)
			file2.write("%s\n" % Instruction2)
			return
	elif opcode in I_r and opcode != "JALR":
    		value=random.randint(0,522240)
    		j = random.randint(1,29)
    		k = random.randint(1,29)
    		pvalue = value if value < 2047 else 2047 
    		valueimm = random.randint(-pvalue, pvalue) 
    		l=format(valueimm if valueimm >= 0 else (1<<12) + valueimm, '012b')
    		list1=list(l)
    		if(opcode=="LD"):
                    list1[11]='0'
                    list1[10]='0'
                    list1[9]='0'
                    l="".join(list1)
    		if(opcode=="LW" or opcode=="LWU"):
                    list1[11]='0'
                    list1[10]='0'
                    l="".join(list1)
    		if(opcode=="LH"):
                    list1[11]='0'
                    l="".join(list1)
    		if(opcode=="LHU"):
                    list1[11]='0'
                    l="".join(list1)
    		if((opcode=="LB" or opcode=="LBU") and list1[9]==1 and list1[10]==1 and list1[11]==1):
                    list1[11]='0'
                    l="".join(list1)
    		PC = PC + 2
    		bin_value='{0:020b}'.format(value)
    		Instruction = "LUI " + canregr['r' + str(k)] +" "+bin_value
    		Instruction2 = "LUI " + 'r' + str(k) +" "+bin_value 
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
    		Instruction = "SRLI " + canregr['r' + str(k)] + ' '+canregr['r' + str(k)] +" "+'001001'
    		Instruction2 = "SRLI " + 'r' + str(k) + ' '+'r' + str(k) +" "+'001001'
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
    		
    		Instruction =   opcode +' '+canregr['r' + str(j)] + ' '+canregr['r' + str(k)] +" "+str(l) 
    		Instruction2 =   opcode +' '+'r' + str(j) + ' '+'r' + str(k) +" "+str(l) 
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
    		return
	elif opcode in I_f:
    		j = random.randint(1,29)
    		k = random.randint(1,29)
    		l = getbits(12)
    		Instruction =   opcode +" f"+ str(j) +" f"+ str(k)+" "+str(l) 
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction)
    		return
	elif opcode in S_r:
    		value=random.randint(0,522240)
    		j = random.randint(1,29)
    		k = random.randint(1,29)
    		pvalue = value if value < 2047 else 2047 
    		valueimm = random.randint(-pvalue, pvalue) 
    		l=format(valueimm if valueimm >= 0 else (1<<12) + valueimm, '012b')
    		list1=list(l)
    		#s = repr(value) + '   ' + repr(valueimm) + '    ' + repr(l)
    		#print s
    		if(opcode=="SD"):
    		    list1[11]='0'
    		    list1[10]='0'
    		    list1[9]='0'
    		    l="".join(list1)
    		if(opcode=="SW"):
    		    list1[11]='0'
    		    list1[10]='0'
    		    l="".join(list1)
    		elif(opcode=="SH"):
    		    list1[11]='0'
    		    l="".join(list1)
    		elif(opcode=="SB" and list1[9]==1 and list1[10]==1 and list1[11]==1):
    		    list1[11]='0'
    		    l="".join(list1)
    		PC = PC + 2
    		bin_value='{0:020b}'.format(value)
    		Instruction = "LUI " + canregr['r' + str(j)] +" "+bin_value
    		Instruction2 = "LUI " + 'r' + str(j) +" "+bin_value 
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
    		Instruction = "SRLI " + canregr['r' + str(j)] + ' '+canregr['r' + str(j)] +" "+'001001'
    		Instruction2 = "SRLI " + 'r' + str(j) + ' '+'r' + str(j) +" "+'001001'
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
    		Instruction =   opcode +' '+canregr['r' + str(j)] + ' '+canregr['r' + str(k)] +" "+str(l) 
    		Instruction2 =   opcode +' '+'r' + str(j) + ' '+'r' + str(k) +" "+str(l) 
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
    		return
	elif opcode in SB_r and pagesize - PC > 128 and opcode == "BNE" and PC-LJump >216:
    		#j = random.randint(1,29)
    		#k = random.randint(1,29)
    		LJump = PC
    		l = getbits(6)
		if l == "000000":
			return
    		Instruction =   opcode +' '+canregr['r' + "30"] + ' '+canregr['r' + "31"] +" 0000"+str(l)+"00" 
    		Instruction2 =   opcode +' '+'r' + "30" + ' '+'r' + "31" +" 0000"+str(l)+"00" 
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
    		return
	elif opcode in SB_r and PC-LJump>216 and opcode != "BNE":
    		#j = random.randint(1,29)
		#k = random.randint(1,29)
		LJump = PC
		branch(opcode)
		Instruction = "LUI sp 00000000000000000000"
		Instruction2 = "LUI r30 00000000000000000000"
		file1.write("%s\n" % Instruction)
		file2.write("%s\n" % Instruction2)
		Instruction = "LUI tp 00000000000000000000"
		Instruction2 = "LUI r31 00000000000000000000"
		file1.write("%s\n" % Instruction)
		file2.write("%s\n" % Instruction2)
		Instruction = "ADDI tp tp 000000000101"
		Instruction2 = "ADDI r31 r31 000000000101"
		file1.write("%s\n" % Instruction)
		file2.write("%s\n" % Instruction2)
		PC = PC + 3
		
		return
	elif opcode in U_r:
    		j = random.randint(1,29)
    		l = getbits(20)
    		Instruction =   opcode +' '+canregr['r' + str(j)] +" "+str(l)
    		Instruction2 =   opcode +' '+'r' + str(j) +" "+str(l)
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
    		return
	elif opcode in UJ_r and pagesize-PC > 128 and PC-LJump >216:
    		LJump = PC
    		j = getbits(6) + "0"
		if j == "0000000":	
			return	
    		k = random.randint(1,29)
    		Instruction =   opcode +' '+canregr['r' + str(k)] +" 0000000000000"+ str(j) 
    		Instruction2 =   opcode +' '+'r' + str(k) +" 0000000000000"+ str(j) 
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
    		return
	elif opcode in Solo:
    		Instruction =   opcode 
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction)
    		return
	elif opcode in O_r:
    		j = random.randint(1,29)
    		Instruction =   opcode +' '+canregr['r' + str(j)]
    		Instruction2 =   opcode +' '+canregr['r' + str(j)]
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
    		return
	elif opcode in O_f:
    		j = random.randint(1,29)
    		Instruction =   opcode +" f"+ str(j)
    		file1.write("%s\n" % Instruction)
    		return
	elif opcode in OI_r:
    		j = random.randint(1,29)
    		k = random.randint(1,29)
    		Instruction =   opcode +' '+canregr['r' + str(j)]+' '+canregr['r' + str(k)]
    		Instruction =   opcode +' '+'r' + str(j)+' '+'r' + str(k)
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
    		return
	elif opcode in OI_f:
    		j = random.randint(1,29)
    		k = random.randint(1,29)
    		Instruction =   opcode +" f"+ str(j)+" f"+str(k)
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction)
    		return
	elif opcode in shamt_r:
		j = random.randint(1,29)
		k = random.randint(1,29)
		l = getbits(6)
		if opcode in {'SLLI', 'SRLI', 'SRAI'}:
			Instruction =   opcode +' '+canregr['r' + str(j)] + ' '+canregr['r' + str(k)] +" "+str(l) 
			Instruction2 =   opcode +' '+'r' + str(j) + ' '+'r' + str(k) +" "+str(l) 
		else :
			Instruction =   opcode +' '+canregr['r' + str(j)] + ' '+canregr['r' + str(k)] +" 0"+str(l[0:5]) 
			Instruction2 =   opcode +' '+'r' + str(j) + ' '+'r' + str(k) +" 0"+str(l[0:5]) 
		file1.write("%s\n" % Instruction)
		file2.write("%s\n" % Instruction2)
		return
	elif opcode in R_rm_f:
    		j = random.randint(1,29)
    		k = random.randint(1,29)
    		l = random.randint(1,29)
		o = getbits(3)
		Instruction =   opcode +" f"+ str(j) +" f"+ str(k)+" f"+str(l) + " " + str(o)
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction)
		return
	elif opcode in R4_rm_f:
    		j = random.randint(1,29)
    		k = random.randint(1,29)
    		l = random.randint(1,29)
		o = random.randint(1,29)
		p = getbits(3)
		Instruction =   opcode +" f"+ str(j) +" f"+ str(k)+" f"+str(l) + " f" + str(o) + " " + str(p)
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction)
		return
	elif opcode in OI_rm_f:
    		j = random.randint(1,29)
    		k = random.randint(1,29)
    		o = getbits(3)
		Instruction =   opcode +" f"+ str(j) +" f"+ str(k)+ " " + str(o)
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction)
		return
	else :
    		Instruction =   "ADDI zero zero 000000000000"
    		Instruction2 =   "ADDI zero zero 000000000000"
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)


def getbits(d ) :
    x =""
    for a in range (0,d): 
        x = x + str(random.randint(0,1))
    return x

def branch(opcode):
	global PC
	if opcode == "BEQ":
		Instruction = "ADDI"+" "+canregr["r30"]+" "+canregr["r30"]+" 000000000001"
		Instruction2 = "ADDI"+" "+"r30"+" "+"r30"+" 000000000001"
		file1.write("%s\n" % Instruction)
		file2.write("%s\n" % Instruction2)
		PC =PC +1
		l = getbits(6)
		Instruction =   opcode +' '+canregr['r' + "30"] + ' '+canregr['r' + "31"] +" 1111"+str(l)+"00" 
		Instruction2 =   opcode +' '+'r' + "30" + ' '+'r' + "31" +" 1111"+str(l)+"00" 
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
	elif opcode == "BLT" or opcode == "BLTU" :
		Instruction = "ADDI"+" "+canregr["r30"]+" "+canregr["r30"]+" 000000000001"
		Instruction2 = "ADDI"+" "+"r30"+" "+"r30"+" 000000000001"
		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
		PC =PC +1
		l = getbits(6)
		Instruction =   opcode +' '+canregr['r' + "30"] + ' '+canregr['r' + "31"] +" 1111"+str(l)+"00" 
		Instruction2 =   opcode +' '+'r' + "30" + ' '+'r' + "31" +" 1111"+str(l)+"00" 
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
	elif opcode == "BGE" or opcode == "BGEU" :
		Instruction = "ADDI"+" "+canregr["r30"]+" "+canregr["r30"]+" 000000000001"
		Instruction2 = "ADDI"+" "+"r30"+" "+"r30"+" 000000000001"
		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
		PC = PC+1
		l = getbits(6)
		Instruction =   opcode +' '+canregr['r' + "31"] + ' '+canregr['r' + "30"] +" 1111"+str(l)+"00" 
		Instruction2 =   opcode +' '+'r' + "31" + ' '+'r' + "30" +" 1111"+str(l)+"00" 
    		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
    


#Main Program Starts Here
def random_ASM_generator(choice):

	global PC
	global pagesize
	global ps

#Inputs for distribution of different instructions classes in the 4KB/8KB instruction page
	list1 =[ ]
	per1    = 40 
	list1.append(per1)
	per2    = 15 
	list1.append(per2)
	per3    = 25 
	list1.append(per3)
	per4    = 20 
	list1.append(per4)
	per5    = 0
	list1.append(per5)
	per6    = 0
	list1.append(per6)
	per7    = 0
	list1.append(per7)
	per8    = 0
	list1.append(per8)
	per9    = 0
	list1.append(per9)
	per10    = 0
	list1.append(per10)
	per11    = 0
	list1.append(per11)
	per12    = 0
	list1.append(per12)
	per13    = 0
	list1.append(per13)
	per14    = 0
	list1.append(per14)
	per15    = 0
	list1.append(per15)
	per16    = 0
	list1.append(per16)
	per17    = 0
	list1.append(per17)
	per	= per1 + per2 + per3 + per4 + per5 + per6 + per7 + per8 + per9 + per10 + per11 + per12 + per13 + per14 + per15 + per16 + per17

	if per > 100:
		sys.exit("Bye ! Next Time please provide correct inputs aggregating 100 or less")

	

#Calculating Actual Number of Instructions in a page as per input percentage distributions
	num1 = pagesize*per1/100
#print num1
	num2 = pagesize*per2/100
#print num2
	num3 = pagesize*per3/100
#print num3
	num4 = pagesize*per4/100
#print num4
	num5 = pagesize*per5/100
#print num5
	num6 = pagesize*per6/100
#print num6
	num7 = pagesize*per7/100
#print num7
	num8 = pagesize*per8/100
#print num8
	num9 = pagesize*per9/100
#print num9
	num10 = pagesize*per10/100
#print num10
	num11 = pagesize*per11/100
#print num11
	num12 = pagesize*per12/100
#print num12
	num13 = pagesize*per13/100
#print num13
	num14 = pagesize*per14/100
	num15 = pagesize*per15/100
	num16 = pagesize*per16/100
	num17 = pagesize*per17/100

	num	= num1 + num2 + num3 + num4 + num5 + num6 + num7 + num8 + num9 + num10 + num11 + num12 + num13 + num14 + num15 + num16 + num17
#print num

#Calculating number of nop required
	if num < pagesize:
		num18 = pagesize - num
	else:
		num18 = 0
#print num14

	octyp1 = [line.strip() for line in open("./AAPG/category_grouping/RV32I-Arth-Logi-Comp.txt", 'r')]
	size1 = len(octyp1)
	octyp2 = [line.strip() for line in open("./AAPG/category_grouping/RV32I-Branch-Counter-Jumplink-Shift.txt", 'r')]
	size2 = len(octyp2)
	octyp3 = [line.strip() for line in open("./AAPG/category_grouping/RV32I-Loads-Stores-Sync_system.txt", 'r')]
	size3 = len(octyp3)
	octyp4 = [line.strip() for line in open("./AAPG/category_grouping/RV64I.txt", 'r')]
	size4 = len(octyp4)
	octyp5 = [line.strip() for line in open("./AAPG/category_grouping/RV32M.txt", 'r')]
	size5 = len(octyp5)
	octyp6 = [line.strip() for line in open("./AAPG/category_grouping/RV64M.txt", 'r')]
	size6 = len(octyp6)
	octyp7 = [line.strip() for line in open("./AAPG/category_grouping/RV32A.txt", 'r')]
	size7 = len(octyp7)
	octyp8 = [line.strip() for line in open("./AAPG/category_grouping/RV64A.txt", 'r')]
	size8 = len(octyp8)
	octyp9 = [line.strip() for line in open("./AAPG/category_grouping/RV32F-Arth-mulladd.txt", 'r')]
	size9 = len(octyp9)
	octyp10 = [line.strip() for line in open("./AAPG/category_grouping/RV32F-Config.txt", 'r')]
	size10 = len(octyp10)
	octyp11 = [line.strip() for line in open("./AAPG/category_grouping/RV32F-load-store-move-covert.txt", 'r')]
	size11 = len(octyp11)
	octyp12 = [line.strip() for line in open("./AAPG/category_grouping/RV32F-load-store-move-covert.txt", 'r')]
	size12 = len(octyp12)
	octyp13 = [line.strip() for line in open("./AAPG/category_grouping/RV64F.txt", 'r')]
	size13 = len(octyp13)
	octyp14 = [line.strip() for line in open("./AAPG/category_grouping/RV32D-Arth-mulladd.txt", 'r')]
	size14 = len(octyp14)
	octyp15 = [line.strip() for line in open("./AAPG/category_grouping/RV32D-load-store-move-covert.txt", 'r')]
	size15 = len(octyp15)
	octyp16 = [line.strip() for line in open("./AAPG/category_grouping/RV32D-Sign-minmax-compare.txt", 'r')]
	size16 = len(octyp16)
	octyp17 = [line.strip() for line in open("./AAPG/category_grouping/RV64D.txt", 'r')]
	size17 = len(octyp17)

	#choice = int(raw_input("Please provide the seed for PRNG: "))
	random.seed(choice)

	count = 0
	Instruction = "LUI sp 00000000000000000000"
	Instruction2 = "LUI r30 00000000000000000000"
	file1.write("%s\n" % Instruction)
	file2.write("%s\n" % Instruction2)
	Instruction = "LUI tp 00000000000000000000"
	Instruction2 = "LUI r31 00000000000000000000"
	file1.write("%s\n" % Instruction)
	file2.write("%s\n" % Instruction2)
	Instruction = "ADDI tp tp 000000000101"
	Instruction2 = "ADDI r31 r31 000000000101"
	file1.write("%s\n" % Instruction)
	file2.write("%s\n" % Instruction2)
	PC = PC + 3
	index=1
        while(index<30):
            value=random.randint(0,1048576)
            bin_value='{0:020b}'.format(value)
            Instruction = "LUI " + canregr['r' + str(index)] +" "+bin_value
            Instruction2 = "LUI " + 'r' + str(index) +" "+bin_value 
            file1.write("%s\n" % Instruction)
            file2.write("%s\n" % Instruction2)
            index=index+1
            PC=PC+1

	while (count <= (pagesize - 1 - num18)):
 		i= random.randint(0,16)
		if  list1[i] != 0:
			if ((i == 0) and (num1 > 0)):
				j=random.randint(0,size1-1)
				opcode_checker(octyp1[j])
				PC = PC + 1
				num1=num1-1
				count=count+1
			elif ((i == 1) and (num2 > 0)):
				j=random.randint(0,size2-1)
				opcode_checker(octyp2[j])
				PC = PC + 1
				num2=num2-1
				count=count+1
			elif ((i == 2) and (num3 > 0)):
				j=random.randint(0,size3-1)
				opcode_checker(octyp3[j])
				PC = PC + 1
				num3=num3-1
				count=count+1
			elif ((i == 3) and (num4 > 0)):
				j=random.randint(0,size4-1)
				opcode_checker(octyp4[j])
				PC = PC + 1
				num4=num4-1
				count=count+1
			elif ((i == 4) and (num5 > 0)):
				j=random.randint(0,size5-1)
				opcode_checker(octyp5[j])
				PC = PC + 1
				num5=num5-1
				count=count+1
			elif ((i == 5) and (num6 > 0)):
				j=random.randint(0,size6-1)
				opcode_checker(octyp6[j])
				PC = PC + 1
				num6=num6-1
				count=count+1
			elif ((i == 6) and (num7 > 0)):
				j=random.randint(0,size7-1)
				opcode_checker(octyp7[j])
				PC = PC + 1
				num7=num7-1
				count=count+1
			elif ((i == 7) and (num8 > 0)):
				j=random.randint(0,size8-1)
				opcode_checker(octyp8[j])
				PC = PC + 1
				num8=num8-1
				count=count+1
			elif ((i == 8) and (num9 > 0)):
				j=random.randint(0,size9-1)
				opcode_checker(octyp9[j])
				PC = PC + 1
				num9=num9-1
				count=count+1
			elif ((i == 9) and (num10 > 0)):
				j=random.randint(0,size10-1)
				opcode_checker(octyp10[j])
				PC = PC + 1
				num10=num10-1
				count=count+1
			elif ((i == 10) and (num11 > 0)):
				j=random.randint(0,size11-1)
				opcode_checker(octyp11[j])
				PC = PC + 1
				num11=num11-1
				count=count+1
			elif ((i == 11) and (num12 > 0)):
				j=random.randint(0,size12-1)
				opcode_checker(octyp12[j])
				PC = PC + 1
				num12=num12-1
				count=count+1
			elif ((i == 12) and (num13 > 0)):
				j=random.randint(0,size13-1)
				opcode_checker(octyp13[j])
				PC = PC + 1
				num13=num13-1
				count=count+1  
			elif ((i == 13) and (num14 > 0)):
				j=random.randint(0,size14-1)
				opcode_checker(octyp14[j])
				PC = PC + 1
				num14=num14-1
				count=count+1  
			elif ((i == 14) and (num15 > 0)):
				j=random.randint(0,size15-1)
				opcode_checker(octyp15[j])
				PC = PC + 1
				num15=num15-1
				count=count+1  
			elif ((i == 15) and (num16 > 0)):
				j=random.randint(0,size16-1)
				opcode_checker(octyp16[j])
				PC = PC + 1
				num16=num16-1
				count=count+1  
			elif ((i == 16) and (num17 > 0)):
				j=random.randint(0,size17-1)
				opcode_checker(octyp17[j])
				PC = PC + 1 
				num17=num17-1
				count=count+1  


	while ( num18 > 0):
		Instruction =   "ADDI zero zero 000000000000"
		Instruction2 =   "ADDI r0 r0 000000000000"
		num18 = num18 - 1
		PC = PC + 1
		file1.write("%s\n" % Instruction)
    		file2.write("%s\n" % Instruction2)
	return
#ps = int(raw_input("Please provide the page size in KB: "))

#if __name__ == '__main__':
#	try:
#		choice = int(raw_input("Please enter the seed value for Random generator :"))
#
#	except ValueError:
#		print "Not a number"
#
choice = random.randint(1,10000)
random_ASM_generator(choice)
#file1.write("Illegal")
#file2.write("Illegal")
file2.close()
file1.close()
