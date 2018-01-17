import sys 

file1 = open("./AAPG/output/input.hex","w")


dict = {'0000': '0','0001':'1','0010':'2','0011':'3','0100':'4','0101':'5','0110':'6','0111':'7','1000':'8','1001':'9','1010':'A','1011':'B','1100':'C','1101':'D','1110':'E','1111':'F'} 


for line in open("./AAPG/bin.s", 'r'):

	file1.write(dict[line[0:4]])
	file1.write(dict[line[4:8]])
	file1.write(dict[line[8:12]])
	file1.write(dict[line[12:16]])
	file1.write(dict[line[16:20]])
	file1.write(dict[line[20:24]])
	file1.write(dict[line[24:28]])
	file1.write(dict[line[28:32]])
	file1.write("\n")

