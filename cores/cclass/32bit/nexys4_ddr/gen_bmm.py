import sys
import re

# check arguments
if len(sys.argv) != 5:
    print("Wrong arguments\nbmm_gen in out bus-width mem-size")
    exit()

# read the ramb search result
f = open(sys.argv[1], "r")
lines = f.readlines()
f.close()
count=0
rams = []

for i, line in enumerate(lines):
	if('RAMB36' in line):
		
		ram_match=lines[i-2].rsplit('\n')[0]
		line=lines[i].split()
		line=line[3].split('_')
		loc=line[1]
		rams.append((count,ram_match,loc))
		count=count+1
if int(sys.argv[3]) % len(rams) != 0:
    print("Cannot divide memory bus evenly into BRAMs!")
    exit()

rams=sorted(rams,reverse=True);

DW = int(sys.argv[3]) / len(rams)
MS = "%#010x"%(int(sys.argv[4])-1)

f = open(sys.argv[2], "w")
f.write('ADDRESS_SPACE dmem_memory RAMB32 [0x00000000:{0}]\n'.format(MS))
f.write("  BUS_BLOCK\n")
for r in rams:
    f.write('    {0} [{1}:{2}] LOC = {3};\n'.format(r[1], r[0]*DW+DW-1,r[0]*DW, r[2]))
f.write("  END_BUS_BLOCK;\n")
f.write("END_ADDRESS_SPACE;\n")
f.close()
