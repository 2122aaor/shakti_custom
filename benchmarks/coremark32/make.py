#!/usr/bin/env python3
import subprocess
import sys
import os
import parseObjdump

bitwidth=32
iterations=1
if sys.version_info<(3,):
	sys.stdout.write("\n\nRequires python3 !!!\nExiting\n\n")
	sys.exit(1)

try:
	riscv_path = subprocess.check_output('which riscv64-unknown-elf-gcc',shell=True,stderr=None)
	riscv_path = riscv_path.decode('utf-8').replace('/bin/riscv64-unknown-elf-gcc','').strip()
except:
	print("\n\nPlease install RISCV toolchain with the modified spike\n\n")
	exit(1)


sources = ('core_list_join.c', 'core_main.c', 'core_matrix.c', 'core_state.c', 'core_util.c', 'core_portme.c','ee_printf.c', 'crt.S', 'endProgram.S')
cc = 'riscv64-unknown-elf-gcc'
ld = 'riscv64-unknown-elf-ld'
objdump = 'riscv64-unknown-elf-objdump'
cc_flags= '-c -I. -DITERATIONS='+str(iterations)+' -march=RV32IMAFD -static  -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf'
#cc_flags= '-c -I. -static  -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf'
linker_script='linkerScript.ld'
ld_flags='-static -nostdlib -nostartfiles -melf32lriscv'
#ld_flags='-static -nostdlib -nostartfiles '
ld_endFlags='-L{0}/riscv32-unknown-elf/lib -lc'.format(riscv_path)
os.makedirs('build',exist_ok=True)

def shellRun(command):
	print(command)
	subprocess.call(command,shell=True)

if len(sys.argv)>1:
	target=sys.argv[1]
else:
	target='all'

if target in ['clean', 'all']:
	print('rm build/* ')
	subprocess.call('rm build/* 2>/dev/null',shell=True)

if target in ['compile', 'all']:
	for source in sources:
		command='{} {} -o build/{}.o {}'.format(cc,cc_flags,source.split('.')[0],source)
		shellRun(command)

if target in ['all', 'link']:
	objects_files=['build/'+el.split('.')[0]+'.o' for el in sources]
	command='{} {} {} -o build/coremark.bin -T {} {}'.format(ld, ld_flags, ' '.join(objects_files), linker_script, ld_endFlags)
	shellRun(command)

if target in ['all']:
	command='{} -s build/coremark.bin > build/objdump.txt'.format(objdump)
	shellRun(command)
	command='{} -D build/coremark.bin > build/disassembled.txt'.format(objdump)
	shellRun(command)
	command='elf2hex 8 32768 build/coremark.bin > temphex'
	shellRun(command)
	command='./gen_usable_hex.py'
	shellRun(command)
	command='mv code.hex build/'
	shellRun(command)
	command='mv temphex build/'
	shellRun(command)
