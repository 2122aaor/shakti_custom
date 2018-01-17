#!/usr/bin/env python3
import subprocess
import sys
import os
import parseObjdump

bitwidth=32

if sys.version_info<(3,):
	sys.stdout.write("\n\nRequires python3 !!!\nExiting\n\n")
	sys.exit(1)

try:
	riscv_path = subprocess.check_output('which riscv64-unknown-elf-gcc',shell=True,stderr=None)
	riscv_path = riscv_path.decode('utf-8').replace('/bin/riscv64-unknown-elf-gcc','').strip()
except:
	print("\n\nPlease install RISCV toolchain with the modified spike\n\n")
	exit(1)

sources = ( 'dhrystone.c', 'dhrystone_main.c', 'endProgram.S', 'crt.S', 'ee_printf.c' )
cc = 'riscv64-unknown-elf-gcc'
ld = 'riscv64-unknown-elf-ld'
objdump = 'riscv64-unknown-elf-objdump'
linker_script='linkerScript.ld'
if(bitwidth==32):
	ld_endFlags='-L{0}/riscv32-unknown-elf/lib -lc'.format(riscv_path)
	cc_flags= '-c -I. -march=RV32IMAFD -static  -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf'
	ld_flags='-static -nostdlib -nostartfiles -melf32lriscv'
else:
	cc_flags= '-c -I. -static  -std=gnu99 -O2 -faggressive-loop-optimizations -ffast-math -fno-common -fno-builtin-printf'
	ld_flags='-static -nostdlib -nostartfiles '
	ld_endFlags='-L{0}/riscv64-unknown-elf/lib -lc'.format(riscv_path)


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
	command='{} {} {} -o build/dhrystone.bin -T {} {}'.format(ld, ld_flags, ' '.join(objects_files), linker_script, ld_endFlags)
	shellRun(command)

if target in ['all']:
	command='{} -s build/dhrystone.bin > build/objdump.txt'.format(objdump)
	shellRun(command)
	command='{} -D build/dhrystone.bin > build/disassembled.txt'.format(objdump)
	shellRun(command)
	command='elf2hex 8 32768 build/dhrystone.bin > temphex'
	shellRun(command)
	command='./gen_usable_hex.py'
	shellRun(command)
	command='mv code.hex build/'
	shellRun(command)
	command='mv temphex build/'
	shellRun(command)
