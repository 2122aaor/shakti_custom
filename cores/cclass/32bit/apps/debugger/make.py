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
	riscv_path = subprocess.check_output('which riscv32-unknown-elf-gcc',shell=True,stderr=None)
	riscv_path = riscv_path.decode('utf-8').replace('/bin/riscv32-unknown-elf-gcc','').strip()
except:
	print("\n\nPlease install RISCV toolchain with the modified spike\n\n")
	exit(1)

sources = ('start.S', 'regd.S', 'clearscreen.S', 'memd.S', 'soft_reset.S', 'modmem.S', 'serial_ld1.S','endProgram.S', 'crt.S' )
cc = 'riscv32-unknown-elf-gcc'
ld = 'riscv32-unknown-elf-ld'
objdump = 'riscv32-unknown-elf-objdump'
linker_script='linkerScript.ld'
if(bitwidth==32):
	ld_endFlags='-L{0}/riscv32-unknown-elf/lib -lc'.format(riscv_path)
	cc_flags= '-c -I. -mcmodel=medany -static  -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf'
	ld_flags='-static -nostdlib -nostartfile'
else:
	cc_flags= '-c -I. -static  -std=gnu99 -O2 -faggressive-loop-optimizations -ffast-math -fno-common -fno-builtin-printf'
	ld_flags='-static -nostdlib -nostartfiles '
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
	command='{} {} {} -o build/debug.bin -T {} {}'.format(ld, ld_flags, ' '.join(objects_files), linker_script, ld_endFlags)
	shellRun(command)

if target in ['all']:
	command='{} -s build/debug.bin > build/objdump.txt'.format(objdump)
	shellRun(command)
	command='{} -D build/debug.bin > build/disassembled.txt'.format(objdump)
	shellRun(command)
	command='elf2hex 4 16384 build/debug.bin 2147483648 > build/code.hex'
	shellRun(command)
