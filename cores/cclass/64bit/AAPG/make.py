#!/usr/bin/env python3

#Author: Nandu Raj P (nndurj@gmail.com)
import sys

#if sys.version_info<(3,):
#  sys.stdout.write("\n\nRequires python3 !!!\nExiting\n\n")
#  sys.exit(1)

import subprocess
import os
from commonStuff import *
import parseObjdump as pars
import traceback
import aapg
import time
seedFile=open('randomSeed.txt','r')
seeds=seedFile.readlines()
count=1

try:
	makeType=sys.argv[1]
except:
	makeType='all'

#fileList=('output.o','output.s','output.riscv','objdump.txt','code.hex','memory.hex','spike_register_dump.txt'\
    #	,'spike_disassembled.txt', 'spike_mem_dump.txt','spike_mem.txt', 'log.txt')

try: 
  subprocess.check_output('which riscv64-unknown-elf-gcc',shell=True,stderr=None)
except Exception as e:
  print(e)
  print("\n\nPlease install modified riscv-tools\n\n")

def compileASM():
  print('Compiling Assembly Program')
  subprocess.call('riscv64-unknown-elf-gcc -c -o output.o output.s',shell=True)

def linkObject():
  print('linking')
  subprocess.call('riscv64-unknown-elf-ld -o output.riscv output.o -T ../linkerScript.ld',shell=True)

def genObjdump():
    print('objdump')
    subprocess.call('riscv64-unknown-elf-objdump -d output.riscv > disaassembled.txt',shell=True)
    subprocess.call('riscv64-unknown-elf-objdump -s output.riscv > objdump.txt',shell=True)
    subprocess.call('elf2hex '+str(lineWidthOfMainMemory*2)+' 32768 output.riscv > temphex',shell=True)
    subprocess.call('mv temphex code.hex',shell=True)
#    subprocess.call('../gen_usable_hex.py',shell=True)
#    aapg.genhex(sperateInstrDataMemory)

def runSpike():
    #addressLimits=pars.parseObjectFile('objdump.txt',bitwidth)
    #for key,value in addressLimits.items() :
    #  logFile.write('{0}={1}\n'.format(key,value))
    #logFile.close()
    #print ('Running Spike')
    #subprocess.call('spike -s -m2048 --ms={0} --me={1} --isa=RV{2}IMAFDC output.riscv'.format(addressLimits['memoryStartAddress'],addressLimits['memoryEndAddress'],bitwidth),shell=True)
    subprocess.call('spike -s --isa=RV{0}IMAFDC output.riscv'.format(bitwidth),shell=True)
    subprocess.call('mv dump_raw.txt spike_register_dump.txt',shell=True)
    subprocess.call('mv dump_raw2.txt spike_fregister_dump.txt',shell=True)
    subprocess.call('mv dump_disassembled.txt spike_disassembled.txt',shell=True)
    #subprocess.call('mv memoryDump.txt spike_mem_dump.txt',shell=True)
    #subprocess.call('mv memoryDumpReadable.txt spike_mem.txt',shell=True)

def runRTLSim():
    subprocess.call('ln -s ../../bin/out RTL.out',shell=True)
    subprocess.call('ln -s ../../bin/out.so RTL.out.so',shell=True)
    subprocess.call('./RTL.out > rtl_log.txt',shell=True)

def makeDir(dirName):
  if dirName in os.listdir('.'):
    subprocess.call('rm -r {0}/* 2>/dev/null'.format(dirName),shell=True)
  else:
    subprocess.call('mkdir {}'.format(dirName),shell=True)

try:
  if makeType=='clean':
    subprocess.call('rm -rf result 2>/dev/null',shell=True)
    subprocess.call('rm -rf build 2>/dev/null', shell=True)
    print('Cleaned')
  else:
    makeDir('result')
    makeDir('build')
    os.chdir('build')
  if(makeType=='all'):
    for seed in seeds:
      logFile=open('log.txt','w')
      print ('\nGenerating Test Case - '+str(count))
      aapg.random.seed(seed.strip())
      aapg.aapgMain('output.s')
      compileASM()
      linkObject()
      genObjdump()
      runSpike();
      print ('Running RTL Simulation')
      runRTLSim();
      stdoutdata=subprocess.getoutput("diff -qiw rtl_register_dump.txt spike_register_dump.txt")
      if not stdoutdata:
        print("Integer Results Match")
      else:
        print(" *!*!*!*!*! Integer Results DO NOT Match *!*!*!*!*!")
        break
      stdoutdata=subprocess.getoutput("diff -qiw rtl_fregister_dump.txt spike_fregister_dump.txt")
      if not stdoutdata:
        print("Floating Results Match")
      else:
        print(" *!*!*!*!*! Floating Results DO NOT Match *!*!*!*!*!")
        break
      subprocess.call('mkdir ../result/test{0}'.format(count),shell=True)
      subprocess.call('rm RTL.out*',shell=True)
      subprocess.call('mv * ../result/test{0}'.format(count),shell=True)
      if(count==numberOfTests):
        break
      count=count+1
  elif makeType=='gen_and_spike':
    for seed in seeds:
      logFile=open('log.txt','w')
      print ('\nGenerating Test Case - '+str(count))
      aapg.random.seed(seed.strip())
      aapg.aapgMain('output.s')
      compileASM()
      linkObject()
      genObjdump()
      runSpike();
      subprocess.call('mkdir ../result/test{0}'.format(count),shell=True)
      subprocess.call('mv * ../result/test{0}'.format(count),shell=True)
      if(count==numberOfTests):
        break
      count=count+1
  elif makeType=='gen_only':
    for seed in seeds:
      logFile=open('log.txt','w')
      print ('\nGenerating Test Case - '+str(count))
      aapg.random.seed(seed.strip())
      aapg.aapgMain('output.s')
      compileASM()
      linkObject()
      genObjdump()
      subprocess.call('mkdir ../result/test{0}'.format(count),shell=True)
      subprocess.call('mv * ../result/test{0}'.format(count),shell=True)
      if(count==numberOfTests):
        break
      count=count+1
  elif makeType=='link':
    linkObject()
  elif makeType=='compile':
    compileASM()
except Exception as err:
  traceback.print_tb(err.__traceback__)
  exit(1)
