#!/usr/bin/env python

import config

infile=open('temphex','r')
outfile=open('code.hex','w')
for lineno,line in enumerate(infile):
  line=line.strip();
  outfile.write(line[len(line)/2:len(line)]+'\n'+line[0:len(line)/2]+'\n')
  
  if(config.sperateInstrDataMemory==True):
    if('0000006f' in line):
      outfile.close();
      outfile=open('memory.hex','w');

infile.close()
outfile.close()
