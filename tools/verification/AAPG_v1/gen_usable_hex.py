#!/usr/bin/env python

import config
import textwrap

infile=open('temphex','r')
outfile=open('code.hex','w')
for lineno,line in enumerate(infile):
  if('@' in line):
    continue
  else:
    line=textwrap.wrap(line,8)
    for i in range(len(line)):
      outfile.write(line[i])
      outfile.write("\n") 
    if(config.sperateInstrDataMemory==True):
      if('0000006f' in line):
        outfile.close();
        outfile=open('memory.hex','w');

infile.close()
outfile.close()
