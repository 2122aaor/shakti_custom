#!/usr/bin/env python

import textwrap

infile=open('build/temphex','r')
outfile=open('build/code.hex','w')
for lineno,line in enumerate(infile):
  if('@' in line):
    continue
  else:
    line=textwrap.wrap(line,8)
    for i in range(len(line)):
      outfile.write(line[i])
      outfile.write("\n") 

for i in range(1,1000):
  outfile.write("00000000\n");

infile.close()
outfile.close()
