#!/usr/bin/env python

import sys
import re

infile=open('temphex','r')
outfile=open('code.hex','w')
for lineno,line in enumerate(infile):
  outfile.write(line[8:16]+'\n'+line[0:8]+'\n')

infile.close()
outfile.close()
