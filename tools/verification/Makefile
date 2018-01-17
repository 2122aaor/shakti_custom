#Makefile to generate assembly instructions and execute them using a functional model.

ADIR = ./AAPG
A_ODIR = ./AAPG/output
IDIR = ./ISS/
I_ODIR = ./ISS/output

all: $(IDIR)ISS $(ADIR)/bin.o generate dump

$(IDIR)ISS: $(IDIR)/ISS.cpp
	g++ -o $@ -std=c++0x $(IDIR)ISS.cpp

$(ADIR)/bin.o: $(ADIR)/inst2binary.c
	gcc -o $@ $(ADIR)/inst2binary.c

generate: 
	python $(ADIR)/risc-v.py  
	$(ADIR)/bin.o $(A_ODIR)/assembly_newlib.s
	python $(ADIR)/hexa.py
	cp -v $(A_ODIR)/memory_dcache.txt $(A_ODIR)/input.hex $(IDIR)

dump:
	$(IDIR)ISS 

verify:
	sdiff -W regdump.txt ../../cores/iclass/output.txt > error_log.txt

