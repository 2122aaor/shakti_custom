SHELL := /bin/bash # Use bash syntax

CSMITH_INCDIR = /tools/csmith/runtime
xlen=32
RUNS=10
all:
	@ln -f -s ../../bin/* .
	@n=1 ; while [[ $$n -le $(RUNS) ]] ; do \
	echo "----------------Test $$n ----------------";\
	make spike || { echo SKIP; make remove; continue; } ;\
	echo "Running test on SHAKTI C-32";\
	timeout 3m ./out > log || { echo SIM TIMED OUT; exit; };\
	cat app_log; echo " ";\
	diff -iqw app_log output.riscv || { echo RTL AND SPIKE DO NOT MATCH; exit; } ;\
	mkdir -p testdir_$$n ;\
	mv rtl_* test.* output.* log code.mem testdir_$$n/ ; \
	((n=n+1)) ; \
	done;



spike: test.host test.riscv
	@echo "Running test on HOST";
	@timeout 2 ./test.host > output.host 
	@cat output.host
	@echo "Running test on SPIKE"
	@spike --isa=rv32g test.riscv > output.riscv 
	@cat output.riscv
	@diff -iqw output.host output.riscv

test.host: test.c
	@gcc -m$(xlen) -w -Os -I $(CSMITH_INCDIR) -o test.host test.c

test.riscv: test.c crt.o syscalls.spike syscalls.shakti
	@riscv$(xlen)-unknown-elf-gcc -w -Os -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf  -c -I $(CSMITH_INCDIR) test.c -o test.o
	@riscv32-unknown-elf-gcc -T link.ld -I $(CSMITH_INCDIR) test.o syscalls.spike crt.o -o test.riscv -static -nostartfiles -lgcc -lm
	@riscv32-unknown-elf-gcc -T link.ld -I $(CSMITH_INCDIR) test.o syscalls.shakti crt.o -o test.shakti -static -nostartfiles -lgcc -lm
	@riscv32-unknown-elf-objdump -D test.riscv > test.riscv.dump
	@riscv32-unknown-elf-objdump -D test.shakti > test.shakti.dump
	@elf2hex 4 32768 test.shakti 2147483648 > code.mem

test.c:
	@echo "integer size = 4" > platform.info
	@echo "pointer size = 4" >> platform.info
	@csmith --no-packed-struct -o test.c
	@gawk '/Seed:/ {print$$2,$$3;}' test.c

crt.o:
	@riscv32-unknown-elf-gcc -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf -D__ASSEMBLY__=1 -c -I $(CSMITH_INCDIR) crt.S -o crt.o

syscalls.spike:
	@cp syscalls.c.spike syscalls.c
	@riscv32-unknown-elf-gcc -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf  -c -I $(CSMITH_INCDIR) syscalls.c -o syscalls.spike
	@rm -f syscalls.c

syscalls.shakti:
	@cp syscalls.c.shakti syscalls.c
	@riscv32-unknown-elf-gcc -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf  -c -I $(CSMITH_INCDIR) syscalls.c -o syscalls.shakti
	@rm -f syscalls.c

clean: 
	@rm -rf platform.info test.* output.host output.riscv app_log code.mem crt.o syscalls.spike syscalls.shakti testdir* out out.so rtl_* log config_string.hex *.txt
	@find -type l -delete
	@echo "cleaned"
remove:
	@rm -f platform.info test.c test.host test.riscv output.host output.riscv test.o
