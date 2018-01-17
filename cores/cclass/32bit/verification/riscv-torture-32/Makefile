SHELL := /bin/bash # Use bash syntax

SBT ?= java -Xmx1G -Xss8M -XX:MaxPermSize=128M -jar sbt-launch.jar
TEST := output/test.S
OPTIONS := $(empty)
SUITE := output
CONFIG := config/default.config
COMMIT := none
empty :=
space := $(empty) $(empty)
cfgopt := $(space)-f$(space)
gitopt := $(space)-g$(space)
CFG := $(subst $(space),$(cfgopt),$(CONFIG))
GITCMT := $(subst $(space),$(gitopt),$(COMMIT))
RUNS:= 1

.phony: gen ctest rtest itest igentest cgentest rgentest \
cnight rnight crnight csuite rsuite \

gen:
	@cd output;\
	ln -f -s ../../../bin/* ./ ;\
	cd ..;\
	n=1 ; while [[ $$n -le $(RUNS) ]] ; do \
	echo "----------------Test $$n ----------------";\
	$(SBT) 'generator/run $(OPTIONS)';\
	cd output; make; \
	spike -s --isa=rv32g test;\
	./out > log ;\
	diff -iqw spike_register_dump.txt rtl_register_dump.txt || { echo RTL AND SPIKE REG-DUMPs NOT MATCH; exit; } ;\
	diff -iqw spike_fregister_dump.txt rtl_fregister_dump.txt || { echo RTL AND SPIKE FREG-DUMPs DO NOT MATCH; exit; } ;\
	mkdir -p run_$$n ; \
	mv test* code.mem spike* rtl* log* app_log run_$$n ; \
	echo " Test $$n successfull";\
	cd .. ;\
	((n=n+1)) ; \
	done;
	@rm output/dump*

clean:
	@cd output; rm -rf run* code.mem test* dump* spike* rtl* out out.so log app_log config_string.hex
	@find -type l -delete
	@echo "Cleaned"

