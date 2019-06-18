# Do not use make's built-in rules and variables
# (this increases performance and avoids hard-to-debug behaviour);
MAKEFLAGS += -r

MODULES=\
	  fifo \
	  fifo_32b \
	  encoder \
	  decoder \
	  inverter

TESTBENCH=\
	  inverter_tb

GHDL_CMD=ghdl

BINDIR=bin

.PHONY: clean dump wave

all: compile

run:
	$(GHDL_CMD) -r $(TESTBENCH)

compile:
	mkdir -p bin
	$(GHDL_CMD) -i --workdir=$(BINDIR) --work=work $(addprefix src/,$(addsuffix .vhdl,$(MODULES) $(TESTBENCH)))
	$(GHDL_CMD) -m --workdir=$(BINDIR) --work=work $(TESTBENCH)

dump:
	rm -f dump
	ghdl -r $(TESTBENCH) --trace-signals > dump

wave:
	rm -f wave
	ghdl -r $(TESTBENCH) --vcd=wave

clean:
	$(GHDL_CMD) --clean --workdir=$(BINDIR)
