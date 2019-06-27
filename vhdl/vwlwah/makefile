# Do not use make's built-in rules and variables
# (this increases performance and avoids hard-to-debug behaviour);
MAKEFLAGS += -r

PACKAGES =\
		  utils
MODULES=\
	  fifo_bb \
	  fifo_32b \
	  fifo_b32 \
	  encoder \
	  decoder \
	  logic_or \
	  inverter

TESTBENCH=\
	  logic_or_tb

GHDL_CMD=ghdl

BINDIR=bin

.PHONY: clean dump wave

all: compile

run:
	$(GHDL_CMD) -r $(TESTBENCH)

compile:
	mkdir -p bin
	$(GHDL_CMD) -i --workdir=$(BINDIR) --std=08 --work=work $(addprefix src/,$(addsuffix .vhdl, $(PACKAGES)))
	$(GHDL_CMD) -i --workdir=$(BINDIR) --std=08 --work=work $(addprefix src/,$(addsuffix .vhdl, $(MODULES)))
	$(GHDL_CMD) -i --workdir=$(BINDIR) --std=08 --work=work $(addprefix tb/,$(addsuffix .vhdl, $(TESTBENCH)))
	$(GHDL_CMD) -m --workdir=$(BINDIR) --std=08 --work=work $(TESTBENCH)

dump:
	rm -f dump
	$(GHDL_CMD) -r $(TESTBENCH) --trace-signals > dump

wave:
	rm -f wave.ghw
	$(GHDL_CMD) -r $(TESTBENCH) --wave=wave.ghw

clean:
	$(GHDL_CMD) --clean --workdir=$(BINDIR)
