# Do not use make's built-in rules and variables
# (this increases performance and avoids hard-to-debug behaviour);
MAKEFLAGS += -r

MODULES=\
	  fifo_bb \
	  fifo_32b \
	  fifo_b32 \
	  encoder \
	  decoder \
	  logic_or \
	  inverter

TESTBENCH=\
	  fifo_b32_tb

GHDL_CMD=ghdl

BINDIR=bin

.PHONY: clean dump wave

all: compile

run:
	$(GHDL_CMD) -r $(TESTBENCH)

compile:
	mkdir -p bin
	$(GHDL_CMD) -i --workdir=$(BINDIR) --work=work $(addprefix src/,$(addsuffix .vhdl, $(MODULES)))
	$(GHDL_CMD) -i --workdir=$(BINDIR) --work=work $(addprefix tb/,$(addsuffix .vhdl, $(TESTBENCH)))
	$(GHDL_CMD) -m --workdir=$(BINDIR) --work=work $(TESTBENCH)

dump:
	rm -f dump
	ghdl -r $(TESTBENCH) --trace-signals > dump

wave:
	rm -f wave.ghw
	ghdl -r $(TESTBENCH) --wave=wave.ghw

clean:
	$(GHDL_CMD) --clean --workdir=$(BINDIR)
