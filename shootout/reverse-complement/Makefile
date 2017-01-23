TOP = ../..
include $(TOP)/mk/boilerplate.mk

# Override default SRCS; the default is all source files, but
# we don't want to include revcomp-c.c
SRCS = Main.hs

# These values are only used in this file. They are ignored by the
# executable itself.
FAST_OPTS = 250000
NORM_OPTS = 2500000
SLOW_OPTS = 25000000  # official shootout setting

# The benchmark game also uses -fllvm, which we can't since it might
# not be available on the developer's machine.
HC_OPTS += -O2 -XBangPatterns -funfolding-use-threshold=32 -XMagicHash \
	-XUnboxedTuples

#------------------------------------------------------------------
# Create input

fasta-c : ../fasta/fasta-c.c
	$(CC) -std=gnu99 -O3 -fomit-frame-pointer $< -o $@

revcomp-input250000.txt : fasta-c
	./fasta-c $(FAST_OPTS) | tr -d '\r' > $@

revcomp-input2500000.txt : fasta-c
	./fasta-c $(NORM_OPTS) | tr -d '\r' > $@

revcomp-input25000000.txt : fasta-c
	./fasta-c $(SLOW_OPTS) | tr -d '\r' > $@

ifeq "$(mode)" "slow"
 INPUT_FILE = revcomp-input25000000.txt
else
 ifeq "$(mode)" "fast"
  INPUT_FILE = revcomp-input250000.txt
 else
  INPUT_FILE = revcomp-input2500000.txt
 endif
endif

SRC_RUNTEST_OPTS += -i $(INPUT_FILE)

all boot :: $(INPUT_FILE)

#------------------------------------------------------------------
# Create output to validate against

revcomp-c : revcomp-c.o
	gcc $< -o $@ -pthread

reverse-complement.faststdout : revcomp-c
	./revcomp-c < $(INPUT_FILE) > $@

reverse-complement.stdout : revcomp-c
	./revcomp-c < $(INPUT_FILE) > $@

reverse-complement.slowstdout : revcomp-c
	./revcomp-c < $(INPUT_FILE) > $@

# Since the stdout files are created during the run the runstdtest
# script doesn't correctly pick them up, so we have to specify them
# explicitly here.
ifeq "$(mode)" "slow"
 STDOUT_FILE = reverse-complement.slowstdout
else
 ifeq "$(mode)" "fast"
  STDOUT_FILE = reverse-complement.faststdout
 else
  STDOUT_FILE = reverse-complement.stdout
 endif
endif

SRC_RUNTEST_OPTS += -o1 $(STDOUT_FILE) 

all boot :: $(STDOUT_FILE)

include $(TOP)/mk/target.mk
