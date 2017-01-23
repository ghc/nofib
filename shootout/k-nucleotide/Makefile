TOP = ../..
include $(TOP)/mk/boilerplate.mk

# These values are only used in this file. They are ignored by the
# executable itself.
FAST_OPTS = 250000
NORM_OPTS = 2500000
SLOW_OPTS = 25000000  # official shootout setting

# The benchmark game also uses -fllvm, which we can't since it might
# not be available on the developer's machine.
HC_OPTS += -O2 -XBangPatterns -package bytestring

#------------------------------------------------------------------
# Create input

fasta-c : ../fasta/fasta-c.c
	$(CC) -std=gnu99 -O3 -fomit-frame-pointer $< -o $@

knucleotide-input250000.txt : fasta-c
	./fasta-c $(FAST_OPTS) | tr -d '\r' > $@

knucleotide-input2500000.txt : fasta-c
	./fasta-c $(NORM_OPTS) | tr -d '\r' > $@

knucleotide-input25000000.txt : fasta-c
	./fasta-c $(SLOW_OPTS) | tr -d '\r' > $@

ifeq "$(mode)" "slow"
 INPUT_FILE = knucleotide-input25000000.txt
else
 ifeq "$(mode)" "fast"
  INPUT_FILE = knucleotide-input250000.txt
 else
  INPUT_FILE = knucleotide-input2500000.txt
 endif
endif

SRC_RUNTEST_OPTS += -i $(INPUT_FILE)

all boot :: $(INPUT_FILE)

include $(TOP)/mk/target.mk
