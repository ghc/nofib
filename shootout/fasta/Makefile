TOP = ../..
include $(TOP)/mk/boilerplate.mk

# Override default SRCS; the default is all source files, but
# we don't want to include fasta-c.c
SRCS = Main.hs

FAST_OPTS = 250000
NORM_OPTS = 2500000
SLOW_OPTS = 25000000  # official shootout setting

# The benchmark game also uses -fllvm, which we can't since it might
# not be available on the developer's machine.
HC_OPTS += -O2 -XBangPatterns -XOverloadedStrings -package bytestring

#------------------------------------------------------------------
# Create output to validate against

fasta-c : fasta-c.o
	gcc $< -o $@

fasta.faststdout : fasta-c
	./fasta-c $(FAST_OPTS) > $@

fasta.stdout : fasta-c
	./fasta-c $(NORM_OPTS) > $@

fasta.slowstdout : fasta-c
	./fasta-c $(SLOW_OPTS) > $@

# Since the stdout files are created during the run the runstdtest
# script doesn't correctly pick them up, so we have to specify them
# explicitly here.
ifeq "$(mode)" "slow"
 STDOUT_FILE = fasta.slowstdout
else
 ifeq "$(mode)" "fast"
  STDOUT_FILE = fasta.faststdout
 else
  STDOUT_FILE = fasta.stdout
 endif
endif

SRC_RUNTEST_OPTS += -o1 $(STDOUT_FILE)

all boot :: $(STDOUT_FILE)

include $(TOP)/mk/target.mk
