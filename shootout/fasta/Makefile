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

# FIXME: You have to run make twice for the runstdtest.prl script to
# find the various stdout files correctly.

fasta-c : fasta-c.o
	gcc $< -o $@

fasta.faststdout : fasta-c
	./fasta-c $(FAST_OPTS) > $@

fasta.stdout : fasta-c
	./fasta-c $(NORM_OPTS) > $@

fasta.slowstdout : fasta-c
	./fasta-c $(SLOW_OPTS) > $@

STDOUT_FILES = fasta.faststdout fasta.stdout fasta.slowstdout

all boot :: $(STDOUT_FILES)

include $(TOP)/mk/target.mk
