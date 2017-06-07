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

fasta-c : fasta-c.c
	$(CC) -std=gnu99 -O3 -fomit-frame-pointer $< -o $@

fasta.faststdout : fasta-c
	./fasta-c $(FAST_OPTS) > $@

fasta.stdout : fasta-c
	./fasta-c $(NORM_OPTS) > $@

fasta.slowstdout : fasta-c
	./fasta-c $(SLOW_OPTS) > $@

# Since we only decide here what the INPUT_FILE is, it's required to first run
# `make boot` and only than `make` (otherwise `make` doesn't "see" the file and
# doesn't call `runstdtest` correctly)
ifeq "$(mode)" "slow"
 STDOUT_FILE = fasta.slowstdout
else
 ifeq "$(mode)" "fast"
  STDOUT_FILE = fasta.faststdout
 else
  STDOUT_FILE = fasta.stdout
 endif
endif

boot :: $(STDOUT_FILE)

include $(TOP)/mk/target.mk
