TOP = ..
include $(TOP)/mk/boilerplate.mk

# Leave the net* tests out of it for the moment, runstdtest is 
# geared towards batch programs
#
# Also leave out stable* tests; the only one has a funny mixture of C
# and Haskell in it, so it needs attention.

SUBDIRS = $(wildcard frontend* arith* bugs cg* ghclib* io* obfuscated93 po*)

# io018 is not interesting here (it spins)
SUBDIRS := $(filter-out io018,$(SUBDIRS))

include $(TOP)/mk/target.mk

