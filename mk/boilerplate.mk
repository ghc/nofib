#################################################################################
#
#			    nofib/mk/boilerplate.mk
#
#		Boilerplate Makefile for an fptools project
#
#################################################################################

# Begin by slurping in the boilerplate from one level up, 
# with standard TOP-mangling
# Remember, TOP is the top level of the innermost level

default : all

show:
	@echo '$(VALUE)="$($(VALUE))"'

NOFIB_TOP := $(TOP)
include $(NOFIB_TOP)/../mk/config.mk
GHC_TOP := $(TOP)
TOP := $(NOFIB_TOP)

# Turn off -Werror for nofib. This allows you to use nofib in a tree
# built with validate.
WERROR=

# NoFibSubDirs controls which set of tests should be run
# You can run one or more of
#	imaginary 
#	spectral
#	real
#	parallel
#	PRIVATE
#	PENDING
#	UNUSED
NoFibSubDirs = imaginary spectral real

# The different ways to build nofib. Default is just to mirror
# what is done for the ghc prelude libraries.
#
NoFibWays = $(filter-out v,$(GhcLibWays))

# Haskell compiler options for nofib
NoFibHcOpts = -O

# Number of times to run each program
NoFibRuns = 5

# -----------------------------------------------------------------
# Everything after this point
# augments or overrides previously set variables.
# (these files are optional, so `make' won't fret if it
#  cannot get to them).
# -----------------------------------------------------------------

WAYS=$(NoFibWays)

TOP := $(GHC_TOP)
-include $(GHC_TOP)/mk/build.mk
TOP := $(NOFIB_TOP)

SRC_HC_OPTS += $(NoFibHcOpts) -Rghc-timing

ifeq "$(WithNofibHc)" ""
HC	   = $(GHC_TOP)/$(GHC_STAGE2)
MKDEPENDHS := $(GHC_TOP)/$(GHC_STAGE2)  # ToDo: wrong, if $(WithNofibHc) isn't GHC.
else
HC	   = $(WithNofibHc)
MKDEPENDHS := $(WithNofibHc)
endif

MKDEPENDC := $(GHC_TOP)/$(MKDEPENDC)
RUNTEST   = $(NOFIB_TOP)/runstdtest/runstdtest

USE_NEW_MKDEPEND_FLAGS = YES
# USE_NEW_MKDEPEND_FLAGS := $(shell if test `$(MKDEPENDHS) --numeric-version | sed -e "s/\./0/" -e "s/\..*//"` -ge 609; then echo YES; else echo NO; fi)

include $(NOFIB_TOP)/mk/ghc-paths.mk
include $(NOFIB_TOP)/mk/ghc-opts.mk
include $(NOFIB_TOP)/mk/paths.mk
include $(NOFIB_TOP)/mk/opts.mk

-include .depend
