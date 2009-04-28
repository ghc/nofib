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

NOFIB_TOP := $(TOP)
include $(NOFIB_TOP)/../mk/config.mk
GHC_TOP := $(TOP)
TOP := $(NOFIB_TOP)

# Turn off -Werror for nofib. This allows you to use nofib in a tree
# built with validate.
WERROR=

# -----------------------------------------------------------------
# Everything after this point
# augments or overrides previously set variables.
# (these files are optional, so `make' won't fret if it
#  cannot get to them).
# -----------------------------------------------------------------

WAYS=$(NoFibWays)

SRC_HC_OPTS += $(NoFibHcOpts) -Rghc-timing

HC	   = $(GHC_TOP)/$(GHC_STAGE2)
MKDEPENDHS := $(GHC_TOP)/$(GHC_STAGE2)  # ToDo: wrong, if $(WithNofibHc) isn't GHC.
MKDEPENDC := $(GHC_TOP)/$(MKDEPENDC)
RUNTEST   = $(NOFIB_TOP)/runstdtest/runstdtest

USE_NEW_MKDEPEND_FLAGS = YES
# USE_NEW_MKDEPEND_FLAGS := $(shell if test `$(MKDEPENDHS) --numeric-version | sed -e "s/\./0/" -e "s/\..*//"` -ge 609; then echo YES; else echo NO; fi)

include $(NOFIB_TOP)/mk/ghc-paths.mk
include $(NOFIB_TOP)/mk/ghc-opts.mk
include $(NOFIB_TOP)/mk/paths.mk
include $(NOFIB_TOP)/mk/opts.mk

-include .depend
