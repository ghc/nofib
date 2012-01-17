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
include $(NOFIB_TOP)/../mk/tree.mk
include $(NOFIB_TOP)/../mk/config.mk
GHC_TOP := $(TOP)
TOP := $(NOFIB_TOP)

RM = rm -f
SIZE = size
STRIP = strip

# Turn off -Werror for nofib. This allows you to use nofib in a tree
# built with validate.
WERROR=

# Benchmarks controls which set of tests should be run
# You can run one or more of
#	imaginary 
#	spectral
#	real
#
#	gc
#	hackage
#	parallel
#	repa
#	shootout
#	smp
Benchmarks = imaginary spectral real
NoFibSubDirs = $(addprefix benchmarks/, $(Benchmarks))

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

# -package array is needed for GHC 7.0.1 and later, as the haskell98 package
# is no longer linked by default.  We would like to use
#    -hide-all-packages -package haskell2010
# instead, but there is at least one program that uses a non-haskell2010
# library module (fibheaps uses Control.Monad.ST)
SRC_HC_OPTS += -package array

ifeq "$(WithNofibHc)" ""
HC	   = $(GHC_TOP)/$(GHC_STAGE2)
MKDEPENDHS := $(GHC_TOP)/$(GHC_STAGE2)  # ToDo: wrong, if $(WithNofibHc) isn't GHC.
else
HC	   = $(WithNofibHc)
MKDEPENDHS := $(WithNofibHc)
endif

define get-ghc-rts-field # $1 = result variable, $2 = field name
$1 := $$(shell '$$(HC)' +RTS --info | grep '^ .("$2",' | tr -d '\r' | sed -e 's/.*", *"//' -e 's/")$$$$//')
endef

$(eval $(call get-ghc-rts-field,HC_VERSION,GHC version))

define ghc-ge # $1 = major version, $2 = minor version
HC_VERSION_GE_$1_$2 := $$(shell if [ `echo $$(HC_VERSION) | sed 's/\..*//'` -gt $1 ]; then echo YES; else if [ `echo $$(HC_VERSION) | sed 's/\..*//'` -ge $1 ] && [ `echo $$(HC_VERSION) | sed -e 's/[^.]*\.//' -e 's/\..*//'` -ge $2 ]; then echo YES; else echo NO; fi; fi)
endef

$(eval $(call ghc-ge,6,13))

MKDEPENDC := $(GHC_TOP)/$(MKDEPENDC)
RUNTEST   = $(NOFIB_TOP)/runstdtest/runstdtest

USE_NEW_MKDEPEND_FLAGS = YES
# USE_NEW_MKDEPEND_FLAGS := $(shell if test `$(MKDEPENDHS) --numeric-version | sed -e "s/\./0/" -e "s/\..*//"` -ge 609; then echo YES; else echo NO; fi)

include $(NOFIB_TOP)/mk/ghc-paths.mk
include $(NOFIB_TOP)/mk/ghc-opts.mk
include $(NOFIB_TOP)/mk/paths.mk
include $(NOFIB_TOP)/mk/opts.mk

-include .depend
