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

NOFIB_TOP := $(TOP)
TOP := $(TOP)/..
include $(TOP)/mk/boilerplate.mk
TOP:=$(NOFIB_TOP)

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

HC	   = $(WithNofibHc)
MKDEPENDHS := $(WithNofibHc)  # ToDo: wrong, if $(WithNofibHc) isn't GHC.
USE_NEW_MKDEPEND_FLAGS := $(shell if test `$(MKDEPENDHS) --numeric-version | sed -e "s/\./0/" -e "s/\..*//"` -ge 609; then echo YES; else echo NO; fi)

include $(NOFIB_TOP)/mk/paths.mk
include $(NOFIB_TOP)/mk/opts.mk
