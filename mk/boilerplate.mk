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
# ( FPTOOLS_TOP, which will be set while processing
#   toplevel boilerplate, is the fptools top )

NOFIB_TOP := $(TOP)
TOP := $(TOP)/..
include $(TOP)/mk/boilerplate.mk
TOP:=$(NOFIB_TOP)


# -----------------------------------------------------------------
# Everything after this point
# augments or overrides previously set variables.
# (these files are optional, so `make' won't fret if it
#  cannot get to them).
# -----------------------------------------------------------------

WAYS=$(NoFibWays)

SRC_HC_OPTS += $(NoFibHcOpts) -Rghc-timing

HC	   = $(WithNofibHc)
MKDEPENDHS = $(WithNofibHc)  # ToDo: wrong, if $(WithNofibHc) isn't GHC.

include $(NOFIB_TOP)/mk/paths.mk
include $(NOFIB_TOP)/mk/opts.mk
include $(NOFIB_TOP)/mk/suffix.mk
