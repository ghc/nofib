#################################################################################
#
#			    nofib/mk/paths.mk
#
# 	This file defines Make variables for standard directories
#	and file lists
#
#################################################################################


# Define NOFIB_PROG.  In ..../nofib/imaginary/exp3_8, PROG is exp3_8 by default.
#
NOFIB_PROG = $(notdir $(shell pwd))$(_way)

# Eventually, have the binary purged
CLEAN_FILES += $(NOFIB_PROG)
