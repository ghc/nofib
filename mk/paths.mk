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

#
# If tests are expected to end in compilation failure,
# set the flag HC_MAY_FAIL to YES (before including boilerplate,mk !)
#
# Options to the runstdtest that we wrap around the `real' HC below
# can be set through HC_RUNSTDTEST_OPTS
#
ifeq "$(HC_MAY_FAIL)" "YES"
HC:=$(RUNSTDTEST) -x1 $(HC_RUNSTDTEST_OPTS) -- $(HC)
endif
