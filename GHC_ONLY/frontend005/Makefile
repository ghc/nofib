TOP = ../..
include $(TOP)/mk/boilerplate.mk
#
# Compilation failure is not dealt with particularly well
# in the current nofib setup, so we hack.
#
NOFIB_PROG=
all :: $(HS_OBJS)
SRC_HC_OPTS=-noC
HC:= $(RUNTEST) $(HC) $(EXTRA_RUNTEST_OPTS) -x 1 -o2 frontend005.stderr --
include $(TOP)/mk/target.mk

