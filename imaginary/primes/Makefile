TOP = ../..
include $(TOP)/mk/boilerplate.mk

# Override default SRCS, or we get cseive.c as well.
SRCS = Main.hs

# Need big heap to run
SRC_RUNTEST_OPTS = +RTS -H16m -RTS

include $(TOP)/mk/target.mk

