TOP = ../..
include $(TOP)/mk/boilerplate.mk
-include opts.mk

# Override default SRCS; the default is all source files
#SRCS=Tree.hs partree.hs
SRC_RUNTEST_OPTS = 120 120
WAY_mp_SRC_RUNTEST_OPTS = +RTS -qt2 -RTS
SRC_HC_OPTS += -package concurrent

include $(TOP)/mk/target.mk

