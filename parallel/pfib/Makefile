TOP = ../..
include $(TOP)/mk/boilerplate.mk
-include opts.mk

# Override default SRCS; the default is all source files
SRCS=Main.hs
SRC_RUNTEST_OPTS += 30
SRC_HC_OPTS += -package concurrent

include $(TOP)/mk/target.mk
