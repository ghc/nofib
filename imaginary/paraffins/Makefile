TOP = ../..
include $(TOP)/mk/boilerplate.mk

# Override default SRCS; the default is all source files, but
# we don't want to include paraffins.c
SRCS=Main.hs
SRC_RUNTEST_OPTS += +RTS -H20m -RTS
SRC_HC_OPTS += -syslib lang # for Int(fromInt)

include $(TOP)/mk/target.mk

