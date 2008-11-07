TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -fglasgow-exts

NORM_OPTS = 300000

ifeq "$(HEAP)" "LARGE"
SRC_RUNTEST_OPTS += +RTS -H128m -RTS
endif
ifeq "$(HEAP)" "OLD"
SRC_RUNTEST_OPTS += +RTS -H10m -RTS
endif

include $(TOP)/mk/target.mk
