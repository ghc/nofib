TOP = ../..
include $(TOP)/mk/boilerplate.mk

OBJS=$(patsubst %.hs,%.o, $(patsubst %.lhs,%.o,$(wildcard *.hs *.lhs)))

SRC_RUNTEST_OPTS += +RTS -K1m -RTS
SRC_RUNTEST_OPTS += -o1 hidden.stdout2 -o1 hidden.stdout3 objects/four.plate

include $(TOP)/mk/target.mk

