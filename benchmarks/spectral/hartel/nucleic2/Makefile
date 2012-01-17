TOP = ../../../..
include $(TOP)/mk/boilerplate.mk

# Without this we get the C version too!
SRCS = $(wildcard *.lhs *.hs)

SRC_HC_OPTS += -cpp -fglasgow-exts
SRC_RUNTEST_OPTS += -o1 nucleic2.stdout1 -o1 nucleic2.stdout2

include $(TOP)/mk/target.mk

