TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -fglasgow-exts
SRC_RUNTEST_OPTS += ebnf2ps.stdin apat -o1 ebnf2ps.stdout

# the default HS_SRCS doesn't include .ly files...
HS_SRCS += HappyParser.hs
HappyParser_HAPPY_OPTS += -g

include $(TOP)/mk/target.mk

