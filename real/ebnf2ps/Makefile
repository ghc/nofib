TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -fglasgow-exts -package lang

PROG_ARGS = ebnf2ps.stdin apat
STDIN_FILE = /dev/null

# the default HS_SRCS doesn't include .ly files...
HS_SRCS += HappyParser.hs
GHC_HAPPY_OPTS = -g

include $(TOP)/mk/target.mk

