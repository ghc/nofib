TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = GA.hs \
       Population/List.hs \
       Chromosome/GP.hs \
       examples/GPTest.hs
PROG_ARGS += 1000
HC_OPTS += -i. -package array -package base -package haskell98 -package mtl
include $(TOP)/mk/target.mk
