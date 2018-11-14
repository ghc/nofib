TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_RUNTEST_OPTS += +RTS -H50m -RTS

FAST_OPTS = -nt 8 -dt 6 -nv 15 -dv 8 -de 8
NORM_OPTS = -nt 80 -dt 10 -nv 100 -dv 10 -de 10
SLOW_OPTS = -nt 80 -dt 10 -nv 100 -dv 10 -de 10

#PROG_ARGS =

include $(TOP)/mk/target.mk

