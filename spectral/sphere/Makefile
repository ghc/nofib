TOP = ../..

FAST_OPTS = 100
NORM_OPTS = 100
SLOW_OPTS = 200

ifeq "$(mode)" "slow"
SRC_RUNTEST_OPTS += -o1 spheres.slowppm
else
SRC_RUNTEST_OPTS += -o1 spheres.ppm
endif

include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/target.mk


SRC_SUPERCOMP_HC_OPTS =
