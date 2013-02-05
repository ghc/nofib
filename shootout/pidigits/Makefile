TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/target.mk

FAST_OPTS = 10000
NORM_OPTS = 10000
SLOW_OPTS = 10000

# The benchmark game also uses -fllvm, which we can't since it might
# not be available on the developer's machine.
HC_OPTS += -O2
