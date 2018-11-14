TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/target.mk

FAST_OPTS = 12
NORM_OPTS = 17
SLOW_OPTS = 20  # official shootout setting

# The benchmark game also uses -fllvm, which we can't since it might
# not be available on the developer's machine.
HC_OPTS += -XBangPatterns -funbox-strict-fields
SRC_RUNTEST_OPTS += +RTS -K128M -H200m -RTS
