TOP = ../..
include $(TOP)/mk/boilerplate.mk

# Arguments to test program
SRC_RUNTEST_OPTS += +RTS -K3m -H24m -RTS
PROG_ARGS = runtime_files/27000.1 runtime_files/27000.2

include $(TOP)/mk/target.mk

FAST_OPTS = runtime_files/1500.1 runtime_files/1500.2
NORM_OPTS = runtime_files/8000.1 runtime_files/8000.2
SLOW_OPTS = runtime_files/27000.1 runtime_files/27000.2
