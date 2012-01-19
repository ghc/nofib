TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = src/Algebra/AbelianGroup/IntLinEq.hs \
       src/Algebra/AbelianGroup/UnificationMatching.hs \
       src/Algebra/AbelianGroup/Main.hs
STDIN_FILE = eqn.txt
HC_OPTS += -isrc -package base -package containers
include $(TOP)/mk/target.mk
