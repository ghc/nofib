TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = Data/Set/TernarySet/Internal.hs \
       Data/Set/TernarySet.hs \
       Main.hs
PROG_ARGS += olivertwist.txt +RTS -K16M -RTS
STDIN_FILE = words
HC_OPTS += -i. -package base -package binary
CLEAN_FILES += olivertwist.txt.bin
include $(TOP)/mk/target.mk
