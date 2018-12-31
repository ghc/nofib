TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -fglasgow-exts

SRC_RUNTEST_OPTS += +RTS -H100m -RTS

CLEAN_FILES += compress2.stdin

include $(TOP)/mk/target.mk

boot :: compress2.stdin

CAT_FILES = compress2.faststdin *.hs compress2.faststdin *.hs

compress2.stdin : compress2.faststdin
	cat $(CAT_FILES) $(CAT_FILES) $(CAT_FILES) $(CAT_FILES) $(CAT_FILES) > compress2.stdin
