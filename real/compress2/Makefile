TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_RUNTEST_OPTS += -stdout-binary

SRC_HC_OPTS += -fglasgow-exts

SRC_RUNTEST_OPTS += +RTS -H100m -RTS

include $(TOP)/mk/target.mk

boot :: compress2.stdin compress2.faststdin compress2.stdout

CAT_FILES = compress2.faststdin *.hs compress2.faststdin *.hs

compress2.stdin : compress2.faststdin
	cat $(CAT_FILES) $(CAT_FILES) $(CAT_FILES) $(CAT_FILES) $(CAT_FILES) > compress2.stdin

compress2.stdout : compress2.stdin compress2
	./compress2 < compress2.stdin > compress2.stdout

clean ::
	rm -f compress2.stdin compress2.stdout
