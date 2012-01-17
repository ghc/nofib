TOP = ../../..
include $(TOP)/mk/boilerplate.mk

NORM_OPTS = -- -B$(FPTOOLS_TOP) -c Expected.hs -O -fforce-recomp

EXCLUDED_SRCS = Expected.hs

ifeq "$(HEAP)" "LARGE"
SRC_RUNTEST_OPTS += +RTS -H128m -RTS
endif

update ::
	(cd $(FPTOOLS_TOP)/compiler; \
	rm stage2/ghc.exe; \
	make stage=2)
	cp $(FPTOOLS_TOP)/compiler/stage2/ghc.exe .

include $(TOP)/mk/target.mk
