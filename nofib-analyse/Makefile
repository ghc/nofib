TOP=..
include $(TOP)/mk/boilerplate.mk

PROG = nofib-analyse

$(PROG): *.hs
	$(BOOT_HC) -O -cpp --make Main -o $(PROG)

all :: $(PROG)

boot :: $(PROG)

# This clean hierarchy mirrors mk/ghc-target.mk.
# See mk/ghc-paths.mk for the semantics.

.PHONY: mostlyclean clean distclean maintainer-clean

mostlyclean::
	rm -f CmdLine.hi GenUtils.hi Main.hi Slurp.hi
	rm -f CmdLine.o  GenUtils.o  Main.o  Slurp.o

clean:: mostlyclean
	rm -f $(PROG)

distclean:: clean
maintainer-clean:: distclean
