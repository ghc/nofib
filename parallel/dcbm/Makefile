TOP = ../../..
PROG = dcbm
EXTRA_HCFLAGS = -cpp
SRCS = Types.hs Delay.hs DbParallel.hs Fwif.hs Primitives.hc DCBM.hs Main.hs
include $(TOP)/nofib/mk/nofib.mk

Primitives.hc : Primitives.lhc
	$(RM) Primitives.hc
	lit2pgm Primitives.lhc
Primitives.hi : Primitives.lhi
	$(RM) Primitives.hi
	lit2pgm -o Primitives.hi Primitives.lhi
