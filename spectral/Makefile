TOP = ..
include $(TOP)/mk/boilerplate.mk

SSUBDIRS = \
  ansi atom awards banner boyer boyer2 calendar cichelli circsim clausify \
  constraints cryptarithm1 cryptarithm2 cse eliza expert fibheaps fish \
  fft2 gcd hartel integer life knights mandel mandel2 minimax multiplier para \
  power pretty primetest puzzle rewrite scc simple sorting sphere treejoin

# compreals	no suitable test data
# salishan	no Haskell code!

ifneq "$(HWL_NOFIB_HACK)" ""
NOT_THESE += awards boyer2 knights power simple
# knights: Sort.lhs:15: Module `PrelNum' does not export `Num(fromInt)'
# power: exit(1)!!
# simple: mismatched outputs
# HWL: tmp disabled for testing GUM-merged version          (20/3/01)

SUBDIRS = $(filter-out $(NOT_THESE), $(SSUBDIRS))
else
SUBDIRS = $(SSUBDIRS)
endif

include $(TOP)/mk/target.mk


