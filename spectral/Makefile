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
NOT_THESE += awards boyer2 power simple
# power: exit(1)!!
# simple: mismatched outputs
# HWL: tmp disabled for testing GUM-merged version          (20/3/01)

else
ifeq "$(NoFibWithGHCi)" "YES"
NOT_THESE = circsim constraints cryptarithm1 cryptaritm2
else
NOT_THESE = 
endif
endif

# Some tests can't be speeded up very well
ifeq "$(mode)" "fast"
NOT_THESE += cryptarithm1
endif

SUBDIRS = $(filter-out $(NOT_THESE), $(SSUBDIRS))

include $(TOP)/mk/target.mk


