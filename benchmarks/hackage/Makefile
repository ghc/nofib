TOP = ../..
include $(TOP)/mk/boilerplate.mk

SSUBDIRS = \
  Agum Bzlib Cpsa Crypto Fgl Fst Funsat Gf HaLeX Happy Hgalib \
  Palindromes Pappy QuickCheck Regex Simgi TernaryTrees Xsact

SUBDIRS = $(filter-out $(NOT_THESE), $(SSUBDIRS))

include $(TOP)/mk/target.mk

