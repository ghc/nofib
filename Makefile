# $Id: Makefile,v 1.2 1996/11/26 15:44:36 dnt Exp $

TOP = ..

SUBDIRS =

ifeq ($(IncludeImaginaryNoFibTests), YES)
  SUBDIRS += imaginary
endif

ifeq ($(IncludeSpectralNoFibTests), YES)
  SUBDIRS += spectral
endif

ifeq ($(IncludeRealNoFibTests), YES)
  SUBDIRS += real
endif

ifeq ($(IncludePENDINGNoFibTests), YES)
  SUBDIRS += PENDING
endif

ifeq ($(IncludeUNUSEDNoFibTests), YES)
  SUBDIRS += UNUSED
endif

ifeq ($(IncludeGHC_ONLYNoFibTests), YES)
  SUBDIRS += GHC_ONLY
endif

ifeq ($(IncludePRIVATENoFibTests), YES)
  SUBDIRS += PRIVATE
endif

ifeq ($(IncludeParallelNoFibTests), YES)
  SUBDIRS += parallel
endif

include $(TOP)/nofib/mk/nofib.mk
