# $Id: Makefile,v 1.3 1996/11/27 18:43:03 dnt Exp $

TOP = ..
include $(TOP)/nofib/mk/site.mk

SUBDIRS =

ifeq ($(ImaginaryNoFibTests), YES)
  SUBDIRS += imaginary
endif

ifeq ($(SpectralNoFibTests), YES)
  SUBDIRS += spectral
endif

ifeq ($(RealNoFibTests), YES)
  SUBDIRS += real
endif

ifeq ($(PENDINGNoFibTests), YES)
  SUBDIRS += PENDING
endif

ifeq ($(UNUSEDNoFibTests), YES)
  SUBDIRS += UNUSED
endif

ifeq ($(GHC_ONLYNoFibTests), YES)
  SUBDIRS += GHC_ONLY
endif

ifeq ($(PRIVATENoFibTests), YES)
  SUBDIRS += PRIVATE
endif

ifeq ($(ParallelNoFibTests), YES)
  SUBDIRS += parallel
endif

include $(TOP)/nofib/mk/nofib.mk
