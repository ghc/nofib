TOP = ..
include $(TOP)/mk/boilerplate.mk

SSUBDIRS = anna bspt cacheprof compress compress2 fem fluid fulsom gamteb gg \
  grep hidden hpg infer lift maillist mkhprog parser pic prolog \
  reptile rsa scs symalg veritas

# Omitted: 
#	HMMS	binary file endian-ness problems

ifneq "$(HWL_NOFIB_HACK)" ""
NOT_THESE += cacheprof hidden pic
# cacheprof: **** expected exit status 0 not seen ; got 139
# hidden: expected stdout not matched by reality
# expected stdout not matched by reality
# HWL: tmp disabled for testing GUM-merged version          (20/3/01)

SUBDIRS = $(filter-out $(NOT_THESE), $(SSUBDIRS))
else
SUBDIRS = $(SSUBDIRS)
endif

include $(TOP)/mk/target.mk

