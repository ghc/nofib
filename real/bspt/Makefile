TOP = ../..
include $(TOP)/mk/boilerplate.mk
-include opts.mk

# Needed for the fromInt import in GeomNumb; mkdependHS doesn't seem to
# see the {-# OPTIONS flag in the source file.

SRC_HC_OPTS += -syslib exts

include $(TOP)/mk/target.mk

