TOP = ..
include $(TOP)/mk/boilerplate.mk

# Leave the net* tests out of it for the moment, runstdtest is 
# geared towards batch programs
SUBDIRS = $(wildcard frontend* arith* bugs cg* ghclib* io* obfuscated93 po* stable*)

include $(TOP)/mk/target.mk

