#################################################################################
#
#			    nofib/Makefile
#
#		Toplevel Makefile for the nofib project
#
# 		$Id: Makefile,v 1.4 1997/03/14 08:02:40 simonpj Exp $
#
#################################################################################

TOP = .
include $(TOP)/mk/boilerplate.mk

# Set up which parts of the nofib suite that is to be
# run. See $(FPTOOLS_TOP)/mk/config.mk, which tells you how
# to set NoFibSubDirs
#
# As usual,if you want to override these, create
# $(FPTOOLS)/mk/build.mk containing the flags and options
# you want to use in a build tree.
SUBDIRS = $(NoFibSubDirs)


# Include the standard targets, one of which
# causes make to descend into the SUBDIRS.
include $(TOP)/mk/target.mk

