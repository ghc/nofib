TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -cpp

# Bah.hs is a test file, which we don't want in SRCS,
# so we list the ones we do want explicitly
SRCS =  Csg.hs Fulsom.hs Interval.hs Kolor.hs Matrix.hs Oct.hs Patchlevel.hs \
	Quad.hs Raster.hs Shapes.hs Types.hs Vector.hs

SRC_RUNTEST_OPTS += 7

include $(TOP)/mk/target.mk

