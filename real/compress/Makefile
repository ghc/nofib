TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRCS = BinConv.hs BinTest.hs Decode.hs Defaults.hs Encode.hs Main.hs PTTrees.hs Uncompress.hs

Lzw_HC_OPTS = -cpp

SRC_RUNTEST_OPTS += -i ../../spectral/hartel/listcompr/listcompr.stdout

include $(TOP)/mk/target.mk

