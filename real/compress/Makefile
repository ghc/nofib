TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRCS = BinConv.hs BinTest.hs Decode.hs Defaults.hs Encode.hs Main.hs PTTrees.hs Uncompress.hs

CLEAN_FILES += compress.stdin

Lzw_HC_OPTS = -cpp

include $(TOP)/mk/target.mk

boot :: compress.stdin

compress.stdin : compress.faststdin
	cat compress.faststdin *.hs *.c compress.faststdin *.hs *.c compress.faststdin > compress.stdin
