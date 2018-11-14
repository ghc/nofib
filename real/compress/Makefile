TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_RUNTEST_OPTS += -stdout-binary

SRCS = BinConv.hs BinTest.hs Decode.hs Defaults.hs Encode.hs Main.hs PTTrees.hs Uncompress.hs

Lzw_HC_OPTS = -cpp

include $(TOP)/mk/target.mk

boot :: compress.stdin compress.faststdin compress.stdout

compress.stdin : compress.faststdin
	cat compress.faststdin *.hs *.c compress.faststdin *.hs *.c compress.faststdin > compress.stdin

compress.stdout : compress.stdin compress
	./compress < compress.stdin > compress.stdout


clean ::
	rm compress.stdin compress.stdout
