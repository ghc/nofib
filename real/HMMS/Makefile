# this program uses "native" files on a Sun; so
#  it may not work on other platforms (without
#  regenerating the "native" files -- feel free...).
#  See "setup" below.
#

TOP=../..
include $(TOP)/mk/boilerplate.mk

# if ! sparc_HOST_ARCH
#all depend runtests ::
#	@echo '*** HMMS test only works on SPARC machines ****'
#else
#

# extra stuff for "setup" is at the end 

HS_SRCS =  Alignments.lhs	\
	   BalBinSTrees.lhs	\
	   Extrema.lhs		\
	   HmmConstants.lhs	\
	   HmmDensities.lhs	\
	   HmmDigraphs.lhs	\
	   Lists.lhs		\
	   MathTypes.lhs	\
	   MaybeStateT.lhs	\
	   NativeIO.lhs	\
	   Phones.lhs		\
	   PlainTextIO.lhs	\
           Pronunciations.lhs	\
	   StateT.lhs		\
	   Viterbi.lhs		\
           BatchAlign.lhs      \
	   BatchTranscribe.lhs \
	   ConvertLinearDic.lhs

# OBJS_O is for the BatchAlign pgm only -- important

HS_OBJS =   Alignments.o  	\
	    BalBinSTrees.o  	\
	    Extrema.o  		\
	    HmmConstants.o  	\
	    HmmDensities.o  	\
	    HmmDigraphs.o  	\
	    Lists.o  		\
	    MathTypes.o  	\
	    MaybeStateT.o  	\
	    NativeIO.o  	\
	    Phones.o  		\
	    PlainTextIO.o  	\
	    Pronunciations.o  	\
	    StateT.o  		\
	    Viterbi.o  		\
	    BatchAlign.o

PROG_ARGS = hmms-data/h9 hmms-data/h9.ties hmms-data/h9.dgs sentences
SRC_HC_OPTS += -fglasgow-exts -syslib util -cpp

OBJS_BatchTranscribe  =	BalBinSTrees.o	    \
			Pronunciations.o    \
			Lists.o		    \
			MaybeStateT.o	    \
			PlainTextIO.o	    \
			StateT.o	    \
			Phones.o	    \
			BatchTranscribe.o

OBJS_ConvertLinearDic = Phones.o	    \
			ConvertLinearDic.o

BatchTranscribe : $(OBJS_BatchTranscribe)
	$(HC) -o BatchTranscribe $(HC_OPTS) $(OBJS_BatchTranscribe) -syslib hbc

ConvertLinearDic : $(OBJS_ConvertLinearDic)
	$(HC) -o ConvertLinearDic $(HC_OPTS) $(OBJS_ConvertLinearDic) -syslib hbc

setup : BatchTranscribe ConvertLinearDic
	@echo "*** Running part of app/test/demo to set up ***"
	@echo " "
	@echo Running ConvertLinearDic
	@echo " "
	./ConvertLinearDic dictionary.0
	@echo " "
	@echo Running BatchTranscribe
	@echo " "
	./BatchTranscribe  dictionary.0.dgs sentences
	@echo " "
	@echo "*** Ready to run BatchAlign (via: make runtests) ***"

#
# Standard trgts
# 
include $(TOP)/mk/target.mk
