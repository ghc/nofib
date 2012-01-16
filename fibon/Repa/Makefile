TOP = ../..
include $(TOP)/mk/boilerplate.mk

SSUBDIRS = Blur Canny FFTHighPass2d FFT3d Laplace MMult Sobel Volume

SUBDIRS = $(filter-out $(NOT_THESE), $(SSUBDIRS))

include $(TOP)/mk/target.mk

