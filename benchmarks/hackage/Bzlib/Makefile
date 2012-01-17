TOP = ../../..
include $(TOP)/mk/boilerplate.mk
#Get the right path to HSC2HS_INPLACE
#Stream.hs is generated from a .hsc file
HSC2HS_INPLACE=$(GHC_TOP)/$(INPLACE_BIN)/$(GHC_HSC2HS_PGM)
SRCS = Codec/Compression/BZip/Stream.hs \
       Codec/Compression/BZip/Internal.hs \
       Codec/Compression/BZip.hs \
       Main.hs
PROG_ARGS += mito.aa.bz2
HC_OPTS += -i. -igenerated -package base -package bytestring -lbz2
CLEAN_FILES += mito.aa.bz2.roundtrip Codec/Compression/BZip/Stream.hs Codec/Compression/BZip/Stream_hsc.c
include $(TOP)/mk/target.mk
