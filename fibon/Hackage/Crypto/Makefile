TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = Codec/Encryption/BlowfishAux.hs \
       Codec/Encryption/DESAux.hs \
       Codec/Encryption/RSA/NumberTheory.hs \
       Data/Digest/MD5Aux.hs \
       Data/LargeWord.hs \
       Codec/Encryption/DES.hs \
       Codec/Encryption/Modes.hs \
       Codec/Utils.hs \
       Codec/Encryption/Blowfish.hs \
       Codec/Encryption/Padding.hs \
       Codec/Encryption/RSA.hs \
       Codec/Encryption/RSA/EMEOAEP.hs \
       Codec/Encryption/RSA/MGF.hs \
       Data/Digest/MD5.hs \
       Data/Digest/SHA1.hs \
       Codec/Encryption/AESAux.hs \
       Codec/Encryption/AES.hs \
       Codec/Binary/BubbleBabble.hs \
       Main.hs
PROG_ARGS += plain.txt
HC_OPTS += -i. -package array -package base -package bytestring -package pretty -package random
include $(TOP)/mk/target.mk
