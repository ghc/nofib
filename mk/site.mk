#-----------------------------------------------------------------------------
# $Id: site.mk,v 1.3 1996/11/27 18:43:19 dnt Exp $

#-----------------------------------------------------------------------------
# Set these flags to YES or NO to determine which sets of tests wil be run

ImaginaryNoFibTests = YES
SpectralNoFibTests  = YES
RealNoFibTests      = YES
PENDINGNoFibTests   = YES
UNUSEDNoFibTests    = YES
GHC_ONLYNoFibTests  = YES
PRIVATENoFibTests   = YES
ParallelNoFibTests  = YES

#-----------------------------------------------------------------------------
# Haskell compiler

#HC = $(TOP)/ghc/driver/ghc
HC = ghc-2.01
HCFLAGS = -H32m -K2m

#-----------------------------------------------------------------------------
# Flags to use when we run a test

RUNTESTFLAGS = +RTS -H48m -K32m --RTS

#-----------------------------------------------------------------------------
# Set WAYS according to which ways you want to build the nofib suite

WAYS = normal

#WAYS = normal mc mr mt mp mg 2s 1s du p t a b c d e f g h i j k l m n o p A B

# ================================================================
# BUILDS stuff: main sequential ones

HCFLAGS_normal =
HCFLAGS_p      = -prof
HCFLAGS_t      =
HCFLAGS_u      =

# === builds: concurrent and parallel ============================

HCFLAGS_mc =
HCFLAGS_mr =
HCFLAGS_mt =
HCFLAGS_mp =
HCFLAGS_mg =

# === builds: non-std garbage collectors ==========================

HCFLAGS_2s = -gc-2s
HCFLAGS_1s = -gc-1s
HCFLAGS_du = -gc-du

# === builds: "user ways" =======================================

HCFLAGS_a =
HCFLAGS_b =
HCFLAGS_c =
HCFLAGS_d =
HCFLAGS_e =
HCFLAGS_f =
HCFLAGS_g =
HCFLAGS_h =
HCFLAGS_i =
HCFLAGS_j =
HCFLAGS_k =
HCFLAGS_l =
HCFLAGS_m =
HCFLAGS_n =
HCFLAGS_o =
HCFLAGS_A =
HCFLAGS_B =
