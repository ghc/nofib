TOP = ../../..
PROG = anna
SRCS = BaseDefs.hs MyUtils.hs Utils.hs AbstractVals2.hs \
       SuccsAndPreds2.hs AbstractMisc.hs Dependancy.hs \
       MakeDomains.hs  Parser2.hs  PrettyPrint.hs  LambdaLift5.hs  \
       TypeCheck5.hs EtaAbstract.hs  DomainExpr.hs AbsConc3.hs Apply.hs \
       Inverse.hs BarakiMeet.hs BarakiConc3.hs \
       Constructors.hs TExpr2DExpr.hs AbstractEval2.hs \
       PrintResults.hs Simplify.hs SmallerLattice.hs \
       FrontierMisc2.hs FrontierDATAFN2.hs FrontierGENERIC2.hs \
       StrictAn6.hs ReadTable.hs Main.hs

EXTRA_RUNTESTFLAGS = \
  -prescript ./anna.prescript -postscript ./anna.postscript \
  -i big.cor -o1 big.sum.out

include $(TOP)/nofib/mk/nofib.mk
