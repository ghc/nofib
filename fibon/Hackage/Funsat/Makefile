TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = fgl/Data/Graph/Inductive/Internal/FiniteMap.hs \
       fgl/Data/Graph/Inductive/Internal/Thread.hs \
       src/Control/Monad/MonadST.hs \
       src/Funsat/Monad.hs \
       fgl/Data/Graph/Inductive/Graph.hs \
       fgl/Data/Graph/Inductive/Tree.hs \
       fgl/Data/Graph/Inductive/Basic.hs \
       fgl/Data/Graph/Inductive/Query/DFS.hs \
       bitset/Data/BitSet.hs \
       syb/src/Data/Generics/Instances.hs \
       syb/src/Data/Generics/Aliases.hs \
       syb/src/Data/Generics/Schemes.hs \
       syb/src/Data/Generics/Text.hs \
       syb/src/Data/Generics/Twins.hs \
       syb/src/Data/Generics/Builders.hs \
       syb/src/Data/Generics.hs \
       parsec/Text/Parsec/Pos.hs \
       parsec/Text/Parsec/Error.hs \
       parsec/Text/Parsec/Prim.hs \
       parsec/Text/Parsec/String.hs \
       parsec/Text/Parsec/Combinator.hs \
       parsec/Text/Parsec/Char.hs \
       parsec/Text/Parsec/Token.hs \
       parsec/Text/Parsec/ByteString/Lazy.hs \
       parsec/Text/Parsec/ByteString.hs \
       parsec/Text/Parsec.hs \
       src/Text/Tabular.hs \
       parse-dimacs/Language/CNF/Parse/ParseDIMACS.hs \
       generated/Paths_funsat.hs \
       src/Funsat/Types.hs \
       src/Funsat/Utils.hs \
       src/Funsat/Types/Internal.hs \
       src/Funsat/Utils/Internal.hs \
       src/Funsat/Resolution.hs \
       src/Funsat/Solver.hs \
       Main.hs
PROG_ARGS += hole8.cnf par16-5.cnf
HC_OPTS += -i. -isrc -igenerated -ibimap -ibitset -ifgl -iparse-dimacs -iparsec -isyb/src -package array -package base -package bytestring -package containers -package mtl -package pretty -package random -package time -XCPP -XScopedTypeVariables -XCPP -XBangPatterns -XScopedTypeVariables -XTypeSynonymInstances -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances -XFlexibleContexts -XDeriveDataTypeable -XRank2Types

include $(TOP)/mk/target.mk
