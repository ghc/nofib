SRCS_LHS = Main.lhs Bits.lhs LIAS.lhs
OBJS_O   = Main.o   Bits.o   LIAS.o
 
NoFibMultiModuleCompileAndRun(north_lias,-o1 north_lias.stdout)

NoFibHaskellCompile(north_lias,Main,lhs)
NoFibHaskellCompile(north_lias,Bits,lhs)
NoFibHaskellCompile(north_lias,LIAS,lhs)

NoFibDependTarget(north_lias, $(SRCS_LHS))
