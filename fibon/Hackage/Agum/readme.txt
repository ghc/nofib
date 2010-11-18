This package contains a library for unification and matching in
an Abelian group and a program that exercises the library.

$ agum
Abelian group unification and matching -- :? for help
agum> 2x+y=3z
Problem:   2x + y = 3z
Unifier:   [x : g0,y : -2g0 + 3g2,z : g2]
Matcher:   [x : g0,y : -2g0 + 3z]

agum> 2x=x+y
Problem:   2x = x + y
Unifier:   [x : g1,y : g1]
Matcher:   no solution

agum> 64x-41y=a
Problem:   64x - 41y = a
Unifier:   [a : 64g1 - 41g2,x : g1,y : g2]
Matcher:   [x : -16a - 41g6,y : -25a - 64g6]

agum> :quit
