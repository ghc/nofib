module DbParallel where
import Types

import Parallel

infixr  `seqt`, `seqe`, `seqi`, `seql`, `seqm`, `seqd`

#ifndef PAR
(Tip _) `seqt` y = y
_       `seqt` y = y

(Branch _ _ _) `seqe` y = y
_	       `seqe` y = y

(Root _ _ _ _) `seqd` y = y
_            `seqd` y = y

(Ok _) `seqm` y = y
_ `seqm` y = y

0 `seqi` y = y
_ `seqi` y = y

[] `seql` e = e
(x:xs) `seql` e = xs `seql` e

#else
x `seqd` y = x `seq` y
x `seqm` y = x `seq` y
x `seqt` y = x `seq` y
x `seqi` y = x `seq` y
x `seql` y = x `seq` y
#endif

