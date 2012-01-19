module GenMDfa where

import DfaMonad

import MonadState

dfa = Dfa v q s z delta
  where 
	 v = "+-.0123456789"
	 q = [1,2,3,4,5,6]
	 s = 1
	 z = [4,6]
	 -- delta :: st -> sy -> m st 
	 delta 1 '+' = do { accum '+' ; return 2 }
	 delta 1 '-' = do { accum '-' ; return 2 }
	 delta 1 '.' = do { accum '.' ; return 3 }
	 delta 1 '0' = do { accum '0' ; return 4 }
	 delta 1 '1' = do { accum '1' ; return 4 }
	 delta 1 '2' = do { accum '2' ; return 4 }
	 delta 1 '3' = do { accum '3' ; return 4 }
	 delta 1 '4' = do { accum '4' ; return 4 }
	 delta 1 '5' = do { accum '5' ; return 4 }
	 delta 1 '6' = do { accum '6' ; return 4 }
	 delta 1 '7' = do { accum '7' ; return 4 }
	 delta 1 '8' = do { accum '8' ; return 4 }
	 delta 1 '9' = do { accum '9' ; return 4 }
	 delta 2 '+' = do { accum '+' ; return 5 }
	 delta 2 '-' = do { accum '-' ; return 5 }
	 delta 2 '.' = do { accum '.' ; return 3 }
	 delta 2 '0' = do { accum '0' ; return 4 }
	 delta 2 '1' = do { accum '1' ; return 4 }
	 delta 2 '2' = do { accum '2' ; return 4 }
	 delta 2 '3' = do { accum '3' ; return 4 }
	 delta 2 '4' = do { accum '4' ; return 4 }
	 delta 2 '5' = do { accum '5' ; return 4 }
	 delta 2 '6' = do { accum '6' ; return 4 }
	 delta 2 '7' = do { accum '7' ; return 4 }
	 delta 2 '8' = do { accum '8' ; return 4 }
	 delta 2 '9' = do { accum '9' ; return 4 }
	 delta 3 '+' = do { accum '+' ; return 5 }
	 delta 3 '-' = do { accum '-' ; return 5 }
	 delta 3 '.' = do { accum '.' ; return 5 }
	 delta 3 '0' = do { accum '0' ; return 6 }
	 delta 3 '1' = do { accum '1' ; return 6 }
	 delta 3 '2' = do { accum '2' ; return 6 }
	 delta 3 '3' = do { accum '3' ; return 6 }
	 delta 3 '4' = do { accum '4' ; return 6 }
	 delta 3 '5' = do { accum '5' ; return 6 }
	 delta 3 '6' = do { accum '6' ; return 6 }
	 delta 3 '7' = do { accum '7' ; return 6 }
	 delta 3 '8' = do { accum '8' ; return 6 }
	 delta 3 '9' = do { accum '9' ; return 6 }
	 delta 4 '+' = do { accum '+' ; return 5 }
	 delta 4 '-' = do { accum '-' ; return 5 }
	 delta 4 '.' = do { accum '.' ; return 3 }
	 delta 4 '0' = do { accum '0' ; return 4 }
	 delta 4 '1' = do { accum '1' ; return 4 }
	 delta 4 '2' = do { accum '2' ; return 4 }
	 delta 4 '3' = do { accum '3' ; return 4 }
	 delta 4 '4' = do { accum '4' ; return 4 }
	 delta 4 '5' = do { accum '5' ; return 4 }
	 delta 4 '6' = do { accum '6' ; return 4 }
	 delta 4 '7' = do { accum '7' ; return 4 }
	 delta 4 '8' = do { accum '8' ; return 4 }
	 delta 4 '9' = do { accum '9' ; return 4 }
	 delta 5 '+' = do { accum '+' ; return 5 }
	 delta 5 '-' = do { accum '-' ; return 5 }
	 delta 5 '.' = do { accum '.' ; return 5 }
	 delta 5 '0' = do { accum '0' ; return 5 }
	 delta 5 '1' = do { accum '1' ; return 5 }
	 delta 5 '2' = do { accum '2' ; return 5 }
	 delta 5 '3' = do { accum '3' ; return 5 }
	 delta 5 '4' = do { accum '4' ; return 5 }
	 delta 5 '5' = do { accum '5' ; return 5 }
	 delta 5 '6' = do { accum '6' ; return 5 }
	 delta 5 '7' = do { accum '7' ; return 5 }
	 delta 5 '8' = do { accum '8' ; return 5 }
	 delta 5 '9' = do { accum '9' ; return 5 }
	 delta 6 '+' = do { accum '+' ; return 5 }
	 delta 6 '-' = do { accum '-' ; return 5 }
	 delta 6 '.' = do { accum '.' ; return 5 }
	 delta 6 '0' = do { accum '0' ; return 6 }
	 delta 6 '1' = do { accum '1' ; return 6 }
	 delta 6 '2' = do { accum '2' ; return 6 }
	 delta 6 '3' = do { accum '3' ; return 6 }
	 delta 6 '4' = do { accum '4' ; return 6 }
	 delta 6 '5' = do { accum '5' ; return 6 }
	 delta 6 '6' = do { accum '6' ; return 6 }
	 delta 6 '7' = do { accum '7' ; return 6 }
	 delta 6 '8' = do { accum '8' ; return 6 }
	 delta 6 '9' = do { accum '9' ; return 6 }



accum :: a -> State [a] () 
accum x = modify (\ s -> s++[x])