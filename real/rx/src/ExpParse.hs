-- -*- fundamental -*-

-- parse haskellized expressions

module ExpParse 

( pline -- expression or declaration
)

where


import Maybes
import Char
import Lex
import Monad

import PI
import Ids
import Syntax

import Options

import Prec


--------------------------------------------------------

paParen   p = do { llit "("; x <- p; llit ")"; return x }
paBrace   p = do { llit "{"; x <- p; llit "}"; return x }
paBracket p = do { llit "["; x <- p; llit "]"; return x }
paBackq   p = do { llit "`"; x <- p; llit "`"; return x }

paCommas  p = p `lsepBy` llit ","

--------------------------------------------------------

--------------------------------------------------------

-- elements of expressions

paNat :: PIS Int
paNat = 
    do	{ cs <-	llitp "Nat" (\ cs -> and [isDigit c | c <- cs])
	; return (foldl (\ x y -> 10 * x + (fromEnum y - fromEnum '0')) 0 cs)
	}

paString :: PIS String
paString =
    do	{ cs <- llitp "String" (\ cs -> head cs == '"') -- rely on the lexer
	; return (drop 1 (take (length cs - 1) cs))
	}

paFn :: PIS String
paFn = llitp "Fn" (\ cs -> isAlpha (head cs) && and [isAlphanum' c | c <- cs]) 

paOp :: PIS String
paOp = llitp "Op" (\ cs -> and [not (isAlphanum' c) && not(isDel c) | c <- cs]) 


paFnLikeDef :: Bool -> PIS Id
paFnLikeDef def = 
    		do { cs <-         paFn; makeidS   def cs Fn Fn }
    `mplus`	do { n <-         paNat; makenatS  def n        }
    `mplus`	do { cs <- paParen paOp; makeidS   def cs Op Fn }

paOpLikeDef :: Bool -> PIS Id
paOpLikeDef def = 
    		do { cs <-         paOp; makeidS   def cs Op Op }
    `mplus`	do { cs <- paBackq paFn; makeidS   def cs Fn Op }


-- normally, don't create identifiers
paFnLike = paFnLikeDef False
paOpLike = paOpLikeDef False

------------------------------------------------------------

-- building expressions from elements

paCApp :: PIS Exp
-- a closed (parenthesised) expression
paCApp =
		do { xs <- paBrace (paCommas paExp); return (Coll CSet xs) }
    `mplus`	do { xs <- paBracket (paCommas paExp); return (Coll CList xs) }
    `mplus`	do { xs <- paParen (paCommas paExp)
	   	; case xs of [x] -> return x; _ -> return (Coll CTuple xs) 
           	}
    `mplus` 	do { x <- paFnLike
	  	; x' <- putarityS (idname x) 0
		; return (App x' [])
		}

stairway :: Exp -> [Exp] -> PIS Exp
stairway x xs =
    do	{ opts  <- getopts
	; let at = getopt opts "apply"
	; hat <- makeidS False at Op Op
	; return (foldl	(\ l r -> App hat [l, r] ) x xs)
	}

paMCApp :: PIS Exp
-- a nonempty sequence closed expressions
paMCApp = 
    do	{ x <- paCApp; xs <- lmany paCApp
	; if null xs then return x else
	    do	{ opts <- getopts
		; caseopts opts "implicit"
		    [ ("on", stairway x xs)
		    , ("off", error ("cannot build implicit apply node: "
					++ show (x : xs)))
		    ]
		}
	}
		

paApp :: PIS Exp
-- a function application
paApp = do { x <- paFnLike 
	   ; ys <- lmany paCApp 
	   ; x' <- putarityS (idname x) (length ys)	
	   ; papp x' ys
   	   }

--papp :: Id -> [Exp] -> PIS Exp
papp id args =
    do	{ let nid = idarity id
	; let nargs = length args
	; o <- getopts ; let imp = onoff o "implicit"
	; if not imp then 
	    if nid /= nargs 
	    then error ("arities don't match: " ++ show id ++ show args)
	    else return (App id args)
	  else if nid > nargs
	  	then error ("arguments missing: " ++ show id ++ show args)
	    	else stairway (App id (take nid args)) (drop nid args)
	}


paExp :: PIS Exp
-- sequence App op App op ... App
paExp = 
    do 	{ x <- paApp `mplus` paMCApp
	; xs <- paExpRest
	; return (glue (Left x : xs))
	}

-- we store Left App, Right op (these are Ids in fact)
paExpRest = 
    do 	{ op <- paOpLike; arg <- paApp `mplus` paMCApp; rest <- paExpRest
	; return (Right op : Left arg : rest)
	}
    `mplus` return []

------------------------------------------------------------------


paCmd :: PIS ()
paCmd = 
	do { llit "local"
	   ; pushlocals

	-- just read ids, don't do anything with 'em
	   ; ids <- paCommas (paFnLikeDef True)
	   ; return ()

	   }

    `mplus`	do { llit "unlocal"
	   	; poplocals
		}

    `mplus`	do { llit "global"

	-- bit of trickery here: open new local group, read ids
	   ; pushlocals
	   ; ids <- paCommas (paFnLikeDef True)

	-- this adds most recent local group to global one
	   ; mkglobals
 
	   ; poplocals
	   }

    `mplus`	do { llit "infix"; n <- paNat; ops <- paCommas paOpLike
	   	; sequence_ [ putprecS (idname op) n Nn | op <- ops ]
	   	}
    `mplus` 	do { llit "infixl"; n <- paNat; ops <- paCommas paOpLike
	   	; sequence_ [ putprecS (idname op) n Lft | op <- ops ]
	   	}
    `mplus`	do { llit "infixr"; n <- paNat; ops <- paCommas paOpLike
	   	; sequence_ [ putprecS (idname op) n Rght | op <- ops ]
	   	}

    -- obsolete?
    `mplus` 	do { llit "arity"; n <- paNat; fns <- paCommas paFnLike
	   	; sequence_ [ putarityS (idname fn) n | fn <- fns ]
	   	}

    `mplus` 	do { llit "form"; fn <- paFnLike
	   	; do { llit "="; cs <- paString 
			; putformS (idname fn) (Passive cs)
			}
	   	`mplus` do{ n <- paNat; llit "="; cs <- paString 
			; putformS (idname fn) (Active n cs)
			}
	   	; return ()
	   	}
	



---------------------------------------------------------------------

--paTop :: PIS (Maybe Exp)
paTop  = 
      	do	{ paCmd ; return Nothing }
    `mplus` do	{ x <- paExp ; opt (llit ";"); return (Just x) }
    

-------------------------------------------------------------------
--pline :: (Opts,IdTable) -> [Char] -> (Maybe Exp,(Opts,IdTable))
pline oi cs =
    case myLex (uncomment cs) of
                -- empty input is OK
        [] -> (Nothing, oi)     
                -- otherwise parse one expression
                -- closing semicolon is OK (ignored)
        toks -> case lparse paTop oi toks of
            Right [((x, oi'), [])] -> (x, oi')
            _ -> (Just (App (usercon 0 "error") []), oi)


