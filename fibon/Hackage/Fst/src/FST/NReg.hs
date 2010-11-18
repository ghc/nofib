{-
   **************************************************************
   * Filename      : NReg.hs                                    *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 5 July, 2001                               *
   * Lines         : 78                                         *
   **************************************************************
-}

module FST.NReg ( NReg(..), -- Neutral Regular expression.
              toRReg,   -- If possible, converts NReg to RReg
              toReg,     -- If possible, converts NReg to Reg
              nVarToSymbol
             ) where

import FST.RegTypes
import FST.RRegTypes

{- *******************************************
   * Datatype for neutral regular expression *
   *******************************************
-}

data NReg a = NCross      (NReg a) (NReg a) |
	      NComp       (NReg a) (NReg a) |
	      NUnion      (NReg a) (NReg a) |
	      NProduct    (NReg a) (NReg a) |
	      NStar       (NReg a)          |
	      NIntersect  (NReg a) (NReg a) |
	      NComplement (NReg a)          |
	      NSymbol a                     |
	      NRelation a a                 |
	      NEpsilon                      |
	      NEmptySet                     |
	      NVar String                   |
	      Fun String [NReg a]           |
	      NAll

{- **************************************
   * Convert functions toRReg and toReg *
   **************************************
-}

-- If possible, build a regular expression instead of a
-- regular relation.

toRReg :: Eq a => NReg a -> Maybe (RReg a)
toRReg reg = maybe (nRReg reg) (return.idR) (toReg reg)
 where nRReg (NEmptySet)     = return EmptyR
       nRReg (NRelation a b) = return $ r a b
       nRReg (NComp n1 n2)   = do r1 <- toRReg n1; r2 <- toRReg n2; return $ r1 <.> r2
       nRReg (NCross n1 n2)  = do r1 <- toReg n1; r2 <- toReg n2; return $ r1 <*> r2
       nRReg (NUnion n1 n2)  = case (toRReg n1,toRReg n2) of
        (Just r1,Just r2)  -> return $ r1 <|> r2
        _                  -> do r1 <- toReg n1; r2 <- toReg n2
                                 return $ idR  $ r1 <|> r2
       nRReg (NProduct n1 n2) = case (toRReg n1,toRReg n2) of
        (Just r1,Just r2) -> return $ r1 |> r2
        _                 -> do r1 <- toReg n1;r2 <- toReg n2; return $ idR $ r1 |> r2
       nRReg (NStar n1) = case (toRReg n1) of
        (Just r1) -> return $ star r1
        _         -> do r1 <- toReg n1; return $ idR $ star r1
       nRReg (NIntersect n1 n2) = do r1 <- toReg n1; r2 <- toReg n2
                                     return $ idR $ r1 <&> r2
       nRReg (NComplement n1) = do r1 <- toReg n1; return $ idR $ complement r1
       nRReg _                = Nothing

toReg :: Eq a => NReg a -> Maybe (Reg a)
toReg (NEmptySet)         = return empty
toReg (NEpsilon)          = return eps
toReg (NSymbol a)         = return $ s a
toReg (NAll)              = return allS
toReg (NUnion n1 n2)      = do r1 <- toReg n1; r2 <- toReg n2; return $ r1 <|> r2
toReg (NProduct n1 n2)    = do r1 <- toReg n1; r2 <- toReg n2; return $ r1 |> r2
toReg (NStar n1)          = do r1 <- toReg n1; return $ star r1
toReg (NIntersect n1 n2)  = do r1 <- toReg n1; r2 <- toReg n2; return $ r1 <&> r2
toReg (NComplement n1)    = do r1 <- toReg n1; return $ complement r1
toReg  _                  = Nothing

nVarToSymbol :: NReg String -> NReg String
nVarToSymbol (NCross n1 n2)     = NCross      (nVarToSymbol n1) (nVarToSymbol n2)
nVarToSymbol (NComp n1 n2)      = NComp       (nVarToSymbol n1) (nVarToSymbol n2)
nVarToSymbol (NUnion n1 n2)     = NUnion      (nVarToSymbol n1) (nVarToSymbol n2)
nVarToSymbol (NProduct n1 n2)   = NProduct    (nVarToSymbol n1) (nVarToSymbol n2)
nVarToSymbol (NStar n1)         = NStar       (nVarToSymbol n1)
nVarToSymbol (NIntersect n1 n2) = NIntersect  (nVarToSymbol n1) (nVarToSymbol n2)
nVarToSymbol (NComplement n1)   = NComplement (nVarToSymbol n1)
nVarToSymbol (NVar str)         = NSymbol str
nVarToSymbol n1                 = n1
