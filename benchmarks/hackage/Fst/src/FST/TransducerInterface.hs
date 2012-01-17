{-
   **************************************************************
   * Filename      : TransducerInterface.hs                     *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 6 July, 2001                               *
   * Lines         : 85                                         *
   **************************************************************
-}

module FST.TransducerInterface ( compile,
                             compileN,
                             minimize,
                             determinize,
                             Transducer,
                             states,
                             isFinal,
                             initial,
                             finals,
                             transitions,
                             transitionList,
                             transitionsU,
                             transitionsD,
                             showTransducer,
                             module FST.RRegTypes,
                             module FST.TransducerTypes,
                             numberOfStates,
                             numberOfTransitions,
                             applyUp,
                             applyDown,
                             load,
                             save,
                             emptyTransducer,
                             parseProgram,
                             parseExp,
                             unionT,
                             productT,
                             starT,
                             compositionT
                           ) where

import FST.Parse
import FST.RRegTypes
import FST.RunTransducer
import FST.Transducer
import FST.TransducerTypes
import System.IO.Error (try)
import qualified FST.DeterministicT as D
import qualified FST.LBFT as L
import qualified FST.MinimalTBrzozowski as M

compileN :: Ord a => RReg a -> Sigma a -> Transducer a
compileN reg sigma = L.compileToTransducer reg sigma

determinize :: Ord a => Transducer a -> Transducer a
determinize transducer = D.determinize transducer

minimize :: Ord a => Transducer a -> Transducer a
minimize transducer = M.minimize transducer

compile :: Ord a => RReg a -> Sigma a -> Transducer a
compile rreg sigma = M.minimize $ nullFirstState $ L.compileToTransducer rreg sigma

numberOfStates :: Ord a => Transducer a -> Int
numberOfStates transducer = length $ states transducer

numberOfTransitions :: Ord a => Transducer a -> Int
numberOfTransitions transducer = sum [length (transitionList transducer s) |
                                      s <- states transducer]

load :: FilePath -> IO (Either String (Transducer String))
load file
 = do res <- try (readFile file)
      case res of
       Right str -> return $ Right (read str)
       Left  _   -> return $ Left $
                             "\nError:\tUnable to open \"" ++ file ++"\".\n"

save :: FilePath -> Transducer String -> IO (Either String ())
save file auto
 = do res <- try (writeFile file $ show auto)
      case res of
       Right _ -> return $ Right ()
       Left  _   -> return $ Left $
                             "\nError:\tUnable to save to \"" ++ file ++"\".\n"

emptyTransducer :: Ord a => Transducer a
emptyTransducer = compile EmptyR []
