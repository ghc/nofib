{-# LANGUAGE BangPatterns #-}
module Main(main) where

import Control.Monad
import qualified Data.ByteString.Char8 as B
import System.Environment
import System.Exit
import Text.Regex.Base.Context
import Text.Regex.PDeriv.ByteString.LeftToRight  as LR
import Text.Regex.PDeriv.ByteString.LeftToRightD as LD
import Text.Regex.PDeriv.ByteString.Posix        as PX
import Text.Regex.PDeriv.ByteString.RightToLeft  as RL
import Text.Regex.PDeriv.ByteString.TwoPasses    as TP

main = do
  (cnt, files) <- parseArgs
  forM_ [1..cnt] $ \_ -> do
    mapM_ (process e1) files
    mapM_ (process e2) files
    mapM_ (process e3) files
    mapM_ (process e4) files
    mapM_ (process e5) files
  where
    e1 = (LR.compile, LR.defaultCompOpt, LR.defaultExecOpt, LR.regexec)
    e2 = (LD.compile, LD.defaultCompOpt, LD.defaultExecOpt, LD.regexec) 
    e3 = (PX.compile, PX.defaultCompOpt, PX.defaultExecOpt, PX.regexec) 
    e4 = (RL.compile, RL.defaultCompOpt, RL.defaultExecOpt, RL.regexec) 
    e5 = (TP.compile, TP.defaultCompOpt, TP.defaultExecOpt, TP.regexec) 

mkEngine (c, cOpt, eOpt, exe) =
  let regex = case c cOpt eOpt pattern of
                Left  s -> error ("ERROR: " ++ s)
                Right r -> r
  in
  (exe, regex)
  
pattern = B.pack "^(.*) ([A-Za-z]{2}) ([0-9]{5})(-[0-9]{4})?$"

process engine f = do
  fLines <- B.lines `liftM` B.readFile f
  putStrLn $ show (count 0 fLines)
  where
    (match, regex)  = mkEngine engine 
    count !n []     = n
    count !n (x:xs) =
      case match regex x of
        Left s  -> error "Bad match attempt"
        Right a -> case a of
                      Just _  -> let n1 = n+1 in n1 `seq` count n1 xs
                      Nothing -> count n xs
parseArgs = do
  args <- getArgs
  case args of
    (cnt:files) -> case reads cnt of [(c,"")] -> return (c, files); _ -> usage
    _           -> usage
    where
      usage = putStrLn "usage: pderiv iters [FILE ..]" >> exitFailure
    

