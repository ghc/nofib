module Main where

main :: IO ()
main = print (last [1, 2])

null xs = case xs of
            _:_ -> False
            []  -> True

not b = if b then False else True

-- lst : {xs | not (null xs)} -> Ok
last_rec last xs = case xs of
                     y:ys -> case ys of
                               []   -> y
                               z:zs -> last ys

last_check last xs = last_rec last_ok (if not (null xs) then xs else error "UNR")
  where last_ok xs = last (case not (null xs) of True -> xs)

{-# SUPERCOMPILE last #-}
last = last_check last