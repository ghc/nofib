-- Pretty print the input

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module Main (main) where

import System.IO
import CPSA.Lib.CPSA
import CPSA.Lib.Entry

main :: IO ()
main =
    do
      (p, (output, margin)) <- start filterOptions filterInterp
      h <- outputHandle output
      go (writeSEexprLn h margin) p
      hClose h

go :: (SExpr Pos -> IO ()) -> PosHandle -> IO ()
go f p =
    loop
    where
      loop =
          do
            x <- readSExpr p
            case x of
              Nothing ->
                  return ()
              Just sexpr ->
                  do
                    f sexpr
                    loop

writeSEexprLn :: Handle -> Int -> SExpr a -> IO ()
writeSEexprLn h margin sexpr =
    do
      writeSExpr h margin sexpr
      hPutStrLn h ""
