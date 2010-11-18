-- Generate views of a sequence of preskeletons

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module Main (main) where

import Numeric
import System.IO
import System.IO.Error
import System.Console.GetOpt
import CPSA.Lib.CPSA (PosHandle, SExpr, Pos)
import CPSA.Lib.Entry
import CPSA.Graph.Config
import CPSA.Graph.Loader
import CPSA.Graph.CompactView
import CPSA.Graph.ExpandedView
import CPSA.Graph.LaTeXView

-- Runtime parameters

data Params = Params
    { file :: Maybe FilePath,   -- Nothing specifies standard output
      format :: Format,         -- Output format
      margin :: Int }           -- Output line length
    deriving Show

data Format = XML | SVG | LaTeX deriving Show

main :: IO ()
main =
    do
      (p, params) <- start options interp
      sexprs <- readSExprs p
      preskels <- try (loadDefs sexprs)
      case preskels of
        Left err -> abort (ioeGetErrorString err)
        Right (_, []) ->
            abort "Empty input"
        Right (cmts, preskels) ->
            do
              h <- outputHandle (file params)
              case format params of
                LaTeX ->
                    do
                      hPutStrLn h "\\documentclass[12pt]{article}"
                      hPutStrLn h ("% " ++ cpsaVersion)
                      latexView h (margin params) cmts preskels
                _ ->
                    do
                      hPutStrLn h "<?xml version=\"1.0\"?>"
                      hPutStrLn h ("<!-- " ++ cpsaVersion ++ " -->")
                      case format params of
                        XML -> expandedView h (config False)
                               (margin params) cmts preskels
                        SVG -> compactView h (config True) preskels
                        LaTeX -> error "Bad case in main"

readSExprs :: PosHandle -> IO [SExpr Pos]
readSExprs p =
    loop []
    where
      loop xs =
          do
            x <- readSExpr p
            case x of
              Nothing ->
                  return $ reverse xs
              Just x ->
                  loop (x:xs)

-- Command line option flags
data Flag
    = Help                      -- Help
    | Info                      -- Version information
    | Expanded                  -- Select expanded format in XML
    | Compact                   -- Select compact format in SVG
    | Text                      -- Select text format in LaTeX
    | Margin String             -- Output line length
    | Output String             -- Output file name
      deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['o'] ["output"]   (ReqArg Output "FILE") "output FILE",
      Option ['x'] ["expanded"] (NoArg Expanded)
      "use expanded format (default)",
      Option ['c'] ["compact"]  (NoArg Compact)        "use compact format",
      Option ['l'] ["latex"]    (NoArg Text)           "use LaTeX format",
      Option ['m'] ["margin"]   (ReqArg Margin "INT")
      ("set output margin (default " ++ show defaultMargin ++ ")"),
      Option ['h'] ["help"]     (NoArg Help)           "show help message",
      Option ['v'] ["version"]  (NoArg Info)           "show version number" ]

-- Interpret option flags
interp :: [Flag] -> IO Params
interp flags =
    loop flags Nothing XML defaultMargin -- By default, no output file
    where                                -- and use expanded format
      loop [] file format margin =
          return Params { file = file,
                          format = format,
                          margin = margin }
      loop (Output name : flags) Nothing format margin =
          loop flags (Just name) format margin
      loop (Expanded : flags) file _ margin =
          loop flags file XML margin
      loop (Compact : flags) file _ margin =
          loop flags file SVG margin
      loop (Text : flags) file _ margin =
          loop flags file LaTeX margin
      loop (Margin value : flags) file format _ =
          case readDec value of
            [(margin, "")] ->
                loop flags file format margin
            _ ->
                do
                  msg <- usage options ["Bad value for margin\n"]
                  abort msg
      loop (Info : _) _ _ _ =
          success cpsaVersion
      loop (Help : _) _ _ _ =
          do                    -- Show help then exit with success
            msg <- usage options []
            success msg
      loop _ _ _ _ =
           do                   -- Show help then exit with failure
             msg <- usage options ["Bad option combination\n"]
             abort msg

-- Default configuration.  The lengths are in points, however the more
-- natural choice is a font relative unit of length such as ems,
-- however FireFox doesn't support these units yet.
config :: Bool -> Config
config compact =
    Config { units = "pt",
             font = font,
             stroke = 0.08 * font,
             dash = 0.50 * font,
             gap = 0.20 * font,
             tx = 4.16 * font,
             ty = 6.25 * font,
             ta = 1.75 * font,
             td = 1.16 * font,
             dx = 8.33 * font,
             dy = 6.25 * font,
             mx = 3.33 * font,
             my = 3.33 * font,
             br = 0.50 * font,
             compact = compact}
    where
      font = 12
