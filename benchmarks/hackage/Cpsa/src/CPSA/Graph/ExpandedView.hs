-- Generates an expanded view of CPSA S-expressions as a compound
-- document that contains SVG within XHTML.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Graph.ExpandedView (expandedView) where

import qualified Data.Set as S
import Data.List (intersperse)
import System.IO
import CPSA.Lib.CPSA
import CPSA.Graph.XMLOutput
import CPSA.Graph.Config
import CPSA.Graph.SVG
import CPSA.Graph.Loader
import CPSA.Graph.Preskeleton
import CPSA.Graph.Tree

expandedView :: Handle -> Config -> Int -> [SExpr Pos] -> [Preskel] -> IO ()
expandedView h conf margin cmts ps =
    do
      hPutList h (header ps)
      comments h margin cmts
      let f = forest ps
      case f of
        [t] ->
            tdrawer h conf margin False t
        _ ->
            do
              toc h f
              mapM_ (tdrawer h conf margin True) f
      hPutList h closer
      hClose h

header :: [Preskel] -> [String]
header ps =
    ["<html xmlns=\"http://www.w3.org/1999/xhtml\">",
     "<head>",
     " <meta http-equiv=\"content-type\"" ++
     " content=\"application/xhtml+xml; charset=UTF-8\" />",
     title ps,
     "</head>",
     "<body>"]
    where
      title [] = " <title>CPSA</title>"
      title (k : _) = " <title>CPSA " ++ protocol k ++ "</title>"

comments :: Handle -> Int -> [SExpr Pos] -> IO ()
comments h margin cmts =
    do
      hPutStrLn h ""
      let xs = concat $ intersperse "\n" $ map (pp margin indent) cmts
      hPutStrLn h $ show $ mc "pre" [] xs

closer :: [String]
closer =
    ["", "</body>", "</html>"]

hPutList :: Handle -> [String] -> IO ()
hPutList h xs = mapM_ (hPutStrLn h) xs

-- Generates a list of trees within the document when there are more
-- than one.
toc :: Handle -> Forest -> IO ()
toc h f =
    do
      hPutStrLn h ""
      hPutStr h $ "<p id=\"" ++ topid ++ "\">Trees:"
      mapM_ (anchor h treeid . label . vertex) f
      hPutStrLn h ".</p>"

topid :: String
topid = "top"

anchor :: Handle -> (Int -> String) -> Int -> IO ()
anchor h id n =
    hPutStr h $ " <a href=\"#" ++ id n ++ "\">" ++ show n ++ "</a>"

-- Generates an SVG document root and puts in into a div element.
docRoot :: Config -> Float -> Float -> [Element] -> Element
docRoot conf w h es =
    ec "div" [] [ec "svg" attrs es]
    where
      attrs = [("width", showL w ++ units conf),
               ("height", showL h ++ units conf),
               ("xmlns", "http://www.w3.org/2000/svg"),
               ("xmlns:xlink", "http://www.w3.org/1999/xlink"),
               ("version", "1.1"),
               ("viewBox", viewbox),
               ("font-size", showL (font conf))]
      viewbox = "0 0 " ++ showL w ++ " " ++ showL h

-- Draws one tree
tdrawer :: Handle -> Config -> Int -> Bool -> Tree -> IO ()
tdrawer h conf margin toc t =
    do
      hPutStrLn h ""
      let id = label (vertex t)
      hPutStr h $ "<p id=\"" ++ treeid id ++ "\">Tree"
      case toc of
        True -> anchor h (\_ -> topid) id
        False -> hPutStr h $ " " ++ show id
      hPutStrLn h ".</p>"
      hPutStrLn h ""
      let (width, height, es) = tree conf t
      hPutStrLn h $ show $ docRoot conf width height $
                rect conf 0 0 width height : es
      hPutSExpr h margin (protSrc (vertex t))
      mapM_ (kdrawer h conf margin id) (collectPreskels t)

treeid :: Int -> String
treeid label = "t" ++ show label

-- Collects the preskeletons within a tree and sorts them by label.
collectPreskels :: Tree -> [Tree]
collectPreskels t =
    S.toAscList $ f S.empty t
    where
      f s t = foldl f (S.insert t s) (children t)

-- Draws one item in the tree--a preskeleton.
kdrawer :: Handle -> Config -> Int -> Int -> Tree -> IO ()
kdrawer h conf margin tid t =
    do
      hPutStrLn h ""
      let k = vertex t
      let id = label k
      hPutStr h $ "<p id=\"" ++ itemid id ++ "\">Item"
      anchor h (\_ -> treeid tid) id
      case parent k of
        Nothing -> return ()
        Just p ->
            do
              hPutStr h ", Parent:"
              anchor h itemid p
      titledList h "Child" "Children" $ map (label . vertex) (children t)
      titledList h "Seen Child" "Seen Children" $ seen k
      hPutStrLn h ".</p>"
      hPutStrLn h ""
      let (width, height, e) = kdraw conf 0 0 k
      hPutStrLn h $ show $ docRoot conf width height [defs conf, e]
      hPutSExpr h margin (preskelSrc k)

itemid :: Int -> String
itemid label = "i" ++ show label

-- Handle singular vs. plural.
titledList :: Handle -> String -> String -> [Int] -> IO ()
titledList _ _ _ [] =
    return ()
titledList h singular _ [id] =
    do
      hPutStr h $ ", " ++ singular ++ ":"
      anchor h itemid id
titledList h _ plural ls =
    do
      hPutStr h $ ", " ++ plural ++ ":"
      mapM_ (anchor h itemid) ls

hPutSExpr :: Handle -> Int -> SExpr Pos -> IO ()
hPutSExpr h margin sexpr =
    do
      hPutStrLn h ""
      hPutStrLn h $ show $ mc "pre" [] (pp margin indent sexpr)

-- S-expression pretty print parameters
indent :: Int
indent = 2
