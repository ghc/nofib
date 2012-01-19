-- Generate an SVG drawing of a tree of preskeletons

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Graph.Tree (Tree (..), Forest, forest, tree) where

import qualified Data.List as L
import CPSA.Graph.XMLOutput
import CPSA.Graph.Config
import CPSA.Graph.SVG
import CPSA.Graph.Loader

-- The preskeletons in the output are assembled together for display
-- into trees based on the parent relation.  In reality, the
-- relationship between preskeletons is not tree-like, but includes
-- other edges as a result of a preskeleton having cohort members that
-- have been seen before.  These members are called a tree node's
-- duplicates, and their children are displayed somewhere else in the
-- display.

data Tree = Tree
    { vertex :: Preskel,
      duplicates :: Forest,     -- Preskeletons already seen
      children :: Forest,       -- Freshly discovered preskeletons
      alive :: Bool,            -- Is preskeleton alive?
      width :: Int,             -- Number of leaf nodes
      height :: Int }           -- Longest distance to a leaf plus one
    deriving Show

instance Eq Tree where
    t0 == t1 = vertex t0 == vertex t1

instance Ord Tree where
    compare t0 t1 = compare (vertex t0) (vertex t1)

makeTree :: Preskel -> [Tree] -> [Tree] -> Tree
makeTree k dups kids =
    Tree { vertex = k, duplicates = dups, children = kids,
           alive = True,        -- The correct value is set later
           width = x (dups ++ kids), height = y (dups ++ kids) }
    where
      x [] = 1                  -- Compute the width and height
      x kids = sum (map width kids)
      y kids = 1 + foldl max 0 (map height kids)

type Forest = [Tree]

-- Assemble preskeletons into a forest and then set the alive flag
forest :: [Preskel] -> Forest
forest ks = map setLiveness (assemble ks)

-- Assemble preskeletons into a forest
assemble :: [Preskel] -> Forest
assemble ks =
    map loops (spanning ks)
    where
      loops t =                 -- Add in the other edges
          let k = vertex t in   -- in the forest
          makeTree k (map (duplicate k) (seen k))
                       (map loops (children t))
      duplicate k tag =
          case L.find (\k -> label k == tag) ks of
            Just k -> makeTree k [] []
            Nothing ->
                error ("Tree: Cannot find " ++ show tag ++
                       " seen in skeleton " ++ show (label k))

-- Compute the spanning forest of the preskeletons
spanning :: [Preskel] -> Forest
spanning [] = []
spanning (k:ks) =
    makeTree k [] kids : rest
    where
      (kids, rest) = partition (label k) (assemble ks)
      partition _ [] = ([], [])
      partition tag (t:ts) =
          let (kids, rest) = partition tag ts in
          case parent (vertex t) of
            Just parent' ->
                if parent' == tag then
                    (t:kids, rest)
                else
                    (kids, t:rest)
            Nothing ->
                (kids, t:rest)

-- Set the alive flag in each preskeleton.
setLiveness :: Tree -> Tree
setLiveness t = updateLiveness (live t) t

-- Extract the non-dead preskeletons from a tree.  A preskeleton is
-- dead if it is known to be unrealized, and all of its children are
-- unrealized.  Because of duplicates, process of computing the list
-- must be iterated.
live :: Tree -> [Preskel]
live t =
    loop []
    where
      decend ks t =
          let ks' = foldl decend ks (kids t) in
          if contain ks' (vertex t) || dead ks' t then
              ks'
          else
              vertex t : ks'
      dead ks t =
          maybe False (not . null) (unrealized (vertex t))
          && all (not . contain ks . vertex) (kids t)
      loop old =
          let new = decend old t in
          if length new == length old then
              old
          else
              loop new
      kids t = duplicates t ++ children t

updateLiveness :: [Preskel] -> Tree -> Tree
updateLiveness live t =
    t { duplicates = map (updateLiveness live) (duplicates t),
        children = map (updateLiveness live) (children t),
        alive = contain live (vertex t) }

-- Does list contain a given preskeleton?
contain :: [Preskel] -> Preskel -> Bool
contain [] _ = False
contain (k:ks) k' =
    label k == label k' || contain ks k'

-- Draw tree view of preskeleton relations
tree :: Config -> Tree -> (Float, Float, [Element])
tree conf t =
    if compact conf then
        vtree conf t
    else
        htree conf t

-- Draw a vertical tree
vtree :: Config -> Tree -> (Float, Float, [Element])
vtree conf t =
    (w, h, snd $ folddup (loop x y) (mx conf, top) t)
    where
      tw = tx conf * fromIntegral (width t - 1)  -- Tree width
      th = ty conf * fromIntegral (height t - 1) -- Tree height
      x = mx conf + tw / 2
      y = my conf
      top = [button conf x y False t]
      loop :: Float -> Float -> Bool -> (Float, [Element]) ->
              Tree -> (Float, [Element])
      loop x1 y1 dup (w, es) t =
          (w + tx conf + tw, es')
          where
            x2 = w + tw / 2
            y2 = y1 + ty conf
            es'' = button conf x2 y2 dup t :
                   line conf x1 (y1 + td conf) x2 (y2 - ta conf) : es
            es' = snd $ folddup (loop x2 y2) (w, es'') t
            tw = tx conf * fromIntegral (width t - 1)
      w = 2 * mx conf + tw                       -- Diagram width
      h = 2 * my conf + th                       -- Diagram height

-- Draw a horizontal tree
htree :: Config -> Tree -> (Float, Float, [Element])
htree conf t =
    (w, h, snd $ folddup (loop x y) (my conf, top) t)
    where
      tw = tx conf * fromIntegral (height t - 1)  -- Tree width
      th = ty conf * fromIntegral (width t - 1)   -- Tree height
      x = mx conf
      y = my conf + th / 2
      top = [button conf x (y - td conf) False t]
      loop :: Float -> Float -> Bool -> (Float, [Element]) ->
              Tree -> (Float, [Element])
      loop x1 y1 dup (h, es) t =
          (h + ty conf + th, es')
          where
            x2 = x1 + tx conf
            y2 = h + th / 2
            es'' = button conf x2 (y2 - td conf) dup t :
                   line conf x1 y1 x2 y2 : es
            es' = snd $ folddup (loop x2 y2) (h, es'') t
            th = ty conf * fromIntegral (width t - 1)
      w = 2 * mx conf + tw                       -- Diagram width
      h = 2 * my conf + th                       -- Diagram height

folddup :: (Bool -> a -> Tree -> a) -> a -> Tree -> a
folddup f z t =
    foldl (f False) (foldl (f True) z (duplicates t)) (children t)

button :: Config -> Float -> Float -> Bool -> Tree -> Element
button conf x y dup t =
    kbutton conf x y kind (label (vertex t))
    where
      kind =
          case (alive t, dup) of
            (True, False) -> AliveTree
            (True, True) -> AliveDup
            (False, False) -> DeadTree
            (False, True) -> DeadDup
