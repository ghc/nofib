-- Main routine for the CPSA solver.  Provides the top-level search loop

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

{-# LANGUAGE CPP #-}

module Main (main) where

import Numeric
import System.IO
import System.IO.Error
#if defined HAVE_PAR
import Control.Parallel
#endif
import qualified Data.List as L
import System.Console.GetOpt
import CPSA.Lib.SExpr
import CPSA.Lib.Entry
import CPSA.Lib.Algebra
import CPSA.Lib.Strand
import CPSA.Lib.Cohort
import CPSA.Lib.Loader
import CPSA.Lib.Displayer
import CPSA.Lib.Expand
import qualified CPSA.Basic.Algebra
import qualified CPSA.DiffieHellman.Algebra

-- Default limit on the number of steps used to solve one problem.
defaultStepLimit :: Int
defaultStepLimit = 2000

-- Default limit on the number of strands is a skeleton.
defaultStrandBound :: Int
defaultStrandBound = 8

-- Default algebra
defaultAlgebra :: String
defaultAlgebra = CPSA.Basic.Algebra.name

-- Runtime parameters

data Params = Params
    { file :: Maybe FilePath,   -- Nothing specifies standard output
      alg :: String,            -- Name of the algebra
      analyze :: Bool,          -- False when only expanding macros
      noIsoChk :: Bool,         -- True when not performing isomorphism checks
      displacement :: Bool,     -- True when using displacement
      checkNoncesFirst :: Bool, -- True when checking nonces first
      tryOldStrandsFirst :: Bool, -- True when visiting old strands first
      tryYoungNodesFirst :: Bool, -- True when visiting young nodes first
      limit :: Int,             -- Step count limit
      bound :: Int,             -- Strand cound bound
      margin :: Int }           -- Output line length
    deriving Show

-- Entry point
main :: IO ()
main =
    do
      (p, params) <- start options $ interp algs
      sexprs <- readSExprs p
      sexprs <- try (expand sexprs) -- Expand macros
      case sexprs of
        Left err -> abort (ioeGetErrorString err)
        Right sexprs ->
            if analyze params then
                select params sexprs
            else
                prettyPrint params sexprs

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

-- Algebra specific section

-- Algebra names
algs :: [String]
algs = [CPSA.Basic.Algebra.name, CPSA.DiffieHellman.Algebra.name]

-- Select the algebra and go.
select :: Params -> [SExpr Pos] -> IO ()
select params sexprs =
    case alg params of
      name | name == CPSA.Basic.Algebra.name ->
               go name CPSA.Basic.Algebra.origin params sexprs
           | name == CPSA.DiffieHellman.Algebra.name ->
               go name CPSA.DiffieHellman.Algebra.origin params sexprs
           | otherwise ->
               abort ("Bad algebra: " ++ name)

-- Load protocols and preskeletons and print run time information
go :: Algebra t p g s e c => String -> g -> Params -> [SExpr Pos] -> IO ()
go name origin params sexprs =
    do
      preskels <- try (loadSExprs name origin sexprs)
      case preskels of          -- Load protocols and preskeletons
        Left err -> abort (ioeGetErrorString err)
        Right preskels ->
            do
              h <- outputHandle (file params)
              let m = margin params
              -- Print run time information
              writeComment h m cpsaVersion
              writeComment h m "All input read"
              case noIsoChk params of
                True -> writeComment h m "Isomorphism checking disabled"
                False -> return ()
              case limit params /= defaultStepLimit of
                True -> writeComment h m $
                        "Step count limited to " ++ show (limit params)
                False -> return ()
              case bound params /= defaultStrandBound of
                True -> writeComment h m $
                        "Strand count bounded at " ++ show (bound params)
                False -> return ()
              case displacement params of
                True -> writeComment h m "Displacement enabled"
                False -> return ()
              case checkNoncesFirst params of
                True -> writeComment h m "Nonces checked first"
                False -> return ()
              case tryOldStrandsFirst params of
                True -> writeComment h m "Old strands tried first"
                False -> return ()
              case tryYoungNodesFirst params of
                True -> writeComment h m "Younger nodes tried first"
                False -> return ()
              -- Analyze
              solve params h preskels 0

-- Just pretty the expanded macros
prettyPrint :: Params -> [SExpr a] -> IO ()
prettyPrint params sexprs =
    do
      let m = margin params
      h <- outputHandle (file params)
      writeComment h m cpsaVersion
      writeComment h m "Expanded macros"
      mapM_ (writeLnSEexpr h m) sexprs
      hClose h
      return ()

-- Command line option flags
data Flag
    = Output String             -- Output file name
    | Limit String              -- Step count limit
    | Bound String              -- Strand count bound
    | Margin String             -- Output line length
    | Expand                    -- Expand macros only
    | NoIsoChk                  -- Disable isomorphism checks
    | Displacement              -- Enable displacement
    | CheckNoncesFirst          -- Check nonces first
    | TryOldStrandsFirst        -- Try old strands first
    | TryYoungNodesFirst        -- Try young nodes first
    | Algebra String            -- Algebra
    | Algebras                  -- Show algebras
    | Help                      -- Help
    | Info                      -- Version information
      deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['o'] ["output"]   (ReqArg Output "FILE")  "output FILE",
      Option ['l'] ["limit"]    (ReqArg Limit "INT")
      ("step count limit (default " ++ show defaultStepLimit ++ ")"),
      Option ['b'] ["bound"]    (ReqArg Bound "INT")
      ("strand count bound (default " ++ show defaultStrandBound ++ ")"),
      Option ['m'] ["margin"]   (ReqArg Margin "INT")
      ("set output margin (default " ++ show defaultMargin ++ ")"),
      Option ['e'] ["expand"]   (NoArg Expand)
      "expand macros only; don't analyze",
      Option ['n'] ["noisochk"] (NoArg NoIsoChk)
      "disable isomorphism checks",
      Option ['d'] ["displacement"] (NoArg Displacement)
      "enable displacement",
      Option ['c'] ["check-nonces"] (NoArg CheckNoncesFirst)
      "check nonces first",
      Option ['t'] ["try-old-strands"] (NoArg TryOldStrandsFirst)
      "try old strands first",
      Option ['r'] ["reverse-nodes"] (NoArg TryYoungNodesFirst)
      "try younger nodes first",
      Option ['a'] ["algebra"]  (ReqArg Algebra "STRING")
      ("algebra (default " ++ defaultAlgebra ++ ")"),
      Option ['s'] ["show-algebras"] (NoArg Algebras)  "show algebras",
      Option ['h'] ["help"]     (NoArg Help)      "show help message",
      Option ['v'] ["version"]  (NoArg Info)      "show version number" ]

-- Interpret option flags
interp :: [String] -> [Flag] -> IO Params
interp algs flags =
    loop flags Params { file = Nothing, -- Default parameter values
                        alg = defaultAlgebra,
                        analyze = True,
                        noIsoChk = False,
                        displacement = False,
                        checkNoncesFirst = False,
                        tryOldStrandsFirst = False,
                        tryYoungNodesFirst = False,
                        limit = defaultStepLimit,
                        bound = defaultStrandBound,
                        margin = defaultMargin }
    where
      loop [] params = return params
      loop (Output name : flags)  params
          | file params == Nothing =
              loop flags $ params { file = Just name }
      loop (Limit value : flags) params =
          case readDec value of
            [(limit, "")] ->
                loop flags $ params { limit = limit }
            _ ->
                do
                  msg <- usage options ["Bad value for step limit\n"]
                  abort msg
      loop (Bound value : flags) params =
          case readDec value of
            [(bound, "")] ->
                loop flags $ params { bound = bound }
            _ ->
                do
                  msg <- usage options ["Bad value for strand bound\n"]
                  abort msg
      loop (Margin value : flags) params =
          case readDec value of
            [(margin, "")] ->
                loop flags $ params { margin = margin }
            _ ->
                do
                  msg <- usage options ["Bad value for margin\n"]
                  abort msg
      loop (Expand : flags) params =
          loop flags $ params { analyze = False }
      loop (NoIsoChk : flags) params =
          loop flags $ params { noIsoChk = True }
      loop (Displacement : flags) params =
          loop flags $ params { displacement = True }
      loop (CheckNoncesFirst : flags) params =
          loop flags $ params { checkNoncesFirst = True }
      loop (TryOldStrandsFirst : flags) params =
          loop flags $ params { tryOldStrandsFirst = True }
      loop (TryYoungNodesFirst : flags) params =
          loop flags $ params { tryYoungNodesFirst = True }
      loop (Algebra name : flags) params
          | elem name algs = loop flags $ params { alg = name }
          | otherwise =
              abort ("Algebra " ++ name ++ " not one of\n" ++ unlines algs)
      loop (Algebras : _) _ =
          success $ unlines algs
      loop (Help : _) _ =
          do                    -- Show help then exit with success
            msg <- usage options []
            success msg
      loop (Info : _) _ =
          success cpsaVersion
      loop _ _ =
           do                   -- Show help then exit with failure
             msg <- usage options ["Bad option combination\n"]
             abort msg

-- Parameter driven S-expression printer
wrt :: Params -> Handle -> SExpr a -> IO ()
wrt p h sexpr =
    writeLnSEexpr h (margin p) sexpr

-- A labeled and linked preskeleton
data Algebra t p g s e c => LPreskel t p g s e c
    = LPreskel { content :: Preskel t p g s e c,
                 label :: Int,
                 parent :: Maybe (LPreskel t p g s e c) }
      deriving Show

-- A skeleton that has been seen before need not be reanalyzed.
-- Instead, one looks up the label of the skeleton seen before, and
-- returns it.  What follows is the data structure used to store
-- information in the seen history used for the isomorphism check.
-- The integer is the label of the seen skeleton.
type IPreskel t p g s e c = (Gist t p g s e c, Int)

-- Is the skeleton summarized by gist g isomorphic to one with the
-- given label?
wasSeen :: Algebra t p g s e c => Gist t p g s e c ->
           IPreskel t p g s e c -> Bool
wasSeen g (g', _) = g == g'

-- A seen history as a list.

newtype Algebra t p g s e c => Seen t p g s e c = Seen [IPreskel t p g s e c]

-- Create a singleton seen history
hist :: Algebra t p g s e c => IPreskel t p g s e c -> Seen t p g s e c
hist ik = Seen [ik]

-- Add an element to the seen history.
remember :: Algebra t p g s e c => IPreskel t p g s e c ->
            Seen t p g s e c -> Seen t p g s e c
remember ik (Seen seen) = Seen (ik : seen)

-- Find an element of the seen history that satisfies a predicate.
recall :: Algebra t p g s e c => (IPreskel t p g s e c -> Bool) ->
          Seen t p g s e c -> Maybe (IPreskel t p g s e c)
recall f (Seen seen) = L.find f seen

-- Create an empty seen history
void :: Algebra t p g s e c => Seen t p g s e c
void = Seen []

-- Merge two seen histories.
merge :: Algebra t p g s e c => Seen t p g s e c ->
         Seen t p g s e c -> Seen t p g s e c
merge (Seen xs) (Seen ys) = Seen (xs ++ ys)

-- Contains the result of applying the cohort reduction rule.  The
-- last position is used to hold the reverse of the labels of the
-- seen children
data Algebra t p g s e c => Reduct t p g s e c  =
    Reduct !(LPreskel t p g s e c) !Int !Bool ![Preskel t p g s e c] ![Int]

seqList :: [a] -> [a]
seqList xs =
    loop xs
    where
      loop [] = xs
      loop (y : ys) = seq y (loop ys)

#if defined HAVE_PAR

parMap :: (a -> b) -> [a] -> [b]
parMap _ [] = []
parMap f (x:xs) =
    par y (pseq ys (y:ys))
    where
      y = f x
      ys = parMap f xs

#else

parMap :: (a -> b) -> [a] -> [b]
parMap = map

#endif

-- Entry point for analysis
solve :: Algebra t p g s e c => Params -> Handle ->
         [Preskel t p g s e c] -> Int -> IO ()
solve _ h [] _ =                -- Done
    hClose h
solve p h (k : ks) n =
    do
      wrt p h (displayProt (protocol k)) -- show protocol
      case firstSkeleton k of
        Nothing ->              -- Input cannot be made into a skeleton
            do
              let lk = LPreskel k n Nothing
              wrt p h (commentPreskel lk [] (unrealized k) False
                       "Input cannot be made into a skeleton--nothing to do")
              solve p h ks (n + 1)
        Just k' ->
            if (gist k) == (gist k') then -- Input was a skeleton
                let lk' = LPreskel k' n Nothing in
                mode p h ks (n + limit p) (n + 1)
                         (hist (gist k', n)) [lk']
            else                -- Input was not a skeleton
                do
                  let lk = LPreskel k n Nothing
                  wrt p h (commentPreskel lk [] (unrealized k) False "")
                  let lk' = LPreskel k' (n + 1) (Just lk)
                  mode p h ks (n + limit p) (n + 2)
                           (hist (gist k', n + 1))  [lk']

-- Select reduction mode, noIsoChk or normal
mode :: Algebra t p g s e c => Params -> Handle ->
        [Preskel t p g s e c] -> Int -> Int -> Seen t p g s e c ->
        [LPreskel t p g s e c] -> IO ()
mode p h ks m n seen todo =
    if noIsoChk p then
        fast p h ks m n todo     -- Peform no isomorphism checks
    else
        breadth p h ks m n seen todo

-- Function breadth handles one level of the derivation tree.
-- This ensures a breadth first derivation order.
breadth :: Algebra t p g s e c => Params -> Handle ->
           [Preskel t p g s e c] -> Int -> Int -> Seen t p g s e c ->
           [LPreskel t p g s e c] -> IO ()
breadth p h ks _ n _ [] =       -- Empty todo list
    do
      wrt p h (comment "Nothing left to do")
      solve p h ks n            -- Solve next problem
breadth p h ks m n seen todo =
    step p h ks m seen n void [] (parMap (branch p seen) todo)

-- Function step handles one skeleton in one level of the tree.
step :: Algebra t p g s e c => Params -> Handle ->
        [Preskel t p g s e c] -> Int -> Seen t p g s e c -> Int ->
        Seen t p g s e c -> [LPreskel t p g s e c] ->
        [Reduct t p g s e c] -> IO ()
step p h ks m oseen n seen todo [] = -- Empty reducts
    breadth p h ks m n (merge seen oseen) (reverse todo)
step p h _ m _ n _ todo reducts
    | n > m =                   -- Check step count
        do
          wrt p h (comment "Step limit exceeded--aborting run")
          dump p h (mktodo reducts todo) "Step limit exceeded"
step p h _ _ _ _ _ todo reducts@(Reduct lk _ _  _  _ : _)
    | nstrands (content lk) >= bound p = -- Check strand count
        do
          wrt p h (comment "Strand bound exceeded--aborting run")
          dump p h (mktodo reducts todo)  "Strand bound exceeded"
step p h ks m oseen n seen todo (Reduct lk size cols kids dups : reducts)
    | size <= 0 =               -- Interpret empty reducts
        do
          let ns = unrealized (content lk)
          let shape = null ns
          wrt p h  (commentPreskel lk [] ns shape
                    (if shape then "" else "empty cohort"))
          step p h ks m oseen n seen todo reducts
    | otherwise =
        do
          let (n', seen', todo', dups') =
                  foldl (next lk) (n, seen, todo, dups) kids
          let ns = unrealized (content lk)
          let u = size - length dups'
          let msg = shows size $ showString " in cohort - " $
                         shows u " not yet seen"
          wrt p h  (commentPreskel lk (reverse dups') ns cols msg)
          step p h ks m oseen n' seen' todo' reducts

-- Expands one branch in the derivation tree.
branch :: Algebra t p g s e c => Params -> Seen t p g s e c ->
          LPreskel t p g s e c -> Reduct t p g s e c
branch p seen lk =
    Reduct lk (length kids) cols
               (seqList $ reverse unseen) (seqList dups)
    where
      kids = reduce (params p) (content lk)
      cols = all collapsed kids
      (unseen, dups) =
          foldl (duplicates seen) ([], []) kids

params :: Params -> Mode
params p =
    Mode { noGeneralization = noIsoChk p,
           addDisplacements = displacement p,
           nonceFirstOrder = checkNoncesFirst p,
           visitOldStrandsFirst = tryOldStrandsFirst p,
           reverseNodeOrder = tryYoungNodesFirst p}

-- Is preskeleton the result of a collapsing operation?
collapsed :: Algebra t p g s e c => Preskel t p g s e c -> Bool
collapsed k =
    case operation k of
      Collapsed _ _ -> True
      _ -> False

duplicates :: Algebra t p g s e c => Seen t p g s e c ->
              ([Preskel t p g s e c], [Int]) ->
                  Preskel t p g s e c -> ([Preskel t p g s e c], [Int])
duplicates seen (unseen, dups) kid =
    case recall (wasSeen $ gist kid) seen of
      Just (_, label) -> (unseen, label : dups)
      Nothing -> (kid : unseen, dups)

-- Make a todo list for dump
mktodo :: Algebra t p g s e c => [Reduct t p g s e c] ->
          [LPreskel t p g s e c] -> [LPreskel t p g s e c]
mktodo reducts todo =
    map (\(Reduct lk _ _ _ _) -> lk) reducts ++ reverse todo

type Next t p g s e c =
    (Int, Seen t p g s e c, [LPreskel t p g s e c], [Int])

-- Update state variables used by step.
next :: Algebra t p g s e c => LPreskel t p g s e c ->
        Next t p g s e c -> Preskel t p g s e c ->
        Next t p g s e c
next p (n, seen, todo, dups) k =
    let g = gist k in
    case recall (wasSeen g) seen of
      Just (_, label) ->
          (n, seen, todo, label : dups)
      Nothing ->
          (n + 1, remember (g, n) seen, lk : todo, dups)
          where
            lk = LPreskel k n (Just p) -- Label a preskeleton here

-- This function reduces without checking for isomorphisms
fast :: Algebra t p g s e c => Params -> Handle ->
        [Preskel t p g s e c] -> Int -> Int ->
        [LPreskel t p g s e c] -> IO ()
fast p h ks _ n [] =            -- Empty todo list
    do
      wrt p h (comment "Nothing left to do")
      solve p h ks n
fast p h _ m n todo
    | n > m =                   -- Check step count
        do
          wrt p h (comment "Step limit exceeded--aborting run")
          dump p h todo "Step limit exceeded"
fast p h _ _ _ todo@(lk : _)
    | nstrands (content lk) >= bound p = -- Check strand count
        do
          wrt p h (comment "Strand bound exceeded--aborting run")
          dump p h todo "Strand bound exceeded"
fast p h ks m n (lk : todo) =
    do
      let ns = unrealized (content lk)
      let ks' = reduce (params p) (content lk)
      let msg = show (length ks') ++ " in cohort"
      wrt p h (commentPreskel lk [] ns (null ns) msg)
      let (n', todo') = foldl (children lk) (n, []) ks'
      fast p h ks m n' (todo ++ reverse todo')

children :: Algebra t p g s e c => LPreskel t p g s e c ->
            (Int, [LPreskel t p g s e c]) ->
            Preskel t p g s e c -> (Int, [LPreskel t p g s e c])
children p (n, todo) k =        -- Label a preskeleton here
    (n + 1, LPreskel k n (Just p) : todo)

-- Print partial results in a form that works with analysis tools
dump :: Algebra t p g s e c => Params -> Handle ->
        [LPreskel t p g s e c] -> String -> IO ()
dump _ h [] msg =
    do
      hClose h
      abort msg
dump p h (lk : lks) msg =
    do
      let ns = unrealized $ content lk
      wrt p h (commentPreskel lk [] ns False "aborted")
      dump p h lks msg

-- Add a label, maybe a parent, a list of seen preskeletons isomorphic
-- to some members of this skeleton's cohort, and a list of unrealized
-- nodes.  If it's a shape, note this fact.  Add a comment if present.
commentPreskel :: Algebra t p g s e c => LPreskel t p g s e c ->
                  [Int] -> [Node] -> Bool -> String -> SExpr ()
commentPreskel lk seen unrealized shape msg =
    displayPreskel (content lk) l
    where
      l = L () [S () "label", N () (label lk)] : p
      p = maybe s (\p -> L () [S () "parent", N () (label p)] : s) (parent lk)
      s | null seen = r
        | otherwise = L () (S () "seen" : kids) : r
      kids = map (N ()) (L.sort (L.nub seen))
      r = L () (S () "unrealized" : nodes) : notes
      nodes = map displayNode (L.sort unrealized)
      notes = if shape then L () [S () "shape"] : msgs else msgs
      msgs = if null msg then [] else [comment msg]
