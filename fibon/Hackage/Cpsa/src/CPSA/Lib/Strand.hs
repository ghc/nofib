-- Strand Space data structures

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.Strand (Instance, mkInstance, bldInstance, mkListener,
    role, env, trace, height, listenerTerm, Sid, Node, mkPreskel,
    firstSkeleton, Pair, Preskel, gen, protocol, insts, orderings,
    pov, knon, kunique, kcomment, nstrands, kvars, kterms, uniqOrig,
    preskelWellFormed, verbosePreskelWellFormed, Strand, GraphStrand,
    nodes, Vertex, GraphNode, preds, event, graphNode, strands, Gist,
    gist, contract, augment, inheritRnon, inheritRunique, addListener,
    Cause (..), Direction (..), Method (..), Operation (..),
    operation, prob, homomorphism, toSkeleton, generalize, collapse)
    where

import Control.Monad
import qualified Data.List as L
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Maybe as M
import CPSA.Lib.Utilities
import CPSA.Lib.SExpr
import CPSA.Lib.Algebra
import CPSA.Lib.Protocol

{--
import System.IO.Unsafe
z :: Show a => a -> b -> b
z x y = seq (unsafePerformIO (print x)) y

zz :: Show a => a -> a
zz x = z x x

zb :: Show a => a -> Bool -> Bool
zb a False = z a False
zb _ b = b

zn :: Show a => a -> Maybe b -> Maybe b
zn x Nothing = z x Nothing
zn _ y = y

zf :: Show a => a -> Bool -> Bool
zf x False = z x False
zf _ y = y

zt :: Show a => a -> Bool -> Bool
zt x True = z x True
zt _ y = y

zi :: Algebra t p g s e c => Instance t p g s e c -> String
zi inst =
    show (map f e)
    where
      domain = rvars (role inst)
      e = reify domain (env inst)
      range = map snd e
      f (x, t) = (displayTerm (context domain) x,
                  displayTerm (context range) t)
      context ts = addToContext emptyContext ts
--}

-- Compile time switches for expermentation.

-- Sanity check: ensure no role variable occurs in a skeleton.
useCheckVars :: Bool
useCheckVars = False

-- Instances and Strand Identifiers

-- An Instance is an instance of a role, in the sense that each
-- variable in the role's trace has been replaced by a term.  The
-- trace of the instance might be shorter that the role's trace, but
-- only truncating nodes from the end of the trace.

-- A preskeleton stores its strands as a ordered sequence of
-- instances.  A strand is the position of an instance in the
-- sequence.  Duplicates are allowed in the sequence, as two strands
-- can be instantiated from the same role in the same way.

type Sid = Int                  -- Strand Identifier

data Instance t p g s e c = Instance
    { role :: Role t p g s e c, -- Role from which this was
                                -- instantiated

      env :: !e,                -- The environment used to build this
                                -- instance's trace from its role's
                                -- trace

      trace :: ![Event t p g s e c], -- Instance's trace

      height :: !Int }          -- Height of the instance
    deriving Show

-- Create a fresh instance of the given height.  The environment
-- specifies how to map some variables in the role's trace.  Unmapped
-- variables are instantiated with fresh variables to avoid naming
-- conflicts.
mkInstance :: Algebra t p g s e c => g -> Role t p g s e c ->
              e -> Int -> (g, Instance t p g s e c)
mkInstance gen role env height =
    let trace = rtrace role
        rheight = length trace in
    if height < 1 || height > rheight then
        error "Strand.mkInstance: Bad strand height"
    else
        let (gen', env') = grow (rvars role) gen env
            trace' = map (evtMap $ instantiate env') (take height trace) in
        -- Ensure every variable in the range of the environment
        -- occurs in the trace.
        case bldInstance role trace' gen' of
          Nothing -> error "Strand.mkInstance: Not an instance"
          Just (gen'', inst) -> (gen'', inst)

-- For each term that matches itself in the environment, extend the
-- mapping so that the term maps to one with a fresh set of variables.
-- It is an error if a variable in one of the terms is explicitly
-- mapped to itself in the initial environment.
grow :: Algebra t p g s e c => [t] -> g -> e -> (g, e)
grow [] gen env = (gen, env)
grow (t : ts) gen env =
    case match t t (gen, env) of
      Nothing -> grow ts gen env -- Term already mapped
      Just _ ->                  -- Otherwise make a fresh mapping
          let (gen', t') = clone gen t in
          case match t t' (gen', env) of
            Nothing -> error "Strand.grow: Internal error"
            Just (gen'', env') -> grow ts gen'' env'

-- Build an instance from a role and a trace.  Returns Nothing if the
-- trace is not an instance of the given role.
bldInstance :: Algebra t p g s e c => Role t p g s e c ->
               Trace t p g s e c -> g -> Maybe (g, Instance t p g s e c)
bldInstance _ [] _ = error "Strand.bldInstance: Bad trace"
bldInstance role trace gen =
    loop (rtrace role) trace (gen, emptyEnv) -- Loop builds env
    where
      loop _ [] (gen, env) =        -- Trace can be shorter than role's trace
          Just (gen, makeInstance role env trace)
      loop (In t : c) (In t' : c') ge =
          maybe Nothing (loop c c') (match t t' ge)
      loop (Out t : c) (Out t' : c') ge =
          maybe Nothing (loop c c') (match t t' ge)
      loop _ _ _ = Nothing

makeInstance :: Algebra t p g s e c => Role t p g s e c -> e ->
                Trace t p g s e c -> Instance t p g s e c
makeInstance role env trace =
    Instance { role = role,
               env = env,
               trace = trace,
               height = length trace }

-- This is the only place a role is generated with an empty name.
-- This is what marks a strand as a listener.
mkListener :: Algebra t p g s e c => g -> t ->
              (g, Instance t p g s e c)
mkListener gen term =
    (gen'', Instance { role = mkRole "" vars [In rterm, Out rterm] [] [] [],
                       env = env,
                       trace = [In term, Out term],
                       height = 2 })
    where
      (gen', rterm) = clone gen term -- Make role term
      vars = addVars [] rterm        -- Collect vars in role term
      (gen'', env) =
          case match rterm term (gen', emptyEnv) of
            Nothing -> error msg
            Just (gen'', env) -> (gen'', env)
      msg = "Strand.mkListener: cannot generate an environment."

listenerTerm :: Algebra t p g s e c => Instance t p g s e c -> Maybe t
listenerTerm inst =
    if "" /= rname (role inst) then
        Nothing                 -- Not a listener strand
    else
        Just $ evtTerm (trace inst !! 0)

-- Nodes, Pairs, and Graphs

-- A node is composed of two integers, a strand identifier and a
-- position.  The position identifies an event in the strand's trace.
-- The second integer must be non-negative and less than the strand's
-- height

type Node = (Sid, Int)

-- A pair gives an ordering of two nodes, meaning the first node is
-- before the second one.

type Pair = (Node, Node)

-- Graphs of preskeletons

-- A strand is what is referred to by a strand ID.

data GraphStrand e i                 -- e for event, i for instance
    = GraphStrand { inst :: i,
                    nodes :: [GraphNode e i],
                    sid :: Sid }

-- The Strand ID determines equality and orderings
instance Eq (GraphStrand e i) where
    s0 == s1 = sid s0 == sid s1

instance Ord (GraphStrand e i) where
    compare s0 s1 = compare (sid s0) (sid s1)

instance Show (GraphStrand e i) where
    showsPrec _ s = shows (sid s)

-- A vertex is what is referred to by a node.

data GraphNode e i                 -- e for event, i for instance
    = GraphNode { event :: e,
                  preds :: [GraphNode e i], -- Immediate preds including
                  strand :: GraphStrand e i,  -- strand succession edges
                  pos :: Int }  -- The position of the node in the strand

-- The node determines equality and orderings
instance Eq (GraphNode e i) where
    n0 == n1 = (strand n0, pos n0) == (strand n1, pos n1)

instance Ord (GraphNode e i) where
    compare n0 n1 = compare (strand n0, pos n0) (strand n1, pos n1)

instance Show (GraphNode e i) where
    showsPrec _ n = let (s, p) = graphNode n in
                    showChar '(' . shows s . showString ", " .
                    shows p . showChar ')'

-- The node of a vertex
graphNode :: GraphNode e i -> Node
graphNode n = (sid (strand n), pos n)

type GraphEdge e i = (GraphNode e i, GraphNode e i)

-- The pair of an edge
graphPair :: GraphEdge e i -> Pair
graphPair (n0, n1) = (graphNode n0, graphNode n1)

graphEdges :: [GraphStrand e i] -> [GraphEdge e i]
graphEdges strands =
    [ (dst, src) | s <- strands, src <- nodes s, dst <- preds src ]

data Graph e i
    = Graph { gstrands :: [GraphStrand e i],
              gedges :: [GraphEdge e i] }

-- The graph associated with a preskeleton
graph :: (i -> [d]) -> (i -> Int) -> [i] -> [Pair] -> Graph d i
graph trace height insts pairs =
    Graph { gstrands = strands,
            gedges = map getEdge pairs }
    where
      strands = [ GraphStrand { inst = inst,
                                nodes = nodes !! sid,
                                sid = sid } |
                  (sid, inst) <- zip [0..] insts ]
      nodes = [ [ GraphNode { event = trace (inst strand) !! pos,
                              preds = preds (sid, pos),
                              strand = strand,
                              pos = pos } |
                  pos <- nats (height (inst strand)) ] |
                (sid, strand) <- zip [0..] strands ]
      preds n = map getNode (entry n)
      getNode (s, p) = nodes !! s !! p
      getEdge (n0, n1) = (getNode n0, getNode n1)
      entry n = enrich n [ n0 | (n0, n1) <- pairs, n1 == n ]
      -- add strand succession edges
      enrich (s, p) ns
          | p > 0 = (s, p - 1) : ns
          | otherwise = ns

-- Does start node precede end node?
graphPrecedes :: GraphNode e i -> GraphNode e i -> Bool
graphPrecedes start end =
    let predecessors = preds end in
    any (== start) predecessors || any (graphPrecedes start) predecessors

-- Compute the transitive reduction
graphReduce :: [GraphEdge e i] -> [GraphEdge e i]
graphReduce orderings =
    filter essential orderings
    where
      essential (dst, src) =
          loop dst (L.delete dst (preds src)) [src]
      loop _ [] _ = True        -- No other path found
      loop dst (n : ns) seen
          | n == dst = False    -- There is another path
          | elem n seen = loop dst ns seen
          | otherwise = loop dst (preds n ++ ns) (n : seen)

-- Compute the transitive closure
-- This routine returns pairs that are not well ordered.
-- Deal with it!
graphClose :: [GraphEdge e i] -> [GraphEdge e i]
graphClose orderings =
    filter (not . sameStrands) (loop orderings False orderings)
    where
      loop orderings False [] = orderings
      loop orderings True [] =
          loop orderings False orderings -- restart loop
      loop orderings repeat ((n0, n1) : pairs) =
          inner orderings repeat pairs [(n, n1) | n <- preds n0]
      inner orderings repeat pairs [] =
          loop orderings repeat pairs
      inner orderings repeat pairs (p : rest)
          | elem p orderings = inner orderings repeat pairs rest
          | otherwise = inner (p : orderings) True pairs rest
      sameStrands (n0, n1) = strand n0 == strand n1

-- Preskeltons

data Preskel t p g s e c = Preskel
    { gen :: !g,
      protocol :: Prot t p g s e c,
      insts :: ![Instance t p g s e c],
      strands :: ![Strand t p g s e c],
      orderings :: ![Pair],
      edges :: ![Edge t p g s e c],
      knon :: ![t],             -- A list of atoms
      kunique :: ![t],          -- A list of atoms
      kcomment :: [SExpr ()],   -- Comments from the input
      korig :: ![(t, [Node])],  -- This is an association list with a
                                -- pair for each element of kunique.
                                -- The value associated with a term
                                -- is a list of the nodes at which it
                                -- originates--the term's provenance.
      pov :: Maybe (Preskel t p g s e c), -- Point of view, the
                                          -- original problem statement.
      strandids :: ![Sid],
      tc :: [Pair],             -- Transitive closure of orderings
                                -- Used only during generalization
      operation :: Operation t p g s e c,
      prob :: [Sid] }        -- A map from the strands in the original
    deriving Show               -- problem statement, the pov, into
                                -- these strands.

-- The pov skeleton is the only skeleton that should have Nothing in
-- its pov field.

type Strand t p g s e c
    = GraphStrand (Event t p g s e c) (Instance t p g s e c)

type Vertex t p g s e c
    = GraphNode (Event t p g s e c) (Instance t p g s e c)

type Edge t p g s e c
    = GraphEdge (Event t p g s e c) (Instance t p g s e c)

-- Data structure for tracking the causes for the creation of
-- preskeletons.

data Cause t p g s e c
    = Cause Direction Node t (Set t)
    deriving Show

data Direction = Encryption | Nonce deriving Show

data Method t p g s e c
    = Deleted Node
    | Weakened Pair
    | Separated t
    | Forgot t deriving Show

-- The operation used to generate the preskeleteton is either new via
-- the loader, a contraction, a regular augmentation, a listener
-- augmentation, or a mininization.  The augmentation includes a role
-- name and instance height.
data Operation t p g s e c
    = New
    | Contracted s (Cause t p g s e c)
    | Displaced Int Int String Int (Cause t p g s e c)
    | AddedStrand String Int (Cause t p g s e c)
    | AddedListener t (Cause t p g s e c)
    | Generalized (Method t p g s e c)
    | Collapsed Int Int
      deriving Show

-- Create a preskeleton.  The point of view field is not filled in.
-- This version is exported for use by the loader.  This preskeleton
-- must be consumed by firstSkeleton.
mkPreskel :: Algebra t p g s e c => g -> Prot t p g s e c ->
             [Instance t p g s e c] -> [Pair] -> [t] -> [t] ->
             [SExpr ()] -> Preskel t p g s e c
mkPreskel gen protocol insts orderings non unique comment =
    k { kcomment = comment }
    where
      k = newPreskel gen protocol insts orderings non unique New prob Nothing
      prob = strandids k        -- Fixed point on k is okay.

-- Strand functions

strandInst :: Algebra t p g s e c => Preskel t p g s e c ->
              Sid -> Instance t p g s e c
strandInst k strand = insts k !! strand

nstrands :: Algebra t p g s e c => Preskel t p g s e c -> Int
nstrands k = length (strandids k)

-- Convert the preskeleton made by the loader into the first skeleton
-- used in the search.
firstSkeleton :: Algebra t p g s e c => Preskel t p g s e c ->
                 Maybe (Preskel t p g s e c)
firstSkeleton k =
    do
      k <- wellFormedPreskel k
      k' <- toSkeleton False k   -- only k' should have pov = Nothing
      return $ k' { prob = strandids k', pov = Just k' }

-- Create a preskeleton.  The node ordering relation is put into the
-- preds field of each instance in this function.  The maybe uniquely
-- originating term data is also filled in.  This version is used
-- within this module.
newPreskel :: Algebra t p g s e c => g -> Prot t p g s e c ->
             [Instance t p g s e c] -> [Pair] -> [t] -> [t] ->
             Operation t p g s e c -> [Sid] ->
             Maybe (Preskel t p g s e c) -> Preskel t p g s e c
newPreskel gen protocol insts orderings non unique oper prob pov =
    let orderings' = L.nub orderings
        unique' = L.nub unique
        g = graph trace height insts orderings'
        strands = gstrands g
        edges = gedges g
        orig = map (originationNodes strands) unique'
        tc = filter pairWellOrdered (graphClose $ graphEdges strands)
        k = Preskel { gen = gen,
                      protocol = protocol,
                      insts = insts,
                      strands = strands,
                      orderings = orderings',
                      edges = edges,
                      knon = L.nub non,
                      kunique = unique',
                      kcomment = [],
                      korig = orig,
                      tc = map graphPair tc,
                      strandids = nats (length insts),
                      operation = oper,
                      prob = prob,
                      pov = pov } in
        if useCheckVars then
            checkVars k
        else k

checkVars :: Algebra t p g s e c => Preskel t p g s e c ->
             Preskel t p g s e c
checkVars k =
    foldl f k rolevars
    where
      skelvars = S.fromList $ kvars k
      rolevars = concatMap (rvars . role) (insts k)
      f k v
        | S.member v skelvars =
            error ("Strand.checkVars: role var in skel " ++ show k)
        | otherwise = k

vertex  :: Algebra t p g s e c => Preskel t p g s e c -> Node ->
           Vertex t p g s e c
vertex k (s, p) =
    nodes (strands k !! s) !! p

originationNodes :: Algebra t p g s e c => [Strand t p g s e c] ->
                    t -> (t, [Node])
originationNodes strands u =
    (u, [ (sid strand, p) |
          strand <- reverse strands,
          p <- M.maybeToList $ originationPos u (trace (inst strand)) ])

uniqOrig :: Algebra t p g s e c => Preskel t p g s e c -> [t]
uniqOrig k =
    do
      (t, [_]) <- reverse (korig k)
      return t

-- A preskeleton is well formed if the ordering relation is acyclic,
-- each atom declared to be uniquely-originating is carried in
-- some preskeleton term, and every variable that occurs in each base
-- term declared to be non-originating occurs in some preskeleton
-- term, and the atom must never be carried by any term, and
-- every uniquely originating role term mapped by an instance is
-- mapped to a term that originates on the instance's strand.

preskelWellFormed :: Algebra t p g s e c => Preskel t p g s e c -> Bool
preskelWellFormed k =
    varSubset (knon k) terms &&
    all nonCheck (knon k) &&
    all uniqueCheck (kunique k) &&
    wellOrdered k && acyclicOrder k &&
    roleOrigCheck k
    where
      terms = kterms k
      nonCheck t = all (not . carriedBy t) terms
      uniqueCheck t = any (carriedBy t) terms

-- Do notation friendly preskeleton well formed check.
wellFormedPreskel :: Algebra t p g s e c => Preskel t p g s e c ->
                     Maybe (Preskel t p g s e c)
wellFormedPreskel k =
    assert preskelWellFormed k

-- A version of preskelWellFormed that explains why a preskeleton is
-- not well formed.
verbosePreskelWellFormed :: (Monad m, Algebra t p g s e c) =>
                            Preskel t p g s e c -> m ()
verbosePreskelWellFormed k =
    do
      failwith "a variable in non-orig is not in some trace"
                   $ varSubset (knon k) terms
      mapM_ nonCheck $ knon k
      mapM_ uniqueCheck $ kunique k
      failwith "ordered pairs not well formed" $ wellOrdered k
      failwith "cycle found in ordered pairs" $ acyclicOrder k
      failwith "an inherited unique doesn't originate in its strand"
                   $ roleOrigCheck k
    where
      terms = kterms k
      nonCheck t =
          failwith (showString "non-orig " $ showst t " carried")
                       $ all (not . carriedBy t) terms
      uniqueCheck t =
          failwith (showString "uniq-orig " $ showst t " not carried")
                       $ any (carriedBy t) terms

failwith :: Monad m => String -> Bool -> m ()
failwith msg test =
    case test of
      True -> return ()
      False -> fail msg

showst :: Algebra t p g s e c => t -> ShowS
showst t =
    shows $ displayTerm (addToContext emptyContext [t]) t

-- Do the nodes in the orderings have the right direction?
wellOrdered :: Algebra t p g s e c => Preskel t p g s e c -> Bool
wellOrdered k =
    all pairWellOrdered (edges k)

pairWellOrdered :: Algebra t p g s e c => Edge t p g s e c -> Bool
pairWellOrdered (n0, n1) =
    case (event n0, event n1) of
      (Out _, In _) -> True
      _ -> False

-- The terms used in the strands in this preskeleton.
-- Should this return a set, or a multiset?
kterms :: Algebra t p g s e c => Preskel t p g s e c -> [t]
kterms k = iterms (insts k)

-- The terms used in a list of instances.
iterms :: Algebra t p g s e c => [Instance t p g s e c] -> [t]
iterms insts =
    foldl addSTerms [] insts
    where
      addSTerms ts s =
          foldl (flip $ adjoin . evtTerm) ts (trace s)

-- The node orderings form an acyclic order if there are no cycles.
-- Use depth first search to detect cycles.  A graph with no node with
-- an indegree of zero is cyclic and must not be checked with depth
-- first search.
acyclicOrder :: Algebra t p g s e c => Preskel t p g s e c -> Bool
acyclicOrder k =
    all (not . backEdge numbering) edges
    where
      edges = graphEdges (strands k)
      -- The starting set contains the last node in every strand
      start = map (last . nodes) (strands k)
      -- Remove nodes that have non-zero indegree
      start' = foldl (flip L.delete) start (map fst edges)
      numbering = dfs preds start'

-- Variables in this preskeleton, excluding ones in roles, and ones
-- that only occur in a cause.
kvars :: Algebra t p g s e c => Preskel t p g s e c -> [t]
kvars k =
    S.elems $ foldl f S.empty (insts k)
    where
      f s i = foldl g s (reify (rvars (role i)) (env i))
      g s (_, t) = foldVars (flip S.insert) s t

-- Ensure each role unique origination assumption mapped by an
-- instance originates in the instance's strand.
roleOrigCheck :: Algebra t p g s e c => Preskel t p g s e c -> Bool
roleOrigCheck k =
    all strandRoleOrig (strands k) -- Check each strand
    where
      strandRoleOrig strand =   -- Check each role unique used in strand
          all (uniqRoleOrig strand) $ ruorig $ role $ inst strand
      uniqRoleOrig strand (ru, pos)
          | pos < height (inst strand) =
              case lookup (instantiate (env $ inst strand) ru) (korig k) of
                Nothing -> True     -- role term not mapped
                Just ns -> any (\(s, p)-> sid strand == s && p == pos) ns
          | otherwise = True

-- Isomorphism Check

-- Are two skeletons equivalent?  Two skeletons are equivalent if they
-- are isomorphic.  A key efficiency requirement in the implementation
-- of the cryptograhic protocol shapes analysis algorithm is to ensure
-- only one member of each equivalence class of skeletons is analyzed,
-- and the results of that analysis is immediately used for all other
-- members of the equivalence class.

-- To meet this requirement, a list of skeletons that have been seen
-- is maintained.  Before a skeleton is put on a to do list for
-- analysis, it is checked to see if it is ismorphic to one already
-- seen.  If so, the results of the analysis for the ismorphic
-- skeleton is used instead of putting the skeleton on the to do list.

-- Once a skeleton has been printed, the only reason for saving it is
-- for isomorphism checking.  The isomorphism check is performed
-- frequently, so an specialized data structure is used.  The gist of
-- a skeleton is all that is needed for the test for equivalence.

data Gist t p g s e c = Gist
    { ggen :: g,
      gtraces :: [(Int, Trace t p g s e c)],
      gorderings :: [Pair],
      gnon :: [t],             -- A list of non-originating terms
      gunique :: [t],          -- A list of uniquely-originating terms
      nvars :: !Int,           -- Number of variables
      ntraces :: !Int,         -- Number of traces
      norderings :: !Int,      -- Number of orderings
      nnon :: !Int,            -- Number of non-originating terms
      nunique :: !Int }        -- Number of uniquely-originating terms
    deriving Show

gist :: Algebra t p g s e c => Preskel t p g s e c ->
        Gist t p g s e c
gist k =
    Gist { ggen = gen k,
           gtraces = gtraces,
           gorderings = gorderings,
           gnon = gnon,
           gunique = gunique,
           nvars = length (kvars k),
           ntraces = length gtraces,
           norderings = length gorderings,
           nnon = length gnon,
           nunique = length gunique }
    where
      gtraces = map (\i -> (height i, trace i)) (insts k)
      gorderings = orderings k
      gnon = knon k
      gunique = kunique k

-- Test to see if two preskeletons are isomorphic

-- First, ensure the two preskeletons have:
-- 1. The same number of variables
-- 2. The same number of strands
-- 3. The same number of node orderings
-- 4. The same number of terms in knon and kunique

-- Next compute the plausible permutations and substitutions.

-- Next, for each permutation of the strands, eliminate the ones that
-- map a strand to another of a different length, and don't cause the
-- node orderings to be equal.

-- For permutations that meet the previous conditions, see if there is
-- a renaming that maps every strand trace in one preskeleton into the
-- appropriate one in the other preskeleton.  Finally, check to see if
-- the renaming works in the nr and ur terms.

instance Algebra t p g s e c => Eq (Gist t p g s e c) where
    g == g' = isomorphic g g'

isomorphic :: Algebra t p g s e c => Gist t p g s e c ->
              Gist t p g s e c -> Bool
isomorphic g g' =
    nvars g == nvars g' &&
    ntraces g == ntraces g' &&
    norderings g == norderings g' &&
    nnon g == nnon g' &&
    nunique g == nunique g' &&
    any (tryPerm g g') (permutations g g')

-- Extend a permutation while extending a substitution
-- Extend by matching later strands first
permutations :: Algebra t p g s e c => Gist t p g s e c ->
                Gist t p g s e c -> [((g, e), [Sid])]
permutations g g' =
    map rev $ perms (ggen g', emptyEnv)
                    (reverse $ gtraces g)
                    (reverse $ nats $ ntraces g)
    where
      perms env [] [] = [(env, [])]
      perms env ((h, c):hcs) xs =
          [ (env'', x:ys) |
            x <- xs,
            let (h', c') = gtraces g' !! x,
            h == h',
            env' <- M.maybeToList $ jibeTraces c c' env,
            (env'', ys) <- perms env' hcs (L.delete x xs) ]
      perms _ _ _ = error "Strand.permutations: lists not same length"
      rev (env, xs) = (env, reverse xs)

-- Length of matched traces must agree.
jibeTraces :: Algebra t p g s e c => Trace t p g s e c ->
              Trace t p g s e c -> (g, e) -> Maybe (g, e)
jibeTraces [] [] ge = Just ge
jibeTraces (In t : c) (In t' : c') ge =
    maybe Nothing (jibeTraces c c') (match t t' ge)
jibeTraces (Out t : c) (Out t' : c') ge =
    maybe Nothing (jibeTraces c c') (match t t' ge)
jibeTraces _ _ _ = Nothing

{-
-- Here is the permutation algorithm used

permutations :: Int -> [[Int]]
permutations n =
    perms (nats n)
    where
      perms []  = [[]]
      perms xs = [ x:ys | x <- xs, ys <- perms (L.delete x xs) ]

-- Here is the usual algorithm

-- Returns a list of all the permutations of the natural numbers less
-- that the argument.  The identity permutation is the first one in
-- the returned list.  The code is based on a function in Haskell 1.3.
permutations :: Int -> [[Int]]
permutations n =
    perms (nats n)
    where
      perms [] = [[]]
      perms (x:xs) = [zs | ys <- perms xs, zs <- interleave x ys]
      interleave x [] = [[x]]
      interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)
-}

tryPerm :: Algebra t p g s e c => Gist t p g s e c ->
           Gist t p g s e c -> ((g, e), [Sid]) -> Bool
tryPerm g g' (env, perm) =
    checkOrigs g g' env &&
    containsMapped (permutePair perm) (gorderings g') (gorderings g)

-- containsMapped f xs ys is true when list xs contains each element
-- in ys after being mapped with function f.
containsMapped :: Eq a => (a -> a) -> [a] -> [a] -> Bool
containsMapped f xs ys =
    all (flip elem xs) (map f ys)

permutePair :: [Sid] -> Pair -> Pair
permutePair perm (n, n') = (permuteNode perm n, permuteNode perm n')

permuteNode :: [Sid] -> Node -> Node
permuteNode perm (strand, pos) = (perm !! strand, pos)

checkOrigs :: Algebra t p g s e c => Gist t p g s e c ->
              Gist t p g s e c -> (g, e) -> Bool
checkOrigs g g' env =
    not (null
         [ env'' |
           env' <- checkOrig env (gnon g) (gnon g'),
           env'' <- checkOrig env' (gunique g) (gunique g'),
           matchRenaming env'' ])

-- Try all permutations as done above
checkOrig :: Algebra t p g s e c => (g, e) -> [t] -> [t] -> [(g, e)]
checkOrig env [] [] = [env]
checkOrig env (t:ts) ts' =
    [ env'' |
      t' <- ts',
      env' <- M.maybeToList $ match t t' env,
      env'' <- checkOrig env' ts (L.delete t' ts') ]
checkOrig _ _ _ = error "Strand.checkOrig: lists not same length"

-- Preskeleton Reduction System (PRS)

-- The PRS reduces a preskeleton to skeleton or fails.  Along the way,
-- it applies the associtated homomorphism to a node and computes a
-- substitution.  Thus if skel (k, n, empty) = Just (k', n', sigma), then
-- pi,sigma is a homomorphism from k to k', n' = pi(n).

type PRS t p g s e c = (Preskel t p g s e c, Node, s)

-- Returns the preskeleton that results from applying a substitution
-- or nothing if fails to preserve the nodes at which each maybe
-- uniquely originating term originates, or produces a strand that is
-- not an instance of its role.

ksubst :: Algebra t p g s e c => PRS t p g s e c ->
          (g, s) -> Maybe (PRS t p g s e c)
ksubst (k, n, hsubst) (gen, subst) =
    do
      (gen', insts') <- foldMapM (substInst subst) gen (insts k)
      let non' = map (substitute subst) (knon k)
      let unique' = map (substitute subst) (kunique k)
      let operation' = substOper subst (operation k)
      let k' = newPreskel gen' (protocol k) insts'
               (orderings k) non' unique' operation' (prob k) (pov k)
      k' <- wellFormedPreskel k'
      case validateSubst k subst k' of
        True -> return (k', n, compose subst hsubst)
        False -> Nothing

-- Monad version of mapAccumR
foldMapM :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
foldMapM _ acc [] = return (acc, [])
foldMapM f acc (x:xs) =
    do
      (acc', xs') <- foldMapM f acc xs
      (acc'', x') <- f acc' x
      return (acc'', x':xs')

substInst :: Algebra t p g s e c => s -> g -> Instance t p g s e c ->
             Maybe (g, Instance t p g s e c)
substInst subst gen i =
    bldInstance (role i) (map (evtMap $ substitute subst) (trace i)) gen

substOper :: Algebra t p g s e c => s ->
             Operation t p g s e c ->
             Operation t p g s e c
substOper _ New = New
substOper subst (Contracted s cause) =
    Contracted (compose subst s) (substCause subst cause)
substOper _ m@(Displaced _ _ _ _ _) = m
substOper subst (AddedStrand role height cause) =
    AddedStrand role height (substCause subst cause)
substOper subst (AddedListener t cause) =
    AddedListener (substitute subst t) (substCause subst cause)
substOper _ m@(Generalized _) = m
substOper _ m@(Collapsed _ _) = m

substCause :: Algebra t p g s e c => s ->
              Cause t p g s e c ->
              Cause t p g s e c
substCause subst (Cause dir n t escape) =
    Cause dir n (substitute subst t) (S.map (substitute subst) escape)

-- Ensure origination nodes a preserved as required to be a homomorphism
validateSubst :: Algebra t p g s e c => Preskel t p g s e c ->
                 s -> Preskel t p g s e c -> Bool
validateSubst k subst k' =
    all check (korig k)
    where
      check (u, ns) =
          case lookup (substitute subst u) (korig k') of
            Nothing -> False
            Just ns' -> all (flip elem ns') ns

-- A compression (s is to be eliminated)
compress :: Algebra t p g s e c => PRS t p g s e c ->
            Sid -> Sid -> Maybe (PRS t p g s e c)
compress (k, n, hsubst) s s' =
    do
      let perm = updatePerm s s' (strandids k)
      orderings' <- normalizeOrderings (permuteOrderings perm (orderings k))
      let k' =
              newPreskel
              (gen k)
              (protocol k)
              (deleteNth s (insts k))
              orderings'
              (knon k)
              (kunique k)
              (operation k)
              (updateProb perm (prob k))
              (pov k)
      k'' <- wellFormedPreskel k'
      return (k'', permuteNode perm n, hsubst)

permuteOrderings :: [Sid] -> [Pair] -> [Pair]
permuteOrderings perm orderings = map (permutePair perm) orderings

updatePerm :: Int -> Int -> [Sid] -> [Sid]
updatePerm old new perm =
    map f perm
    where
      f i =
          let j = if old == i then new else i in
          if j > old then j - 1 else j

-- Eliminates implied intrastrand orderings and fails if it finds a
-- reverse intrastrand ordering.
normalizeOrderings :: [Pair] -> Maybe [Pair]
normalizeOrderings orderings =
    loop [] orderings
    where
      loop acc [] = Just acc
      loop acc (p@((s0, p0), (s1, p1)) : ps)
          | s0 /= s1 = loop (p : acc) ps
          | p0 < p1 = loop acc ps
          | otherwise = Nothing

updateProb :: [Sid] -> [Sid] -> [Sid]
updateProb mapping prob =
    map (mapping !!) prob

-- Hulling or Ensuring Unique Origination

-- This is the starting point of the Preskeleton Reduction System
hull :: Algebra t p g s e c => Bool -> PRS t p g s e c ->
        Maybe (PRS t p g s e c)
hull prune (k, n, hsubst) =
    loop (korig k)
    where
      -- No uniques originate on more than one strand
      loop [] = enrich prune (k, n, hsubst)
      -- Found a pair that needs hulling
      loop ((_, (s, _) : (s', _) : _) : _) =
          do
            (s'', s''', subst) <- unifyStrands k s s'
            prs <- ksubst (k, n, hsubst) subst
            prs' <- compress prs s'' s'''
            hull prune prs'
      loop(_ : orig) = loop orig

-- See if two strands unify.  They can be of differing heights.  The
-- second strand returned may be longer.
unifyStrands :: Algebra t p g s e c => Preskel t p g s e c ->
                Sid -> Sid -> Maybe (Sid, Sid, (g, s))
unifyStrands k s s' =
    let i = strandInst k s
        i' = strandInst k s' in
    if height i > height i' then
        unifyStrands k s' s
    else
        do
          (gen', subst) <- unifyTraces (trace i) (trace i') (gen k, emptySubst)
          return (s, s', (gen', subst))

-- Unify traces where the first trace is allowed to be shorter than
-- the second trace.
unifyTraces :: Algebra t p g s e c => Trace t p g s e c ->
               Trace t p g s e c -> (g, s) -> Maybe (g, s)
unifyTraces [] _ subst = Just subst
unifyTraces (In t : c) (In t' : c') subst =
    maybe Nothing (unifyTraces c c') (unify t t' subst)
unifyTraces (Out t : c) (Out t' : c') subst =
    maybe Nothing (unifyTraces c c') (unify t t' subst)
unifyTraces _ _ _ = Nothing

-- Order Enrichment

-- Adds orderings so that a skeleton respects origination.

enrich :: Algebra t p g s e c => Bool -> PRS t p g s e c ->
          Maybe (PRS t p g s e c)
enrich prune (k, n, hsubst) =
    let o = foldl (addOrderings k) (orderings k) (kunique k) in
    if length o == length (orderings k) then
        maybePrune prune (k, n, hsubst)    -- Nothing to add
    else
        do
          let k' =
                  newPreskel
                  (gen k)
                  (protocol k)
                  (insts k)
                  o
                  (knon k)
                  (kunique k)
                  (operation k)
                  (prob k)
                  (pov k)
          k' <- wellFormedPreskel k'
          maybePrune prune (k', n, hsubst)

maybePrune :: Algebra t p g s e c => Bool -> PRS t p g s e c ->
              Maybe (PRS t p g s e c)
maybePrune True prs = prune prs
maybePrune False prs = reduce prs

origNode :: Algebra t p g s e c => Preskel t p g s e c ->
            t -> Maybe Node
origNode k t =
    case lookup t (korig k) of
      Nothing -> error "Strand.origNode: term not in kunique"
      Just [] -> Nothing
      Just [n] -> Just n
      Just _ -> error "Strand.origNode: not a hulled skeleton"

addOrderings :: Algebra t p g s e c => Preskel t p g s e c ->
                [Pair] -> t -> [Pair]
addOrderings k orderings t =
    case origNode k t of
      Nothing -> orderings
      Just n@(s, _) ->
          foldl f orderings (L.delete s (strandids k))
          where
            f orderings s =
                case gainedPos t (trace (strandInst k s)) of
                  Nothing -> orderings
                  Just pos -> adjoin (n, (s, pos)) orderings

-- At what position is a term gained in a trace?
gainedPos :: Algebra t p g s e c => t ->
             Trace t p g s e c -> Maybe Int
gainedPos t c =
    loop 0 c
    where
      loop _ [] = Nothing       -- Term is not carried
      loop pos (Out t' : c)
          | t `carriedBy` t' = Nothing -- Term is not gained
          | otherwise = loop (pos + 1) c
      loop pos (In t' : c)
          | t `carriedBy` t' = Just pos -- Found it
          | otherwise = loop (pos + 1) c

-- Redundant Strand Elimination (also known as pruning)

prune :: Algebra t p g s e c => PRS t p g s e c -> Maybe (PRS t p g s e c)
prune (k, n, hsubst) =
    pruneStrands (k, n, hsubst) (strandids k) (strandids k)

pruneStrands :: Algebra t p g s e c => PRS t p g s e c ->
                     [Sid] -> [Sid] -> Maybe (PRS t p g s e c)
pruneStrands prs [] _ =
    reduce prs                  -- No redundant strands found
pruneStrands prs@(k, _, _) (_:ss) [] =
    pruneStrands prs ss (strandids k)
pruneStrands prs (s:ss) (s':ss')
    | s == s' = pruneStrands prs (s:ss) ss'
    | otherwise =
        maybe (pruneStrands prs (s:ss) ss')       -- Try next pair
              prune                               -- Success
              (pruneStrand prs s s')
    where
      -- A strand is redundant if there is an environment that maps
      -- the trace of s into a prefix of the trace of s', but changes
      -- no other traces in the preskeleton.
      pruneStrand prs@(k, _, _) s s' =
          do
            env <- matchTraces
                   (trace (strandInst k s))
                   (trace (strandInst k s'))
                   (gen k, emptyEnv)
            (gen', env') <- idempotentEnvFor env
                            (concatMap (tterms . trace) $ deleteNth s $ insts k)
            case matchRenaming (gen', env') of
              True -> return ()
              False -> Nothing
            case origCheck k env' of
              True -> return ()
              False -> Nothing
            case all (precedesCheck k s s') (edges k) of
              True -> return ()
              False -> Nothing
            prs <- ksubst prs (gen', substitution env')
            compress prs s s'

matchTraces :: Algebra t p g s e c => Trace t p g s e c ->
               Trace t p g s e c -> (g, e) -> Maybe (g, e)
matchTraces [] _ env = Just env -- Pattern can be shorter
matchTraces (In t : c) (In t' : c') env =
    maybe Nothing (matchTraces c c') (match t t' env)
matchTraces (Out t : c) (Out t' : c') env =
    maybe Nothing (matchTraces c c') (match t t' env)
matchTraces _ _ _ = Nothing

origCheck :: Algebra t p g s e c => Preskel t p g s e c -> e -> Bool
origCheck k env =
    check (kunique k) && check (knon k)
    where
      check orig =
          all (pred orig) orig
      pred set item =
          elem (instantiate env item) set

precedesCheck :: Algebra t p g s e c => Preskel t p g s e c ->
                 Sid -> Sid -> Edge t p g s e c -> Bool
precedesCheck k s s' (gn0, gn1)
    | s == sid (strand gn0) = graphPrecedes (vertex k (s', pos gn0)) gn1
    | s == sid (strand gn1) = graphPrecedes gn0 (vertex k (s', pos gn1))
    | otherwise = True

-- Transitive Reduction

-- An edge is essential if its removal eliminates all paths from its
-- source to its destination.  This function removes all non-essential
-- edges from the ordering relation.

reduce :: Algebra t p g s e c => PRS t p g s e c -> Maybe (PRS t p g s e c)
reduce (k, n, hsubst) =
    let o = map graphPair (graphReduce (edges k)) in
    if length o == length (orderings k) then
        Just (k, n, hsubst)     -- Nothing to take away
    else
        do
          let k' =
                  newPreskel
                  (gen k)
                  (protocol k)
                  (insts k)
                  o
                  (knon k)
                  (kunique k)
                  (operation k)
                  (prob k)
                  (pov k)
          k'' <- wellFormedPreskel k'
          return (k'', n, hsubst)

-- Returns the skeleton associated with a preskeleton or nothing when
-- there isn't one.  Manufacture a node and a term, and then drop them
-- afterwards.
toSkeleton :: Algebra t p g s e c => Bool -> Preskel t p g s e c ->
              Maybe (Preskel t p g s e c)
toSkeleton prune k =
    do
      (k, _, _) <- hull prune (k, (0, 0), emptySubst)
      return k

-- Contraction

contract :: Algebra t p g s e c => Preskel t p g s e c -> Node ->
            Cause t p g s e c -> (g, s) -> Maybe (PRS t p g s e c)
contract k n cause subst =
    do
      prs <- ksubst (k { operation = Contracted emptySubst cause },
                       n, emptySubst) subst
      hull True prs

-- Regular Augmentation

-- Apply a substitution and then augment.  Augmentations add an
-- instance and one ordered pair.

augment :: Algebra t p g s e c => Bool -> Preskel t p g s e c ->
           Node -> Cause t p g s e c -> Role t p g s e c ->
           (g, s) -> Instance t p g s e c -> [PRS t p g s e c]
augment useDisplacement k0 n cause role subst inst =
    case substAndAugment useDisplacement k0 n cause role subst inst of
      Nothing -> []
      Just prs@(k, n, hsubst) ->
          case useDisplacement of
            False -> [prs]
            True ->
                case hull True prs of
                  Nothing -> augDisplace k0 k n hsubst
                  Just prs -> prs : augDisplace k0 k n hsubst

substAndAugment :: Algebra t p g s e c => Bool -> Preskel t p g s e c ->
                   Node -> Cause t p g s e c -> Role t p g s e c ->
                   (g, s) -> Instance t p g s e c -> Maybe (PRS t p g s e c)
substAndAugment useDisplacement k n cause role subst inst =
    do
      let operation' = AddedStrand (rname role) (height inst) cause
      prs <- ksubst (k { operation = operation' }, n, emptySubst) subst
      aug useDisplacement prs inst

aug :: Algebra t p g s e c => Bool -> PRS t p g s e c ->
       Instance t p g s e c -> Maybe (PRS t p g s e c)
aug useDisplacement (k, n, hsubst) inst =
    do
      let insts' = (insts k) ++ [inst]
      let pair = ((length (insts k), height inst - 1), n)
      let orderings' = pair : orderings k
      let non' = inheritRnon inst ++ (knon k)
      let unique' = inheritRunique inst ++ (kunique k)
      let k' = newPreskel (gen k) (protocol k) insts'
           orderings' non' unique' (operation k) (prob k) (pov k)
      k' <- wellFormedPreskel k'
      case useDisplacement of
        False ->
            do
              (k', n, hsubst) <- augCollapses k (k', n, hsubst) (nstrands k)
              hull True (k', n, hsubst)
        True -> return (k', n, hsubst)

-- Inherit non-originating atoms if the traces is long enough
inheritRnon :: Algebra t p g s e c => Instance t p g s e c -> [t]
inheritRnon i =
    inherit i (rnorig (role i))

-- Inherit uniquely originating atoms if the traces is long enough
inheritRunique :: Algebra t p g s e c => Instance t p g s e c -> [t]
inheritRunique i =
    inherit i (ruorig (role i))

inherit :: Algebra t p g s e c => Instance t p g s e c -> [(t, Int)] -> [t]
inherit i rorigs =
    map (instantiate (env i) . fst) $ filter f rorigs
    where
      f (_, pos) = pos < height i

augCollapses :: Algebra t p g s e c => Preskel t p g s e c ->
                PRS t p g s e c -> Sid -> Maybe (PRS t p g s e c )
augCollapses k0 prs@(k, _, _) s =
    case findCollapses k s of
      s':_ ->
          do
            (_, _, subst) <- unifyStrands k s s'
            augSubst k0 prs subst
      _ -> return prs

-- Find the strands that share an origination point with s
findCollapses :: Algebra t p g s e c => Preskel t p g s e c -> Sid -> [Sid]
findCollapses k s =
    L.nub [ s' | (_, nodes) <- korig k,
                 let ss = map fst nodes,
                 elem s ss,
                 s' <- ss,
                 s' /= s]

augSubst :: Algebra t p g s e c => Preskel t p g s e c ->
            PRS t p g s e c -> (g, s) -> Maybe (PRS t p g s e c)
augSubst k0 (k, n, hsubst) (gen, subst) =
    do
      (gen', insts') <- foldMapM (substInst subst) gen (insts k)
      let non' = map (substitute subst) (knon k)
      let unique' = map (substitute subst) (kunique k)
      let operation' = substOper subst (operation k)
      let k' = newPreskel gen' (protocol k) insts'
               (orderings k) non' unique' operation' (prob k) (pov k)
      k' <- wellFormedPreskel k'
      let hsubst' = compose subst hsubst
      case validateSubst k0 hsubst' k' of
        True -> return (k', n, hsubst')
        False -> Nothing

augDisplace :: Algebra t p g s e c => Preskel t p g s e c ->
               Preskel t p g s e c -> Node -> s -> [PRS t p g s e c]
augDisplace k0 k n hsubst =
    let s = nstrands k - 1 in
    [k' | s' <- nats s,
          k' <- M.maybeToList $ augDisplaceStrands k0 k n hsubst s s']

augDisplaceStrands :: Algebra t p g s e c => Preskel t p g s e c ->
                      Preskel t p g s e c -> Node -> s ->
                      Sid -> Sid -> Maybe (PRS t p g s e c)
augDisplaceStrands k0 k n hsubst s s' =
    do
      (s, s', subst) <- unifyStrands k s s'
      let op = addedToDisplaced (operation k) s s'
      prs <- augSubst k0 (k { operation = op}, n, hsubst) subst
      prs <- compress prs s s'
      hull True prs

addedToDisplaced :: Algebra t p g s e c => Operation t p g s e c ->
                    Int -> Int -> Operation t p g s e c
addedToDisplaced (AddedStrand role height cause) s s' =
    Displaced s s' role height cause
addedToDisplaced _ _ _ = error "Strand.addedToDisplaced: Bad operation"

-- Listener Augmentation

addListener :: Algebra t p g s e c => Preskel t p g s e c -> Node ->
               Cause t p g s e c -> t -> Maybe (PRS t p g s e c)
addListener k n cause t =
    do
      k' <- wellFormedPreskel k'
      hull True (k', n, emptySubst)
    where
      k' = newPreskel gen' (protocol k) insts' orderings'
           (knon k) (kunique k) (AddedListener t cause) (prob k) (pov k)
      (gen', inst) = mkListener (gen k) t
      insts' = insts k ++ [inst]
      pair = ((length (insts k), 1), n)
      orderings' = pair : orderings k

-- Homomorphisms

-- Find a substitution that demonstrates the existence of a
-- homomorphism between the two skeletons using the given
-- strand mapping function.

homomorphism :: Algebra t p g s e c => Preskel t p g s e c ->
                Preskel t p g s e c -> [Sid] -> Maybe e
homomorphism k k' mapping =
    do
      (_, env) <- findReplacement k k' mapping
      case validateEnv k k' mapping env of
        True -> Just env
        False -> Nothing

findReplacement :: Algebra t p g s e c => Preskel t p g s e c ->
                   Preskel t p g s e c -> [Sid] -> Maybe (g, e)
findReplacement k k' mapping =
    foldM (matchStrand k k' mapping) (gen k', emptyEnv) (strandids k)

matchStrand :: Algebra t p g s e c => Preskel t p g s e c ->
               Preskel t p g s e c -> [Sid] -> (g, e) -> Sid -> Maybe (g, e)
matchStrand k k' mapping env s =
    matchTraces (trace (strandInst k s)) (trace (strandInst k' s')) env
    where
      s' = mapping !! s

validateEnv :: Algebra t p g s e c => Preskel t p g s e c ->
               Preskel t p g s e c -> [Sid] -> e -> Bool
validateEnv k k' mapping env =
    all (flip elem (knon k')) (map (instantiate env) (knon k)) &&
    all (flip elem (kunique k')) (map (instantiate env) (kunique k)) &&
    validateEnvOrig k k' mapping env &&
    all (flip elem (tc k')) (permuteOrderings mapping (orderings k))

validateEnvOrig :: Algebra t p g s e c => Preskel t p g s e c ->
                   Preskel t p g s e c -> [Sid] -> e -> Bool
validateEnvOrig k k' mapping env =
    all check (korig k)
    where
      check (u, ns) =
          case lookup (instantiate env u) (korig k') of
            Nothing -> error "Strand.validateEnv: term not in kunique"
            Just ns' -> all (flip elem ns') (map (permuteNode mapping) ns)

-- Given a realized skeleton k, generate candidates for minimization.
-- A candidate is a preskeleton and a strand mapping from the
-- candidate to k.  The preskeleton need not be well formed, as that
-- test is applied elsewhere.

type Candidate t p g s e c = (Preskel t p g s e c, [Sid])

addIdentity :: Algebra t p g s e c => Preskel t p g s e c ->
               Candidate t p g s e c
addIdentity k = (k, strandids k)

generalize :: Algebra t p g s e c => Preskel t p g s e c ->
              [Candidate t p g s e c]
generalize k = deleteNodes k
               (weakenOrderings k
                (forgetAssumption k
                 (separateVariables k [])))

-- Node deletion

{-

delete node n in k

1. if (s, 0) part of prob return Nothing
2. if not initial node, truncate instance of node else delete instance
3. weaken ordering when filtering it (see shortenOrdering)
4. drop nons that aren't mentioned anywhere
5. drop uniques that aren't carried anywhere
6. update prob upon instance deletion
-}

deleteNodes :: Algebra t p g s e c => Preskel t p g s e c ->
               [Candidate t p g s e c] -> [Candidate t p g s e c]
deleteNodes k cs =
    foldr f cs [ node | strand <- strands k, node <- nodes strand ]
    where                       -- Function f called for each node
      f n cs = maybe cs (: cs) (deleteNode k n) --  in the skeleton

deleteNode :: Algebra t p g s e c => Preskel t p g s e c ->
              Vertex t p g s e c -> Maybe (Preskel t p g s e c, [Sid])
deleteNode k n
    | p == 0 && elem s (prob k) = Nothing
    | p == 0 =
        do
          let mapping = deleteNth s (strandids k)
          k' <- deleteNodeRest k (gen k) (s, p) (deleteNth s (insts k))
                (deleteOrderings s (tc k)) (updatePerm s s (prob k))
          return (k', mapping)
    | otherwise =
        do
          let mapping = strandids k
          let i = inst (strand n)
          (gen', i') <- bldInstance (role i) (take p (trace i)) (gen k)
          k' <- deleteNodeRest k gen' (s, p) (replaceNth i' s (insts k))
                (shortenOrderings (s, p) (tc k)) (prob k)
          return (k', mapping)
    where
      p = pos n
      s = sid (strand n)

-- Update orderings when a strand is eliminated (p == 0)
deleteOrderings :: Sid -> [Pair] -> [Pair]
deleteOrderings s ps =
    M.mapMaybe deleteOrdering ps
    where
      deleteOrdering (n0@(s0, _), n1@(s1, _))
          | s == s0 || s == s1 = Nothing
          | otherwise = Just (adjust n0, adjust n1)
      adjust n@(s', p')
          | s' > s = (s' - 1, p')
          | otherwise = n

-- Update orderings when a strand is shortened (p > 0)
shortenOrderings :: Node -> [Pair] -> [Pair]
shortenOrderings (s, p) ps =
    M.mapMaybe shortenOrdering ps
    where
      shortenOrdering (n0@(s0, p0), n1@(s1, p1))
          | s == s0 && p <= p0 = Nothing
          | s == s1 && p <= p1 = Nothing
          | otherwise = Just (n0, n1)

deleteNodeRest :: Algebra t p g s e c => Preskel t p g s e c ->
                  g -> Node -> [Instance t p g s e c] -> [Pair] ->
                  [Sid] -> Maybe (Preskel t p g s e c)
deleteNodeRest k gen n insts' orderings prob =
    Just k'
    where
      k' = newPreskel gen (protocol k) insts'
           orderings non' unique' (Generalized (Deleted n)) prob (pov k)
      -- Drop nons that aren't mentioned anywhere
      non' = filter mentionedIn (knon k)
      mentionedIn t = varSubset [t] terms
      terms = iterms insts'
      -- Drop uniques that aren't carried anywhere
      unique' = filter carriedIn (kunique k)
      carriedIn t = any (carriedBy t) terms

-- Node ordering weakening

-- To weaken, create a candidate for each element in the current
-- ordering, which is already the result of a transitive reduction.
-- Weaken by computing the transitive closure of the ordering, and
-- then remove the selected element from the current ordering.  After
-- computing the transitive closure, filter out the edges that are not
-- well ordered, i.e. originate at a reception node or terminate at a
-- transmission node.  Also, filter out edges that link nodes in the
-- same strand.  The preskeleton constructor function performs a
-- transitive reduction on the generated ordering.

weakenOrderings :: Algebra t p g s e c => Preskel t p g s e c ->
                   [Candidate t p g s e c] -> [Candidate t p g s e c]
weakenOrderings k cs =
    foldr (weakenOrdering k) cs (orderings k)

weakenOrdering :: Algebra t p g s e c => Preskel t p g s e c ->
                  Pair -> [Candidate t p g s e c] ->
                  [Candidate t p g s e c]
weakenOrdering k p cs =
    weaken k p (L.delete p (tc k)) : cs

weaken :: Algebra t p g s e c => Preskel t p g s e c ->
          Pair -> [Pair] -> Candidate t p g s e c
weaken k p orderings =
    addIdentity k'
    where
      k' = newPreskel (gen k) (protocol k) (insts k)
           orderings (knon k) (kunique k)
           (Generalized (Weakened p)) (prob k) (pov k)

-- Origination assumption forgetting

-- Delete each non-originating term that is not specified by a
-- role.  Do the same for each uniquely-originating term.

forgetAssumption :: Algebra t p g s e c => Preskel t p g s e c ->
                    [Candidate t p g s e c] -> [Candidate t p g s e c]
forgetAssumption k cs =
    forgetNonTerm k (skelNons k) (forgetUniqueTerm k (skelUniques k) cs)

-- Non-originating terms

forgetNonTerm :: Algebra t p g s e c => Preskel t p g s e c -> [t] ->
                    [Candidate t p g s e c] -> [Candidate t p g s e c]
forgetNonTerm _ [] cs = cs
forgetNonTerm k (t : ts) cs =
    addIdentity k' : forgetNonTerm k ts cs
    where
      k' = k { knon = knon', operation = Generalized (Forgot t) }
      knon' = L.delete t (knon k)

skelNons :: Algebra t p g s e c => Preskel t p g s e c -> [t]
skelNons k =
    filter (flip notElem ru) (knon k)
    where
      ru = [u | i <- insts k, u <- inheritRnon i]

-- Uniquely-originating terms

forgetUniqueTerm :: Algebra t p g s e c => Preskel t p g s e c -> [t] ->
                    [Candidate t p g s e c] -> [Candidate t p g s e c]
forgetUniqueTerm _ [] cs = cs
forgetUniqueTerm k (t : ts) cs =
    addIdentity k' : forgetUniqueTerm k ts cs
    where
      k' = k { kunique = kunique', operation = Generalized (Forgot t) }
      kunique' = L.delete t (kunique k)

skelUniques :: Algebra t p g s e c => Preskel t p g s e c -> [t]
skelUniques k =
    filter (flip notElem ru) (kunique k)
    where
      ru = [u | i <- insts k, u <- inheritRunique i]

-- Variable separation

-- A location is a strand, a role variable, and a position in the term
-- associated with the role variable.

-- step one: extract places
--
-- for each maplet in each environment in each strand
--   for each variable V in the range of the maplet
--     let P be the places at which V occurs
--     associate V with the location described by each element in P
--
-- step two: generate preskeletons
--
-- for each variable V in the skeleton K
--  let V' be a clone of V
--  let L be the set of locations associated with V from above
--  for each subset L' of L
--    for each instance I in K
--      update the environment of I by replacing V' at the locations
--      given by L' that refer to I, and use the modified environment
--      to update the instance
--    let K' be the result of updating instances in K as above
--    if V occurs in non, add terms with V replaced by V' to
--      the non's of K'
--    if V occurs in unique, add terms with V replaced by V'
--      to the unique's of K'
--    add K' to the list of generated preskeletons

separateVariables :: Algebra t p g s e c => Preskel t p g s e c ->
                     [Candidate t p g s e c] -> [Candidate t p g s e c]
separateVariables k cs =
    foldr (separateVariable k (extractPlaces k)) cs (kvars k)

-- A location is a strand, a role variable, and a position in the term
-- associated with the role variable.

type Location t p g s e c = (Sid, t, p)

-- Returns a list of pairs.  For each occurrence of a preskeleton
-- variable in every instance, there is a pair where the first element
-- is the variable, and the second as the location at which it occurs.
extractPlaces :: Algebra t p g s e c => Preskel t p g s e c ->
                 [(t, Location t p g s e c)]
extractPlaces k =
    foldl extractPlacesFromStrand [] (strands k)

extractPlacesFromStrand :: Algebra t p g s e c =>
                           [(t, Location t p g s e c)] ->
                           Strand t p g s e c ->
                           [(t, Location t p g s e c)]
extractPlacesFromStrand ps s =
    foldl (extractPlacesFromMaplet (sid s)) ps (instAssocs (inst s))

instAssocs :: Algebra t p g s e c => Instance t p g s e c -> [(t, t)]
instAssocs i =
    reify (rvars (role i)) (env i)

extractPlacesFromMaplet :: Algebra t p g s e c => Sid ->
                           [(t, Location t p g s e c)] -> (t, t) ->
                           [(t, Location t p g s e c)]
extractPlacesFromMaplet s ps (v, t) =
    foldl f ps (foldVars (flip adjoin) [] t)
    where
      f ps var = foldl (g var) ps (places var t)
      g var ps p = (var, (s, v, p)) : ps

-- For each variable, generate candidates by generating a fresh
-- variable for subsets of the locations associated with the variable.
separateVariable :: Algebra t p g s e c => Preskel t p g s e c ->
                    [(t, Location t p g s e c)] -> t ->
                    [Candidate t p g s e c] -> [Candidate t p g s e c]
separateVariable k ps t cs =
    sepVar (locsFor ps t)
    where
      sepVar [] = cs
      sepVar [_] = cs
      sepVar locs = foldr (changeLocations k env gen'' t') cs (parts locs)
      (gen'', env) = matchAlways t t' (gen', emptyEnv)
      parts locs = map (map (locs !!)) (subsets (length locs))
      (gen', t') = clone (gen k) t

-- Extract the locations for a given variable
locsFor :: Algebra t p g s e c => [(t, Location t p g s e c)] ->
           t -> [Location t p g s e c]
locsFor ps t =
    map snd (filter (\(t', _) -> t == t') (reverse ps)) -- Why reverse?

matchAlways :: Algebra t p g s e c => t -> t -> (g, e) -> (g, e)
matchAlways t t' env =
      maybe envError id (match t t' env)
      where
        envError = error "Strand.matchAlways: bad match"

-- Change the given locations and create the resulting preskeleton
changeLocations :: Algebra t p g s e c => Preskel t p g s e c ->
                   e -> g -> t -> [Location t p g s e c] ->
                   [Candidate t p g s e c] -> [Candidate t p g s e c]
changeLocations k env gen t locs cs =
    addIdentity k0 : addIdentity k1 : cs
    where
      k0 = newPreskel gen' (protocol k) insts' (orderings k)
           non unique0 (Generalized (Separated t)) (prob k) (pov k)
      k1 = newPreskel gen' (protocol k) insts' (orderings k)
           non unique1 (Generalized (Separated t)) (prob k) (pov k)
      (gen', insts') = changeStrands locs t gen (strands k)
      non = knon k ++ map (instantiate env) (knon k)
      unique0 = kunique k ++ unique'
      unique1 = map (instantiate env) (kunique k) ++ unique'
      -- Ensure all role unique assumptions are in.
      unique' = concatMap inheritRunique insts'

changeStrands :: Algebra t p g s e c => [Location t p g s e c] -> t ->
                 g -> [Strand t p g s e c] -> (g, [Instance t p g s e c])
changeStrands locs copy gen strands =
    maybe err id (foldMapM (changeStrand locs copy) gen strands)
    where
      err = error "Strand.changeStrands: bad strand build"

-- Create an new environment incorporating changes, and from that,
-- create the new strand.
changeStrand :: Algebra t p g s e c => [Location t p g s e c] ->
                t -> g -> Strand t p g s e c -> Maybe (g, Instance t p g s e c)
changeStrand locs copy gen s =
    bldInstance (role i) trace'  gen'
    where
      i = inst s
      (gen', env') = foldl f (gen, emptyEnv)
             (map (changeMaplet locs copy (sid s)) (instAssocs i))
      f env (v, t) = matchAlways v t env
      trace' = map (evtMap $ instantiate env') trace
      trace = take (height i) (rtrace (role i))

-- Change a maplet
changeMaplet :: Algebra t p g s e c => [Location t p g s e c] ->
                t -> Sid -> (t, t) -> (t, t)
changeMaplet [] _ _ maplet = maplet
changeMaplet ((s', v', p) : locs) copy s (v, t) =
    changeMaplet locs copy s (v, t')
    where
      t' = if s' == s && v' == v then replace copy p t else t

-- Return the set of subsets of natural numbers less than n
subsets :: Int -> [[Int]]
subsets n
    | n < 0 = error $ "Utilities.subsets: Bad argument " ++ show n
    | n == 0 = []
    | otherwise =
        [n - 1] : subset ++ map (n - 1 :) subset
        where
          subset = subsets (n - 1)

-- Collapse a shape by unifying strands.

collapse :: Algebra t p g s e c => Preskel t p g s e c ->
            [Preskel t p g s e c]
collapse k =
    [k' | s <- strandids k, s' <- nats s,
          k' <- M.maybeToList $ collapseStrands k s s']

collapseStrands :: Algebra t p g s e c => Preskel t p g s e c ->
                   Sid -> Sid -> Maybe (Preskel t p g s e c)
collapseStrands k s s' =
    do
      (s, s', subst) <- unifyStrands k s s'
      prs <- ksubst (k { operation = Collapsed s s' },
                     (0, 0), emptySubst) subst
      prs <- compress prs s s'
      (k, _, _) <- hull True prs
      return k
