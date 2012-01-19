-- Computes the cohort associated with a skeleton or its generalization

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.Cohort (Mode(..), reduce, unrealized) where

import Control.Monad
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.List as L
import Data.Maybe
import CPSA.Lib.Algebra
import CPSA.Lib.Protocol
import CPSA.Lib.Strand

{-- Debugging support
import System.IO.Unsafe
import qualified CPSA.Lib.Utilities as U

z :: Show a => a -> b -> b
z x y = seq (unsafePerformIO (print x)) y

zz :: Show a => a -> a
zz x = z x x

zn :: Show a => a -> Maybe b -> Maybe b
zn x Nothing = z x Nothing
zn _ y = y

zf :: Show a => a -> Bool -> Bool
zf x False = z x False
zf _ y = y

zt :: Algebra t p g s e c => t -> String
zt t =
    show (displayTerm (addToContext emptyContext [t]) t)

zs :: Algebra t p g s e c => Set t -> String
zs s =
    show $ map (displayTerm (addToContext emptyContext ts)) ts
    where
      ts = S.toList s

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

-- Include the escape set in the set of target terms
useEcapeSetInTargetTerms :: Bool
useEcapeSetInTargetTerms = False -- True

-- Filter a cohort for skeletons that solve the test.  Turn off only
-- to debug the other parts of the test solving algorithm.
useSolvedFilter :: Bool
useSolvedFilter = True

-- Filter COWS function output so that it returns only MGUs.  Turn off
-- to checking that an algebra's MGU filter is working.
useMguFilter :: Bool
useMguFilter = True

-- Penetrator derivable predicate and checking for unrealized skeletons.

derivable :: Algebra t p g s e c => Set t -> Set t -> t -> Bool
derivable avoid sent term =
    let (knowns, unknowns) = decompose sent avoid in
    buildable knowns unknowns term

-- Returns the nodes in a preskeleton that are not realized.
unrealized :: Algebra t p g s e c => Preskel t p g s e c -> [Node]
unrealized k =
    foldl unrealizedInStrand [] (strands k)
    where
      (a, _) = avoid k
      unrealizedInStrand acc s =
          fst $ foldl unrealizedInNode (acc, S.empty) (nodes s)
      unrealizedInNode (acc, ns) n =
          case event n of
            Out _ -> (acc, ns)
            In t ->
                let ns' = addSendingBefore ns n
                    ts = S.map (evtTerm . event) ns' in
                case derivable a ts t of
                  True -> (acc, ns')
                  False -> (graphNode n : acc, ns')

addSendingBefore :: Algebra t p g s e c => Set (Vertex t p g s e c) ->
                    Vertex t p g s e c  -> Set (Vertex t p g s e c)
addSendingBefore s n =
    foldl addSending s (preds n)
    where
      addSending s n
        | S.member n s = s
        | otherwise = addSendingBefore (addIfSending s n) n
      addIfSending s n =
          case event n of
            In _ -> s
            Out _ -> S.insert n s

-- Returns that atoms that cannot be guess when determining if a
-- term is derivable from some other terms, and the atoms that
-- uniquely originate in this skeleton.
avoid :: Algebra t p g s e c => Preskel t p g s e c -> (Set t, [t])
avoid k =
    (S.union (S.fromList (knon k)) (S.fromList u), u)
    where
      u = uniqOrig k

-- Suppose k --v,p-> k', where k |-pi,sigma-> k'.  Let t=msg(k, v)@p,
-- t'=sigma(t), T=sigma(esc(k, v, t)), and t"=msg(k', pi(v)).
-- Position p is solved in k' from k at v if:
--
-- 1. some member of anc(t", p) is in T', or
--
-- 2. for some t in outpred(k', pi(v)), t' is not carried only within
--    T in t, or
--
-- 3. the decryption key for an element of T is derivable, or
--
-- 4. t' is an encryption and the encryption key for t' is derivable.
--
-- Haskell variables:
-- ct     = t
-- pos    = p
-- ek     = encription key if ct is an encyption else nothing
-- escape = esc(k, v, t)
-- k      = k
-- (s, p) = v
-- subst  = sigma
solved :: Algebra t p g s e c => t -> p -> Maybe t -> Set t ->
          Preskel t p g s e c -> Node -> s -> Bool
solved ct pos ek escape k (s, p) subst =
    isAncestorInSet escape' t pos ||
    any (not . carriedOnlyWithin ct' escape') (S.toList ts) ||
    any (maybe False (derivable a ts) . decryptionKey) (S.toList escape') ||
    maybe False (derivable a ts) ek
    where
      v = nodes (strands k !! s) !! p -- Look up vertex in k
      t = evt id err (event v)        -- Term at v
      err = const $ error "Cohort.solved: got an outbound term"
      ct' = substitute subst ct       -- Mapped critical term
      escape' = S.map (substitute subst) escape
      vs = addSendingBefore S.empty v
      ts = S.map (evtTerm . event) vs -- Outbound predecessors
      (a, _) = avoid k

maybeSolved :: Algebra t p g s e c => t -> p -> Maybe t -> Set t ->
               Preskel t p g s e c -> Node -> s -> Bool
maybeSolved ct pos ek escape k n subst =
    not useSolvedFilter || solved ct pos ek escape k n subst

data Mode = Mode 
    { noGeneralization :: Bool,
      addDisplacements :: Bool,
      nonceFirstOrder :: Bool,
      visitOldStrandsFirst :: Bool,
      reverseNodeOrder :: Bool }
    deriving Show

-- Abort if there is an unrealized node without a test, otherwise
-- return a list of skeletons that solve one test.  If the skeleton is
-- realized, try to generalize it, but only when noIsoChk is false.
reduce :: Algebra t p g s e c => Mode -> Preskel t p g s e c ->
          [Preskel t p g s e c]
reduce mode k =
    firstJust (map (testStrand mode k u a) ss)
                  (whenRealized k) -- Skeleton is realized
    where
      ss = strandVisitOrder mode (strands  k)
      (a, u) = avoid k
      whenRealized k = 
          if noGeneralization mode then [] else realizedReductions k

strandVisitOrder :: Mode -> [a] -> [a]
strandVisitOrder mode ss =
    if visitOldStrandsFirst mode then
        ss                      -- Visit old strands first
    else
        reverse ss     -- Visit recently added strands first (default)

-- These reductions use isomorphism checking.
realizedReductions :: Algebra t p g s e c => Preskel t p g s e c ->
                      [Preskel t p g s e c]
realizedReductions k =
    let ks = maximize k in
    if null ks then             -- If k is a shape, collapse it
        collapse k
    else                        -- else generalize it
        ks

-- Returns the first Just value in a list or the default when there is
-- none.
firstJust :: [Maybe a] -> a -> a
firstJust [] x = x
firstJust (Just x : _) _ = x
firstJust (Nothing : xs) x = firstJust xs x

-- Look for a test node in a strand
testStrand :: Algebra t p g s e c => Mode -> Preskel t p g s e c ->
              [t] -> Set t -> Strand t p g s e c ->
              Maybe [Preskel t p g s e c]
testStrand mode k u a s =
    loop S.empty (nodeVisitOrder mode $ nodes s)
    where
      loop _ [] = Nothing
      loop ns (n : nodes) =
          case event n of
            Out _ -> loop ns nodes
            In t ->
                let ns' = addSendingBefore ns n
                    ts = S.map (evtTerm . event) ns'
                    (ts', a') = decompose ts a
                    der = buildable ts' a' in -- Derivable before node
                if der t then
                    loop ns' nodes
                else
                    Just $ testNode mode k u ts der (graphNode n) t

nodeVisitOrder :: Mode -> [a] -> [a]
nodeVisitOrder mode nodes =
    if reverseNodeOrder mode then
        reverse nodes           -- Visit latest nodes first
    else
        nodes                  -- Visit earliest nodes first (default)

-- Look for a critical term that makes this node a test node.
testNode :: Algebra t p g s e c => Mode -> Preskel t p g s e c ->
            [t] -> Set t -> (t -> Bool) -> Node -> t ->
            [Preskel t p g s e c]
testNode mode k u ts derivable n t =
    loop cts
    where
      loop [] = error ("Cohort.testNode missing test at " ++ show n)
      loop ((ct, ek) : cts) =
          case escapeSet ts derivable ct of
            Nothing -> loop cts
            Just escape ->
                places (carriedPlaces ct t)
                where
                  places [] = loop cts -- Find position at which
                  places (p : ps)      -- ct has escaped
                      | isAncestorInSet escape t p = places ps
                      | otherwise = solveNode mode k ct p ek n t escape
      cts =                     -- Potential critical messages
          if nonceFirstOrder mode then
              map f (filter (flip carriedBy t) u) ++
              map g (filter h (encryptions t))
          else
              map g (filter h (encryptions t)) ++
              map f (filter (flip carriedBy t) u)
      f ct = (ct, Nothing)
      g (ct, ek) = (ct, Just ek)
      h (_, ek) = not (derivable ek)

-- Compute the escape set
escapeSet :: Algebra t p g s e c => Set t ->
             (t -> Bool) -> t -> Maybe (Set t)
escapeSet ts derivable ct =
    foldM f S.empty (S.toList ts)
    where
      f e t =
          do
            es <- protectors derivable ct t
            return (foldl (flip S.insert) e es)

carriedOnlyWithin :: Algebra t p g s e c => t -> Set t -> t -> Bool
carriedOnlyWithin target escape source =
    all (isAncestorInSet escape source) (carriedPlaces target source)

-- isAncestorInSet set source position is true if there is one ancestor of
-- source at position that is in the set.
isAncestorInSet :: Algebra t p g s e c => Set t -> t -> p -> Bool
isAncestorInSet set source position =
    any (flip S.member set) (ancestors source position)

-- Solve critical message at position pos at node n.
-- ct = t @ pos
-- t  = msg(k, n)
solveNode :: Algebra t p g s e c => Mode -> Preskel t p g s e c ->
             t -> p -> Maybe t -> Node -> t -> Set t ->
             [Preskel t p g s e c]
solveNode mode k ct pos ek n t escape =
    cons ++ augs ++ lsns
    where
      cons = contractions k ct pos ek n t escape cause
      augs = augmentations (addDisplacements mode) k ct pos ek n escape cause
      lsns = addListeners k ct pos ek n t escape cause
      cause = Cause (dir ek) n ct escape

-- Contractions

-- Contract the critical message at the given position.
contractions :: Algebra t p g s e c => Preskel t p g s e c ->
                t -> p -> Maybe t -> Node -> t -> Set t ->
                Cause t p g s e c -> [Preskel t p g s e c]
contractions k ct pos ek n t escape cause =
    [ k | let anc = ancestors t pos,
          subst <- mgus $ solve escape anc (gen k, emptySubst),
          (k, n, subst') <- maybeToList $ contract k n cause subst,
          maybeSolved ct pos ek escape k n subst' ]

solve :: Algebra t p g s e c => Set t -> [t] -> (g, s) -> [(g, s)]
solve escape ancestors subst =
    [ s | e <- S.toList escape,
          a <- ancestors,
          s <- maybeToList $ unify a e subst ]

carriedOnlyWithinAtSubst :: Algebra t p g s e c =>
                            t -> Set t -> t -> (g, s) -> Bool
carriedOnlyWithinAtSubst  ct escape t (_, subst) =
    carriedOnlyWithin ct' escape' t'
    where
      ct' = substitute subst ct
      escape' = S.map (substitute subst) escape
      t' = substitute subst t

fold :: Algebra t p g s e c => t -> Set t -> t -> (g, s) -> [(g, s)]
fold ct escape t (gen, subst) =
    [ (gen', compose subst' subst) |
      (gen', subst') <- foldl f [(gen, emptySubst)] (carriedPlaces ct' t') ]
    where
      ct' = substitute subst ct
      escape' = S.map (substitute subst) escape
      t' = substitute subst t
      f substs p =
          [ s | subst <- substs, s <- solve escape' (ancestors t' p) subst ]

-- Filter out non-most general unifiers
mgus :: Algebra t p g s e c => [(g, s)] -> [(g, s)]
mgus substs =
    if useMguFilter then
        loop substs []
    else
        substs
    where
      loop [] acc = acc
      loop (subst : substs) acc
          | any (f subst) substs || any (f subst) acc =
              loop substs acc
          | otherwise = loop substs (subst : acc)
      f subst subst' = moreGeneral subst' subst

{-
-- Given finite maps f and g, find all maps h such that f = h o g
-- -1 is a map is the wildcard.

maps :: [Int] -> [Int] -> Maybe [Int]
maps f g =
    loop f g [] (-1)

loop :: [Int] -> [Int] -> [(Int, Int)] -> Int -> Maybe [Int]
loop [] [] h n =
    return $ map (maybe (-1) id . flip lookup h) (nats (n + 1))
loop (x:xs) (y:ys) h n =
    case lookup y h of
      Nothing -> loop xs ys ((y,x) : h) (max y n)
      Just x' | x' == x -> loop xs ys h n
              | otherwise -> Nothing
loop _ _ _ _ = Nothing
-}

dir :: Maybe a -> Direction
dir Nothing = Nonce
dir _ = Encryption

-- Augmentations

augmentations :: Algebra t p g s e c => Bool -> Preskel t p g s e c ->
                t -> p -> Maybe t -> Node -> Set t ->
                Cause t p g s e c -> [Preskel t p g s e c]
augmentations displacement k ct pos ek n escape cause =
    [ k' | r <- roles (protocol k),
           k' <- roleAugs displacement k ct pos ek n escape cause r ]

roleAugs :: Algebra t p g s e c => Bool -> Preskel t p g s e c ->
            t -> p -> Maybe t -> Node -> Set t -> Cause t p g s e c ->
            Role t p g s e c -> [Preskel t p g s e c]
roleAugs displacement k ct pos ek n escape cause role =
    [ k' | (subst', inst) <- transformingNode ct escape role subst,
           (k', n', subst'') <-
               augment displacement k n cause role subst' inst,
           maybeSolved ct pos ek escape k' n' subst'' ]
    where
      subst = cloneRoleVars (gen k) role

-- Generate a fresh set of role variables
cloneRoleVars :: Algebra t p g s e c => g -> Role t p g s e c -> (g, s)
cloneRoleVars gen role =
    grow (rvars role) gen emptyEnv
    where
      grow [] gen env = (gen, substitution env)
      grow (t : ts) gen env =
          let (gen', t') = clone gen t in
          case match t t' (gen', env) of
            Nothing -> error "Cohort.grow: Internal error"
            Just (gen'', env') -> grow ts gen'' env'

transformingNode :: Algebra t p g s e c => t -> Set t ->
                    Role t p g s e c -> (g, s) ->
                    [((g, s), Instance t p g s e c)]
transformingNode ct escape role subst =
    loop 1 [] [] (rtrace role)
    where
      targets = S.toList (targetTerms ct escape)
      -- loop height past acc trace
      loop _ _ acc [] = acc
      loop ht past acc (In t : c) =
          loop (ht + 1) (In t : past) acc c
      loop ht past acc (Out t : c) =
          loop (ht + 1) (Out t : past) acc' c
          where
            substs = carriedBindings targets t subst
            substs' =
                if useMguFilter then
                    mgus $ cowt ct escape past substs
                else
                    cowt ct escape past substs
            acc' = maybeAug ct escape role ht substs' acc t

-- Terms considered for binding with the carried terms in an outbound
-- term.
targetTerms :: Algebra t p g s e c => t -> Set t -> Set t
targetTerms ct escape =
    if useEcapeSetInTargetTerms then
       targetTermsWithEscapeSet
    else
       S.difference  targetTermsWithEscapeSet escape
    where
      targetTermsWithEscapeSet = S.fold f (S.singleton ct) escape
      f t ts =
          foldl (flip S.insert) ts
                (concatMap (ancestors t) (carriedPlaces ct t))

-- Find bindings for terms in the test.
carriedBindings :: Algebra t p g s e c => [t] -> t -> (g, s) -> [(g, s)]
carriedBindings targets outbound subst =
    [ s |
      subterm <- S.toList (foldCarriedTerms (flip S.insert) S.empty outbound),
      target <- targets,
      s <- maybeToList $ unify subterm target subst ]

-- Ensure the critical term is carried only within the escape set of
-- every term in the past using fold from cows.
cowt :: Algebra t p g s e c => t -> Set t ->
        Trace t p g s e c -> [(g, s)] -> [(g, s)]
cowt ct escape c substs =
    concatMap (cowt0 ct escape c) substs

-- Handle one substitution at a time.
cowt0 :: Algebra t p g s e c => t -> Set t ->
         Trace t p g s e c -> (g, s) -> [(g, s)]
cowt0 ct escape c subst =
    if all (f subst) c then     -- Substitution works
        [subst]
    else                        -- Substitution needs refinement
        cowt ct escape c (foldn ct escape c [subst])
    where
      f subst evt =
          carriedOnlyWithinAtSubst ct escape (evtTerm evt) subst

-- Apply fold to each message in the trace.
foldn :: Algebra t p g s e c => t -> Set t ->
         Trace t p g s e c -> [(g, s)] -> [(g, s)]
foldn _ _ [] substs = substs
foldn ct escape (evt : c) substs =
    foldn ct escape c (concatMap (fold ct escape (evtTerm evt)) substs)

-- If the outbound term is carried only within, no transforming node
-- was found, otherwise, add a candidate augmentation to the
-- accumulator.
maybeAug :: Algebra t p g s e c => t -> Set t ->
            Role t p g s e c -> Int -> [(g, s)] ->
            [((g, s), Instance t p g s e c)] -> t ->
            [((g, s), Instance t p g s e c)]
maybeAug ct escape role ht substs acc t =
    foldl f acc $ L.filter testNotSolved substs
    where
      testNotSolved (_, subst) =
          not $ carriedOnlyWithin
                  (substitute subst ct)
                  (S.map (substitute subst) escape)
                  (substitute subst t)
      f acc (gen, subst) =
          maybe acc (\(gen, inst) -> ((gen, subst), inst) : acc) inst'
          where
            inst' = bldInstance role itrace gen
            itrace = map (evtMap $ substitute subst) (take ht (rtrace role))

-- Listener augmentations

addListeners :: Algebra t p g s e c => Preskel t p g s e c ->
                t -> p -> Maybe t -> Node -> t -> Set t ->
                Cause t p g s e c -> [Preskel t p g s e c]
addListeners k ct pos ek n t escape cause =
    [ k' | t' <- filter (/= t) (S.toList (escapeKeys ek escape)),
           (k', n', subst) <- maybeToList $ addListener k n cause t',
           maybeSolved ct pos ek escape k' n' subst ]

escapeKeys :: Algebra t p g s e c => Maybe t -> Set t -> Set t
escapeKeys ek escape =
    S.fold f e escape
    where
      f e s = maybe s (flip S.insert s) (decryptionKey e)
      e = maybe S.empty S.singleton ek

-- Maximize a realized skeleton if possible

maximize :: Algebra t p g s e c => Preskel t p g s e c ->
            [Preskel t p g s e c]
maximize k =
    loop (generalize k)         -- Generalize generates candidates
    where
      loop [] = []              -- Realized skeleton k is a shape
      loop ((k', mapping) : rest) =
          case specialization k k' mapping of -- Test a candidate
            Nothing -> loop rest   -- Not a specialization, try again
            Just k' -> [k']        -- Found a more minimal skeleton

-- Test to see if realized skeleton k is a specialization of
-- preskeleton k' using the given strand mapping.  Returns the
-- skeleton associated with k' if it refines k.

specialization :: Algebra t p g s e c => Preskel t p g s e c ->
                  Preskel t p g s e c -> [Sid] ->
                  Maybe (Preskel t p g s e c)
specialization k k' mapping
    | not (preskelWellFormed k') = Nothing
    | otherwise =
        do
          k'' <- toSkeleton False k'
          case realized k'' && gist k /= gist k'' &&
               refines k'' (pov k'') (prob k'') &&
               refines k (Just k') mapping of
            True -> Just k''
            False -> Nothing
        where
          realized = null . unrealized
          refines _ Nothing _ =
              error "Cohort.specialization: cannot find point of view"
          refines k (Just k') mapping =
              maybe False (const True) (homomorphism k' k mapping)
