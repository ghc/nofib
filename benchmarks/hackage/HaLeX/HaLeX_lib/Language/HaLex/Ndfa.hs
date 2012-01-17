-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HaLex.Ndfa
-- Copyright   :  (c) João Saraiva 2001,2002,2003,2004,2005
-- License     :  LGPL
--
-- Maintainer  :  jas@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- Non-Deterministic Finite Automata in Haskell.
--
-- Code Included in the Lecture Notes on
--           Language Processing (with a functional flavour).
--
-----------------------------------------------------------------------------


module Language.HaLex.Ndfa (
             -- * Data type
               Ndfa (..)
             -- * Acceptance
             , ndfaaccept
             , ndfawalk
             , epsilon_closure
             -- * Transformation
             , ttNdfa2Ndfa
             -- * Transitions
             , ndfaTransitionsFromTo
             , ndfadestinationsFrom
             , transitionTableNdfa
             , ndfareachedStatesFrom
             -- * Printing
             , toHaskell
             , renameNdfa
             , showNdfaDelta
             -- * Properties of 'Ndfa'
             , sizeNdfa
             , ndfadeadstates
             -- * Properties of States
             , ndfaIsStDead
             , ndfanumberIncomingArrows
             , ndfanumberOutgoingArrows
             ) where

import Data.List
import Language.HaLex.Util
import Language.HaLex.Dfa

-----------------------------------------------------------------------------
-- * Data type

-- | Type of Non-Deterministic Finite Automata. Parameterized with
--   the type @st@ of states and @sy@ of symbols.

data Ndfa st sy = Ndfa [sy]                     -- Vocabulary
                       [st]                     -- Finite set of states
                       [st]                     -- The set of start state
                       [st]                     -- The set of final states
                       (st -> Maybe sy -> [st]) -- Transition function

-----------------------------------------------------------------------------
-- * Acceptance

-- | Test whether the given automaton accepts the given list of
--   input symbols.

ndfaaccept :: Ord st
           => Ndfa st sy    -- ^ Automaton
           -> [sy]          -- ^ Input symbols
           -> Bool

ndfaaccept (Ndfa _ _ s z delta) symbs =
       (ndfawalk delta (epsilon_closure delta s) symbs) `intersect` z /= []


-- | Execute the transition function of a 'Ndfa' on an initial state
--   and list of input symbol. Return the final state when all input
--   symbols have been consumed.

ndfawalk :: Ord st
         => (st -> Maybe sy -> [st])   -- ^ Transition function
         -> [st]                       -- ^ Initial states
         -> [sy]                       -- ^ Input symbols
         -> [st]                       -- ^ Reached states

ndfawalk delta sts []     = sts
ndfawalk delta sts (x:xs) =
        ndfawalk delta (epsilon_closure delta (delta' delta sts (Just x))) xs


-- | Apply the transition function to a list of states.
delta' :: Eq st
       => (st -> (Maybe sy) -> [st])   -- ^ Transition function
       -> [st]                         -- ^ Current states
       -> (Maybe sy)                   -- ^ Symbol to consume
       -> [st]                         -- ^ Reached states

delta' delta []       sy = []
delta' delta (st:sts) sy = (delta st sy) `union` (delta' delta sts sy)


-- | Compute the eplison closure of a 'Ndfa'.
epsilon_closure :: Ord st
                => (st -> Maybe sy -> [st]) -- ^ Transition function
                -> [st]                     -- ^ Current states
                -> [st]                     -- ^ Reached states
epsilon_closure delta = limit f
  where f sts = sort (sts `union` (delta' delta sts Nothing))


-----------------------------------------------------------------------------
-- * Printing


-- | Print a 'Ndfa' as a Haskell function.
instance (Eq st , Show st, Show sy) => Show (Ndfa st sy) where
      showsPrec p (Ndfa v q s z delta) =
                showString("ndfa = Ndfa v q s z delta") .
                showString ("\n  where \n\t v = ") .
                showList v .
                showString ("\n\t q = ") .
                showList q .
                showString ("\n\t s = ") .
                shows s .
                showString ("\n\t z = ") .
                showList z .
                showString ("\n\t -- delta :: st -> sy -> st \n") .
                (showNdfaDelta q v delta) .
                showString ("\t delta _ _ = []\n")

-- | Helper function to show the transition function of a 'Ndfa'.
showNdfaDelta q v d = foldr (.) (showChar '\n') f
  where f     = zipWith3 showF m n q'
        (m,n) = unzip l'
        q'    = map (uncurry d) l'
        l'    = filter ((/= []) . (uncurry d)) l
        l     = [(a, c) | a <- q
                        , b <- v
                        , c <- [(Just b)]
                        ]  ++ [ (a,Nothing) | a <- q ]

        showF st sy st' = showString("\t delta ") .
                          shows st .
                          showString(" (") .
                          shows sy .
                          showString(") = ") .
                          shows st' .
                          showChar('\n')

-- | Produce the transition table of a given finite automaton.
toHaskell :: Show fa
          => fa        -- ^ Automaton
          -> [Char]    -- ^ Haskell module or file name
          -> IO ()
toHaskell fa modulename =  writeFile (modulename ++ ".hs")
                                ("module " ++ modulename ++ " where\n\n" ++
                                "import Language.HaLex.Dfa\n\n" ++
                                "import Language.HaLex.Ndfa\n\n" ++
                                (show fa))



-----------------------------------------------------------------------------
-- * Transitions


-- | Compute the labels with the same (giving) origin and destination

ndfaTransitionsFromTo :: Eq st
                      => (st -> Maybe sy -> [st]) -> [sy] -> st -> st -> [Maybe sy]
ndfaTransitionsFromTo delta vs o d = [ v
                                     | v <- vs'
                                     , d `elem` delta o v
                                     ]
  where vs' = map Just vs ++ [Nothing]


-- | Compute the destination states giving the origin state

ndfadestinationsFrom :: Ord st
                     => (st -> Maybe sy -> [st])      -- ^ Transition Function
                     -> [sy]                          -- ^ Vocabulary
                     -> st                            -- ^ Origin
                     -> [st]                          -- ^ Destination States
ndfadestinationsFrom delta vs o = concat (o'' : [ ndfawalk delta o' [v] | v <- vs ])
  where o'  = epsilon_closure delta [o]
        o'' = delete o o'

-- | Compute the states that can be reached from a given state
--   according to a given transition function and vocabulary


ndfareachedStatesFrom :: Ord st
                      => (st -> Maybe sy -> [st])     -- ^ Transition Function
                      -> [sy]                         -- ^ Vocabulary
                      -> st                           -- ^ Origin
                      -> [st]                         -- ^ Reached States
ndfareachedStatesFrom d v origin = nub $ origin : qs
  where qs      = limit stPath' (ndfadestinationsFrom d v origin)
        stPath' = stPath d v


stPath  :: Ord st => (st -> Maybe sy -> [st]) -> [sy] -> [st] -> [st]
stPath d v sts = sort $ nub $ (concat $ map (ndfadestinationsFrom d v) sts) ++ sts


-----------------------------------------------------------------------------
-- * Transformation


-- | Produce the transition table of a given 'Ndfa'.
--
-- Given a 'Ndfa' it returns a list of triples of the form
-- @
--                 (Origin,Symbol,Destination)
-- @
-- defining all the transitions of the 'Ndfa'.
--

transitionTableNdfa :: Ndfa st sy            -- ^ Automaton
                    -> [(st,Maybe sy,st)]    -- ^ Transition table
transitionTableNdfa (Ndfa vs qs s z delta) = [ (q,Just v,r)
                                             | q <- qs , v <- vs
                                             , r <- delta q (Just v)
                                             ] ++
                                             [ (q,Nothing,r)
                                             | q <- qs
                                             , r <- delta q Nothing
                                             ]


-- | Reconstruct a 'Ndfa' from a transition table.
--   Given a 'Ndfa' expressed by a transition table
--   (ie a list of triples of the form
-- @
--                    (Origin,Maybe Symbol,Destination)
-- @
--   it constructs a 'Ndfa'. The other elements of the
--   input tuple are the vocabulary, a set of
--   states, and the sets of  initial and final states
--
ttNdfa2Ndfa :: (Eq st, Eq sy)
            => ([sy],[st],[st],[st],[(st,Maybe sy,st)])   -- ^ Tuple-based 'Ndfa'
            -> Ndfa st sy                                 -- ^ Automaton
ttNdfa2Ndfa (vs,qs,s,z,tt) = Ndfa vs qs s z d
  where d st sy =  lookupTT st sy tt

        lookupTT st sy ((a,b,c) : []) | st == a && sy == b  = [c]
                                      | otherwise           = []
        lookupTT st sy ((a,b,c) : xs) | st == a && sy == b  = c : lookupTT st sy xs
                                      | otherwise           = lookupTT st sy xs






-- | Renames a 'Ndfa'.

renameNdfa :: Eq st
           => Ndfa st sy                -- ^ Automaton
           -> Int                       -- ^ Initial integer number
           -> Ndfa Int sy               -- ^ Renamed Automaton
renameNdfa (Ndfa v q s z d) istid = (Ndfa v q' s' z' d')
  where newSts   = zipWith (\ a b -> (a,b)) q [istid .. ]
        q'       = old2new newSts q
        s'       = old2new newSts s
        z'       = old2new newSts z
        d' st sy = old2new newSts (d (lookupSnd newSts st) sy)


old2new :: Eq st => [(st,Int)] -> [st] -> [Int]
old2new nsts sts = map (lookupFst nsts) sts

lookupFst :: Eq st => [(st,Int)] -> st -> Int
lookupFst nsts ost = snd $ head (filter (\ (a,b) -> a == ost) nsts)


lookupSnd :: [(st,Int)] -> Int -> st
lookupSnd nsts nst = fst $ head (filter (\ (a,b) -> b == nst) nsts)


-----------------------------------------------------------------------------
-- * Properties of 'Ndfa'


-- | Compute the dead states of a 'Ndfa'
--

ndfadeadstates :: Ord st
               => Ndfa st sy        -- ^ Automaton
               -> [st]              -- ^ Dead States
ndfadeadstates (Ndfa v qs s z d) = filter (ndfaIsStDead d v z) qs


-- | The size of an automaton is the number of its states.

sizeNdfa :: Ndfa st sy        -- ^ Automaton
         -> Int               -- ^ Size
sizeNdfa (Ndfa _ q _ _ _) = length q




-----------------------------------------------------------------------------
-- * Properties of States


-- | Checks whether a 'Ndfa' state is dead or not.


ndfaIsStDead :: Ord st
             => (st -> Maybe sy -> [st])        -- ^ Transition Function
             -> [sy]                            -- ^ Vocabulary
             -> [st]                            -- ^ Set of Final States
             -> st                              -- ^ State
             -> Bool
ndfaIsStDead d v z st = ndfareachedStatesFrom d v st `intersect` z == []

-- | Checks whether a 'Ndfa' state is a sync state or not
--


isSyncState :: Ord st => st -> [sy] -> [st] -> (st -> Maybe sy -> [st]) -> Bool
isSyncState st vs z d = (not (st `elem` z)) && (and qs)
  where qs = [ [st] == (d st (Just v))
               && (([st] == d st Nothing) || ([] == d st Nothing))
             | v <- vs
             ]


-- | Compute the number of incoming arrows for a given state
--

ndfanumberIncomingArrows :: Eq st
                         => (st -> Maybe sy -> [st])   -- ^ Transition Function
                         -> [sy]                       -- ^ Vocabulary
                         -> [st]                       -- ^ Set of States
                         -> st                         -- ^ Destination
                         -> Int                        -- ^ Number of Arrows
ndfanumberIncomingArrows d vs qs dest =
        length  [ q
                | v <- vs
                , q <- qs
                , (dest `elem` d q (Just v)) || (dest `elem` d q Nothing)
                ]



-- | Compute the number of outgoing arrows for a given state


ndfanumberOutgoingArrows :: Ord st
                         => (st -> Maybe sy -> [st])   -- ^ Transition Function
                         -> [sy]                       -- ^ Vocabulary
                         -> st                         -- ^ Origin
                         -> Int                        -- ^ Number of Arrows

ndfanumberOutgoingArrows d v o = length $ ndfadestinationsFrom d v o
