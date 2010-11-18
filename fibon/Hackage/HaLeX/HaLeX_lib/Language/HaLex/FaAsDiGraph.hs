-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HaLex.FaAsDiGraph
-- Copyright   :  (c) João Saraiva 2001,2002,2003,2004,2005
-- License     :  LGPL
-- 
-- Maintainer  :  jas@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- Finite Automata as Directed Graphs in GraphViz. 
-- Code Included in the Lecture Notes on 
--             Language Processing (with a functional flavour).   
--
-----------------------------------------------------------------------------

module Language.HaLex.FaAsDiGraph ( 
                      ndfa2graphviz
                    , ndfa2graphviz2file
                    , dfa2graphviz 
                    , dfa2graphviz2file
                    , tographviz
                    , tographvizIO
                    , dfa2DiGraphWithNoSyncSt
                    , dfaDiGraphWithNoSyncStIO
                    , genOneArrow
                    ) where

import Language.HaLex.RegExp
import Language.HaLex.RegExp2Fa
import Language.HaLex.Ndfa
import Language.HaLex.Dfa
import Language.HaLex.FaOperations
import Language.HaLex.Minimize 


-- | Print a 'Ndfa' in GraphViz
ndfa2graphviz ndfa name = tographviz ndfa name "circle" "LR" show 

-- | Print a 'Ndfa' in GraphViz in a file
ndfa2graphviz2file ndfa name = writeFile (name++".dot") (ndfa2graphviz ndfa  name) 


-- | Print a 'Dfa' in GraphViz
dfa2graphviz dfa name = tographviz (dfa2ndfa dfa) name "circle" "LR" show 

-- | Print a 'Dfa' in GraphViz in a file
dfa2graphviz2file dfa name = writeFile (name++".dot") (dfa2graphviz dfa  name) 



-- | Print a 'Ndfa' in GraphViz

tographviz :: (Eq sy, Show sy, Ord st , Show st) 
        => Ndfa st sy            -- ^ Automaton
        -> [Char]                -- ^ Graph's name
        -> [Char]                -- ^ Node's shape
        -> [Char]                -- ^ Orientation
        -> (st -> [Char])        -- ^ Show function to print the state ids
        -> [Char]
tographviz ndfa@(Ndfa v q s z delta) name shape orientation showState = 
  "digraph " ++ name ++ " {\n " 
             ++ "rankdir = " ++ orientation ++ " ;\n " 
             ++ (showElemsListPerLine (showStates q)) ++ "\n " 
             ++ (showElemsListPerLine (showInitialStates s)) ++ "\n " 
             ++ (showElemsListPerLine (showFinalStates' z))
             ++ (showElemsListPerLine (showNdfaArrows'' ndfa)) 
             ++ "node [shape=none, lavel=initialState, style = invis];\n"
             ++ (createInitialArrows (mirroredInitialStates s) s)
             ++ "\n}"
  where 
    showElemsListPerLine :: [String] -> String
    showElemsListPerLine []    = ""
    showElemsListPerLine (h:t) = ((showString h) "\n ") ++ 
                                 (showElemsListPerLine t) 

    showStates qs = [(showState q) ++ 
                     " [shape=" ++ shape ++" , label=" ++ (showState q) ++ " ,color=black];" 
                    | q <- qs , not (ndfaIsStDead delta v z q ) ]
 
    showInitialStates ss = map showInitialState ss

    showInitialState  s = (showState s) 
                          ++ " [shape=" ++ shape ++ " , label= " ++ (showState s) 
                          ++ ", color=green];\n " 

--    showFinalStates' :: Show a => [a] -> [String]
    showFinalStates' zs = [ (showState z) 
                          ++ " [shape=double" ++ shape ++" , color=red];" | z <- zs ]

--     showNdfaArrows' :: (Eq sy,  Show sy) => Ndfa st sy -> [String]
    showNdfaArrows' ndfa
       = map (\ (o,l,d) -> genOneArrow (showState o) (show l) (showState d)) 
             ((groupMoves . transitionTableNdfa) ndfa)

--     showNdfaArrows'' :: (Ord st, Eq sy, Show st, Show sy) => Ndfa st sy -> [String]
    showNdfaArrows'' ndfa@(Ndfa v q s z delta)
         = map (\ (o,l,d) -> if (ndfaIsStDead delta v z o) || (ndfaIsStDead delta v z d) then ""
                             else genOneArrow (showState o) (showListMaybe l) (showState d)) 
                                  ((groupMoves . transitionTableNdfa) ndfa)


-- Creating the incoming arrows for the initial states
-- (for each state we create an invisible node and a arrow connecting to the initial one)

    mirroredInitialStates = map (\state -> "_newState" ++ (show state)) 
    createInitialArrows [] []         = " "
    createInitialArrows (x:xs) (y:ys) = x ++ " -> " ++ (showState y) ++ 
                                        "[color = green];\n" ++ 
                                        createInitialArrows xs ys


showListMaybe []     = ""
showListMaybe (x:xs) = case x of
                       Just a -> (show a) ++ if (showListMaybe xs == "") then ""
                                   else ("," ++ showListMaybe xs)
                       Nothing -> "Epsilon" ++ if (showListMaybe xs == "") then ""
                                   else ("," ++ showListMaybe xs)

--
-- Given the Transition Table of a Ndfa it groups the transtions with
-- the same origin and destination into a single transtion, whose transtion
-- is the list of labels of the original transtions.
--

groupMoves []           = []
groupMoves ((o,l,d):rs) = res
  where (l',rs') = groupMoves' (o,l,d) ((o,l,d):rs)
        res      = (o,l',d) : groupMoves rs'


groupMoves' :: (Eq st, Eq sy) => (st,Maybe sy,st) -> [(st,Maybe sy,st)] 
            -> ([Maybe sy],[(st,Maybe sy,st)])
groupMoves' _ []  = ([],[])
groupMoves' (o,l,d) ((o',l',d'):rs) 
             | o==o' && d==d' = (new_label,rs')
             | otherwise      = (l'', (o',l',d') : rs')
       where (l'',rs') = groupMoves' (o,l,d) rs 
             new_label = if l'' == [] then [l'] 
                                      else l' : l''


showNdfaArrows :: (Ord st,Show st,Show sy) => Ndfa st sy -> [String]
showNdfaArrows (Ndfa vs qs s z delta) = [ genOneArrow (show q) (show v) (show r) 
                                        | q <- qs , v <- vs 
                                        , r <- delta q (Just v)
                                        , not (ndfaIsStDead delta vs z r )
                                        , not (ndfaIsStDead delta vs z q )
                                        ] ++ 
                                        [ genOneArrow (show q) "Epsilon" (show r)
                                        | q <- qs
                                        , r <- delta q Nothing
                                        , not (ndfaIsStDead delta vs z r )
                                        , not (ndfaIsStDead delta vs z q )
                                        ]

genOneArrow orin label dest = orin  
                              ++ " -> " ++ dest 
                              ++ " [label = " ++ (show label) ++ "];"



tographvizIO ndfa name shape orientation showState = 
   writeFile (name++".dot") (tographviz ndfa name shape orientation showState)


dfa2DiGraphWithNoSyncSt dfa name = dfa2graphviz dfa name

dfa2DiGraphIO dfa name fn = writeFile (fn++".gph") (dfa2graphviz dfa  name ) 

dfaDiGraphWithNoSyncStIO dfa name fn = writeFile fn (dfa2graphviz dfa  name) 

-- dfa2DiGraphIO'' :: (Show sy, Ord sy , Eq st) => Dfa st sy -> [Char] -> IO ()
dfa2DiGraphIO'' dfa name = dfa2DiGraphIO (beautifyDfa dfa) name name


