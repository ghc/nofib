-- Time-stamp: <Wed Mar 21 2001 17:08:35 Stardate: [-30]6363.57 hwloidl>
--
-- partree
-- parallel map over a tree 
-- To be used for GranSim
-----------------------------------------------------------------------------

#if 0 && defined(GLA_EXTS)

-- Currently unused; should go into another dir!
 
module Main(mainPrimIO) where

import PreludeGlaST
import Parallel
import Tree

mainPrimIO = getArgsPrimIO  `thenPrimIO` \ a ->
             munch_input (args_to_IntList a)

munch_input [n] = appendChanPrimIO stdout ("\npartree " ++ show n ++ " = " ++ (show (partree n)) ++ "\n") `seqPrimIO` returnPrimIO ()

#else

module Main(main) where

import Parallel
import Tree

main = getArgs exit ( \ args ->
       munch_input (args_to_IntList args) )

munch_input [n] = appendChan stdout ("\npartree " ++ show n ++ " = " ++ (show (partree n)) ++ "\n") exit done

#endif

args_to_IntList a = if length a < 1
		      then error "Usage: partree <n>\n"
		      else map (\ a1 -> fst ((readDec a1) !! 0)) a

nfib :: Int -> Int
nfib 0 = 1
nfib 1 = 1
nfib n = nfib (n-1) + nfib (n-2) + 1

foo :: Int -> Int
foo n 
 | n < 5 = foo (5+n)
 | n > 20 = foo (n `rem` 20)
 | otherwise = nfib n

bar :: Int -> Int
bar n = (force_tree t) `par` x
        where forest = [ let 
                           l = take n (iterate (+i) i)
                         in
                           list2tree l 
                       | i <- [1..n] ]
              t = foldl1 (^:) forest
              x = tree_fold (\x y -> (x+y) `quot` 2) 0 t 

partree :: Int -> Int
partree n = (force_tree t) `par` (tree_fold max 0 t)
            where t = par_tree_map bar (list2tree [1..n])

