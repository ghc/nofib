> {-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies,
>     FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-} 
> -- | This module defines data types, type classes and instances for NFA
> module Text.Regex.PDeriv.Nfa where 

> import Data.List 

> -- | The type class of Nfa
> class Nfa s a | s -> a where
>    pDeriv :: s -> a -> [s]
>    sigma :: s -> [a]
>    empty :: s -> Bool

> -- | A function that builds an NFA 
> buildNFA :: (Nfa s a, Eq s, Eq a) => s -> NFA s a
> buildNFA init = {- all `seq` delta `seq` final `seq` -}
>                 NFA { all_states = all
>                     , delta_states = delta
>                     , init_states = [init]
>                     , final_states = final }
>   where
>      sig = sigma init
>      (all, delta) = sig `seq` builder [] [] [init]
>      final = all `seq` [s | s <- all, empty s]
>      builder acc_states acc_delta [] = (acc_states, acc_delta)
>      builder acc_states acc_delta curr_states =
>        let all_sofar_states = acc_states ++ curr_states
>            new_delta = [ (s, l, s') | s <- curr_states, l <- sig, s' <- pDeriv s l]
>            -- new_states = nub [new_s | s <- curr_states, l <- sig , 
>                         --- new_s <- pDeriv s l, not (elem new_s all_sofar_states)]
>            new_states = all_sofar_states `seq` nub [ s' | (_,_,s') <- new_delta, not (s' `elem` all_sofar_states) ]
>            acc_delta_next  = (acc_delta ++ new_delta)
>        in {- all_sofar_states `seq` new_delta `seq` new_states `seq` -} builder all_sofar_states acc_delta_next new_states

> -- | the NFA data type
> data NFA s a = NFA { all_states :: [s]
>                    , delta_states :: [(s, a, s)]
>                    , init_states :: [s]
>                    , final_states :: [s] }
>                 deriving Show


> -- | the optimized NFA using Int to represent states, IntMap to represent delta
> data SNFA s a = SNFA { mapping_states :: s -> Int
>                        , sall_states :: [Int]
>                        , sdelta_states :: [(Int,a,Int)]
>                        , sinit_states :: [Int]
>                        , sfinal_states :: [Int] }
>                 


> instance Show a => Show (SNFA s a) where
>   show s = "SNFA:\n" ++ show (sall_states s) ++ "\n"
>                      ++ show (sdelta_states s) ++ "\n"
>                      ++ show (sinit_states s) ++ "\n"
>                      ++ show (sfinal_states s)

> -- | The function 'toSNFA' converts from an NFA to an SNFA
> toSNFA :: (Eq s, Eq a)  => NFA s a -> SNFA s a
> toSNFA (NFA { all_states = all
>             , delta_states = delta
>             , init_states = init
>             , final_states = final }) = 
>     let -- generate mapping from states to Int
>         mapping = all `seq` \x -> let (Just i) = findIndex (x ==) all in i 
>         sall_sts = [0..(length all)-1]
>         sdelta_sts = mapping `seq` delta `seq` ( map (\ (p,x,q) -> (mapping p,x,mapping q)) delta)
>         sfinal_sts = mapping `seq` final `seq` ( map mapping final )
>     in  {- sall_sts `seq` sdelta_sts `seq` sfinal_sts `seq` -}
>             SNFA { mapping_states = mapping
>                  , sall_states = sall_sts
>                  , sdelta_states = sdelta_sts
>                  , sinit_states = [0]
>                  , sfinal_states = sfinal_sts }


> nofAllStates (NFA {all_states = all}) = length all
> nofDelta (NFA {delta_states = delta}) = length delta
> nofInitStates (NFA {init_states = init}) = length init
> nofFinalStates (NFA {final_states = final}) = length final 