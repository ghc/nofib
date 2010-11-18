{-
   **************************************************************
   * Filename      : StateMonad.hs                              *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 5 July, 2001                               *
   * Lines         : 47                                         *
   **************************************************************
-}

module FST.StateMonad ( STM(..),    -- type for the state monad.
                    setState,   -- set the internal state.
                    fetchState, -- fetch and increment the internal state.
                    run         -- run the state monad.
                    ) where

import FST.AutomatonTypes (State)

{- **********************************************************
   * Type and instance of the State Monad                   *
   **********************************************************
-}

newtype STM a = STM(State -> (a,State))

instance Monad STM where
 return   x       = STM(\s -> (x,s))
 (STM m) >>=  f   = STM(\s -> let (a,s1) = m s in
                          unSTM (f a) s1)

unSTM :: STM a -> State -> (a,State)
unSTM (STM f) = f

{- **********************************************************
   * Functions on the state monad.                          *
   **********************************************************
-}

setState :: State -> STM ()
setState s = STM (\_ -> ((),s))

fetchState :: STM State
fetchState = STM (\s -> (s,(s+1)))

run :: STM a -> State -> a
run stM s = let (a,_) = (unSTM stM) s in a
