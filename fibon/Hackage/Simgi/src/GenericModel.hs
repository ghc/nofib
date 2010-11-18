{-----------------------------------------------------------------
 
  (c) 2009-2010 Markus Dittrich,
      National Resource for Biomedical Supercomputing &
      Carnegie Mellon University

 
  This program is free software; you can redistribute it 
  and/or modify it under the terms of the GNU General Public 
  License Version 3 as published by the Free Software Foundation. 
 
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License Version 3 for more details.
 
  You should have received a copy of the GNU General Public 
  License along with this program; if not, write to the Free 
  Software Foundation, Inc., 59 Temple Place - Suite 330, 
  Boston, MA 02111-1307, USA.

--------------------------------------------------------------------}

-- | data structures needed for defining a stochastic model
module GenericModel ( defaultRateList
                    , Event(..)
                    , EventAction(..)
                    , EventTriggerCombinator(..)
                    , EventTriggerPrimitive(..)
                    , GillespieState
                    , initialModelState
                    , MathExpr(..)
                    , ModelState(..)
                    , MoleculeMap
                    , Output(..)
                    , OutputItem(..)
                    , Rate
                    , RateList
                    , Reaction(..)
                    , SymbolTable(..)
                    , VariableMap
                    , VariableValue
                    ) where


-- imports
import Control.Monad.State
import qualified Data.Map as M
import Data.List((\\))
import Data.Word
import Prelude
import qualified System.Random.Mersenne.Pure64 as MT



-- local imports
import RpnData


--import Debug.Trace 


-- | A MoleculeMap keeps track of the current number of molecules
type MoleculeMap = M.Map String Int


-- | an OutputItem equals one output item and can be
-- an arbitrary function of molecule counts, variables, and
-- simulation time
data OutputItem = Name String | Expression RpnStack


-- | A VariableMap holds all definied variables and their 
-- current value.
-- NOTE: variables may change their each iteration since
-- they may be time dependent.
type VariableMap = M.Map String MathExpr



-- | SymbolTable holds all names we know about such as molecule
-- names, variable names ...
data SymbolTable = SymbolTable { molSymbols :: MoleculeMap
                               , varSymbols :: VariableMap
                               }



-- | generic data type for a mathematical expression. This could
-- either be a constant or an expression inside an RpnStack
data MathExpr = Constant Double | Function RpnStack
              deriving(Show)



-- | make MathExpr an instance of Eq
-- We allow only comparison of constants with each other
-- and RPN stacks with each other
instance Eq MathExpr where
 
  (Constant x)  == (Constant y)  =   x  == y
  (Function f1) == (Function f2) =   f1 == f2
  _             == _             =   False



-- | data type for variable values which are of type MathExpr
-- i.e. they can either be a number or a function involving, e.g.,
-- TIME or molecule counts
type VariableValue = Double



-- | data type for reaction rates which are of type MathExpr
type Rate = MathExpr



-- | List of reactions and corresponding rates
type RateList    = [Double]

defaultRateList :: RateList
defaultRateList = [] 


-- | an actor is a description of a molecular species participating
-- in a reaction (needed for computing h_mu in Gillespie's 
-- notation) and a function mapping a molecule count to the
-- proper h_mu value (needed e.g. for cases where we have
-- 2X terms where h_mu would be 0.5*X*(X-1).
type Actor = (String, Double -> Double)



-- | for each elementary reaction i we need to keep track of
--   
--   rate  : the reaction rate c_i or rate function 
--   actors: a list of Actors
--   react : a list of tuples (i,j) describing that the reaction
--           changes the count of molecule i by j 
data Reaction = Reaction { rate       :: Rate
                         , actors     :: [Actor]
                         , reaction   :: [(String,Int)]
                         }


-- | make Reaction an instance of Eq so we can compare them
-- (used in our unit tests)
instance Eq Reaction where

  x == y  =  compare_reactions x y

    where
    -- | compare two reactions and return True if they
    -- are equal and false otherwise
    compare_reactions :: Reaction -> Reaction -> Bool
    compare_reactions 
      (Reaction { rate     = rate1
                , actors   = actors1
                , reaction = reaction1 })
      (Reaction { rate     = rate2
                , actors   = actors2 
                , reaction = reaction2 }) =

      let
        rateComp  = rate1 == rate2
        actorComp = compare_actors actors1 actors2

        -- since reaction is assembled from Data.Map in the
        -- input parser we can't use == here
        reactComp = ( reaction1 \\ reaction2 ) == []
      in
        rateComp && actorComp && reactComp


    -- | compare two actors
    compare_actors :: [Actor] -> [Actor] -> Bool
    compare_actors [] []  = True
    compare_actors xs ys  = and $ zipWith compare_actor_elem xs ys


    -- | compare an two actor elements
    compare_actor_elem :: Actor -> Actor -> Bool
    compare_actor_elem act1 act2 =
      
      let
        testNum  = 133.0 :: Double
        nameComp = fst act1 == fst act2
        funcComp = (snd act1) testNum == (snd act2) testNum
      in
        nameComp && funcComp




-- | data type describing an action triggered during an event
-- It consists of a String tracking the molecule affected
-- as well as a mathematic expression describing the new molecule
-- count for this molecule
data EventAction = EventAction { evtName   :: String
                               , evtAct    :: MathExpr
                               }



-- | data type describing an expression that triggers a 
-- user event
data EventTriggerPrimitive = EventTriggerPrimitive 
  { trigLeftExpr  :: RpnStack
  , trigRelation  :: Double -> Double -> Bool
  , trigRightExpr :: RpnStack
  }


-- | combinators that can be used to combine EventTriggerPrimitives
data EventTriggerCombinator = AndCombinator | OrCombinator



-- | data type keeping track of possible events occuring during
-- the simulation. Each event consist of a
--
--   <trigger>: list of expressions each evaluating to a bool.
--              event is triggered if all expression evaluate to true
--              FIXME: In the future we should support more complex 
--                      boolen operations involving &&, ||, etc.
-- 
--   <action>: a list of semicolon separated expressions of the form 
--
--              mol/var = <numerical expression>
--
--             changing the value of mol/var by <numerical expression>
--
data Event = 
  Event { evtTrigger :: ([EventTriggerPrimitive], [EventTriggerCombinator])
        , evtActions :: [EventAction]
        }



-- | Our model state
data ModelState = ModelState { molCount      :: MoleculeMap
                             , rates         :: RateList
                             , reactions     :: [Reaction]
                             , randNums      :: [Double]
                             , seed          :: Word64
                             , randGen       :: MT.PureMT
                             , events        :: [Event]
                             , systemVol     :: Double
                             , currentTime   :: Double
                             , currentIter   :: Integer
                             , maxTime       :: Double
                             , outputBufferSize :: Integer
                             , outputFreq    :: Integer
                             , outputRequest :: [OutputItem]
                             , outputCache   :: [Output]
                             , outfileName   :: String
                             , variables     :: VariableMap
                             }

type GillespieState a = State ModelState a



-- | data structure for keeping track of our output
data Output = Output { iteration :: Integer
                     , time      :: Double
                     , outputData :: [Double] 
                     }
  deriving(Show)



-- | initial model state to be partially filled by the 
-- parser from the input deck
initialModelState :: ModelState
initialModelState = ModelState { molCount         = M.empty
                               , rates            = []
                               , reactions        = []
                               , randNums         = []
                               , events           = []
                               , seed             = 1
                               , randGen          = MT.pureMT 1
                               , systemVol        = 1.0
                               , currentTime      = 0.0
                               , currentIter      = 0
                               , maxTime          = 0.0
                               , outputBufferSize = 10000
                               , outputFreq       = 1000
                               , outputRequest    = []
                               , outputCache      = []
                               , outfileName      = ""
                               , variables        = M.empty
                               }


