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

-- | the main compute Engine
module Engine ( compute_trigger
              , create_initial_output
              , create_initial_state
              , execute_actions
              , gillespie_driver
              , module GenericModel
              ) where


-- imports
import Control.Monad.State
import qualified Data.Map as M
import Prelude
import Text.Printf
import System.Random(randomR)
import qualified System.Random.Mersenne.Pure64 as MT


-- local imports
import ExtraFunctions
import GenericModel
import IO
import RpnCalc


-- | main simulation driver
-- the simulator either stops when
-- 1) the number of iterations is exhausted
-- 2) the current time is > t_max, if t_max is set to
--    zero t_max is treated as being infinity 
gillespie_driver :: FileHandle -> Double -> Integer -> ModelState -> IO ()
gillespie_driver handle simTime dmpIter state =  
  let 
    (output, outState)  = runState run_gillespie $ state 
    (curTime, newState) = update_state dmpIter outState
    reversedOutput      = reverse output
  in
    -- write output to console and the output file
    (write_info $ head reversedOutput)
    >> (write_to_handle handle reversedOutput)

    -- next iteration if we're not at the end
    >> if curTime < simTime
         then gillespie_driver handle simTime dmpIter newState
         else return ()



-- | updates the state for the next iteration
update_state :: Integer -> ModelState -> (Double,ModelState)    
update_state dataDumpIter 
             state@(ModelState { currentTime      = t 
                               , outputBufferSize = it
                               }) 
  = (t, state { outputBufferSize = it + dataDumpIter, outputCache = [] })



-- | actual compute loop
run_gillespie :: GillespieState [Output]
run_gillespie = get

  >>= \inState@(ModelState { molCount         = in_mols
                           , reactions        = in_reacts
                           , randGen          = rGen
                           , events           = molEvents
                           , currentTime      = t
                           , currentIter      = it
                           , maxTime          = t_max
                           , outputBufferSize = it_max
                           , outputFreq       = freq
                           , outputRequest    = outputVars
                           , outputCache      = output
                           , variables        = theVars
                           }) ->


    -- compute and update the next state
    let 
      -- generate two random numbers
      (r1,rGen1) = randomR (0.0 :: Double, 1.0) rGen
      (r2,rGen2) = randomR (0.0 :: Double, 1.0) rGen1

      -- update state
      symbols    = SymbolTable in_mols theVars
      out_rates  = compute_rates symbols in_reacts t []
      a_0        = sum out_rates
      tau        = (-1.0/a_0) * log(r1)
      t_new      = t+tau
      mu         = get_mu (a_0*r2) out_rates
      out_mols   = update_molcount in_mols in_reacts mu
      newSymbols = (symbols { molSymbols = out_mols })
      evt_syms   = handle_events molEvents newSymbols t_new
      new_output = generate_output freq it t_new newSymbols outputVars output
      newState   = inState { molCount    = (molSymbols evt_syms)
                           , rates       = out_rates
                           , randGen     = rGen2
                           , currentTime = t_new
                           , currentIter = it+1
                           , outputCache  = new_output
                           , variables   = (varSymbols evt_syms)
                           }
    in

    -- this prevents simulation from getting stuck
    -- FIXME: We need to come up with mechanism to propagate
    -- error message corresponding to cases such as this one 
    -- to the user!
    if (is_equal tau 0.0)
      then let finalState = newState { currentTime = t_max } in
           put finalState >> return output
      else 
        if ( it_max == it || t >= t_max )
          then return output
          else put newState >> run_gillespie



-- | handle all user defined events and return the adjusted
-- number of molecules
-- WARNING: We should probably check the Event Stack before we use
-- it to compute stuff; at least make sure molecule exist
handle_events :: [Event] -> SymbolTable -> Double -> SymbolTable
handle_events [] symbols     _ = symbols
handle_events (x:xs) symbols t = 
  let
    newSymbols = handle_single_event x symbols t
  in
    handle_events xs newSymbols t



-- | handle a single user event
handle_single_event :: Event -> SymbolTable -> Double -> SymbolTable
handle_single_event evt symbols t =
  let
    triggers     = evtTrigger evt
    actions      = evtActions evt
    triggerVal   = compute_trigger symbols t triggers
  in 
    if triggerVal
      then execute_actions actions symbols t 
      else symbols



-- | compute the value of a trigger
compute_trigger :: SymbolTable -> Double 
                -> ([EventTriggerPrimitive],[EventTriggerCombinator]) -> Bool
compute_trigger _       _ ([],_) = False -- this is should never happen
compute_trigger symbols t ((x:xs),combs) = compute_trigger_h (eval_trigger x) xs combs

  where
    compute_trigger_h acc []     _      = acc
    compute_trigger_h acc _      []     = acc
    compute_trigger_h acc (y:ys) (c:cs) = 

      case c of
        AndCombinator -> compute_trigger_h (acc && (eval_trigger y)) ys cs
        OrCombinator  -> compute_trigger_h (acc || (eval_trigger y)) ys cs

                                          
    eval_trigger e = (trigRelation e) (leftTrigger e) (rightTrigger e)
    leftTrigger    = rpn_compute symbols t . trigLeftExpr 
    rightTrigger   = rpn_compute symbols t . trigRightExpr



-- | handle all actions associated with a user event
execute_actions :: [EventAction] -> SymbolTable -> Double 
                -> SymbolTable
execute_actions [] symbols _     = symbols
execute_actions (x:xs) symbols t =
  let
    newSymbol = execute_single_action x symbols t
  in
    execute_actions xs newSymbol t



-- | handle a single event triggered action
execute_single_action :: EventAction -> SymbolTable -> Double
                      -> SymbolTable
execute_single_action eventAction symbols t =
  let
    aName  = evtName eventAction
    action = evtAct eventAction
  in
    case action of
      Constant c   -> adjust_count aName c 

      Function rpn -> let
                        newCount = rpn_compute symbols t rpn
                      in
                        adjust_count aName newCount 
  
      where
        -- adjust either a molecule count or the value of a
        -- variable
        adjust_count key val = case M.member key (molSymbols symbols) of

          True  -> symbols { molSymbols = 
                             M.insert key (to_int val) (molSymbols symbols) }
          False -> symbols { varSymbols = 
                             M.insert key (Constant val) (varSymbols symbols) }



-- | generate a new Output data structure based on the current
-- molecule counts
generate_output :: Integer -> Integer -> Double -> SymbolTable
                -> [OutputItem] -> [Output] -> [Output]
generate_output afreq it t symTable outItems outlist  
  | mod it afreq /= 0  = outlist
  | otherwise          = new_out:outlist

    where
      currentOutputList = grab_output_data outItems t it symTable

      new_out = Output { iteration  = it
                       , time       = t
                       , outputData = currentOutputList
                       }



-- | given a list of variable or molecule names, goes through the
-- symbol table, grabs the current values associated with the variables,
-- and returns them as a list
grab_output_data :: [OutputItem] -> Double -> Integer -> SymbolTable 
                 -> [Double]
grab_output_data items aTime iter symbols = 
  foldr (\x acc -> (get_val x aTime iter symbols):acc) [] items

  where
    get_val x t it syms = 
      case x of
        Name n        -> get_string_value n t it syms
        Expression e -> rpn_compute syms t e


    get_string_value s t it syms = 
      case s of
        "TIME"      -> t
        "ITERATION" -> fromInteger it
        _           -> get_val_from_symbolTable s t syms
                                


-- | depending on which reaction happened adjust the number of 
-- molecules in the system
update_molcount :: MoleculeMap -> [Reaction] -> Int -> MoleculeMap
update_molcount theMap rs mID =

  let 
    (Reaction { reaction = react_in }) = rs !! mID
  in
    adjustMap react_in theMap 

  where
    adjustMap :: [(String,Int)] -> MoleculeMap -> MoleculeMap
    adjustMap [] m = m
    adjustMap ((k,a):changes) m = let 
                                    val   = (M.!) m k
                                    m_new = M.insert k (a+val) m
                                  in
                                    adjustMap changes m_new



-- | pick the \mu value for the randomly selected next reaction 
-- reaction to happen
get_mu :: Double -> [Double] -> Int
get_mu val = length . takeWhile ( <val ) . scanl1 (+) 



-- | compute the current value for the reaction probabilities based 
-- on the number of molecules and reaction rates
compute_rates :: SymbolTable -> [Reaction] -> Double 
              -> RateList -> RateList
compute_rates _ [] _ rts = reverse rts
compute_rates symbols ((Reaction {rate = c_in, actors = a_in }):rs) 
  theTime rts = 
  
  case c_in of
    (Constant aRate)    -> compute_rates symbols rs theTime
       ((a_new aRate): rts)
    (Function rateFunc) -> compute_rates symbols rs theTime
       ((a_new . (rpn_compute symbols theTime) $ rateFunc):rts)
 
  where
    mult  = product $ map (\(a,f) -> f . fromIntegral $ 
            (M.!) (molSymbols symbols) a) a_in 
    a_new = (*) mult 
    



-- | initialize the output data structure
create_initial_output :: ModelState -> Output
create_initial_output (ModelState { molCount      = initialMols 
                                  , variables     = initialVars
                                  , outputRequest = outVars
                                  }) =
  
  Output { iteration = 1
         , time      = 0.0
         , outputData = initialOutput
         }


  where
    symbols = SymbolTable initialMols initialVars
    initialOutput = grab_output_data outVars 0.0 0 symbols



-- | set up the initial state
create_initial_state:: ModelState -> Output -> ModelState
create_initial_state state@(ModelState { seed = theSeed}) out = 

  state { rates       = defaultRateList
        , randGen     = MT.pureMT theSeed
        , currentTime = 0.0
        , currentIter = 1
        , outputCache = [out]
        }



-- | routine for writing basic accounting info to stdout
write_info :: Output -> IO ()
write_info (Output {iteration = it, time = t}) = 
    putStrLn $ printf "iteration: %-10d  --> time: %6.5g s" it t 



