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

-- | module responsible for doing some basic input checking
module InputCheck ( check_input 
                  , check_variables_for_cycles
                  , check_variables_for_undefs
                  ) where

-- imports
import Control.Monad.Error()
import qualified Data.List as L
import qualified Data.Map as M
import Prelude

-- local imports
import GenericModel
import RpnData


-- | small function checking for common errors in the input deck
-- since checking is currently fairly simple we'll do it by hand
-- should this ever become more extensive we should probably consider
-- using Control.Monad.Error
check_input :: ModelState -> Either String Bool 
check_input (ModelState { molCount         = theMols
                        , reactions        = theReactions
                        , outputBufferSize = iterCount
                        , outputFreq       = outFreq
                        , variables        = theVars
                        , events           = theEvents
                        , outputRequest    = theOutput
                        }) 
  = check_molecules (M.keys theMols) (react_mols theReactions)
    >> check_positive_outfreq outFreq
    >> check_positive_itercount iterCount
    >> check_variable_names defined_names 
         (extract_variable_names_from_rates theReactions) "reactions"
    >> check_variable_names defined_names
         (extract_variable_names_from_events theEvents) "events"
    >> check_variable_names defined_names
         (extract_variable_names_from_output theOutput) "output"

 where
  -- | extract all reaction participants
  react_mols = L.nub . L.concat . map (map (fst) . reaction) 


  -- | extract all definied names (molecules, variables, ...)
  defined_names = (M.keys theMols) ++ (M.keys theVars)

 
-- | extract all variable and molecule names from the output
-- specs. The way we deal with RpnStacks is a bit clunky
-- (i.e. rewrapping it as a MathExpr) but that way we can
-- re-use extract_variable_names.
-- NOTE: the treatment of the TIME keyword is also a bit inconsistent
-- with the rest of the input format since we presently allow
-- TIME/ITERATION to appear without a expression statements which
-- is not possible when defining variables, rates, ... . Hence,
-- we require a separate String treatment.
extract_variable_names_from_output :: [OutputItem] -> [String]
extract_variable_names_from_output = foldr grab_items [] 
  where
    grab_items x acc = 
      case x of
        Name string      -> if is_keyword string 
                              then acc
                              else (string:acc)

          where
            is_keyword w = case w of
                            "TIME"      -> True
                            "ITERATION" -> True
                            _           -> False

  
        Expression stack -> (extract_variable_names [Function stack]) ++ acc



-- | extract all molecules/variables appearing in reaction rate
-- functions
extract_variable_names_from_rates :: [Reaction] -> [String]
extract_variable_names_from_rates = extract_variable_names . map rate



-- | extract all variable/molecule names appearing in a list of
-- rpn stacks corresponding to some rate of variable definition
-- expression    
extract_variable_names :: [MathExpr] -> [String]
extract_variable_names inputList =
  let
    stacks    = foldr extract_rate_func [] inputList
  in
    L.nub . concat . map (foldr extract_rate_vars []) $ stacks

    where
      extract_rate_func (Function a) acc = (toList a):acc
      extract_rate_func _            acc = acc

      extract_rate_vars (Variable a) acc = a:acc
      extract_rate_vars _            acc = acc



-- | extract all variable/molecule names from expressions inside
-- events, i.e. insider triggers and actions
extract_variable_names_from_events :: [Event] -> [String]
extract_variable_names_from_events theEvents = 

  L.nub $ allActionNames theEvents ++
    (extract_variable_names $ triggerExps theEvents ++ actionExps theEvents)

  where
    allTriggers = concat . foldr ((:) . fst . evtTrigger) []
    triggerExps = foldr (\x acc -> 
                    ((Function $ trigLeftExpr x):(Function $ trigRightExpr x):acc)) 
                    [] . allTriggers
                    
    allActions  = concat . foldr ((:) . evtActions) []
    actionExps  = foldr ((:) . evtAct) [] . allActions

    allActionNames = foldr ((:) . evtName) [] . allActions



-- | make sure all molecules in reactions are defined
check_molecules :: [String] -> [String] -> Either String Bool
check_molecules defMols reactMols = 
  let 
    not_present = reactMols L.\\ defMols
  in
    case null not_present of
      True  -> Right True
      False -> Left $ 
        "Error: The following molecules are not definied: " 
        ++ (L.concat $ L.intersperse "," not_present)


-- | make sure the user entered a positive integer for outputFreq
check_positive_outfreq :: Integer -> Either String Bool
check_positive_outfreq freq = 
  if freq < 0 
    then Left "Error: outputFreq must be a positive integer!"
    else Right True


-- | make sure the user entered a positive integer for outputFreq
check_positive_itercount :: Integer -> Either String Bool
check_positive_itercount iter = 
  if iter < 0 
    then Left "Error: outputIter must be a positive integer!"
    else Right True


-- | make sure the user defined reaction rate function reference
-- only existing molecule names
check_variable_names :: [String] -> [String] -> String -> Either String Bool
check_variable_names defMols rateMols checkType =
  let 
    noMol = rateMols L.\\ defMols
  in
    case null noMol of
      True  -> Right True
      False -> Left $
        "Error: The following molecules or variables defined in the "
        ++ checkType
        ++ " block do not exist:\n -->  " 
        ++ (L.concat $ L.intersperse ", " noMol)



-- | check a variable map for obvious problems such as circular
-- references and others
check_variables_for_cycles :: VariableMap -> Maybe String
check_variables_for_cycles vmap = 
  if null cyclicVariables 
    then Nothing
    else Just $ "Error: circular references for variables \
                \found:" ++ (stringify cyclicVariables)

  where
    stringify = foldr (\x a -> a ++ " " ++ x) ""

    cyclicVariables = foldr check_item [] . M.toList $ vmap

    check_item (_,(Constant _)) acc     = acc
    check_item (s,(Function stack)) acc = cycles ++ acc

      where
        cycles = foldr contains [] $ toList stack
        contains x a = case x of
                         Variable v -> if v == s
                                         then (s:a) 
                                         else a
                         _          -> a


-- | check if all elements in our variable list are defined  
-- we can re-use check_variable_names with only a bit of
-- re-packaging
check_variables_for_undefs :: VariableMap -> Maybe String
check_variables_for_undefs vmap =  
  case check_variable_names (M.keys vmap) 
    (extract_variable_names (M.elems vmap)) "variables" of
    Left s  -> Just s
    Right _ -> Nothing






