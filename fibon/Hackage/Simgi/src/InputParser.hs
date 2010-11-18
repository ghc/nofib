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

-- | input file parser 
module InputParser ( input_parser
                   , parse_events 
                   , parse_reaction
                   ) where

-- imports
import Control.Monad
import qualified Data.Map as M
import Prelude
import TokenParser


-- local imports
import ExtraFunctions
import GenericModel
import InputCheck(check_variables_for_cycles, check_variables_for_undefs)
import RpnCalc (try_evaluate_expression)
import RpnParser

-- import Debug.Trace


-- | main parser entry point
input_parser :: CharParser ModelState ModelState
input_parser = whiteSpace 
               *> (many block_parsers)
               *> eof
               >> getState
            <?> "main parser"


-- | parse each of the possible input blocks
block_parsers :: CharParser ModelState ()
block_parsers = parse_variable_def
             <|> parse_parameter_def
             <|> parse_molecule_def
             <|> parse_reaction_def
             <|> parse_event_def
             <|> parse_output_def 



-- | parser for variable definitions
parse_variable_def :: CharParser ModelState ()
parse_variable_def = join ( updateState <$> 
                            insert_variables <$>
                            parse_def_block "variables" (many parse_variable) )
                     *> check_all_vars
                  <?> "variable definition block" 

  where
    insert_variables :: [(String, MathExpr)] -> ModelState -> ModelState
    insert_variables theVars state = state { variables = M.fromList theVars }

    -- | here we check that all variables can be evaluated (i.e. there are
    -- no self references, all elements are defined and such). 
    -- NOTE: We have to check our variables at parse time since we evaluate
    -- them during later parsing stages which will fail if they are ill 
    -- defined.
    check_all_vars :: CharParser ModelState () 
    check_all_vars = getState >>= \(ModelState {variables = vars}) ->

      case check_variables_for_cycles vars of
        Just cycMsg -> fail cycMsg
        Nothing  -> 

          case check_variables_for_undefs vars of
            Just undefMsg -> fail undefMsg
            Nothing       -> pure ()



-- | parser for a single variable definition
parse_variable :: CharParser ModelState (String, MathExpr)
parse_variable = tuple_it <$> ((try parse_variable_name) )
                 <*> (symbol "=" *> parse_variable_definition)
              <?> "variable definition"
  
  where
    tuple_it one two = (one, two)



-- | parser for variable name
parse_variable_name :: CharParser ModelState String
parse_variable_name = identifier 
                   <?> "variable name"



-- | parse the definition for a variable
-- NOTE: we can not use parse_function_expression since this
-- will try to simplyfy in terms of the yet to be defined Variables
parse_variable_definition :: CharParser ModelState MathExpr
parse_variable_definition =  (try parse_constant_expression)
                             <|> Function <$> (braces parse_infix_to_rpn)
                         <?> "variable value"



-- | parser for the output block
parse_output_def :: CharParser ModelState ()
parse_output_def = parse_def_block "output" (many parse_output_specs) 
                   *> pure ()
                <?> "output block"



-- | parser for the individual output block specifications
parse_output_specs :: CharParser ModelState ()
parse_output_specs = parse_output_list 
                  <|> parse_output_file
                  <?> "output specifications"



-- | parse the list with variables or molecules to be punched to the 
-- output file
parse_output_list :: CharParser ModelState ()
parse_output_list = join (updateState <$> insert_output_list
--                      <$> (brackets (commaSep parse_variable_name)))
                      <$> (brackets (commaSep parse_output_item)))
                 <?> "output list"

  where
    insert_output_list :: [OutputItem] -> ModelState -> ModelState
    insert_output_list outDataList state = state { outputRequest = outDataList }



-- | parse a single item in a output specifier list
parse_output_item :: CharParser ModelState OutputItem
parse_output_item = (Name <$> parse_variable_name)
                 <|> (Expression <$> (braces parse_infix_to_rpn))
                 <?> "output list item"



-- | parse the name of the output file 
-- accepts paths but will NOT create any of the parents
parse_output_file :: CharParser ModelState ()
parse_output_file = join (updateState <$> insert_filename
                     <$> (reserved "outputFile" *> reservedOp "="
                          *> parse_filename ))

  where
    insert_filename aName state = state { outfileName = aName }



-- | parse a filename
parse_filename :: CharParser ModelState String
parse_filename = stringLiteral

-- | parser for event definitions
parse_event_def :: CharParser ModelState ()
parse_event_def = join ( updateState <$> insert_events <$>
                         parse_def_block "events" (many parse_events) ) 
               <?> "event definitions" 

  where
    insert_events :: [Event] -> ModelState -> ModelState
    insert_events newEvents state = state { events = newEvents }




-- | parser for individual events
parse_events :: CharParser ModelState Event
parse_events = Event <$> (parse_trigger) <*> (reservedOp "=>" *> parse_actions)
            <?> "reaction event"



-- | parser for an event trigger
parse_trigger :: CharParser ModelState 
                 ([EventTriggerPrimitive], [EventTriggerCombinator])
parse_trigger = (try parse_trigger_expressions)
                <|> (parens parse_trigger_expressions)
             <?> "event trigger block"




-- | parse a list of trigger expressions
parse_trigger_expressions :: CharParser ModelState 
                             ([EventTriggerPrimitive], [EventTriggerCombinator])
parse_trigger_expressions = combine_it 
                         <$> parse_opt_parenthesized_trigger_expression 
                         <*> (many parse_boolean_trigger_expression)
                         <?> "event trigger"
  
  where
    combine_it e = foldr (\(x,y) (u,v) -> (x:u,y:v) ) ([e],[])



-- | parse a single trigger expression prefixed with a && or ||
parse_boolean_trigger_expression :: CharParser ModelState 
                                    (EventTriggerPrimitive, EventTriggerCombinator)
parse_boolean_trigger_expression = 
  tuple_it <$> parse_boolean_combinator 
           <*> parse_opt_parenthesized_trigger_expression
           <?> "boolean trigger expression"

  where
    tuple_it a b = (b,a)



-- | parse a single trigger expression that is either parenthesized or not
parse_opt_parenthesized_trigger_expression :: CharParser ModelState
                                              EventTriggerPrimitive
parse_opt_parenthesized_trigger_expression = 
  (try parse_single_trigger_expression) 
  <|> (parens parse_single_trigger_expression)
  <?> "optionally parenthesized trigger expression"



-- | parse a single trigger expression
parse_single_trigger_expression :: CharParser ModelState EventTriggerPrimitive
parse_single_trigger_expression = 
  EventTriggerPrimitive <$> parse_infix_to_rpn <*> parse_relational
                        <*> parse_infix_to_rpn
                               <?> "single event trigger expression"
-- | parse a boolean combinator (&& or ||)
parse_boolean_combinator :: CharParser ModelState EventTriggerCombinator
parse_boolean_combinator = try parse_AND <|> parse_OR
                        <?> "boolean combinator"

 


-- | parse an && combinator
parse_AND :: CharParser ModelState EventTriggerCombinator
parse_AND = symbol "&&" *> (pure AndCombinator)
          <?> "&&"
  



-- | parse an || combinator
parse_OR :: CharParser ModelState EventTriggerCombinator
parse_OR = symbol "||" *> (pure OrCombinator)
        <?> "||"




-- | parse a relational expression and return its associated
-- binary function
parse_relational :: CharParser ModelState (Double -> Double -> Bool)
parse_relational =  try ( reservedOp ">=" >> pure (>=) )
                <|> try ( reservedOp "<=" >> pure (<=) )
                <|> try ( reservedOp "==" >> pure (==) )
                <|> ( reservedOp ">" >> pure (>) )
                <|> ( reservedOp "<" >> pure (<) )
                <?> "relational expression"



-- | parser for an event action
parse_actions :: CharParser ModelState [EventAction]
parse_actions = brackets parse_action_expressions
            <?> "event action block"



-- | parser for a list of action expressions
parse_action_expressions :: CharParser ModelState [EventAction]
parse_action_expressions = 
  parse_single_action_expression `sepEndBy` comma
                       <?> "event action expression"



-- | parser for a single event action expression
parse_single_action_expression :: CharParser ModelState EventAction
parse_single_action_expression = EventAction <$> 
  (molname) <*> (reservedOp "=" *> ((try parse_constant_expression)
                                <|> (braces parse_function_expression)))
                              <?> "single event action expression"



-- | parser for simulation parameters
parse_parameter_def :: CharParser ModelState ()
parse_parameter_def = parse_def_block "parameters" (many parse_parameters) 
                      *> pure ()
                   <?> "parameter definitions"



-- | parse the individual parameters
parse_parameters :: CharParser ModelState ()
parse_parameters =  parse_time
                <|> parse_outputBuffer
                <|> parse_outputFreq
                <|> parse_systemVol
                <?> "time, outputBuffer, systemVol, outputFreq"
                    



-- | parse the simulation time specs
parse_time :: CharParser ModelState ()
parse_time = join (updateState <$> insert_time 
               <$> (reserved "time" *> reservedOp "=" 
                  *> parse_and_simplify_to_constant_expression ))
  
  where
    insert_time t state = state { maxTime = t }
                           



-- | parse the value of the simulated system volume
parse_systemVol :: CharParser ModelState ()
parse_systemVol = join (updateState <$> insert_volume
                       <$> (reserved "systemVol" *> reservedOp "="
                        *> (     parse_and_simplify_to_constant_expression
                             <|> parse_systemVol_nil ))) 
               <?> "system volume"

  where
    -- needed to avoid monomorphism warning
    parse_systemVol_nil :: CharParser ModelState Double
    parse_systemVol_nil = reserved "nil" *> pure (-1.0)

    insert_volume vol state = state { systemVol = vol }



-- | parse the output iteration specification if present
parse_outputBuffer :: CharParser ModelState ()
parse_outputBuffer = join (updateState <$> insert_outputBuffer
                     <$> (reserved "outputBuffer" *> reservedOp "="
                      *> parse_and_simplify_to_constant_expression ))

  where
    insert_outputBuffer i state = state { outputBufferSize = to_int i }



-- | parse the output iteration specification if present
parse_outputFreq :: CharParser ModelState ()
parse_outputFreq = join (updateState <$> insert_outputFreq
                        <$> (reserved "outputFreq" *> reservedOp "="
                         *> parse_and_simplify_to_constant_expression ))

  where
    insert_outputFreq i state = state { outputFreq = to_int i }



-- | parser for molecule definitions
parse_molecule_def :: CharParser ModelState ()
parse_molecule_def = join ( updateState <$> insert_molecules <$> 
                            parse_def_block "molecules" (many parse_molecules)) 
                  <?> "molecule definitions"

  where
    insert_molecules :: [(String, Int)] -> ModelState -> ModelState
    insert_molecules theMols state = 
      state { molCount = M.fromList theMols }



-- | parse a molecule name and the number of molecules of this type
parse_molecules :: CharParser ModelState (String,Int)
parse_molecules = make_molecule <$> (try molname) 
                  <*> (symbol "=" *> parse_and_simplify_to_constant_expression)
                <?> "molecule expression"
  where
    make_molecule mol aCount = (mol, to_int aCount)


-- | parser for a molecule name 
-- A molecule name can consist of letters and numbers but has to 
-- start with a letter. The following keywords are reserved
molname :: CharParser ModelState String
molname = identifier
       <?> "molecule name" 


-- | parser for reaction definitions
parse_reaction_def :: CharParser ModelState ()
parse_reaction_def = join ( updateState <$> insert_reactions <$>
                            parse_def_block "reactions" (many parse_reaction) ) 
                  <?> "reaction definitions"
  
  where
    insert_reactions :: [Reaction] -> ModelState -> ModelState
    insert_reactions reacts state = state { reactions = reacts }



-- | parser for a single reaction specification of the type
-- aA + bB + cC + .... -> n1P1 + n2P2 + ......   : rate :
-- NOTE: In order to convert the reaction rates (if requested
--       by the user) we also need to extract the system
--       volume)
parse_reaction :: CharParser ModelState Reaction
parse_reaction = setup_reaction 
                 <$> (parse_react_prod <* reservedOp "->") 
                 <*> parse_react_prod 
                 <*> parse_rate_expression
                 <*> (getState 
                      >>= \(ModelState {systemVol = vol}) -> pure vol)  
  where
    -- | set up a Reaction data structure from the parsed reaction
    setup_reaction r p cin vol = 
      let 
        action  = create_react r p
        hFactor = create_hFact r 
        theRate = if (vol < 0.0) -- no rate conversion for 
                    then cin     -- systemVol = nil
                    else convert_rate cin (M.size r) vol
      in 
        Reaction { rate     = theRate
                 , actors   = hFactor
                 , reaction = action
                 }



    -- | create the list holding the molecule number changes for 
    -- this reactioni. If the net change in molecule number is
    -- zero ( a nop) we remove the action.
    create_react r p = let 
                         reacts = M.map (*(-1)) r 
                       in
                         M.assocs . snd . M.partition (==0)
                          $ M.unionWith (+) reacts p
      


    -- | create the list containing the h factors
    -- WARNING/FIXME: Currently, things are ill defined if the number 
    -- of molecules for species A is below the stoichiometric reactant
    -- coefficient; i.e. if #A = 2 then 3A -> ... does not make sense
    create_hFact :: (M.Map String Int) -> [(String, Double -> Double)]
    create_hFact     = create_hFact_h [] . M.assocs  
    
      where
        create_hFact_h acc [] = acc
        create_hFact_h acc ((k,v):xs) = 
          let 
            v_int = fromIntegral v :: Double
          in
            create_hFact_h ((k,\x -> (1.0/v_int) 
              * generate_lambda v_int x):acc) xs

            where
              generate_lambda :: Double -> Double -> Double
              generate_lambda 1 x   = x
              generate_lambda n x   = (x-n+1) * generate_lambda (n-1) x   


-- | parse list of reactants/products of reaction
-- we expect to parse a stream that looks like
-- n_1 R1 + n_2 R2 + n_3 R3 + .....
-- If n_i is missing we assume it is 1.0
parse_react_prod :: CharParser ModelState (M.Map String Int)
parse_react_prod = (reserved "nil" *> pure (M.empty))
                <|> (M.fromList <$> 
                        ((make_tuple <$> option 1 integer <*> try molname) 
                         `sepBy` reservedOp "+") )
                <?> "reactant or product list"
  
  where
    make_tuple x y = (y, fromInteger x)



-- | parse a number, can be used with 'many' and other parser
-- combinators; integers are automatically promoted to double
parse_number :: CharParser ModelState Double
parse_number = converter <$> naturalOrFloat
            <?> "signed integer or double"
  where
    converter val = case val of
                      Left i  -> (fromInteger i)
                      Right x -> x



-- | parse a positive number, can be used with 'many' and other 
-- parser combinators; integers are automatically promoted to double
parse_positive_number :: CharParser ModelState Double
parse_positive_number = naturalOrFloat 
  >>= \num -> case num of
                Left ival  -> if (ival > 0)
                                then return (fromInteger ival)
                                else pzero
                Right dval -> if (dval > 0)
                                then return dval
                                else pzero

  <?> "unsigned integer or double"         



-- | parser for a def block structure
parse_def_block :: String -> CharParser ModelState a 
                -> CharParser ModelState a
parse_def_block blockName parser = 

  between (reserved blockName )
          (reserved "end")
          (parser)
  <?> "parameter definitions"



-- | parse a simple rate expression
-- FIXME: We can not re-use parse_expression below 
-- since currently the order of function/constant parsing 
-- has to be reversed otherwise rates are always parsed
-- as trivial Functions. 
parse_rate_expression :: CharParser ModelState MathExpr
parse_rate_expression = (try (lineToken parse_constant_expression))
                        <|> (lineToken (braces parse_function_expression)) 
                     <?> "constant or function expression"



-- | parser for a simple rate constant expression
parse_constant_expression :: CharParser ModelState MathExpr
parse_constant_expression = Constant <$> parse_number
                         <?> "rate constant" 



-- | parser for function expressions
-- NOTE: We are trying to simplify expressions based on defined
-- variables
parse_function_expression :: CharParser ModelState MathExpr
parse_function_expression = optimize_if_possible <$> parse_infix_to_rpn 
                            <*> (getState 
                                >>= \(ModelState { variables = vars })
                                    -> pure vars )  
                         <?> "rate function" 
  
  where
    optimize_if_possible x vars = case try_evaluate_expression x vars of 
                                    Right val -> Constant val
                                    Left func -> Function func
 

-- | parser for either a constant or an expression statement that can be
-- simplified to constant. After parsing we simplify any expression statement 
-- to a constant (this should be that case for all variables.
parse_and_simplify_to_constant_expression :: CharParser ModelState Double
parse_and_simplify_to_constant_expression =
  join ( try_convert <$> (     parse_constant_expression 
                           <|> (braces parse_function_expression) ))
    <?> "evaluate math expression"

  where
    try_convert x = 
      case x of 
        Constant c -> return c
        _          -> fail "Error: unknown variable, \
                           \expression could not be evaluated."

