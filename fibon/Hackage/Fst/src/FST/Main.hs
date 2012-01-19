{-
   **************************************************************
   * Filename      : Main.hs                                    *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 6 July, 2001                               *
   * Lines         : 346                                        *
   **************************************************************
-}

module Main where

import FST.TransducerInterface
import FST.FileImport
import FST.RRegTypes
import System.Environment (getArgs)

import FST.Arguments
import FST.Info

main :: IO()
main = do args <- getArgs
          case args of
           []  -> do fstStudio
                     run emptyInfo
           as -> batchMode as

batchMode :: [String] -> IO ()
batchMode cmdopt = case parseBatch cmdopt of
                    Left  err        -> putStrLn err
                    Right (file,cmd)
                     | isFST file -> do res <- open file
			                case res of
			                 Right str -> case (parseProgram str) of
			                               Left err   -> putStrLn err
					               Right reg
					                | isUpB cmd -> let tr = compile reg [] in
					                                case inputB cmd of
                                                                          Just file -> do res <- open file
                                                                                          case res of
                                                                                           Right str -> case outputB cmd of
                                                                                                         Just f -> do res <- saveToFile f str
                                                                                                                      case res of
                                                                                                                       Left err -> putStrLn err
                                                                                                                       _        -> return ()
                                                                                                         _      -> putStrLn $ upB tr str
                                                                                           Left  err -> putStrLn err
                                                                          Nothing   -> do interact (upB tr)
					                | otherwise -> let tr = compile reg [] in
					                                case inputB cmd of
                                                                         Just file -> do res <- open file
                                                                                         case res of
                                                                                          Right str -> case outputB cmd of
                                                                                                        Just f -> do res <- saveToFile f str
                                                                                                                     case res of
                                                                                                                      Left err -> putStrLn err
                                                                                                                      _        -> return ()
                                                                                                        _      -> putStrLn $ downB tr str
                                                                                          Left  err -> putStrLn err
                                                                         Nothing   -> do interact (downB tr)
			                 Left err -> do putStrLn err
                     | isNET file -> do tr <- load file
                                        case tr of
                                         Right tr
                                          | isUpB cmd -> case inputB cmd of
                                                          Just file -> do res <- open file
                                                                          case res of
                                                                           Right str -> case outputB cmd of
                                                                                         Just f -> do res <- saveToFile f str
                                                                                                      case res of
                                                                                                       Left err -> putStrLn err
                                                                                                       _        -> return ()
                                                                                         _      -> putStrLn $ upB tr str
                                                                           Left  err -> putStrLn err
                                                          Nothing   -> do interact (upB tr)
                                          | otherwise ->  case inputB cmd of
                                                          Just file -> do res <- open file
                                                                          case res of
                                                                           Right str -> case outputB cmd of
                                                                                         Just f -> do res <- saveToFile f str
                                                                                                      case res of
                                                                                                       Left err -> putStrLn err
                                                                                                       _        -> return ()
                                                                                         _      -> putStrLn $ downB tr str
                                                                           Left  err -> putStrLn err
                                                          Nothing   -> do interact (downB tr)
                                         Left err -> putStrLn err
                     | otherwise  -> putStrLn "Input file must end with *.fst or *.net"

upB :: Transducer String -> String -> String
upB transducer str = case (applyUp transducer (words str)) of
                      -- Modifed by dmp 27 Aug 2010
                      -- Print out only the last solution to avoid
                      -- many MB of output
                      Just xs -> (last $ map unwords xs) ++ "\n"
                      --Just xs -> unlines $ map unwords xs
                      Nothing -> []

downB :: Transducer String -> String -> String
downB transducer str = case (applyDown transducer (words str)) of
                        Just xs -> unlines $ map unwords xs
                        Nothing -> []

run :: Info -> IO ()
run info
 = do prompt
      com <- getLine
      case (parseInteractive (words com)) of
       BuildTransducer
        | expressionRead info ->
            do let tNew = compile (getExpression info) []
               putStrLn ("\nBuilt a deterministic, minimal transducer with "
                         ++ (show (numberOfStates tNew)) ++ " states and "
                         ++ (show (numberOfTransitions tNew)) ++ " transitions.\n")
               run $ updateTransducer tNew info
        | otherwise ->
            do noExpression
               run info
       BuildNTransducer
        | expressionRead info ->
           do let tNew = compileN (getExpression info) []
              putStrLn ("\nBuilt a possibly non-deterministic, non-minimal transducer with "
                        ++ (show (numberOfStates tNew)) ++ " states and "
                        ++ (show (numberOfTransitions tNew)) ++ " transitions.\n")
              run $ updateTransducer tNew info
        | otherwise ->
           do noExpression
              run info
       Minimize
        | transducerBuilt info ->
           do let tNew = minimize (getTransducer info)
              putStrLn ("\nMinimized loaded/built transducer resulting in a transducer with "
                        ++ (show (numberOfStates tNew)) ++ " states and "
                        ++ (show (numberOfTransitions tNew)) ++ " transitions.\n")
              run $ updateTransducer tNew info
        | otherwise ->
           do noTransducer
              run info
       Determinize
        | transducerBuilt info ->
           do let tNew = determinize (getTransducer info)
              putStrLn ("\nDeterminized loaded/built transducer resulting in a transducer with "
                        ++ (show (numberOfStates tNew)) ++ " states and "
                        ++ (show (numberOfTransitions tNew)) ++ " transitions.\n")
              run $ updateTransducer tNew info
        | otherwise ->
           do noTransducer
              run info
       ViewTransducer
        | transducerBuilt info ->
           do putStrLn (showTransducer (getTransducer info))
              run info
        | otherwise ->
           do noTransducer
              run info
       Load file
        | isFST file -> do res <- open file
			   case (res) of
			     Right str -> case (parseProgram str) of
			                   Left err -> do putStrLn err
                                                          run info
					   Right reg -> do putStrLn ("\nLoaded a regular relation from " ++file ++".\n")
					                   run $ updateExpression reg info
			     Left err -> do putStrLn err
				            run info
        | isNET file -> do res <- load file
                           case res of
                            Right t -> do putStrLn ("\nLoaded transducer from file " ++ file ++".\n")
                                          run $ updateTransducer t info
                            Left err -> do putStrLn err
                                           run info
        | isDAT file -> do  res <- open file
			    case (res) of
			     Right str -> do putStrLn ("\nRead input from file "++file++".\n")
			                     run $ updateInput (words str) info
			     Left err -> do putStrLn err
				            run info
        | otherwise -> do putStrLn ("\nUnable to load from " ++ file ++ ". The filename must end with *.fst, *.net or *.dat.\n")
                          run info
       LUnion file1 file2
        | isNET file1  && isNET file2  -> do res1 <- load file1
                                             res2 <- load file2
                                             case (res1,res2) of
                                              (Left err,_) -> do putStrLn err
                                                                 run info
                                              (_,Left err) -> do putStrLn err
                                                                 run info
                                              (Right t1, Right t2) -> do putStrLn "\nLoaded and unified two transducers.\n"
                                                                         run $ updateTransducer (unionT t1 t2) info
        | transducerBuilt info && isNET file1  && isTHIS file2
                                          -> do res <- load file1
                                                case res of
                                                 (Left err) -> do putStrLn err
                                                                  run info
                                                 (Right t1) -> do putStrLn "\nLoaded a transducer, and unified it with the interior transducer.\n"
                                                                  run $ updateTransducer (unionT t1 (getTransducer info)) info
        | transducerBuilt info && isTHIS file1 && isNET file2
                                         -> do res <- load file2
                                               case res of
                                                 (Left err) -> do putStrLn err
                                                                  run info
                                                 (Right t1) -> do putStrLn "\nLoaded a transducer, and unified it with the interior transducer.\n"
                                                                  run $ updateTransducer (unionT t1 (getTransducer info)) info
        | otherwise -> do putStrLn $ "\nUnable to union " ++ file1 ++ " and " ++file2++".\n"
                          run info
       LProduct file1 file2
        | isNET file1  && isNET file2  -> do res1 <- load file1
                                             res2 <- load file2
                                             case (res1,res2) of
                                              (Left err,_) -> do putStrLn err
                                                                 run info
                                              (_,Left err) -> do putStrLn err
                                                                 run info
                                              (Right t1, Right t2) -> do putStrLn "\nLoaded and concatenated two transducers.\n"
                                                                         run $ updateTransducer (productT t1 t2) info
        | transducerBuilt info && isNET file1  && isTHIS file2
                                          -> do res <- load file1
                                                case res of
                                                 (Left err) -> do putStrLn err
                                                                  run info
                                                 (Right t1) -> do putStrLn "\nLoaded a transducer, and concatenated it with the interior transducer.\n"
                                                                  run $ updateTransducer (productT t1 (getTransducer info)) info
        | transducerBuilt info && isTHIS file1 && isNET file2
                                         -> do res <- load file2
                                               case res of
                                                 (Left err) -> do putStrLn err
                                                                  run info
                                                 (Right t1) -> do putStrLn "\nLoaded a transducer, and concatenated it with the interior transducer.\n"
                                                                  run $ updateTransducer (productT t1 (getTransducer info)) info
        | otherwise -> do putStrLn $ "\nUnable to concatenate " ++ file1 ++ " and " ++file2++".\n"
                          run info
       LStar file
        | isNET file -> do res <- load file
                           case res of
                            (Left err) -> do putStrLn err
                                             run info
                            (Right t1) -> do putStrLn "\nLoaded a transducer, and applied Kleene's star.\n"
                                             run $ updateTransducer (starT t1) info
        | transducerBuilt info && isTHIS file -> do putStrLn "\nApplied Kleene's star on interior transducer.\n"
                                                    run $ updateTransducer (starT (getTransducer info)) info
        | otherwise -> do putStrLn $ "\nUnable to apply Kleene's star on " ++ file ++ ".\n"
                          run info
       LComposition file1 file2
         | isNET file1  && isNET file2 -> do res1 <- load file1
                                             res2 <- load file2
                                             case (res1,res2) of
                                              (Left err,_) -> do putStrLn err
                                                                 run info
                                              (_,Left err) -> do putStrLn err
                                                                 run info
                                              (Right t1, Right t2) -> do putStrLn "\nLoaded and composed two transducers.\n"
                                                                         run $ updateTransducer (compositionT t1 t2) info
        | transducerBuilt info && isNET file1  && isTHIS file2
                                          -> do res <- load file1
                                                case res of
                                                 (Left err) -> do putStrLn err
                                                                  run info
                                                 (Right t1) -> do putStrLn "\nLoaded a transducer, and composed it with the interior transducer.\n"
                                                                  run $ updateTransducer (compositionT t1 (getTransducer info)) info
        | transducerBuilt info && isTHIS file1 && isNET file2
                                         -> do res <- load file2
                                               case res of
                                                 (Left err) -> do putStrLn err
                                                                  run info
                                                 (Right t1) -> do putStrLn "\nLoaded a transducer, and composed it with the interior transducer.\n"
                                                                  run $ updateTransducer (compositionT t1 (getTransducer info)) info
        | otherwise -> do putStrLn $ "\nUnable to compose " ++ file1 ++ " and " ++file2++".\n"
                          run info
       Save file
        | isNET file -> do res <- save file (getTransducer info)
                           case res of
                            Right t -> do putStrLn ("\nSaved transducer to file "++file++".\n")
                                          run info
                            Left err -> do putStrLn err
                                           run info
        | outputsRead info -> do res <- saveToFile file (unlines (getOutputs info))
                                 case res of
                                  Right _ -> do putStrLn ("\nSaved outputs to file " ++ file ++".\n")
                                                run info
                                  Left err -> do putStrLn err
                                                 run info
        | otherwise        -> do noOutputs
                                 run info
       StdInReg str -> case (parseExp str) of
	  	        Left err -> do putStrLn err
		 	  	       run info
		        Right reg -> do putStrLn "\nRead a regular relation.\n"
				        run $ updateExpression reg info
       ViewReg
        | expressionRead info -> do putStrLn ("\nExpression:\n" ++ (show (getExpression info)) ++ "\n")
                                    run info
        | otherwise           -> do noExpression
                                    run info
       Quit -> do putStr "\nDo you really want to quit? (y): "
                  s <- getLine
                  case s of
                   "y" -> putStrLn "\nSession ended.\n"
                   "Y" -> putStrLn "\nSession ended.\n"
                   _   -> run info
       ClearMemory -> do run $ clearInfo info
       NoCommand   -> do putStrLn "\nInvalid Command. Type 'h' for help.\n"
                         run info
       ViewInput
        | inputRead info -> do putStrLn ("\nInput: \n" ++ (unwords (getInput info)))
                               run info
        | otherwise      -> do noInput
                               run info
       ViewOutput
        | outputsRead info -> do putStrLn ("\nOutputs : \n" ++ (unlines (getOutputs info)))
                                 run info
        | otherwise   -> do noOutputs
                            run info
       Help        -> do help
                         run info
       ApplyUp
        | transducerBuilt info && inputRead info -> case (applyUp (getTransducer info) (getInput info)) of
                                                     Just res -> do putStrLn "\nInput accepted. Type 'vo' to view outputs.\n"
                                                                    run (updateOutputs (map unwords res) info)
                                                     Nothing  -> do putStrLn "\nInput rejected.\n"
                                                                    run info
        | transducerBuilt  info  -> do noTransducer
                                       run info
        | otherwise              -> do noInput
                                       run info
       ApplyDown
        | transducerBuilt info && inputRead info -> case (applyDown (getTransducer info) (getInput info)) of
                                                     Just res -> do putStrLn "\nInput accepted. Type 'vo' to view outputs.\n"
                                                                    run (updateOutputs (map unwords res) info)
                                                     Nothing  -> do putStrLn "\nInput rejected.\n"
                                                                    run info
        | transducerBuilt info -> do noTransducer
                                     run info
        | otherwise          -> do noInput
                                   run info
       ApplyU inp
        | transducerBuilt info -> case (applyUp (getTransducer info) inp) of
                                   Just res -> do putStrLn "\nInput accepted. Type 'vo' to view outputs.\n"
                                                  run (updateOutputs (map unwords res) info)
                                   Nothing  -> do putStrLn "\nInput rejected.\n"
                                                  run info
        | otherwise  -> do noTransducer
                           run info
       ApplyD inp
        | transducerBuilt info -> case (applyDown (getTransducer info) inp) of
                                   Just res -> do putStrLn "\nInput accepted. Type 'vo' to view outputs.\n"
                                                  run (updateOutputs (map unwords res) info)
                                   Nothing  -> do putStrLn "\nInput rejected.\n"
                                                  run info
        | otherwise  -> do noTransducer
                           run info
