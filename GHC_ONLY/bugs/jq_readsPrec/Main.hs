module Main where

data Vertex = V Int deriving (Text)

main::Dialogue
main = readChan stdin exit (\userInput -> (parseVertex.lines) userInput report)

report::Vertex -> Dialogue
report int = appendChan "stdout" (show int) exit done

parseVertex::[String] -> (Vertex -> Dialogue) -> Dialogue
parseVertex inputLines cont
 = (case inputLines of
      (l1:rest) -> case (reads l1) of
                     [(x,"")] -> cont x
                     other        -> appendChan stdout

                                      ((showString "Error - retype the edges\n".                                      shows other) "")
                                     exit done
      _         -> appendChan stdout "No Vertex" exit done)
