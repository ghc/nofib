module Main
where

import Rsa

main = interact (prompt . keys . lines)

keys (x:y:xs) = makeKeys (read x) (read y)
prompt ks = "\nEnter two random numbers on separate lines:\n" ++
            case ks of
              (n,e,d) -> "The numbers n, e, and d are:\n" ++
                         unlines (map show [n,e,d]) ++ "\n"



 
