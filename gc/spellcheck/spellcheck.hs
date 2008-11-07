import System.Environment (getArgs)
import qualified Data.Set as Set

main = do
  [file1,file2] <- getArgs
  dict <- readFile file1
  input <- readFile file2
  let set = Set.fromList (words dict)
  let tocheck = words input
  print (filter (`Set.notMember` set) tocheck)
