module Main where
    
import Data.Binary
import Data.Set.TernarySet
-- These imports are here for testing mainly
-- import Data.Set.StringSet
-- import Data.Map.TernaryMap
-- import qualified Data.Set.StringSet
-- import qualified Data.Map.TernaryMap
-- import Data.Set
import System.IO
import System.Environment

main = do
    (file:_) <- getArgs -- get file name
    contents <- readFile file -- read the file contents
    -- input <- getContents
    let wds = words contents -- separate the words
        tree = fromList $ wds -- put them in the tree
        newname = (file ++ ".bin")
    -- print . treeSize $ tree
    
    putStr "All input words are in dictionary: "
    print . all (`member` tree) $ wds -- make sure all words are actually in the tree
    
    putStr "Same number of words as input: "
    print (size tree == length wds) -- make sure the same number of words are in the tree
    
    putStr ("Writing " ++ newname ++ "... ")
    encodeFile newname tree -- write the tree to a file as "filename.bin"
    
    putStr "done.\nReading data back in... "
    ntree <- decodeFile newname -- read in the file and decode it
    
    putStr "done.\nRead in data matches original: "
    print (tree == ntree) -- check the read in tree is the same as the one we wrote
        -- 
    -- Comment out these lines for benchmarking.
    putStrLn "\n-- Enter a word to see if it is in the dictionary (^C to exit):"
    interact' ((++ "\n> ") . ("-- " ++) . show . (`member` tree)) -- enter a word to see if it's in the tree
    
interact' :: (String -> String) -> IO ()
interact' f = do
    eof <- isEOF
    if eof
      then return ()
      else do
        line <- getLine
        putStrLn (f line)
        interact' f
