module Main where
import Prog (prog)
import Data.Char (ord)
import Control.Monad (replicateM_)
import System.Environment (getArgs)

hash :: String -> Int
hash = foldr (\c acc -> ord c + acc*31) 0

main = do
    str <- getContents
    [n] <- getArgs
    replicateM_ (read n) $ do
        salt <- length <$> getArgs
        -- str' == str, but don't tell the compiler
        let str' = take (max (length str) salt) str
        print (hash (prog str'))
