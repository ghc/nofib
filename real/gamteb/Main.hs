--
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
import GamtebMain
import Control.Monad
import Data.Char
import System.Environment

hash :: String -> Int
hash = foldr (\c acc -> ord c + acc*31) 0

main = replicateM_ 200 $ do
    (scale:_) <- getArgs
    -- putStr (gamteb (read scale))
    print (hash (gamteb (read scale)))
