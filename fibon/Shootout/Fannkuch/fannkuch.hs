--
-- Contributed to the Haskell wiki by Sebastian Sylvan
--

import System.Environment
import Data.List    (foldl')

rotate n (x:xs) = a ++ x : b
    where (a,b) = splitAt (n-1) xs

perms l = foldr perm' [l] [2..length l]
    where perm' n ls = concat (map (\l -> take n (iterate (rotate n) l)) ls)

flop (1:_) = 0
flop xs = 1 + flop (rev xs)

rev (x:xs) = reverse a ++ x : b
    where (a,b) = splitAt (x-1) xs

fannuch :: [[Int]] -> Int
fannuch xs = foldl' max 0 $ map flop xs

main = do
    [n] <- getArgs
    let xs = perms [1..read n::Int]
    putStr $ unlines $ map (concatMap show) $ take 30 xs
    putStrLn $ "Pfannkuchen(" ++ n ++ ") = " ++ show (fannuch xs)
