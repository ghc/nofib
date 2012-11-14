module Main where

import System.Environment

main :: IO ()
main = do
    [n] <- fmap (map read) getArgs
    --print (root (Node (Node Empty (1 :: Int) Empty) 2 (Node Empty 3 Empty)) (Node (Node Empty 4 Empty) 5 (Node Empty 6 Empty)))
    print (root n)

buildTree n t = case n == 0 of
    True -> t
    False -> buildTree (n-1) (Node t n t)

mapT f xs = case xs of Empty      -> Empty
                       Node l a r -> Node (mapT f l) (f a) (mapT f r)

zipT xs ys = case xs of Empty      -> Empty
                        Node l a r -> case ys of Empty         -> Empty
                                                 Node l' a' r' -> Node (zipT l l') (a, a') (zipT r r')

sizeT :: Tree a -> Int
sizeT Empty        = 0
sizeT (Node l _ r) = 1 + sizeT l + sizeT r

{-# SUPERCOMPILE root #-}
root :: Int -> Int
root n = sizeT (zipMapT (buildTree n Empty) (buildTree n Empty))

zipMapT :: Tree a -> Tree b -> Tree (Either a b, Either a b) -- NB: not fully polymorphic
zipMapT xt yt = zipT (mapT (\x -> Left x) xt) (mapT (\x -> Right x) yt)

data Tree a = Empty | Node (Tree a) a (Tree a)
            deriving (Eq, Show)
