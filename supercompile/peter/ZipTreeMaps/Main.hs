module Main where

main :: IO ()
main = print (root (Node (Node Empty (1 :: Int) Empty) 2 (Node Empty 3 Empty)) (Node (Node Empty 4 Empty) 5 (Node Empty 6 Empty)))

mapT f xs = case xs of Empty      -> Empty
                       Node l a r -> Node (mapT f l) (f a) (mapT f r)

zipT xs ys = case xs of Empty      -> Empty
                        Node l a r -> case ys of Empty         -> Empty
                                                 Node l' a' r' -> Node (zipT l l') (a, a') (zipT r r')

{-# SUPERCOMPILE root #-}
root :: Tree a -> Tree b -> Tree (Either a b, Either a b) -- NB: not fully polymorphic
root xt yt = zipT (mapT (\x -> Left x) xt) (mapT (\x -> Right x) yt)

data Tree a = Empty | Node (Tree a) a (Tree a)
            deriving (Eq, Show)
