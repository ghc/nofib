--! From a Rick Morgan bug report:
--! Single-method class with a locally-polymorphic
--! method.

module Main where

class Seq a where
   seq :: a -> b -> b

instance Seq [a] where
   seq [] y = y
   seq x  y = y

main = print ("hurrah" `seq` "Hello, world!\n")
