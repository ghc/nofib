--!!! tests stack stubbing: if "f" doesn't stub "ns",
--!!! the program has a space leak.

module Main where

main _ = [f (AppendChan stdout "a")
	    (take 1000000 (repeat True))
	    (AppendChan stdout "b")]

f a ns b = if last ns then a else b
