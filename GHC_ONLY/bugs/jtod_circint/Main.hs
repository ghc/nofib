module Main where
import Signal
import Bit

main xs =
  [AppendChan stdout test]

test = stest

type B =  Stream Bit

stest = take 80 (shows z "\n")
  where z = one :: B
