module Main (main) where

import PreludeGlaST

po :: Double -> Double
po rd = 0.5 + 0.5 * erf ((rd / 1.04) / sqrt 2)
  where
    erf :: Double -> Double
    erf x = unsafePerformPrimIO (_ccall_ erf x)

main :: Dialogue
main _ = [AppendChan stdout (shows (po 2.0) "\n")]
