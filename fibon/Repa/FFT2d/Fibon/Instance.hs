{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Fibon.Benchmarks.Repa.FFT2d.Fibon.Instance(
  mkInstance
)
where
import Fibon.BenchmarkInstance

sharedConfig = BenchmarkInstance {
    flagConfig = FlagConfig {
        configureFlags = ["--ghc-option=-threaded"]
      , buildFlags     = []
      , runFlags       = []
      }
    , stdinInput     = Nothing
    , output         = [(OutputFile "out.bmp", Diff "out.expected.bmp")]
    , exeName        = "repa-fft2d"
  }
flgCfg = flagConfig sharedConfig

mkInstance Test = sharedConfig {
        flagConfig = flgCfg {runFlags = words "1 step20.bmp out.bmp"}
    }
mkInstance Ref  = sharedConfig {
        flagConfig = flgCfg {runFlags = words "1 lena.bmp out.bmp"}
    }

