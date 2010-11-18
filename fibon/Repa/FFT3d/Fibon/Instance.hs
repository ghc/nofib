{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Fibon.Benchmarks.Repa.FFT3d.Fibon.Instance(
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
    , output         = [(OutputFile "output000.bmp", Diff "output.expected.bmp")]
    , exeName        = "repa-fft3d"
  }
flgCfg = flagConfig sharedConfig

mkInstance Test = sharedConfig {
        flagConfig = flgCfg {runFlags = words "2 output"}
    }
mkInstance Ref  = sharedConfig {
        flagConfig = flgCfg {runFlags = words "128 output"}
    }

