{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Fibon.Benchmarks.Repa.Laplace.Fibon.Instance(
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
    , output         = [(OutputFile "laplace.bmp", Diff "laplace.expected.bmp")]
    , exeName        = "repa-laplace"
  }
flgCfg = flagConfig sharedConfig

mkInstance Test = sharedConfig {
        flagConfig = flgCfg {runFlags = words "10 pls-100x100.bmp laplace.bmp"}
    }
mkInstance Ref  = sharedConfig {
        flagConfig = flgCfg {runFlags = words "1000 pls-400x400.bmp laplace.bmp"}
    }

