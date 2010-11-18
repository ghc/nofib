{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Fibon.Benchmarks.Repa.MMult.Fibon.Instance(
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
    , output         = [(Stdout, Diff "repa-mmult.stdout.expected")]
    , exeName        = "repa-mmult"
  }
flgCfg = flagConfig sharedConfig

mkInstance Test = sharedConfig {
        flagConfig = flgCfg {runFlags = words "-random 10 10 -random 10 10"}
    }
mkInstance Ref  = sharedConfig {
        flagConfig = flgCfg {runFlags = words "-random 1000 1000 -random 1000 1000"}
    }

