{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Fibon.Benchmarks.Hackage.Regex.Fibon.Instance(
  mkInstance
)
where
import Fibon.BenchmarkInstance

sharedConfig = BenchmarkInstance {
    flagConfig = FlagConfig {
        configureFlags = []
      , buildFlags     = []
      , runFlags       = []
      }
    , stdinInput     = Nothing
    , output         = [(Stdout, Diff "pderiv.stdout.expected")]
    , exeName        = "pderiv"
  }
flgCfg = flagConfig sharedConfig

mkInstance Test = sharedConfig {
        flagConfig = flgCfg {runFlags = ["1", "addr.txt"]}
    }
mkInstance Ref  = sharedConfig {
        flagConfig = flgCfg {runFlags = ["10", "addr.txt"]}
    }

