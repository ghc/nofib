{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Fibon.Benchmarks.Hackage.Funsat.Fibon.Instance(
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
    , output         = [(Stdout, Diff "funsat.stdout.expected")]
    , exeName        = "funsat"
  }
flgCfg = flagConfig sharedConfig

mkInstance Test = sharedConfig {
        flagConfig = flgCfg {runFlags = ["ssa0432-003.cnf"]}
    }
mkInstance Ref  = sharedConfig {
        flagConfig = flgCfg {runFlags = ["hole8.cnf", "par16-5.cnf"]}
    }

