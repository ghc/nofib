{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Fibon.Benchmarks.Hackage.Xsact.Fibon.Instance(
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
    , output         = [(Stdout, Diff "xsact.stdout.expected")]
    , exeName        = "xsact"
  }
flgCfg = flagConfig sharedConfig

mkInstance Test = sharedConfig {
        flagConfig = flgCfg {runFlags = ["-i", "alu.n"]}
    }
mkInstance Ref  = sharedConfig {
        flagConfig = flgCfg {runFlags = ["-i", "pdbnt"]}
    }

