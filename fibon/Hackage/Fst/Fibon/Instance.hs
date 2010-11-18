{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Fibon.Benchmarks.Hackage.Fst.Fibon.Instance(
  mkInstance
)
where
import Fibon.BenchmarkInstance

sharedConfig = BenchmarkInstance {
    flagConfig = FlagConfig {
        configureFlags = []
      , buildFlags     = []
      , runFlags       = ["-u", "soda.fst"]
      }
    , stdinInput     = Just "PLONK"
    , output         = [(Stdout, Diff "fst.stdout.expected")]
    , exeName        = "fst"
  }
flgCfg = flagConfig sharedConfig

mkInstance Test = sharedConfig {
        flagConfig = flgCfg
    }
mkInstance Ref  = sharedConfig {
        flagConfig = flgCfg
    }

