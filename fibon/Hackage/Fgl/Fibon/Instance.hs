{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Fibon.Benchmarks.Hackage.Fgl.Fibon.Instance(
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
    , output         = [(Stdout, Diff "fgl.stdout.expected")]
    , exeName        = "fgl"
  }
flgCfg = flagConfig sharedConfig

mkInstance Test = sharedConfig {
        flagConfig = flgCfg {
          runFlags = [
            "anna.col",
            "david.col",
            "homer.col",
            "huck.col"
          ]
        }
    }
mkInstance Ref  = sharedConfig {
        flagConfig = flgCfg {
          runFlags = [
            "le450_15a.col",
            "le450_15b.col",
            "le450_15c.col",
            "le450_15d.col",
            "le450_25a.col",
            "le450_25b.col",
            "le450_25c.col",
            "le450_25d.col",
            "le450_5a.col",
            "le450_5b.col",
            "le450_5c.col",
            "le450_5d.col"
          ]
        }
    }

