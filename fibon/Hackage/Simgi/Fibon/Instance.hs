{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Fibon.Benchmarks.Hackage.Simgi.Fibon.Instance(
  mkInstance
)
where
import Fibon.BenchmarkInstance

sharedConfig = BenchmarkInstance {
    flagConfig = FlagConfig {
        configureFlags = []
      , buildFlags     = []
      , runFlags       = ["oregonator.sgl"]
      }
    , stdinInput     = Nothing
    , output         = [(Stdout, 
                         Diff       "simgi.stdout.expected"),
                        (OutputFile "oregonator_output.dat", 
                         Diff       "oregonator_output.dat.expected") ]
    , exeName        = "simgi"
  }
flgCfg = flagConfig sharedConfig

mkInstance Test = sharedConfig {
        flagConfig = flgCfg
    }
mkInstance Ref  = sharedConfig {
        flagConfig = flgCfg
    }

