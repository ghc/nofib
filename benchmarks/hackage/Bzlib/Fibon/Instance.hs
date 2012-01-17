{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Fibon.Benchmarks.Hackage.Bzlib.Fibon.Instance(
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
    , output         = []
    , exeName        = "hsbzip"
  }
flgCfg = flagConfig sharedConfig

mkInstance Test = sharedConfig {
      flagConfig = flgCfg {
          runFlags = ["bzlib.cabal.bz2"]
      }
      , output    = [(OutputFile "bzlib.cabal.bz2.roundtrip", 
                      Diff       "bzlib.cabal.bz2")]
    }
mkInstance Ref  = sharedConfig {
      flagConfig = flgCfg {
          runFlags = ["mito.aa.bz2"]
      }
      , output   = [(OutputFile "mito.aa.bz2.roundtrip", 
                     Diff       "mito.aa.bz2")]
    }

