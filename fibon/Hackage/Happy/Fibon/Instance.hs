{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Fibon.Benchmarks.Hackage.Happy.Fibon.Instance(
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
    , output         = [(Stderr, Diff "happy.stderr.expected")]
    , exeName        = "happy"
  }
flgCfg = flagConfig sharedConfig

mkInstance Test = sharedConfig {
      flagConfig = flgCfg {runFlags = ["-t", ".", "CmmParse.y"]}
    , output     = (output sharedConfig) ++
                   [(OutputFile "CmmParse.hs",Diff "CmmParse.hs.expected")]
    }
mkInstance Ref  = sharedConfig {
      flagConfig = flgCfg {runFlags = ["-t", ".",
                                       "Bio.y",
                                       "ErlParser.ly",
                                       "HaskellParser.y",
                                       "TestInput.y"]}
    , output     = (output sharedConfig) ++
                   [(OutputFile "TestInput.hs",Diff "TestInput.hs.expected")]
    }

