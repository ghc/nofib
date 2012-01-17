{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Fibon.Benchmarks.Hackage.Palindromes.Fibon.Instance(
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
    , output         = [(Stdout, Diff "palindromes.stdout.expected")]
    , exeName        = "palindromes"
  }
flgCfg = flagConfig sharedConfig

mkInstance Test = sharedConfig {
        flagConfig = flgCfg {runFlags = ["-s", "small"]}
    }

mkInstance Ref  = sharedConfig {
        flagConfig = flgCfg {runFlags = ["-s", 
                                         "annakarenina.txt",
                                         "huckfinn.txt",
                                         "olivertwist.txt",
                                         "swannsway.txt"
                                         ]}
    }
