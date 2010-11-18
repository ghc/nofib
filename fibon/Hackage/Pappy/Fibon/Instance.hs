{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Fibon.Benchmarks.Hackage.Pappy.Fibon.Instance(
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
    , output         = [(Stdout, Diff "java-parser.stdout.expected")]
    , exeName        = "java-parser"
  }
flgCfg = flagConfig sharedConfig

mkInstance Test = sharedConfig {
        flagConfig = flgCfg {runFlags = ["HexDump.java"]}
    }
mkInstance Ref  = sharedConfig {
        flagConfig = flgCfg {runFlags = [
           "Scar.java", "SPEED.java", "Blowfish.java", "CAST5.java",
           "DES.java", "TestRijndael.java"
           ]
        }
    }

