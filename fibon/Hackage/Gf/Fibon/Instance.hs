{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Fibon.Benchmarks.Hackage.Gf.Fibon.Instance(
  mkInstance
)
where
import Fibon.BenchmarkInstance

sharedConfig = BenchmarkInstance {
    flagConfig = FlagConfig {
        configureFlags = []
      , buildFlags     = []
      , runFlags       = ["--src", "--make", "-f", "haskell"]
      }
    , output         = []
    , stdinInput     = Nothing
    , exeName        = "gf"
  }
flgCfg = flagConfig sharedConfig

mkInstance Test = sharedConfig {
        output         = [(OutputFile "RDF.hs", Diff "RDF.hs.expected")]
      , flagConfig = flgCfg {runFlags = (runFlags flgCfg) ++ ["RDF.gf"]}
    }
mkInstance Ref  = sharedConfig {
        output         = [(OutputFile "Fre.hs", Diff "Fre.hs.expected")]
      , flagConfig = flgCfg {
          runFlags = (runFlags flgCfg) ++ 
                     ["Eng.gf", "EngReal.gf",  "FreDescr.gf",
                      "EngDescr.gf", "Fre.gf", "FreReal.gf"]
        }
    }

