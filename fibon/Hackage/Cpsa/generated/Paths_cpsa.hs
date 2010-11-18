module Paths_cpsa (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [2,1,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/dave/.cabal/bin"
libdir     = "/Users/dave/.cabal/lib/cpsa-2.1.0/ghc-7.1.20101021"
datadir    = "/Users/dave/.cabal/share/cpsa-2.1.0"
libexecdir = "/Users/dave/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "cpsa_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "cpsa_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "cpsa_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "cpsa_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
