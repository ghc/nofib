module Paths_happy (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,18,5], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/dave/.cabal/bin"
libdir     = "/Users/dave/.cabal/lib/happy-1.18.5/ghc-7.1.20101021"
datadir    = "/Users/dave/.cabal/share/happy-1.18.5"
libexecdir = "/Users/dave/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "happy_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "happy_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "happy_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "happy_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
