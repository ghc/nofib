module Paths_gf (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [3,1,6,2], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/dave/.cabal/bin"
libdir     = "/Users/dave/.cabal/lib/gf-3.1.6.2/ghc-6.12.3"
datadir    = "/Users/dave/.cabal/share/gf-3.1.6.2"
libexecdir = "/Users/dave/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "gf_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "gf_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "gf_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "gf_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
