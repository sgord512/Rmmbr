module Paths_rmmbr (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/spencergordon/.cabal/bin"
libdir     = "/Users/spencergordon/.cabal/lib/rmmbr-1.0/ghc-7.0.4"
datadir    = "/Users/spencergordon/.cabal/share/rmmbr-1.0"
libexecdir = "/Users/spencergordon/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "rmmbr_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "rmmbr_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "rmmbr_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "rmmbr_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
