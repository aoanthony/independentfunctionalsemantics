module Paths_barkerproject (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Alex/Dropbox/data1/january_2016/barker/barkerproject/.stack-work/install/x86_64-osx/lts-5.0/7.10.3/bin"
libdir     = "/Users/Alex/Dropbox/data1/january_2016/barker/barkerproject/.stack-work/install/x86_64-osx/lts-5.0/7.10.3/lib/x86_64-osx-ghc-7.10.3/barkerproject-0.1.0.0-3voEAj2yQsR8v79C8raUhR"
datadir    = "/Users/Alex/Dropbox/data1/january_2016/barker/barkerproject/.stack-work/install/x86_64-osx/lts-5.0/7.10.3/share/x86_64-osx-ghc-7.10.3/barkerproject-0.1.0.0"
libexecdir = "/Users/Alex/Dropbox/data1/january_2016/barker/barkerproject/.stack-work/install/x86_64-osx/lts-5.0/7.10.3/libexec"
sysconfdir = "/Users/Alex/Dropbox/data1/january_2016/barker/barkerproject/.stack-work/install/x86_64-osx/lts-5.0/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "barkerproject_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "barkerproject_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "barkerproject_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "barkerproject_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "barkerproject_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
