{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Client (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/HughLavery/Library/Haskell/bin"
libdir     = "/Users/HughLavery/Library/Haskell/ghc-8.2.1-x86_64/lib/Client-0.1.0.0"
dynlibdir  = "/Users/HughLavery/Library/Haskell/ghc-8.2.1-x86_64/lib/x86_64-osx-ghc-8.2.1"
datadir    = "/Users/HughLavery/Library/Haskell/share/ghc-8.2.1-x86_64/Client-0.1.0.0"
libexecdir = "/Users/HughLavery/Library/Haskell/libexec/x86_64-osx-ghc-8.2.1/Client-0.1.0.0"
sysconfdir = "/Users/HughLavery/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Client_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Client_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Client_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Client_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Client_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Client_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
