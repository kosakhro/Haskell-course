{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_testHaskell (
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

bindir     = "/Users/apple/IT/TIEA341/returns/testHaskell/.stack-work/install/x86_64-osx/61afe35ae6a957ad07f7fe0d5c7e208cb0e877b122292024fd759d14edb86457/8.6.5/bin"
libdir     = "/Users/apple/IT/TIEA341/returns/testHaskell/.stack-work/install/x86_64-osx/61afe35ae6a957ad07f7fe0d5c7e208cb0e877b122292024fd759d14edb86457/8.6.5/lib/x86_64-osx-ghc-8.6.5/testHaskell-0.1.0.0-CmCFBnywi4W9k0ZezPTQP5-testHaskell"
dynlibdir  = "/Users/apple/IT/TIEA341/returns/testHaskell/.stack-work/install/x86_64-osx/61afe35ae6a957ad07f7fe0d5c7e208cb0e877b122292024fd759d14edb86457/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/apple/IT/TIEA341/returns/testHaskell/.stack-work/install/x86_64-osx/61afe35ae6a957ad07f7fe0d5c7e208cb0e877b122292024fd759d14edb86457/8.6.5/share/x86_64-osx-ghc-8.6.5/testHaskell-0.1.0.0"
libexecdir = "/Users/apple/IT/TIEA341/returns/testHaskell/.stack-work/install/x86_64-osx/61afe35ae6a957ad07f7fe0d5c7e208cb0e877b122292024fd759d14edb86457/8.6.5/libexec/x86_64-osx-ghc-8.6.5/testHaskell-0.1.0.0"
sysconfdir = "/Users/apple/IT/TIEA341/returns/testHaskell/.stack-work/install/x86_64-osx/61afe35ae6a957ad07f7fe0d5c7e208cb0e877b122292024fd759d14edb86457/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "testHaskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "testHaskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "testHaskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "testHaskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "testHaskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "testHaskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
