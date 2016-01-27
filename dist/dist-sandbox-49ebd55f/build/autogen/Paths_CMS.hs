module Paths_CMS (
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

bindir     = "C:\\Users\\Natalija\\Desktop\\Haskell-Project\\.cabal-sandbox\\bin"
libdir     = "C:\\Users\\Natalija\\Desktop\\Haskell-Project\\.cabal-sandbox\\x86_64-windows-ghc-7.10.2\\CMS-0.1.0.0-1muUfl1n7XQIRK7cZQFCKp"
datadir    = "C:\\Users\\Natalija\\Desktop\\Haskell-Project\\.cabal-sandbox\\x86_64-windows-ghc-7.10.2\\CMS-0.1.0.0"
libexecdir = "C:\\Users\\Natalija\\Desktop\\Haskell-Project\\.cabal-sandbox\\CMS-0.1.0.0-1muUfl1n7XQIRK7cZQFCKp"
sysconfdir = "C:\\Users\\Natalija\\Desktop\\Haskell-Project\\.cabal-sandbox\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CMS_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CMS_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "CMS_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CMS_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CMS_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
