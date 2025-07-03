{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_OpenGL (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [3,0,3,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/bruno/.cabal/store/ghc-9.4.8/OpenGL-3.0.3.0-6b9c22db04e5a15ab08e2df72810a54efec936a03940f7ab18c7307676286e8c/bin"
libdir     = "/home/bruno/.cabal/store/ghc-9.4.8/OpenGL-3.0.3.0-6b9c22db04e5a15ab08e2df72810a54efec936a03940f7ab18c7307676286e8c/lib"
dynlibdir  = "/home/bruno/.cabal/store/ghc-9.4.8/OpenGL-3.0.3.0-6b9c22db04e5a15ab08e2df72810a54efec936a03940f7ab18c7307676286e8c/lib"
datadir    = "/home/bruno/.cabal/store/ghc-9.4.8/OpenGL-3.0.3.0-6b9c22db04e5a15ab08e2df72810a54efec936a03940f7ab18c7307676286e8c/share"
libexecdir = "/home/bruno/.cabal/store/ghc-9.4.8/OpenGL-3.0.3.0-6b9c22db04e5a15ab08e2df72810a54efec936a03940f7ab18c7307676286e8c/libexec"
sysconfdir = "/home/bruno/.cabal/store/ghc-9.4.8/OpenGL-3.0.3.0-6b9c22db04e5a15ab08e2df72810a54efec936a03940f7ab18c7307676286e8c/etc"

getBinDir     = catchIO (getEnv "OpenGL_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "OpenGL_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "OpenGL_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "OpenGL_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "OpenGL_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "OpenGL_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
