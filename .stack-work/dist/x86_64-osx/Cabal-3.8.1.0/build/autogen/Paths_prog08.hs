{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_prog08 (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/shreyasimac/Desktop/projects/sxml-parser-haskell/.stack-work/install/x86_64-osx/b46967e9eaf7b9e98e9ca308e31fdf98719afb484d435925673381002e90fb8c/9.4.7/bin"
libdir     = "/Users/shreyasimac/Desktop/projects/sxml-parser-haskell/.stack-work/install/x86_64-osx/b46967e9eaf7b9e98e9ca308e31fdf98719afb484d435925673381002e90fb8c/9.4.7/lib/x86_64-osx-ghc-9.4.7/prog08-0.1.0.0-7WIFqzQjbCoJ7jHSsKz2Iz"
dynlibdir  = "/Users/shreyasimac/Desktop/projects/sxml-parser-haskell/.stack-work/install/x86_64-osx/b46967e9eaf7b9e98e9ca308e31fdf98719afb484d435925673381002e90fb8c/9.4.7/lib/x86_64-osx-ghc-9.4.7"
datadir    = "/Users/shreyasimac/Desktop/projects/sxml-parser-haskell/.stack-work/install/x86_64-osx/b46967e9eaf7b9e98e9ca308e31fdf98719afb484d435925673381002e90fb8c/9.4.7/share/x86_64-osx-ghc-9.4.7/prog08-0.1.0.0"
libexecdir = "/Users/shreyasimac/Desktop/projects/sxml-parser-haskell/.stack-work/install/x86_64-osx/b46967e9eaf7b9e98e9ca308e31fdf98719afb484d435925673381002e90fb8c/9.4.7/libexec/x86_64-osx-ghc-9.4.7/prog08-0.1.0.0"
sysconfdir = "/Users/shreyasimac/Desktop/projects/sxml-parser-haskell/.stack-work/install/x86_64-osx/b46967e9eaf7b9e98e9ca308e31fdf98719afb484d435925673381002e90fb8c/9.4.7/etc"

getBinDir     = catchIO (getEnv "prog08_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "prog08_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "prog08_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "prog08_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "prog08_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "prog08_sysconfdir") (\_ -> return sysconfdir)




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
