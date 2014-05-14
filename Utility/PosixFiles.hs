{- POSIX files (and compatablity wrappers).
 -
 - This is like System.PosixCompat.Files, except with a fixed rename.
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.PosixFiles (
	module X,
	rename
) where

import System.PosixCompat.Files as X hiding (rename)

#ifndef mingw32_HOST_OS
import System.Posix.Files (rename)
#else
import qualified System.Win32.File as Win32
#endif

{- System.PosixCompat.Files.rename on Windows calls renameFile,
 - so cannot rename directories. 
 -
 - Instead, use Win32 moveFile, which can. It needs to be told to overwrite
 - any existing file. -}
#ifdef mingw32_HOST_OS
rename :: FilePath -> FilePath -> IO ()
rename src dest = Win32.moveFileEx src dest Win32.mOVEFILE_REPLACE_EXISTING
#endif
