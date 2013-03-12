{- disk free space checking 
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module Utility.DiskFree ( getDiskFree ) where

import Common

import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Error

#ifdef WITH_CLIBS

foreign import ccall safe "libdiskfree.h diskfree" c_diskfree
	:: CString -> IO CULLong

getDiskFree :: FilePath -> IO (Maybe Integer)
getDiskFree path = withFilePath path $ \c_path -> do
	free <- c_diskfree c_path
	ifM (safeErrno <$> getErrno)
		( return $ Just $ toInteger free
		, return Nothing
		)
  where
	safeErrno (Errno v) = v == 0

#else

getDiskFree :: FilePath -> IO (Maybe Integer)
getDiskFree _ = return Nothing

#endif
