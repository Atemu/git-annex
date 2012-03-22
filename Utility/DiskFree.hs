{- disk free space checking 
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Utility.DiskFree ( getDiskFree ) where

import Common

import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Error

foreign import ccall unsafe "diskfree.h diskfree" c_diskfree
	:: CString -> IO CULLong

getDiskFree :: String -> IO (Maybe Integer)
getDiskFree path = withFilePath path $ \c_path -> do
	free <- c_diskfree c_path
	ifM (safeErrno <$> getErrno)
		( return $ Just $ toInteger free
		, do
			Errno i <- getErrno
			print i
			return Nothing
		)
	where
		safeErrno (Errno v) = v == 0
