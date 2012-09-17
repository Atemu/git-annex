{- Simple IO exception handling
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Exception where

import Prelude hiding (catch)
import Control.Exception
import Control.Applicative

{- Catches IO errors and returns a Bool -}
catchBoolIO :: IO Bool -> IO Bool
catchBoolIO a = catchDefaultIO False a

{- Catches IO errors and returns a Maybe -}
catchMaybeIO :: IO a -> IO (Maybe a)
catchMaybeIO a = catchDefaultIO Nothing $ Just <$> a

{- Catches IO errors and returns a default value. -}
catchDefaultIO :: a -> IO a -> IO a
catchDefaultIO def a = catchIO a (const $ return def)

{- Catches IO errors and returns the error message. -}
catchMsgIO :: IO a -> IO (Either String a)
catchMsgIO a = either (Left . show) Right <$> tryIO a

{- catch specialized for IO errors only -}
catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

{- try specialized for IO errors only -}
tryIO :: IO a -> IO (Either IOException a)
tryIO = try
