{- named threads
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.NamedThread where

import Assistant.Monad
import Assistant.Types.ThreadName

{- Information about a named thread that can be run. -}
data NamedThread = NamedThread ThreadName (Assistant ())

namedThread :: String -> Assistant () -> NamedThread
namedThread name a = NamedThread (ThreadName name) a
