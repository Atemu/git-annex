{- Windows lock files, using lock pools.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.LockPool.Windows (
	LockHandle,
	lockShared,
	lockExclusive,
	dropLock,
	waitToLock,
) where

import qualified Utility.LockFile.Windows as F
import qualified Utility.LockPool.STM as P
import Utility.LockPool.LockHandle
import Utility.LockPool.STM (LockPool, LockFile, LockMode(..))
import Utility.Monad

import Control.Concurrent.STM
import System.IO
import Data.Maybe
import Control.Applicative
import Prelude

{- Tries to lock a file with a shared lock, which allows other processes to
 - also lock it shared. Fails if the file is exclusively locked. -}
lockShared :: LockFile -> IO (Maybe LockHandle)
lockShared file = tryMakeLockHandle
	(P.tryTakeLock P.lockPool file LockShared)
	(F.lockShared file)

{- Tries to take an exclusive lock on a file. Fails if another process has
 - a shared or exclusive lock.
 -
 - Note that exclusive locking also prevents the file from being opened for
 - read or write by any other process. So for advisory locking of a file's
 - content, a separate LockFile should be used. -}
lockExclusive :: LockFile -> IO (Maybe LockHandle)
lockExclusive file = tryMakeLockHandle
	(P.tryTakeLock P.lockPool file LockExclusive)
	(F.lockExclusive file)

{- If the initial lock fails, this is a BUSY wait, and does not
 - guarentee FIFO order of waiters. In other news, Windows is a POS. -}
waitToLock :: IO (Maybe LockHandle) -> IO LockHandle
waitToLock = F.waitToLock
