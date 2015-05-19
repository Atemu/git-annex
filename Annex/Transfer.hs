{- git-annex transfers
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Transfer (
	module X,
	noObserver,
	upload,
	download,
	runTransfer,
	alwaysRunTransfer,
	noRetry,
	forwardRetry,
) where

import Common.Annex
import Logs.Transfer as X
import Annex.Notification as X
import Annex.Perms
import Utility.Metered
import Utility.LockPool

import Control.Concurrent

type TransferAction = MeterUpdate -> Annex Bool

type TransferObserver = Bool -> Transfer -> TransferInfo -> Annex ()

noObserver :: TransferObserver
noObserver _ _ _ = noop

upload :: UUID -> Key -> AssociatedFile -> RetryDecider -> TransferObserver -> TransferAction -> NotifyWitness -> Annex Bool
upload u key f d o a _witness = runTransfer (Transfer Upload u key) f d o a

download :: UUID -> Key -> AssociatedFile -> RetryDecider -> TransferObserver -> TransferAction -> NotifyWitness -> Annex Bool
download u key f d o a _witness = runTransfer (Transfer Download u key) f d o a

{- Runs a transfer action. Creates and locks the lock file while the
 - action is running, and stores info in the transfer information
 - file.
 -
 - If the transfer action returns False, the transfer info is 
 - left in the failedTransferDir.
 -
 - If the transfer is already in progress, returns False.
 -
 - An upload can be run from a read-only filesystem, and in this case
 - no transfer information or lock file is used.
 -}
runTransfer :: Transfer -> Maybe FilePath -> RetryDecider -> TransferObserver -> TransferAction -> Annex Bool
runTransfer = runTransfer' False

{- Like runTransfer, but ignores any existing transfer lock file for the
 - transfer, allowing re-running a transfer that is already in progress.
 -
 - Note that this may result in confusing progress meter display in the
 - webapp, if multiple processes are writing to the transfer info file. -}
alwaysRunTransfer :: Transfer -> Maybe FilePath -> RetryDecider -> TransferObserver -> TransferAction -> Annex Bool
alwaysRunTransfer = runTransfer' True

runTransfer' :: Bool -> Transfer -> Maybe FilePath -> RetryDecider -> TransferObserver -> TransferAction -> Annex Bool
runTransfer' ignorelock t file shouldretry transferobserver transferaction = do
	info <- liftIO $ startTransferInfo file
	(meter, tfile, metervar) <- mkProgressUpdater t info
	mode <- annexFileMode
	(lck, inprogress) <- liftIO $ prep tfile mode info
	if inprogress && not ignorelock
		then do
			showNote "transfer already in progress, or unable to take transfer lock"
			return False
		else do
			ok <- retry info metervar $ transferaction meter
			liftIO $ cleanup tfile lck
			transferobserver ok t info
			return ok
  where
#ifndef mingw32_HOST_OS
	prep tfile mode info = do
		let lck = transferLockFile tfile
		r <- tryLockExclusive (Just mode) lck
		case r of
			Nothing -> return (Nothing, True)
			Just lockhandle -> ifM (checkSaneLock lck lockhandle)
				( do
					void $ tryIO $ writeTransferInfoFile info tfile
					return (Just lockhandle, False)
				, return (Nothing, True)
				)
#else
	prep tfile _mode info = do
		let lck = transferLockFile tfile
		v <- catchMaybeIO $ lockExclusive lck
		case v of
			Nothing -> return (Nothing, False)
			Just Nothing -> return (Nothing, True)
			Just (Just lockhandle) -> do
				void $ tryIO $ writeTransferInfoFile info tfile
				return (Just lockhandle, False)
#endif
	cleanup _ Nothing = noop
	cleanup tfile (Just lockhandle) = do
		let lck = transferLockFile tfile
		void $ tryIO $ removeFile tfile
#ifndef mingw32_HOST_OS
		void $ tryIO $ removeFile lck
		dropLock lockhandle
#else
		{- Windows cannot delete the lockfile until the lock
		 - is closed. So it's possible to race with another
		 - process that takes the lock before it's removed,
		 - so ignore failure to remove.
		 -}
		dropLock lockhandle
		void $ tryIO $ removeFile lck
#endif
	retry oldinfo metervar run = do
		v <- tryNonAsync run
		case v of
			Right b -> return b
			Left e -> do
				warning (show e)
				b <- getbytescomplete metervar
				let newinfo = oldinfo { bytesComplete = Just b }
				if shouldretry oldinfo newinfo
					then retry newinfo metervar run
					else return False
	getbytescomplete metervar
		| transferDirection t == Upload =
			liftIO $ readMVar metervar
		| otherwise = do
			f <- fromRepo $ gitAnnexTmpObjectLocation (transferKey t)
			liftIO $ catchDefaultIO 0 $ getFileSize f

type RetryDecider = TransferInfo -> TransferInfo -> Bool

noRetry :: RetryDecider
noRetry _ _ = False

{- Retries a transfer when it fails, as long as the failed transfer managed
 - to send some data. -}
forwardRetry :: RetryDecider
forwardRetry old new = bytesComplete old < bytesComplete new
