{- git-annex transfers
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Transfer (
	module X,
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
#ifdef mingw32_HOST_OS
import Utility.LockFile
#endif

import Control.Concurrent

upload :: UUID -> Key -> AssociatedFile -> RetryDecider -> (MeterUpdate -> Annex Bool) -> NotifyWitness -> Annex Bool
upload u key f d a _witness = runTransfer (Transfer Upload u key) f d a

download :: UUID -> Key -> AssociatedFile -> RetryDecider -> (MeterUpdate -> Annex Bool) -> NotifyWitness -> Annex Bool
download u key f d a _witness = runTransfer (Transfer Download u key) f d a

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
runTransfer :: Transfer -> Maybe FilePath -> RetryDecider -> (MeterUpdate -> Annex Bool) -> Annex Bool
runTransfer = runTransfer' False

{- Like runTransfer, but ignores any existing transfer lock file for the
 - transfer, allowing re-running a transfer that is already in progress.
 -
 - Note that this may result in confusing progress meter display in the
 - webapp, if multiple processes are writing to the transfer info file. -}
alwaysRunTransfer :: Transfer -> Maybe FilePath -> RetryDecider -> (MeterUpdate -> Annex Bool) -> Annex Bool
alwaysRunTransfer = runTransfer' True

runTransfer' :: Bool -> Transfer -> Maybe FilePath -> RetryDecider -> (MeterUpdate -> Annex Bool) -> Annex Bool
runTransfer' ignorelock t file shouldretry a = do
	info <- liftIO $ startTransferInfo file
	(meter, tfile, metervar) <- mkProgressUpdater t info
	mode <- annexFileMode
	(fd, inprogress) <- liftIO $ prep tfile mode info
	if inprogress && not ignorelock
		then do
			showNote "transfer already in progress"
			return False
		else do
			ok <- retry info metervar $
		 		bracketIO (return fd) (cleanup tfile) (const $ a meter)
			unless ok $ recordFailedTransfer t info
			return ok
  where
#ifndef mingw32_HOST_OS
	prep tfile mode info = do
		mfd <- catchMaybeIO $
			openFd (transferLockFile tfile) ReadWrite (Just mode)
				defaultFileFlags { trunc = True }
		case mfd of
			Nothing -> return (Nothing, False)
			Just fd -> do
				setFdOption fd CloseOnExec True
				locked <- catchMaybeIO $
					setLock fd (WriteLock, AbsoluteSeek, 0, 0)
				if isNothing locked
					then return (Nothing, True)
					else do
						void $ tryIO $ writeTransferInfoFile info tfile
						return (mfd, False)
#else
	prep tfile _mode info = do
		v <- catchMaybeIO $ lockExclusive (transferLockFile tfile)
		case v of
			Nothing -> return (Nothing, False)
			Just Nothing -> return (Nothing, True)
			Just (Just lockhandle) -> do
				void $ tryIO $ writeTransferInfoFile info tfile
				return (Just lockhandle, False)
#endif
	cleanup _ Nothing = noop
	cleanup tfile (Just lockhandle) = do
		void $ tryIO $ removeFile tfile
#ifndef mingw32_HOST_OS
		void $ tryIO $ removeFile $ transferLockFile tfile
		closeFd lockhandle
#else
		{- Windows cannot delete the lockfile until the lock
		 - is closed. So it's possible to race with another
		 - process that takes the lock before it's removed,
		 - so ignore failure to remove.
		 -}
		dropLock lockhandle
		void $ tryIO $ removeFile $ transferLockFile tfile
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
			liftIO $ catchDefaultIO 0 $
				fromIntegral . fileSize <$> getFileStatus f

type RetryDecider = TransferInfo -> TransferInfo -> Bool

noRetry :: RetryDecider
noRetry _ _ = False

{- Retries a transfer when it fails, as long as the failed transfer managed
 - to send some data. -}
forwardRetry :: RetryDecider
forwardRetry old new = bytesComplete old < bytesComplete new
