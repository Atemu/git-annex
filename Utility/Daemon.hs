{- daemon support
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Daemon where

import Common

import System.Posix

{- Run an action as a daemon, with all output sent to a file descriptor.
 -
 - Can write its pid to a file, to guard against multiple instances
 - running and allow easy termination.
 -
 - When successful, does not return. -}
daemonize :: Fd -> Maybe FilePath -> Bool -> IO () -> IO ()
daemonize logfd pidfile changedirectory a = do
	maybe noop checkalreadyrunning pidfile
	_ <- forkProcess child1
	out
  where
	checkalreadyrunning f = maybe noop (const $ alreadyRunning) 
		=<< checkDaemon f
	child1 = do
		_ <- createSession
		_ <- forkProcess child2
		out
	child2 = do
		maybe noop lockPidFile pidfile 
		when changedirectory $
			setCurrentDirectory "/"
		nullfd <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
		_ <- redir nullfd stdInput
		mapM_ (redir logfd) [stdOutput, stdError]
		closeFd logfd
		a
		out
	redir newh h = do
		closeFd h
		dupTo newh h
	out = exitImmediately ExitSuccess

{- Locks the pid file, with an exclusive, non-blocking lock.
 - Writes the pid to the file, fully atomically.
 - Fails if the pid file is already locked by another process. -}
lockPidFile :: FilePath -> IO ()
lockPidFile file = do
	createDirectoryIfMissing True (parentDir file)
	fd <- openFd file ReadWrite (Just stdFileMode) defaultFileFlags
	locked <- catchMaybeIO $ setLock fd (WriteLock, AbsoluteSeek, 0, 0)
	fd' <- openFd newfile ReadWrite (Just stdFileMode) defaultFileFlags
		{ trunc = True }
	locked' <- catchMaybeIO $ setLock fd' (WriteLock, AbsoluteSeek, 0, 0)
	case (locked, locked') of
		(Nothing, _) -> alreadyRunning
		(_, Nothing) -> alreadyRunning
		_ -> do
			_ <- fdWrite fd' =<< show <$> getProcessID
			renameFile newfile file
			closeFd fd
  where
	newfile = file ++ ".new"

alreadyRunning :: IO ()
alreadyRunning = error "Daemon is already running."

{- Checks if the daemon is running, by checking that the pid file
 - is locked by the same process that is listed in the pid file.
 -
 - If it's running, returns its pid. -}
checkDaemon :: FilePath -> IO (Maybe ProcessID)
checkDaemon pidfile = do
	v <- catchMaybeIO $
		openFd pidfile ReadOnly (Just stdFileMode) defaultFileFlags
	case v of
		Just fd -> do
			locked <- getLock fd (ReadLock, AbsoluteSeek, 0, 0)
			p <- readish <$> readFile pidfile
			return $ check locked p
		Nothing -> return Nothing
  where
	check Nothing _ = Nothing
	check _ Nothing = Nothing
	check (Just (pid, _)) (Just pid')
		| pid == pid' = Just pid
		| otherwise = error $
			"stale pid in " ++ pidfile ++ 
			" (got " ++ show pid' ++ 
			"; expected " ++ show pid ++ " )"

{- Stops the daemon, safely. -}
stopDaemon :: FilePath -> IO ()
stopDaemon pidfile = go =<< checkDaemon pidfile
  where
	go Nothing = noop
	go (Just pid) = signalProcess sigTERM pid
