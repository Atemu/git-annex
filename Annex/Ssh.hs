{- git-annex ssh interface, with connection caching
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Ssh (
	sshParams,
	sshCleanup,
) where

import qualified Data.Map as M

import Common.Annex
import Annex.LockPool
import qualified Git
import qualified Git.Config

{- Generates parameters to ssh to a given host (or user@host) on a given
 - port, with connection caching. -}
sshParams :: (String, Maybe Integer) -> [CommandParam] -> Annex [CommandParam]
sshParams (host, port) opts = go =<< sshInfo (host, port)
	where
		go (Nothing, params) = ret params
		go (Just socketfile, params) = do
			cleanstale
			liftIO $ createDirectoryIfMissing True $ parentDir socketfile
			lockFile $ socket2lock socketfile
			ret params
		ret ps = return $ ps ++ opts ++ portParams port ++ [Param host]
		-- If the lock pool is empty, this is the first ssh of this
		-- run. There could be stale ssh connections hanging around
		-- from a previous git-annex run that was interrupted.
		cleanstale = whenM (null . filter isLock . M.keys <$> getPool) $
			sshCleanup

sshInfo :: (String, Maybe Integer) -> Annex (Maybe FilePath, [CommandParam])
sshInfo (host, port) = do
	caching <- Git.configTrue <$> fromRepo (Git.Config.get "annex.sshcaching" "true")
	if caching
		then do
			dir <- fromRepo $ gitAnnexSshDir
			let socketfile = dir </> hostport2socket host port
		 	return $ (Just socketfile, cacheParams socketfile)
		else return (Nothing, [])

cacheParams :: FilePath -> [CommandParam]
cacheParams socketfile =
	[ Param "-S", Param socketfile
	, Params "-o ControlMaster=auto -o ControlPersist=yes"
	]

portParams :: Maybe Integer -> [CommandParam]
portParams Nothing = []
portParams (Just port) = [Param "-p", Param $ show port]

{- Stop any unused ssh processes. -}
sshCleanup :: Annex ()
sshCleanup = do
	dir <- fromRepo $ gitAnnexSshDir
	liftIO $ createDirectoryIfMissing True dir
	sockets <- filter (not . isLock) <$> liftIO (dirContents dir)
	forM_ sockets cleanup
	where
		cleanup socketfile = do
			-- Drop any shared lock we have, and take an
			-- exclusive lock, without blocking. If the lock
			-- succeeds, nothing is using this ssh, and it can
			-- be stopped.
			let lockfile = socket2lock socketfile
			unlockFile lockfile
			fd <- liftIO $ openFd lockfile ReadWrite (Just stdFileMode) defaultFileFlags
			v <- liftIO $ tryIO $
				setLock fd (WriteLock, AbsoluteSeek, 0, 0)
			case v of
				Left _ -> return ()
				Right _ -> stopssh socketfile
			liftIO $ closeFd fd
		stopssh socketfile = do
			(_, params) <- sshInfo $ socket2hostport socketfile
			_ <- liftIO $ do
				-- "ssh -O stop" is noisy on stderr even with -q
				let cmd = unwords $ toCommand $
					[ Params "-O stop"
					] ++ params
				_ <- boolSystem "sh"
					[ Param "-c"
					, Param $ "ssh " ++ cmd ++ " >/dev/null 2>/dev/null"
					]
				--try $ removeFile socketfile
				return ()
			-- Cannot remove the lock file; other processes may
			-- be waiting on our exclusive lock to use it.
			return ()

hostport2socket :: String -> Maybe Integer -> FilePath
hostport2socket host Nothing = host
hostport2socket host (Just port) = host ++ "!" ++ show port

socket2hostport :: FilePath -> (String, Maybe Integer)
socket2hostport socket
	| null p = (h, Nothing)
	| otherwise = (h, readish p)
	where
		(h, p) = separate (== '!') $ takeFileName socket

socket2lock :: FilePath -> FilePath
socket2lock socket = socket ++ lockExt

isLock :: FilePath -> Bool
isLock f = lockExt `isSuffixOf` f

lockExt :: String
lockExt = ".lock"
