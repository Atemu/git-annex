{- git-annex ssh interface, with connection caching
 -
 - Copyright 2012,2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Ssh (
	sshParams,
	sshCleanup,
) where

import qualified Data.Map as M
import System.Posix.Env

import Common.Annex
import Annex.LockPool
import Annex.Perms
import qualified Build.SysConfig as SysConfig
import qualified Annex
import Config

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
	ret ps = return $ ps ++ opts ++ portParams port ++
		[Param "-T", Param host]
	-- If the lock pool is empty, this is the first ssh of this
	-- run. There could be stale ssh connections hanging around
	-- from a previous git-annex run that was interrupted.
	cleanstale = whenM (not . any isLock . M.keys <$> getPool) $
		sshCleanup

{- Returns a filename to use for a ssh connection caching socket, and
 - parameters to enable ssh connection caching. -}
sshInfo :: (String, Maybe Integer) -> Annex (Maybe FilePath, [CommandParam])
sshInfo (host, port) = go =<< sshCacheDir
  where
	go Nothing = return (Nothing, [])
	go (Just dir) = do
		let socketfile = dir </> hostport2socket host port
		if valid_unix_socket_path socketfile
			then return (Just socketfile, cacheparams socketfile)
			else do
				socketfile' <- liftIO $ relPathCwdToFile socketfile
				if valid_unix_socket_path socketfile'
					then return (Just socketfile', cacheparams socketfile')
					else return (Nothing, [])
	cacheparams :: FilePath -> [CommandParam]
	cacheparams socketfile =
		[ Param "-S", Param socketfile
		, Params "-o ControlMaster=auto -o ControlPersist=yes"
		]

{- ssh connection caching creates sockets, so will not work on a
 - crippled filesystem. A GIT_ANNEX_TMP_DIR can be provided to use
 - a different filesystem. -}
sshCacheDir :: Annex (Maybe FilePath)
sshCacheDir
	| SysConfig.sshconnectioncaching = ifM crippledFileSystem
		( maybe (return Nothing) usetmpdir =<< gettmpdir
		, ifM (fromMaybe True . annexSshCaching <$> Annex.getGitConfig)
			( Just <$> fromRepo gitAnnexSshDir
			, return Nothing
			)
		)
	| otherwise = return Nothing
  where
	gettmpdir = liftIO $ getEnv "GIT_ANNEX_TMP_DIR"
	usetmpdir tmpdir = liftIO $ catchMaybeIO $ do
		createDirectoryIfMissing True tmpdir
		return tmpdir

portParams :: Maybe Integer -> [CommandParam]
portParams Nothing = []
portParams (Just port) = [Param "-p", Param $ show port]

{- Stop any unused ssh processes. -}
sshCleanup :: Annex ()
sshCleanup = go =<< sshCacheDir
  where
	go Nothing = noop
	go (Just dir) = do
		sockets <- filter (not . isLock) <$>
			liftIO (catchDefaultIO [] $ dirContents dir)
		forM_ sockets cleanup
	cleanup socketfile = do
		-- Drop any shared lock we have, and take an
		-- exclusive lock, without blocking. If the lock
		-- succeeds, nothing is using this ssh, and it can
		-- be stopped.
		let lockfile = socket2lock socketfile
		unlockFile lockfile
		mode <- annexFileMode
		fd <- liftIO $ noUmask mode $
			openFd lockfile ReadWrite (Just mode) defaultFileFlags
		v <- liftIO $ tryIO $
			setLock fd (WriteLock, AbsoluteSeek, 0, 0)
		case v of
			Left _ -> noop
			Right _ -> stopssh socketfile
		liftIO $ closeFd fd
	stopssh socketfile = do
		let (host, port) = socket2hostport socketfile
		(_, params) <- sshInfo (host, port)
		-- "ssh -O stop" is noisy on stderr even with -q
		void $ liftIO $ catchMaybeIO $
			withQuietOutput createProcessSuccess $
				proc "ssh" $ toCommand $
					[ Params "-O stop"
					] ++ params ++ [Param host]
		-- Cannot remove the lock file; other processes may
		-- be waiting on our exclusive lock to use it.

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

{- This is the size of the sun_path component of sockaddr_un, which
 - is the limit to the total length of the filename of a unix socket.
 -
 - On Linux, this is 108. On OSX, 104. TODO: Probe
 -}
sizeof_sockaddr_un_sun_path :: Int
sizeof_sockaddr_un_sun_path = 100

{- Note that this looks at the true length of the path in bytes, as it will
 - appear on disk. -}
valid_unix_socket_path :: FilePath -> Bool
valid_unix_socket_path f = length (decodeW8 f) < sizeof_sockaddr_un_sun_path
