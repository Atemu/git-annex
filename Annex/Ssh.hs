{- git-annex ssh interface, with connection caching
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Ssh (
	sshCachingOptions,
	sshCacheDir,
	sshReadPort,
	forceSshCleanup,
	sshCachingEnv,
	sshCachingTo,
	inRepoWithSshCachingTo,
	runSshCaching,
	sshAskPassEnv,
	runSshAskPass
) where

import qualified Data.Map as M
import Data.Hash.MD5
import System.Exit

import Common.Annex
import Annex.LockFile
import qualified Build.SysConfig as SysConfig
import qualified Annex
import qualified Git
import qualified Git.Url
import Config
import Config.Files
import Utility.Env
import Types.CleanupActions
import Annex.Index (addGitEnv)
import Utility.LockFile
#ifndef mingw32_HOST_OS
import Annex.Perms
#endif

{- Generates parameters to ssh to a given host (or user@host) on a given
 - port, with connection caching. -}
sshCachingOptions :: (String, Maybe Integer) -> [CommandParam] -> Annex [CommandParam]
sshCachingOptions (host, port) opts = go =<< sshInfo (host, port)
  where
	go (Nothing, params) = ret params
	go (Just socketfile, params) = do
		prepSocket socketfile
		ret params
	ret ps = return $ ps ++ opts ++ portParams port ++ [Param "-T"]

{- Returns a filename to use for a ssh connection caching socket, and
 - parameters to enable ssh connection caching. -}
sshInfo :: (String, Maybe Integer) -> Annex (Maybe FilePath, [CommandParam])
sshInfo (host, port) = go =<< sshCacheDir
  where
	go Nothing = return (Nothing, [])
	go (Just dir) = do
		r <- liftIO $ bestSocketPath $ dir </> hostport2socket host port
		return $ case r of
			Nothing -> (Nothing, [])
			Just socketfile -> (Just socketfile, sshConnectionCachingParams socketfile)

{- Given an absolute path to use for a socket file,
 - returns whichever is shorter of that or the relative path to the same
 - file.
 -
 - If no path can be constructed that is a valid socket, returns Nothing. -}
bestSocketPath :: FilePath -> IO (Maybe FilePath)
bestSocketPath abssocketfile = do
	relsocketfile <- liftIO $ relPathCwdToFile abssocketfile
	let socketfile = if length abssocketfile <= length relsocketfile
		then abssocketfile
		else relsocketfile
	return $ if valid_unix_socket_path (socketfile ++ sshgarbage)
			then Just socketfile
			else Nothing
  where
	-- ssh appends a 16 char extension to the socket when setting it
	-- up, which needs to be taken into account when checking
	-- that a valid socket was constructed.
	sshgarbage = replicate (1+16) 'X'

sshConnectionCachingParams :: FilePath -> [CommandParam]
sshConnectionCachingParams socketfile = 
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
		let socktmp = tmpdir </> "ssh"
		createDirectoryIfMissing True socktmp
		return socktmp

portParams :: Maybe Integer -> [CommandParam]
portParams Nothing = []
portParams (Just port) = [Param "-p", Param $ show port]

{- Prepare to use a socket file. Locks a lock file to prevent
 - other git-annex processes from stopping the ssh on this socket. -}
prepSocket :: FilePath -> Annex ()
prepSocket socketfile = do
	-- If the lock pool is empty, this is the first ssh of this
	-- run. There could be stale ssh connections hanging around
	-- from a previous git-annex run that was interrupted.
	whenM (not . any isLock . M.keys <$> getLockPool)
		sshCleanup
	-- Cleanup at end of this run.
	Annex.addCleanup SshCachingCleanup sshCleanup

	liftIO $ createDirectoryIfMissing True $ parentDir socketfile
	lockFileShared $ socket2lock socketfile

enumSocketFiles :: Annex [FilePath]
enumSocketFiles = go =<< sshCacheDir
  where
	go Nothing = return []
	go (Just dir) = liftIO $ filter (not . isLock)
		<$> catchDefaultIO [] (dirContents dir)

{- Stop any unused ssh connection caching processes. -}
sshCleanup :: Annex ()
sshCleanup = mapM_ cleanup =<< enumSocketFiles
  where
	cleanup socketfile = do
#ifndef mingw32_HOST_OS
		-- Drop any shared lock we have, and take an
		-- exclusive lock, without blocking. If the lock
		-- succeeds, nothing is using this ssh, and it can
		-- be stopped.
		--
		-- After ssh is stopped cannot remove the lock file;
		-- other processes may be waiting on our exclusive
		-- lock to use it.
		let lockfile = socket2lock socketfile
		unlockFile lockfile
		mode <- annexFileMode
		v <- liftIO $ noUmask mode $ tryLockExclusive (Just mode) lockfile
		case v of
			Nothing -> noop
			Just lck -> do
				forceStopSsh socketfile
				liftIO $ dropLock lck
#else
		forceStopSsh socketfile
#endif

{- Stop all ssh connection caching processes, even when they're in use. -}
forceSshCleanup :: Annex ()
forceSshCleanup = mapM_ forceStopSsh =<< enumSocketFiles

forceStopSsh :: FilePath -> Annex ()
forceStopSsh socketfile = do
	let (dir, base) = splitFileName socketfile
	let params = sshConnectionCachingParams base
	-- "ssh -O stop" is noisy on stderr even with -q
	void $ liftIO $ catchMaybeIO $
		withQuietOutput createProcessSuccess $
			(proc "ssh" $ toCommand $
				[ Params "-O stop"
				] ++ params ++ [Param "localhost"])
				{ cwd = Just dir }
	liftIO $ nukeFile socketfile

{- This needs to be as short as possible, due to limitations on the length
 - of the path to a socket file. At the same time, it needs to be unique
 - for each host.
 -}
hostport2socket :: String -> Maybe Integer -> FilePath
hostport2socket host Nothing = hostport2socket' host
hostport2socket host (Just port) = hostport2socket' $ host ++ "!" ++ show port
hostport2socket' :: String -> FilePath
hostport2socket' s
	| length s > lengthofmd5s = md5s (Str s)
	| otherwise = s
  where
	lengthofmd5s = 32

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

{- Parses the SSH port, and returns the other OpenSSH options. If
 - several ports are found, the last one takes precedence. -}
sshReadPort :: [String] -> (Maybe Integer, [String])
sshReadPort params = (port, reverse args)
  where
	(port,args) = aux (Nothing, []) params
	aux (p,ps) [] = (p,ps)
	aux (_,ps) ("-p":p:rest) = aux (readPort p, ps) rest
	aux (p,ps) (q:rest) | "-p" `isPrefixOf` q = aux (readPort $ drop 2 q, ps) rest
			    | otherwise = aux (p,q:ps) rest
	readPort p = fmap fst $ listToMaybe $ reads p

{- When this env var is set, git-annex runs ssh with parameters
 - to use the socket file that the env var contains.
 -
 - This is a workaround for GIT_SSH not being able to contain
 - additional parameters to pass to ssh. -}
sshCachingEnv :: String
sshCachingEnv = "GIT_ANNEX_SSHCACHING"

{- Enables ssh caching for git push/pull to a particular
 - remote git repo. (Can safely be used on non-ssh remotes.)
 -
 - Like inRepo, the action is run with the local git repo.
 - But here it's a modified version, with gitEnv to set GIT_SSH=git-annex,
 - and sshCachingEnv set so that git-annex will know what socket
 - file to use. -}
inRepoWithSshCachingTo :: Git.Repo -> (Git.Repo -> IO a) -> Annex a
inRepoWithSshCachingTo remote a =
	liftIO . a =<< sshCachingTo remote =<< gitRepo

{- To make any git commands be run with ssh caching enabled, 
 - alters the local Git.Repo's gitEnv to set GIT_SSH=git-annex,
 - and set sshCachingEnv so that git-annex will know what socket
 - file to use. -}
sshCachingTo :: Git.Repo -> Git.Repo -> Annex Git.Repo
sshCachingTo remote g 
	| not (Git.repoIsUrl remote) || Git.repoIsHttp remote = uncached
	| otherwise = case Git.Url.hostuser remote of
		Nothing -> uncached
		Just host -> do
			(msockfile, _) <- sshInfo (host, Git.Url.port remote)
			case msockfile of
				Nothing -> return g
				Just sockfile -> do
					command <- liftIO readProgramFile
					prepSocket sockfile
					liftIO $ do
						g' <- addGitEnv g sshCachingEnv sockfile
						addGitEnv g' "GIT_SSH" command
  where
	uncached = return g

runSshCaching :: [String] -> FilePath -> IO ()
runSshCaching args sockfile = do
	let args' = toCommand (sshConnectionCachingParams sockfile) ++ args
	let p = proc "ssh" args'
	exitWith =<< waitForProcess . processHandle =<< createProcess p

{- When this env var is set, git-annex is being used as a ssh-askpass
 - program, and should read the password from the specified location,
 - and output it for ssh to read. -}
sshAskPassEnv :: String
sshAskPassEnv = "GIT_ANNEX_SSHASKPASS"

runSshAskPass :: FilePath -> IO ()
runSshAskPass passfile = putStrLn =<< readFile passfile
