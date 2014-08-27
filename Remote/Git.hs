{- Standard git remotes.
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Remote.Git (
	remote,
	configRead,
	repoAvail,
) where

import Common.Annex
import Annex.Ssh
import Types.Remote
import Types.GitConfig
import qualified Git
import qualified Git.Config
import qualified Git.Construct
import qualified Git.Command
import qualified Git.GCrypt
import qualified Git.Types as Git
import qualified Annex
import Logs.Presence
import Annex.Transfer
import Annex.UUID
import qualified Annex.Content
import qualified Annex.BranchState
import qualified Annex.Branch
import qualified Annex.Url as Url
import Utility.Tmp
import Config
import Config.Cost
import Annex.Init
import Types.Key
import Types.CleanupActions
import qualified CmdLine.GitAnnexShell.Fields as Fields
import Logs.Location
import Utility.Metered
#ifndef mingw32_HOST_OS
import Utility.CopyFile
#endif
import Utility.Env
import Utility.Batch
import Remote.Helper.Git
import Remote.Helper.Messages
import qualified Remote.Helper.Ssh as Ssh
import qualified Remote.GCrypt
import Config.Files
import Creds
import Annex.CatFile

import Control.Concurrent
import Control.Concurrent.MSampleVar
import qualified Data.Map as M
import Network.URI

remote :: RemoteType
remote = RemoteType {
	typename = "git",
	enumerate = list,
	generate = gen,
	setup = gitSetup
}

list :: Annex [Git.Repo]
list = do
	c <- fromRepo Git.config
	rs <- mapM (tweakurl c) =<< fromRepo Git.remotes
	mapM configRead rs
  where
	annexurl n = "remote." ++ n ++ ".annexurl"
	tweakurl c r = do
		let n = fromJust $ Git.remoteName r
		case M.lookup (annexurl n) c of
			Nothing -> return r
			Just url -> inRepo $ \g ->
				Git.Construct.remoteNamed n $
					Git.Construct.fromRemoteLocation url g

{- Git remotes are normally set up using standard git command, not
 - git-annex initremote and enableremote.
 -
 - For initremote, the git remote must already be set up, and have a uuid.
 - Initremote simply remembers its location.
 -
 - enableremote simply sets up a git remote using the stored location.
 - No attempt is made to make the remote be accessible via ssh key setup,
 - etc.
 -}
gitSetup :: Maybe UUID -> Maybe CredPair -> RemoteConfig -> Annex (RemoteConfig, UUID)
gitSetup Nothing _ c = do
	let location = fromMaybe (error "Specify location=url") $
		Url.parseURIRelaxed =<< M.lookup "location" c
	g <- Annex.gitRepo
	u <- case filter (\r -> Git.location r == Git.Url location) (Git.remotes g) of
		[r] -> getRepoUUID r
		[] -> error "could not find existing git remote with specified location"
		_ -> error "found multiple git remotes with specified location"
	return (c, u)
gitSetup (Just u) _ c = do
	inRepo $ Git.Command.run
		[ Param "remote"
		, Param "add"
		, Param $ fromMaybe (error "no name") (M.lookup "name" c)
		, Param $ fromMaybe (error "no location") (M.lookup "location" c)
		]
	return (c, u)

{- It's assumed to be cheap to read the config of non-URL remotes, so this is
 - done each time git-annex is run in a way that uses remotes.
 -
 - Conversely, the config of an URL remote is only read when there is no
 - cached UUID value. -}
configRead :: Git.Repo -> Annex Git.Repo
configRead r = do
	gc <- Annex.getRemoteGitConfig r
	u <- getRepoUUID r
	case (repoCheap r, remoteAnnexIgnore gc, u) of
		(_, True, _) -> return r
		(True, _, _) -> tryGitConfigRead r
		(False, _, NoUUID) -> tryGitConfigRead r
		_ -> return r

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc
	| Git.GCrypt.isEncrypted r = Remote.GCrypt.chainGen r u c gc
	| otherwise = go <$> remoteCost gc defcst
  where
	defcst = if repoCheap r then cheapRemoteCost else expensiveRemoteCost
	go cst = Just new
	  where
		new = Remote 
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = copyToRemote new
			, retrieveKeyFile = copyFromRemote new
			, retrieveKeyFileCheap = copyFromRemoteCheap new
			, removeKey = dropKey new
			, checkPresent = inAnnex new
			, checkPresentCheap = repoCheap r
			, whereisKey = Nothing
			, remoteFsck = if Git.repoIsUrl r
				then Nothing
				else Just $ fsckOnRemote r
			, repairRepo = if Git.repoIsUrl r
				then Nothing
				else Just $ repairRemote r
			, config = c
			, localpath = localpathCalc r
			, repo = r
			, gitconfig = gc
				{ remoteGitConfig = Just $ extractGitConfig r }
			, readonly = Git.repoIsHttp r
			, availability = availabilityCalc r
			, remotetype = remote
			, mkUnavailable = unavailable r u c gc
			}

unavailable :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
unavailable r u c gc = gen r' u c gc
  where
	r' = case Git.location r of
		Git.Local { Git.gitdir = d } ->
			r { Git.location = Git.LocalUnknown d }
		Git.Url url -> case uriAuthority url of
			Just auth -> 
				let auth' = auth { uriRegName = "!dne!" }
				in r { Git.location = Git.Url (url { uriAuthority = Just auth' })}
			Nothing -> r { Git.location = Git.Unknown }
		_ -> r -- already unavailable

{- Checks relatively inexpensively if a repository is available for use. -}
repoAvail :: Git.Repo -> Annex Bool
repoAvail r 
	| Git.repoIsHttp r = return True
	| Git.GCrypt.isEncrypted r = do
		g <- gitRepo
		liftIO $ do
			er <- Git.GCrypt.encryptedRemote g r
			if Git.repoIsLocal er || Git.repoIsLocalUnknown er
				then catchBoolIO $
					void (Git.Config.read er) >> return True
				else return True
	| Git.repoIsUrl r = return True
	| Git.repoIsLocalUnknown r = return False
	| otherwise = liftIO $ isJust <$> catchMaybeIO (Git.Config.read r)

{- Tries to read the config for a specified remote, updates state, and
 - returns the updated repo. -}
tryGitConfigRead :: Git.Repo -> Annex Git.Repo
tryGitConfigRead r 
	| haveconfig r = return r -- already read
	| Git.repoIsSsh r = store $ do
		v <- Ssh.onRemote r (pipedconfig, return (Left undefined)) "configlist" [] []
		case v of
			Right r'
				| haveconfig r' -> return r'
				| otherwise -> configlist_failed
			Left _ -> configlist_failed
	| Git.repoIsHttp r = store geturlconfig
	| Git.GCrypt.isEncrypted r = handlegcrypt =<< getConfigMaybe (remoteConfig r "uuid")
	| Git.repoIsUrl r = return r
	| otherwise = store $ liftIO $ 
		readlocalannexconfig `catchNonAsync` (const $ return r)
  where
	haveconfig = not . M.null . Git.config

	pipedconfig cmd params = do
		v <- Git.Config.fromPipe r cmd params
		case v of
			Right (r', val) -> do
				when (getUncachedUUID r' == NoUUID && not (null val)) $ do
					warningIO $ "Failed to get annex.uuid configuration of repository " ++ Git.repoDescribe r
					warningIO $ "Instead, got: " ++ show val
					warningIO $ "This is unexpected; please check the network transport!"
				return $ Right r'
			Left l -> return $ Left l

	geturlconfig = do
		uo <- Url.getUrlOptions
		v <- liftIO $ withTmpFile "git-annex.tmp" $ \tmpfile h -> do
			hClose h
			ifM (Url.downloadQuiet (Git.repoLocation r ++ "/config") tmpfile uo)
				( pipedconfig "git" [Param "config", Param "--null", Param "--list", Param "--file", File tmpfile]
				, return $ Left undefined
				)
		case v of
			Left _ -> do
				set_ignore "not usable by git-annex" False
				return r
			Right r' -> do
				-- Cache when http remote is not bare for
				-- optimisation.
				unless (Git.Config.isBare r') $
					setremote "annex-bare" (Git.Config.boolConfig False)
				return r'

	store = observe $ \r' -> do
		g <- gitRepo
		let l = Git.remotes g
		let g' = g { Git.remotes = exchange l r' }
		Annex.changeState $ \s -> s { Annex.repo = g' }

	exchange [] _ = []
	exchange (old:ls) new
		| Git.remoteName old == Git.remoteName new =
			new : exchange ls new
		| otherwise =
			old : exchange ls new

	{- Is this remote just not available, or does
	 - it not have git-annex-shell?
	 - Find out by trying to fetch from the remote. -}
	configlist_failed = case Git.remoteName r of
		Nothing -> return r
		Just n -> do
			whenM (inRepo $ Git.Command.runBool [Param "fetch", Param "--quiet", Param n]) $ do
				set_ignore "does not have git-annex installed" True
			return r
	
	set_ignore msg longmessage = do
		let k = "annex-ignore"
		case Git.remoteName r of
			Nothing -> noop
			Just n -> do
				warning $ "Remote " ++ n ++ " " ++ msg ++ "; setting " ++ k
				when longmessage $
					warning $ "This could be a problem with the git-annex installation on the remote. Please make sure that git-annex-shell is available in PATH when you ssh into the remote. Once you have fixed the git-annex installation, run: git config remote." ++ n ++ "." ++ k ++ " false"
		setremote k (Git.Config.boolConfig True)
	
	setremote k v = case Git.remoteName r of
		Nothing -> noop
		Just n -> do
			let k' = "remote." ++ n ++ "." ++ k
			inRepo $ Git.Command.run [Param "config", Param k', Param v]
		
	handlegcrypt Nothing = return r
	handlegcrypt (Just _cacheduuid) = do
		-- Generate UUID from the gcrypt-id
		g <- gitRepo
		case Git.GCrypt.remoteRepoId g (Git.remoteName r) of
			Nothing -> return r
			Just v -> store $ liftIO $ setUUID r $
				genUUIDInNameSpace gCryptNameSpace v

	{- The local repo may not yet be initialized, so try to initialize
	 - it if allowed. However, if that fails, still return the read
	 - git config. -}
	readlocalannexconfig = do
		s <- Annex.new r
		Annex.eval s $ do
			Annex.BranchState.disableUpdate
			void $ tryNonAsync $ ensureInitialized
			Annex.getState Annex.repo

{- Checks if a given remote has the content for a key in its annex. -}
inAnnex :: Remote -> Key -> Annex Bool
inAnnex rmt key
	| Git.repoIsHttp r = checkhttp
	| Git.repoIsUrl r = checkremote
	| otherwise = checklocal
  where
  	r = repo rmt
	checkhttp = do
		showChecking r
		ifM (Url.withUrlOptions $ \uo -> anyM (\u -> Url.checkBoth u (keySize key) uo) (keyUrls rmt key))
			( return True
			, error "not found"
			)
	checkremote = Ssh.inAnnex r key
	checklocal = guardUsable r (cantCheck r) $
		maybe (cantCheck r) return
			=<< onLocal rmt (Annex.Content.inAnnexSafe key)

keyUrls :: Remote -> Key -> [String]
keyUrls r key = map tourl locs'
  where
	tourl l = Git.repoLocation (repo r) ++ "/" ++ l
	-- If the remote is known to not be bare, try the hash locations
	-- used for non-bare repos first, as an optimisation.
	locs
		| remoteAnnexBare (gitconfig r) == Just False = reverse (annexLocations key)
		| otherwise = annexLocations key
#ifndef mingw32_HOST_OS
	locs' = locs
#else
	locs' = map (replace "\\" "/") locs
#endif

dropKey :: Remote -> Key -> Annex Bool
dropKey r key
	| not $ Git.repoIsUrl (repo r) =
		guardUsable (repo r) (return False) $
			commitOnCleanup r $ onLocal r $ do
				ensureInitialized
				whenM (Annex.Content.inAnnex key) $ do
					Annex.Content.lockContent key
						Annex.Content.removeAnnex
					logStatus key InfoMissing
					Annex.Content.saveState True
				return True
	| Git.repoIsHttp (repo r) = error "dropping from http remote not supported"
	| otherwise = commitOnCleanup r $ Ssh.dropKey (repo r) key

{- Tries to copy a key's content from a remote's annex to a file. -}
copyFromRemote :: Remote -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
copyFromRemote r key file dest _p = copyFromRemote' r key file dest
copyFromRemote' :: Remote -> Key -> AssociatedFile -> FilePath -> Annex Bool
copyFromRemote' r key file dest
	| not $ Git.repoIsUrl (repo r) = guardUsable (repo r) (return False) $ do
		params <- Ssh.rsyncParams r Download
		u <- getUUID
		-- run copy from perspective of remote
		onLocal r $ do
			ensureInitialized
			v <- Annex.Content.prepSendAnnex key
			case v of
				Nothing -> return False
				Just (object, checksuccess) ->
					runTransfer (Transfer Download u key) file noRetry
						(rsyncOrCopyFile params object dest)
						<&&> checksuccess
	| Git.repoIsSsh (repo r) = feedprogressback $ \feeder -> do
		direct <- isDirect
		Ssh.rsyncHelper (Just feeder) 
			=<< Ssh.rsyncParamsRemote direct r Download key dest file
	| Git.repoIsHttp (repo r) = Annex.Content.downloadUrl (keyUrls r key) dest
	| otherwise = error "copying from non-ssh, non-http remote not supported"
  where
	{- Feed local rsync's progress info back to the remote,
	 - by forking a feeder thread that runs
	 - git-annex-shell transferinfo at the same time
	 - git-annex-shell sendkey is running.
	 -
	 - To avoid extra password prompts, this is only done when ssh
	 - connection caching is supported.
	 - Note that it actually waits for rsync to indicate
	 - progress before starting transferinfo, in order
	 - to ensure ssh connection caching works and reuses 
	 - the connection set up for the sendkey.
	 -
	 - Also note that older git-annex-shell does not support
	 - transferinfo, so stderr is dropped and failure ignored.
	 -}
	feedprogressback a = ifM (isJust <$> sshCacheDir)
		( feedprogressback' a
		, a $ const noop
		)
	feedprogressback' a = do
		u <- getUUID
		let fields = (Fields.remoteUUID, fromUUID u)
			: maybe [] (\f -> [(Fields.associatedFile, f)]) file
		Just (cmd, params) <- Ssh.git_annex_shell (repo r) "transferinfo" 
			[Param $ key2file key] fields
		v <- liftIO (newEmptySV :: IO (MSampleVar Integer))
		pidv <- liftIO $ newEmptyMVar
		tid <- liftIO $ forkIO $ void $ tryIO $ do
			bytes <- readSV v
			p <- createProcess $
				 (proc cmd (toCommand params))
					{ std_in = CreatePipe
					, std_err = CreatePipe
					}
			putMVar pidv (processHandle p)
			hClose $ stderrHandle p
			let h = stdinHandle p
			let send b = do
				hPrint h b
				hFlush h
			send bytes
			forever $
				send =<< readSV v
		let feeder = writeSV v . fromBytesProcessed
		let cleanup = do
			void $ tryIO $ killThread tid
			tryNonAsync $
				maybe noop (void . waitForProcess)
					=<< tryTakeMVar pidv
		bracketIO noop (const cleanup) (const $ a feeder)

copyFromRemoteCheap :: Remote -> Key -> FilePath -> Annex Bool
#ifndef mingw32_HOST_OS
copyFromRemoteCheap r key file
	| not $ Git.repoIsUrl (repo r) = guardUsable (repo r) (return False) $ do
		loc <- liftIO $ gitAnnexLocation key (repo r) $
			fromJust $ remoteGitConfig $ gitconfig r
		liftIO $ catchBoolIO $ createSymbolicLink loc file >> return True
	| Git.repoIsSsh (repo r) =
		ifM (Annex.Content.preseedTmp key file)
			( copyFromRemote' r key Nothing file
			, return False
			)
	| otherwise = return False
#else
copyFromRemoteCheap _ _ _ = return False
#endif

{- Tries to copy a key's content to a remote's annex. -}
copyToRemote :: Remote -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
copyToRemote r key file p
	| not $ Git.repoIsUrl (repo r) =
		guardUsable (repo r) (return False) $ commitOnCleanup r $
			copylocal =<< Annex.Content.prepSendAnnex key
	| Git.repoIsSsh (repo r) = commitOnCleanup r $
		Annex.Content.sendAnnex key noop $ \object -> do
			direct <- isDirect
			Ssh.rsyncHelper (Just p)
				=<< Ssh.rsyncParamsRemote direct r Upload key object file
	| otherwise = error "copying to non-ssh repo not supported"
  where
	copylocal Nothing = return False
	copylocal (Just (object, checksuccess)) = do
		-- The checksuccess action is going to be run in
		-- the remote's Annex, but it needs access to the current
		-- Annex monad's state.
		checksuccessio <- Annex.withCurrentState checksuccess
		params <- Ssh.rsyncParams r Upload
		u <- getUUID
		-- run copy from perspective of remote
		onLocal r $ ifM (Annex.Content.inAnnex key)
			( return True
			, do
				ensureInitialized
				runTransfer (Transfer Download u key) file noRetry $ const $
					Annex.Content.saveState True `after`
						Annex.Content.getViaTmpChecked (liftIO checksuccessio) key
							(\d -> rsyncOrCopyFile params object d p)
			)

fsckOnRemote :: Git.Repo -> [CommandParam] -> Annex (IO Bool)
fsckOnRemote r params
	| Git.repoIsUrl r = do
		s <- Ssh.git_annex_shell r "fsck" params []
		return $ case s of
			Nothing -> return False
			Just (c, ps) -> batchCommand c ps
	| otherwise = return $ do
		program <- readProgramFile
		r' <- Git.Config.read r
		environ <- getEnvironment
		let environ' = addEntries 
			[ ("GIT_WORK_TREE", Git.repoPath r')
			, ("GIT_DIR", Git.localGitDir r')
			] environ
		batchCommandEnv program (Param "fsck" : params) $ Just environ'

{- The passed repair action is run in the Annex monad of the remote. -}
repairRemote :: Git.Repo -> Annex Bool -> Annex (IO Bool)
repairRemote r a = return $ do
	s <- Annex.new r
	Annex.eval s $ do
		Annex.BranchState.disableUpdate
		ensureInitialized
		a

{- Runs an action from the perspective of a local remote.
 -
 - The AnnexState is cached for speed and to avoid resource leaks.
 - However, catFileStop is called to avoid git-cat-file processes hanging
 - around on removable media.
 -
 - The repository's git-annex branch is not updated, as an optimisation.
 - No caller of onLocal can query data from the branch and be ensured
 - it gets a current value. Caller of onLocal can make changes to
 - the branch, however.
 -}
onLocal :: Remote -> Annex a -> Annex a
onLocal r a = do
	m <- Annex.getState Annex.remoteannexstate
	case M.lookup (uuid r) m of
		Nothing -> do
			st <- liftIO $ Annex.new (repo r)
			go st $ do
				Annex.BranchState.disableUpdate	
				a
		Just st -> go st a
  where
	cache st = Annex.changeState $ \s -> s
		{ Annex.remoteannexstate = M.insert (uuid r) st (Annex.remoteannexstate s) }
	go st a' = do
		(ret, st') <- liftIO $ Annex.run st $
			catFileStop `after` a'
		cache st'
		return ret

{- Copys a file with rsync unless both locations are on the same
 - filesystem. Then cp could be faster. -}
rsyncOrCopyFile :: [CommandParam] -> FilePath -> FilePath -> MeterUpdate -> Annex Bool
rsyncOrCopyFile rsyncparams src dest p =
#ifdef mingw32_HOST_OS
	dorsync
  where
#else
	ifM (sameDeviceIds src dest) (docopy, dorsync)
  where
	sameDeviceIds a b = (==) <$> getDeviceId a <*> getDeviceId b
	getDeviceId f = deviceID <$> liftIO (getFileStatus $ parentDir f)
	docopy = liftIO $ bracket
		(forkIO $ watchfilesize zeroBytesProcessed)
		(void . tryIO . killThread)
		(const $ copyFileExternal CopyTimeStamps src dest)
	watchfilesize oldsz = do
		threadDelay 500000 -- 0.5 seconds
		v <- catchMaybeIO $
			toBytesProcessed . fileSize
				<$> getFileStatus dest
		case v of
			Just sz
				| sz /= oldsz -> do
					p sz
					watchfilesize sz
			_ -> watchfilesize oldsz
#endif
	dorsync = Ssh.rsyncHelper (Just p) $
		rsyncparams ++ [File src, File dest]

commitOnCleanup :: Remote -> Annex a -> Annex a
commitOnCleanup r a = go `after` a
  where
	go = Annex.addCleanup (RemoteCleanup $ uuid r) cleanup
	cleanup
		| not $ Git.repoIsUrl (repo r) = onLocal r $
			doQuietSideAction $
				Annex.Branch.commit "update"
		| otherwise = void $ do
			Just (shellcmd, shellparams) <-
				Ssh.git_annex_shell (repo r) "commit" [] []
			
			-- Throw away stderr, since the remote may not
			-- have a new enough git-annex shell to
			-- support committing.
			liftIO $ catchMaybeIO $
				withQuietOutput createProcessSuccess $
					proc shellcmd $
						toCommand shellparams
