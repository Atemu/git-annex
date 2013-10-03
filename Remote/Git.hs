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
import Utility.Rsync
import Remote.Helper.Ssh
import Annex.Ssh
import Types.Remote
import Types.GitConfig
import qualified Git
import qualified Git.Config
import qualified Git.Construct
import qualified Git.Command
import qualified Git.GCrypt
import qualified Annex
import Logs.Presence
import Logs.Transfer
import Annex.UUID
import Annex.Exception
import qualified Annex.Content
import qualified Annex.BranchState
import qualified Annex.Branch
import qualified Utility.Url as Url
import Utility.Tmp
import Config
import Config.Cost
import Init
import Types.Key
import qualified Fields
import Logs.Location
import Utility.Metered
#ifndef mingw32_HOST_OS
import Utility.CopyFile
#endif
import Remote.Helper.Git
import qualified Remote.GCrypt

import Control.Concurrent
import Control.Concurrent.MSampleVar
import System.Process (std_in, std_err)
import qualified Data.Map as M
import Control.Exception.Extensible

remote :: RemoteType
remote = RemoteType {
	typename = "git",
	enumerate = list,
	generate = gen,
	setup = error "not supported"
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

{- It's assumed to be cheap to read the config of non-URL remotes, so this is
 - done each time git-annex is run in a way that uses remotes.
 -
 - Conversely, the config of an URL remote is only read when there is no
 - cached UUID value. -}
configRead :: Git.Repo -> Annex Git.Repo
configRead r = do
	g <- fromRepo id
	let c = extractRemoteGitConfig g (Git.repoDescribe r)
	u <- getRepoUUID r
	case (repoCheap r, remoteAnnexIgnore c, u) of
		(_, True, _) -> return r
		(True, _, _) -> tryGitConfigRead r
		(False, _, NoUUID) -> tryGitConfigRead r
		_ -> return r

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc
	| Git.GCrypt.isEncrypted r = Remote.GCrypt.gen r u c gc
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
			, hasKey = inAnnex r
			, hasKeyCheap = repoCheap r
			, whereisKey = Nothing
			, config = M.empty
			, localpath = localpathCalc r
			, repo = r
			, gitconfig = gc
				{ remoteGitConfig = Just $ extractGitConfig r }
			, readonly = Git.repoIsHttp r
			, globallyAvailable = globallyAvailableCalc r
			, remotetype = remote
			}

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
	| otherwise = liftIO $ catchBoolIO $ onLocal r $ return True

{- Tries to read the config for a specified remote, updates state, and
 - returns the updated repo. -}
tryGitConfigRead :: Git.Repo -> Annex Git.Repo
tryGitConfigRead r 
	| haveconfig r = return r -- already read
	| Git.repoIsSsh r = store $ do
		v <- onRemote r (pipedconfig, Left undefined) "configlist" [] []
		case v of
			Right r'
				| haveconfig r' -> return r'
				| otherwise -> configlist_failed
			Left _ -> configlist_failed
	| Git.repoIsHttp r = do
		headers <- getHttpHeaders
		store $ geturlconfig headers
	| Git.GCrypt.isEncrypted r = handlegcrypt =<< getConfigMaybe (remoteConfig r "uuid")
	| Git.repoIsUrl r = return r
	| otherwise = store $ safely $ onLocal r $ do 
		ensureInitialized
		Annex.getState Annex.repo
  where
	haveconfig = not . M.null . Git.config

	-- Reading config can fail due to IO error or
	-- for other reasons; catch all possible exceptions.
	safely a = either (const $ return r) return
			=<< liftIO (try a :: IO (Either SomeException Git.Repo))

	pipedconfig cmd params = try run :: IO (Either SomeException Git.Repo)
	  where
	  	run = withHandle StdoutHandle createProcessSuccess p $ \h -> do
 			fileEncoding h
			val <- hGetContentsStrict h
			r' <- Git.Config.store val r
			when (getUncachedUUID r' == NoUUID && not (null val)) $ do
				warningIO $ "Failed to get annex.uuid configuration of repository " ++ Git.repoDescribe r
				warningIO $ "Instead, got: " ++ show val
				warningIO $ "This is unexpected; please check the network transport!"
			return r'
		p = proc cmd $ toCommand params

	geturlconfig headers = do
		v <- liftIO $ withTmpFile "git-annex.tmp" $ \tmpfile h -> do
			hClose h
			ifM (Url.downloadQuiet (Git.repoLocation r ++ "/config") headers [] tmpfile)
				( pipedconfig "git" [Param "config", Param "--null", Param "--list", Param "--file", File tmpfile]
				, return $ Left undefined
				)
		case v of
			Left _ -> do
				set_ignore "not usable by git-annex"
				return r
			Right r' -> return r'

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
			whenM (inRepo $ Git.Command.runBool [Param "fetch", Param "--quiet", Param n]) $
				set_ignore $ "does not have git-annex installed"
			return r
	
	set_ignore msg = case Git.remoteName r of
		Nothing -> noop
		Just n -> do
			let k = "remote." ++ n ++ ".annex-ignore"
			warning $ "Remote " ++ n ++ " " ++ msg ++ "; setting " ++ k
			inRepo $ Git.Command.run [Param "config", Param k, Param "true"]
		
	handlegcrypt Nothing = return r
	handlegcrypt (Just _cacheduuid) = do
		-- Generate UUID from the gcrypt-id
		g <- gitRepo
		case Git.GCrypt.remoteRepoId g (Git.remoteName r) of
			Nothing -> return r
			Just v -> store $ liftIO $ setUUID r $
				genUUIDInNameSpace gCryptNameSpace v

{- Checks if a given remote has the content for a key inAnnex.
 - If the remote cannot be accessed, or if it cannot determine
 - whether it has the content, returns a Left error message.
 -}
inAnnex :: Git.Repo -> Key -> Annex (Either String Bool)
inAnnex r key
	| Git.repoIsHttp r = checkhttp =<< getHttpHeaders
	| Git.repoIsUrl r = checkremote
	| otherwise = checklocal
  where
	checkhttp headers = do
		showchecking
		liftIO $ ifM (anyM (\u -> Url.check u headers (keySize key)) (keyUrls r key))
			( return $ Right True
			, return $ Left "not found"
			)
	checkremote = do
		showchecking
		onRemote r (check, unknown) "inannex" [Param (key2file key)] []
	  where
		check c p = dispatch <$> safeSystem c p
		dispatch ExitSuccess = Right True
		dispatch (ExitFailure 1) = Right False
		dispatch _ = unknown
	checklocal = guardUsable r unknown $ dispatch <$> check
	  where
		check = liftIO $ catchMsgIO $ onLocal r $
			Annex.Content.inAnnexSafe key
		dispatch (Left e) = Left e
		dispatch (Right (Just b)) = Right b
		dispatch (Right Nothing) = unknown
	unknown = Left $ "unable to check " ++ Git.repoDescribe r
	showchecking = showAction $ "checking " ++ Git.repoDescribe r

keyUrls :: Git.Repo -> Key -> [String]
keyUrls r key = map tourl locs
  where
	tourl l = Git.repoLocation r ++ "/" ++ l
#ifndef mingw32_HOST_OS
	locs = annexLocations key
#else
	locs = map (replace "\\" "/") (annexLocations key)
#endif

dropKey :: Remote -> Key -> Annex Bool
dropKey r key
	| not $ Git.repoIsUrl (repo r) =
		guardUsable (repo r) False $ commitOnCleanup r $ liftIO $ onLocal (repo r) $ do
			ensureInitialized
			whenM (Annex.Content.inAnnex key) $ do
				Annex.Content.lockContent key $
					Annex.Content.removeAnnex key
				logStatus key InfoMissing
				Annex.Content.saveState True
			return True
	| Git.repoIsHttp (repo r) = error "dropping from http repo not supported"
	| otherwise = commitOnCleanup r $ onRemote (repo r) (boolSystem, False) "dropkey"
		[ Params "--quiet --force"
		, Param $ key2file key
		]
		[]

{- Tries to copy a key's content from a remote's annex to a file. -}
copyFromRemote :: Remote -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
copyFromRemote r key file dest _p = copyFromRemote' r key file dest
copyFromRemote' :: Remote -> Key -> AssociatedFile -> FilePath -> Annex Bool
copyFromRemote' r key file dest
	| not $ Git.repoIsUrl (repo r) = guardUsable (repo r) False $ do
		let params = rsyncParams r
		u <- getUUID
		-- run copy from perspective of remote
		liftIO $ onLocal (repo r) $ do
			ensureInitialized
			v <- Annex.Content.prepSendAnnex key
			case v of
				Nothing -> return False
				Just (object, checksuccess) ->
					upload u key file noRetry
						(rsyncOrCopyFile params object dest)
						<&&> checksuccess
	| Git.repoIsSsh (repo r) = feedprogressback $ \feeder -> 
		rsyncHelper (Just feeder) 
			=<< rsyncParamsRemote r Download key dest file
	| Git.repoIsHttp (repo r) = Annex.Content.downloadUrl (keyUrls (repo r) key) dest
	| otherwise = error "copying from non-ssh, non-http repo not supported"
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
		Just (cmd, params) <- git_annex_shell (repo r) "transferinfo" 
			[Param $ key2file key] fields
		v <- liftIO $ (newEmptySV :: IO (MSampleVar Integer))
		tid <- liftIO $ forkIO $ void $ tryIO $ do
			bytes <- readSV v
			p <- createProcess $
				 (proc cmd (toCommand params))
					{ std_in = CreatePipe
					, std_err = CreatePipe
					}
			hClose $ stderrHandle p
			let h = stdinHandle p
			let send b = do
				hPutStrLn h $ show b
				hFlush h
			send bytes
			forever $
				send =<< readSV v
		let feeder = writeSV v . fromBytesProcessed
		bracketIO noop (const $ tryIO $ killThread tid) (const $ a feeder)

copyFromRemoteCheap :: Remote -> Key -> FilePath -> Annex Bool
#ifndef mingw32_HOST_OS
copyFromRemoteCheap r key file
	| not $ Git.repoIsUrl (repo r) = guardUsable (repo r) False $ do
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
		guardUsable (repo r) False $ commitOnCleanup r $
			copylocal =<< Annex.Content.prepSendAnnex key
	| Git.repoIsSsh (repo r) = commitOnCleanup r $
		Annex.Content.sendAnnex key noop $ \object ->
			rsyncHelper (Just p) =<< rsyncParamsRemote r Upload key object file
	| otherwise = error "copying to non-ssh repo not supported"
  where
	copylocal Nothing = return False
	copylocal (Just (object, checksuccess)) = do
		-- The checksuccess action is going to be run in
		-- the remote's Annex, but it needs access to the current
		-- Annex monad's state.
		checksuccessio <- Annex.withCurrentState checksuccess
		let params = rsyncParams r
		u <- getUUID
		-- run copy from perspective of remote
		liftIO $ onLocal (repo r) $ ifM (Annex.Content.inAnnex key)
			( return True
			, do
				ensureInitialized
				download u key file noRetry $ const $
					Annex.Content.saveState True `after`
						Annex.Content.getViaTmpChecked (liftIO checksuccessio) key
							(\d -> rsyncOrCopyFile params object d p)
			)

{- Runs an action on a local repository inexpensively, by making an annex
 - monad using that repository. -}
onLocal :: Git.Repo -> Annex a -> IO a
onLocal r a = do
	s <- Annex.new r
	Annex.eval s $ do
		-- No need to update the branch; its data is not used
		-- for anything onLocal is used to do.
		Annex.BranchState.disableUpdate
		a

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
	sameDeviceIds a b = (==) <$> (getDeviceId a) <*> (getDeviceId b)
	getDeviceId f = deviceID <$> liftIO (getFileStatus $ parentDir f)
	docopy = liftIO $ bracket
		(forkIO $ watchfilesize zeroBytesProcessed)
		(void . tryIO . killThread)
		(const $ copyFileExternal src dest)
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
	dorsync = rsyncHelper (Just p) $
		rsyncparams ++ [File src, File dest]

rsyncHelper :: Maybe MeterUpdate -> [CommandParam] -> Annex Bool
rsyncHelper callback params = do
	showOutput -- make way for progress bar
	ifM (liftIO $ (maybe rsync rsyncProgress callback) params)
		( return True
		, do
			showLongNote "rsync failed -- run git annex again to resume file transfer"
			return False
		)

{- Generates rsync parameters that ssh to the remote and asks it
 - to either receive or send the key's content. -}
rsyncParamsRemote :: Remote -> Direction -> Key -> FilePath -> AssociatedFile -> Annex [CommandParam]
rsyncParamsRemote r direction key file afile = do
	u <- getUUID
	direct <- isDirect
	let fields = (Fields.remoteUUID, fromUUID u)
		: (Fields.direct, if direct then "1" else "")
		: maybe [] (\f -> [(Fields.associatedFile, f)]) afile
	Just (shellcmd, shellparams) <- git_annex_shell (repo r)
		(if direction == Download then "sendkey" else "recvkey")
		[ Param $ key2file key ]
		fields
	-- Convert the ssh command into rsync command line.
	let eparam = rsyncShell (Param shellcmd:shellparams)
	let o = rsyncParams r
	if direction == Download
		then return $ o ++ rsyncopts eparam dummy (File file)
		else return $ o ++ rsyncopts eparam (File file) dummy
  where
	rsyncopts ps source dest
		| end ps == [dashdash] = ps ++ [source, dest]
		| otherwise = ps ++ [dashdash, source, dest]
	dashdash = Param "--"
	{- The rsync shell parameter controls where rsync
	 - goes, so the source/dest parameter can be a dummy value,
	 - that just enables remote rsync mode.
	 - For maximum compatability with some patched rsyncs,
	 - the dummy value needs to still contain a hostname,
	 - even though this hostname will never be used. -}
	dummy = Param "dummy:"

-- --inplace to resume partial files
rsyncParams :: Remote -> [CommandParam]
rsyncParams r = [Params "--progress --inplace"] ++
	map Param (remoteAnnexRsyncOptions $ gitconfig r)

commitOnCleanup :: Remote -> Annex a -> Annex a
commitOnCleanup r a = go `after` a
  where
	go = Annex.addCleanup (Git.repoLocation $ repo r) cleanup
	cleanup
		| not $ Git.repoIsUrl (repo r) = liftIO $ onLocal (repo r) $
			doQuietSideAction $
				Annex.Branch.commit "update"
		| otherwise = void $ do
			Just (shellcmd, shellparams) <-
				git_annex_shell (repo r) "commit" [] []
			
			-- Throw away stderr, since the remote may not
			-- have a new enough git-annex shell to
			-- support committing.
			liftIO $ catchMaybeIO $ do
				withQuietOutput createProcessSuccess $
					proc shellcmd $
						toCommand shellparams
