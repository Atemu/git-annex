{- Standard git remotes.
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Git (
	remote,
	configRead,
	repoAvail,
) where

import qualified Data.Map as M
import Control.Exception.Extensible

import Common.Annex
import Utility.CopyFile
import Utility.Rsync
import Remote.Helper.Ssh
import Types.Remote
import Types.GitConfig
import qualified Git
import qualified Git.Config
import qualified Git.Construct
import qualified Git.Command
import qualified Annex
import Logs.Presence
import Logs.Transfer
import Annex.UUID
import Annex.Exception
import qualified Annex.Content
import qualified Annex.BranchState
import qualified Annex.Branch
import qualified Utility.Url as Url
import Utility.TempFile
import Config
import Init
import Types.Key
import qualified Fields
import Logs.Location

import Control.Concurrent
import Control.Concurrent.MSampleVar
import System.Process (std_in, std_err)

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

repoCheap :: Git.Repo -> Bool
repoCheap = not . Git.repoIsUrl

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex Remote
gen r u _ gc = go <$> remoteCost gc defcst
  where
	defcst = if repoCheap r then cheapRemoteCost else expensiveRemoteCost
	go cst = new
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
			, localpath = if Git.repoIsLocal r || Git.repoIsLocalUnknown r
				then Just $ Git.repoPath r
				else Nothing
			, repo = r
			, gitconfig = gc
			, readonly = Git.repoIsHttp r
			, remotetype = remote
			}

{- Checks relatively inexpensively if a repository is available for use. -}
repoAvail :: Git.Repo -> Annex Bool
repoAvail r 
	| Git.repoIsHttp r = return True
	| Git.repoIsUrl r = return True
	| Git.repoIsLocalUnknown r = return False
	| otherwise = liftIO $ catchBoolIO $ onLocal r $ return True

{- Avoids performing an action on a local repository that's not usable.
 - Does not check that the repository is still available on disk. -}
guardUsable :: Git.Repo -> a -> Annex a -> Annex a
guardUsable r onerr a
	| Git.repoIsLocalUnknown r = return onerr
	| otherwise = a

{- Tries to read the config for a specified remote, updates state, and
 - returns the updated repo. -}
tryGitConfigRead :: Git.Repo -> Annex Git.Repo
tryGitConfigRead r 
	| not $ M.null $ Git.config r = return r -- already read
	| Git.repoIsSsh r = store $ do
		v <- onRemote r (pipedsshconfig, Left undefined) "configlist" [] []
		case (v, Git.remoteName r) of
			(Right r', _) -> return r'
			(Left _, Just n) -> do
				{- Is this remote just not available, or does
				 - it not have git-annex-shell?
				 - Find out by trying to fetch from the remote. -}
				whenM (inRepo $ Git.Command.runBool "fetch" [Param "--quiet", Param n]) $ do
					let k = "remote." ++ n ++ ".annex-ignore"
					warning $ "Remote " ++ n ++ " does not have git-annex installed; setting " ++ k
					inRepo $ Git.Command.run "config" [Param k, Param "true"]
				return r
			_ -> return r
	| Git.repoIsHttp r = do
		headers <- getHttpHeaders
		store $ safely $ geturlconfig headers
	| Git.repoIsUrl r = return r
	| otherwise = store $ safely $ onLocal r $ do 
		ensureInitialized
		Annex.getState Annex.repo
  where
	-- Reading config can fail due to IO error or
	-- for other reasons; catch all possible exceptions.
	safely a = either (const $ return r) return
			=<< liftIO (try a :: IO (Either SomeException Git.Repo))

	pipedconfig cmd params =
		withHandle StdoutHandle createProcessSuccess p $
			Git.Config.hRead r
	  where
		p = proc cmd $ toCommand params

	pipedsshconfig cmd params =
		liftIO (try (pipedconfig cmd params) :: IO (Either SomeException Git.Repo))

	geturlconfig headers = do
		s <- Url.get (Git.repoLocation r ++ "/config") headers
		withTempFile "git-annex.tmp" $ \tmpfile h -> do
			hPutStr h s
			hClose h
			safely $ pipedconfig "git" [Param "config", Param "--null", Param "--list", Param "--file", File tmpfile]

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
	checkhttp headers = liftIO $ go undefined $ keyUrls r key
	  where
		go e [] = return $ Left e
		go _ (u:us) = do
			res <- catchMsgIO $
				Url.check u headers (keySize key)
			case res of
				Left e -> go e us
				v -> return v
	checkremote = do
		showAction $ "checking " ++ Git.repoDescribe r
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

keyUrls :: Git.Repo -> Key -> [String]
keyUrls r key = map tourl (annexLocations key)
  where
	tourl l = Git.repoLocation r ++ "/" ++ l

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
copyFromRemote :: Remote -> Key -> AssociatedFile -> FilePath -> Annex Bool
copyFromRemote r key file dest
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
	 - Note that it actually waits for rsync to indicate
	 - progress before starting transferinfo, in order
	 - to ensure ssh connection caching works and reuses 
	 - the connection set up for the sendkey.
	 -
	 - Also note that older git-annex-shell does not support
	 - transferinfo, so stderr is dropped and failure ignored.
	 -}
	feedprogressback a = do
		u <- getUUID
		let fields = (Fields.remoteUUID, fromUUID u)
			: maybe [] (\f -> [(Fields.associatedFile, f)]) file
		Just (cmd, params) <- git_annex_shell (repo r) "transferinfo" 
			[Param $ key2file key] fields
		v <- liftIO $ newEmptySV
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
		let feeder = writeSV v
		bracketIO noop (const $ tryIO $ killThread tid) (a feeder)

copyFromRemoteCheap :: Remote -> Key -> FilePath -> Annex Bool
copyFromRemoteCheap r key file
	| not $ Git.repoIsUrl (repo r) = guardUsable (repo r) False $ do
		loc <- liftIO $ gitAnnexLocation key (repo r)
		liftIO $ catchBoolIO $ createSymbolicLink loc file >> return True
	| Git.repoIsSsh (repo r) =
		ifM (Annex.Content.preseedTmp key file)
			( copyFromRemote r key Nothing file
			, return False
			)
	| otherwise = return False

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
		let params = rsyncParams r
		u <- getUUID
		-- run copy from perspective of remote
		liftIO $ onLocal (repo r) $ ifM (Annex.Content.inAnnex key)
			( return False
			, do
				ensureInitialized
				download u key file noRetry $
					Annex.Content.saveState True `after`
						Annex.Content.getViaTmpChecked checksuccess key
							(\d -> rsyncOrCopyFile params object d p)
			)

rsyncHelper :: Maybe MeterUpdate -> [CommandParam] -> Annex Bool
rsyncHelper callback params = do
	showOutput -- make way for progress bar
	ifM (liftIO $ (maybe rsync rsyncProgress callback) params)
		( return True
		, do
			showLongNote "rsync failed -- run git annex again to resume file transfer"
			return False
		)

{- Copys a file with rsync unless both locations are on the same
 - filesystem. Then cp could be faster. -}
rsyncOrCopyFile :: [CommandParam] -> FilePath -> FilePath -> MeterUpdate -> Annex Bool
rsyncOrCopyFile rsyncparams src dest p =
	ifM (sameDeviceIds src dest) (docopy, dorsync)
  where
	sameDeviceIds a b = (==) <$> (getDeviceId a) <*> (getDeviceId b)
	getDeviceId f = deviceID <$> liftIO (getFileStatus $ parentDir f)
	dorsync = rsyncHelper (Just p) $
		rsyncparams ++ [Param src, Param dest]
	docopy = liftIO $ bracket
		(forkIO $ watchfilesize 0)
		(void . tryIO . killThread)
		(const $ copyFileExternal src dest)
	watchfilesize oldsz = do
		threadDelay 500000 -- 0.5 seconds
		v <- catchMaybeIO $
			fromIntegral . fileSize
				<$> getFileStatus dest
		case v of
			Just sz
				| sz /= oldsz -> do
					p sz
					watchfilesize sz
			_ -> watchfilesize oldsz

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
rsyncParams r = [Params "-p --progress --inplace"] ++
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
