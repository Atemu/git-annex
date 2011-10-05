{- Standard git remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Git (remote) where

import Control.Exception.Extensible
import qualified Data.Map as M

import Common.Annex
import Utility.CopyFile
import Utility.RsyncFile
import Utility.Ssh
import Types.Remote
import qualified Git
import qualified Annex
import UUID
import qualified Annex.Content
import qualified Utility.Url as Url
import Config
import Init

remote :: RemoteType Annex
remote = RemoteType {
	typename = "git",
	enumerate = list,
	generate = gen,
	setup = error "not supported"
}

list :: Annex [Git.Repo]
list = do
	g <- gitRepo
	return $ Git.remotes g

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex (Remote Annex)
gen r u _ = do
 	{- It's assumed to be cheap to read the config of non-URL remotes,
	 - so this is done each time git-annex is run. Conversely,
	 - the config of an URL remote is only read when there is no
	 - cached UUID value. -}
	let cheap = not $ Git.repoIsUrl r
	r' <- case (cheap, u) of
		(True, _) -> tryGitConfigRead r
		(False, "") -> tryGitConfigRead r
		_ -> return r

	u' <- getUUID r'

	let defcst = if cheap then cheapRemoteCost else expensiveRemoteCost
	cst <- remoteCost r' defcst

	return Remote {
		uuid = u',
		cost = cst,
		name = Git.repoDescribe r',
		storeKey = copyToRemote r',
		retrieveKeyFile = copyFromRemote r',
		removeKey = dropKey r',
		hasKey = inAnnex r',
		hasKeyCheap = cheap,
		config = Nothing,
		repo = r'
	}

{- Tries to read the config for a specified remote, updates state, and
 - returns the updated repo. -}
tryGitConfigRead :: Git.Repo -> Annex Git.Repo
tryGitConfigRead r 
	| not $ M.null $ Git.configMap r = return r -- already read
	| Git.repoIsSsh r = store $ onRemote r (pipedconfig, r) "configlist" []
	| Git.repoIsHttp r = store $ safely geturlconfig
	| Git.repoIsUrl r = return r
	| otherwise = store $ safely $ do
		onLocal r ensureInitialized
		Git.configRead r
	where
		-- Reading config can fail due to IO error or
		-- for other reasons; catch all possible exceptions.
		safely a = do
			result <- liftIO (try a :: IO (Either SomeException Git.Repo))
			case result of
				Left _ -> return r
				Right r' -> return r'

		pipedconfig cmd params = safely $
			pOpen ReadFromPipe cmd (toCommand params) $
				Git.hConfigRead r

		geturlconfig = do
			s <- Url.get (Git.repoLocation r ++ "/config")
			withTempFile "git-annex.tmp" $ \tmpfile h -> do
				hPutStr h s
				hClose h
				pOpen ReadFromPipe "git" ["config", "--list", "--file", tmpfile] $
					Git.hConfigRead r

		store a = do
			r' <- a
			g <- gitRepo
			let l = Git.remotes g
			let g' = Git.remotesAdd g $ exchange l r'
			Annex.changeState $ \s -> s { Annex.repo = g' }
			return r'

		exchange [] _ = []
		exchange (old:ls) new =
			if Git.repoRemoteName old == Git.repoRemoteName new
				then new : exchange ls new
				else old : exchange ls new

{- Checks if a given remote has the content for a key inAnnex.
 - If the remote cannot be accessed, returns a Left error.
 -}
inAnnex :: Git.Repo -> Key -> Annex (Either IOException Bool)
inAnnex r key
	| Git.repoIsHttp r = safely checkhttp
	| Git.repoIsUrl r = checkremote
	| otherwise = safely checklocal
	where
		checklocal = onLocal r (Annex.Content.inAnnex key)
		checkremote = do
			showAction $ "checking " ++ Git.repoDescribe r
			inannex <- onRemote r (boolSystem, False) "inannex" 
				[Param (show key)]
			return $ Right inannex
		checkhttp = Url.exists $ keyUrl r key
		safely a = liftIO (try a ::IO (Either IOException Bool))

{- Runs an action on a local repository inexpensively, by making an annex
 - monad using that repository. -}
onLocal :: Git.Repo -> Annex a -> IO a
onLocal r a = do
	annex <- Annex.new r
	Annex.eval annex a

keyUrl :: Git.Repo -> Key -> String
keyUrl r key = Git.repoLocation r ++ "/" ++ annexLocation key

dropKey :: Git.Repo -> Key -> Annex Bool
dropKey r key
	| Git.repoIsHttp r = error "dropping from http repo not supported"
	| otherwise = onRemote r (boolSystem, False) "dropkey"
		[ Params "--quiet --force"
		, Param $ show key
		]

{- Tries to copy a key's content from a remote's annex to a file. -}
copyFromRemote :: Git.Repo -> Key -> FilePath -> Annex Bool
copyFromRemote r key file
	| not $ Git.repoIsUrl r = rsyncOrCopyFile r (gitAnnexLocation r key) file
	| Git.repoIsSsh r = rsyncHelper =<< rsyncParamsRemote r True key file
	| Git.repoIsHttp r = Url.download (keyUrl r key) file
	| otherwise = error "copying from non-ssh, non-http repo not supported"

{- Tries to copy a key's content to a remote's annex. -}
copyToRemote :: Git.Repo -> Key -> Annex Bool
copyToRemote r key
	| not $ Git.repoIsUrl r = do
		g <- gitRepo
		let keysrc = gitAnnexLocation g key
		-- run copy from perspective of remote
		liftIO $ onLocal r $ do
			ok <- Annex.Content.getViaTmp key $
				rsyncOrCopyFile r keysrc
			Annex.Content.saveState
			return ok
	| Git.repoIsSsh r = do
		g <- gitRepo
		let keysrc = gitAnnexLocation g key
		rsyncHelper =<< rsyncParamsRemote r False key keysrc
	| otherwise = error "copying to non-ssh repo not supported"

rsyncHelper :: [CommandParam] -> Annex Bool
rsyncHelper p = do
	showOutput -- make way for progress bar
	res <- liftIO $ rsync p
	if res
		then return res
		else do
			showLongNote "rsync failed -- run git annex again to resume file transfer"
			return res

{- Copys a file with rsync unless both locations are on the same
 - filesystem. Then cp could be faster. -}
rsyncOrCopyFile :: Git.Repo -> FilePath -> FilePath -> Annex Bool
rsyncOrCopyFile r src dest = do
	ss <- liftIO $ getFileStatus $ parentDir src
	ds <- liftIO $ getFileStatus $ parentDir dest
	if deviceID ss == deviceID ds
		then liftIO $ copyFileExternal src dest
		else do
			params <- rsyncParams r
			rsyncHelper $ params ++ [Param src, Param dest]

{- Generates rsync parameters that ssh to the remote and asks it
 - to either receive or send the key's content. -}
rsyncParamsRemote :: Git.Repo -> Bool -> Key -> FilePath -> Annex [CommandParam]
rsyncParamsRemote r sending key file = do
	Just (shellcmd, shellparams) <- git_annex_shell r
		(if sending then "sendkey" else "recvkey")
		[ Param $ show key
		-- Command is terminated with "--", because
		-- rsync will tack on its own options afterwards,
		-- and they need to be ignored.
		, Param "--"
		]
	-- Convert the ssh command into rsync command line.
	let eparam = rsyncShell (Param shellcmd:shellparams)
	o <- rsyncParams r
	if sending
		then return $ o ++ eparam ++ [dummy, File file]
		else return $ o ++ eparam ++ [File file, dummy]
	where
		-- the rsync shell parameter controls where rsync
		-- goes, so the source/dest parameter can be a dummy value,
		-- that just enables remote rsync mode.
		dummy = Param ":"

rsyncParams :: Git.Repo -> Annex [CommandParam]
rsyncParams r = do
	o <- getConfig r "rsync-options" ""
	return $ options ++ map Param (words o)
	where
 		-- --inplace to resume partial files
		options = [Params "-p --progress --inplace"]
