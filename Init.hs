{- git-annex repository initialization
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Init (
	ensureInitialized,
	isInitialized,
	initialize,
	uninitialize,
	probeCrippledFileSystem
) where

import Common.Annex
import Utility.Tmp
import Utility.Network
import qualified Annex
import qualified Git
import qualified Git.LsFiles
import qualified Git.Config
import qualified Annex.Branch
import Logs.UUID
import Annex.Version
import Annex.UUID
import Utility.UserInfo
import Utility.Shell
import Utility.FileMode
import Config
import Annex.Direct
import Annex.Content.Direct
import Backend

genDescription :: Maybe String -> Annex String
genDescription (Just d) = return d
genDescription Nothing = do
	reldir <- liftIO . relHome =<< fromRepo Git.repoPath
	hostname <- fromMaybe "" <$> liftIO getHostname
#ifndef __WINDOWS__
	let at = if null hostname then "" else "@"
	username <- liftIO myUserName
	return $ concat [username, at, hostname, ":", reldir]
#else
	return $ concat [hostname, ":", reldir]
#endif

initialize :: Maybe String -> Annex ()
initialize mdescription = do
	prepUUID
	setVersion defaultVersion
	checkCrippledFileSystem
	checkFifoSupport
	Annex.Branch.create
	gitPreCommitHookWrite
	createInodeSentinalFile
	u <- getUUID
	describeUUID u =<< genDescription mdescription

uninitialize :: Annex ()
uninitialize = do
	gitPreCommitHookUnWrite
	removeRepoUUID
	removeVersion

{- Will automatically initialize if there is already a git-annex
 - branch from somewhere. Otherwise, require a manual init
 - to avoid git-annex accidentially being run in git
 - repos that did not intend to use it. -}
ensureInitialized :: Annex ()
ensureInitialized = getVersion >>= maybe needsinit checkVersion
  where
	needsinit = ifM Annex.Branch.hasSibling
			( initialize Nothing
			, error "First run: git-annex init"
			)

{- Checks if a repository is initialized. Does not check version for ugrade. -}
isInitialized :: Annex Bool
isInitialized = maybe Annex.Branch.hasSibling (const $ return True) =<< getVersion

{- set up a git pre-commit hook, if one is not already present -}
gitPreCommitHookWrite :: Annex ()
gitPreCommitHookWrite = unlessBare $ do
	hook <- preCommitHook
	ifM (liftIO $ doesFileExist hook)
		( do
			content <- liftIO $ readFile hook
			when (content /= preCommitScript) $
				warning $ "pre-commit hook (" ++ hook ++ ") already exists, not configuring"
		, unlessM crippledFileSystem $
			liftIO $ do
				viaTmp writeFile hook preCommitScript
				p <- getPermissions hook
				setPermissions hook $ p {executable = True}
		)

gitPreCommitHookUnWrite :: Annex ()
gitPreCommitHookUnWrite = unlessBare $ do
	hook <- preCommitHook
	whenM (liftIO $ doesFileExist hook) $
		ifM (liftIO $ (==) preCommitScript <$> readFile hook)
			( liftIO $ removeFile hook
			, warning $ "pre-commit hook (" ++ hook ++ 
				") contents modified; not deleting." ++
				" Edit it to remove call to git annex."
			)

unlessBare :: Annex () -> Annex ()
unlessBare = unlessM $ fromRepo Git.repoIsLocalBare

preCommitHook :: Annex FilePath
preCommitHook = (</>) <$> fromRepo Git.localGitDir <*> pure "hooks/pre-commit"

preCommitScript :: String
preCommitScript = unlines
	[ shebang_local
	, "# automatically configured by git-annex"
	, "git annex pre-commit ."
	]

{- A crippled filesystem is one that does not allow making symlinks,
 - or removing write access from files. -}
probeCrippledFileSystem :: Annex Bool
probeCrippledFileSystem = do
#ifdef __WINDOWS__
	return True
#else
	tmp <- fromRepo gitAnnexTmpDir
	let f = tmp </> "gaprobe"
	liftIO $ do
		createDirectoryIfMissing True tmp
		writeFile f ""
	uncrippled <- liftIO $ probe f
	liftIO $ removeFile f
	return $ not uncrippled
  where
	probe f = catchBoolIO $ do
		let f2 = f ++ "2"
		nukeFile f2
		createSymbolicLink f f2
		nukeFile f2
		preventWrite f
		allowWrite f
		return True
#endif

checkCrippledFileSystem :: Annex ()
checkCrippledFileSystem = whenM probeCrippledFileSystem $ do
	warning "Detected a crippled filesystem."
	setCrippledFileSystem True

	{- Normally git disables core.symlinks itself when the filesystem does
 	 - not support them, but in Cygwin, git does support symlinks, while
 	 - git-annex, not linking with Cygwin, does not. -}
	whenM (coreSymlinks <$> Annex.getGitConfig) $ do
		warning "Disabling core.symlinks."
		setConfig (ConfigKey "core.symlinks")
			(Git.Config.boolConfig False)

	unlessBare $ do
		unlessM isDirect $ do
			warning "Enabling direct mode."
			top <- fromRepo Git.repoPath
			(l, clean) <- inRepo $ Git.LsFiles.inRepo [top]
			forM_ l $ \f ->
				maybe noop (`toDirect` f) =<< isAnnexLink f
			void $ liftIO clean
			setDirect True
		setVersion directModeVersion

probeFifoSupport :: Annex Bool
probeFifoSupport = do
#ifdef __WINDOWS__
	return False
#else
	tmp <- fromRepo gitAnnexTmpDir
	let f = tmp </> "gaprobe"
	liftIO $ do
		createDirectoryIfMissing True tmp
		nukeFile f
		ms <- tryIO $ do
			createNamedPipe f ownerReadMode
			getFileStatus f
		nukeFile f
		return $ either (const False) isNamedPipe ms
#endif

checkFifoSupport :: Annex ()
checkFifoSupport = unlessM probeFifoSupport $ do
	warning "Detected a filesystem without fifo support."
	warning "Disabling ssh connection caching."
	setConfig (annexConfig "sshcaching") (Git.Config.boolConfig False)
