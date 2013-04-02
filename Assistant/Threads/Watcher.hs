{- git-annex assistant tree watcher
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE DeriveDataTypeable, CPP #-}

module Assistant.Threads.Watcher (
	watchThread,
	WatcherException(..),
	checkCanWatch,
	needLsof,
	onAddSymlink,
	runHandler,
) where

import Assistant.Common
import Assistant.DaemonStatus
import Assistant.Changes
import Assistant.Types.Changes
import Assistant.Alert
import Utility.DirWatcher
import Utility.DirWatcher.Types
import Utility.Lsof
import qualified Annex
import qualified Annex.Queue
import qualified Git
import qualified Git.UpdateIndex
import qualified Git.LsFiles as LsFiles
import qualified Backend
import Annex.Content
import Annex.Direct
import Annex.Content.Direct
import Annex.CatFile
import Annex.Link
import Annex.FileMatcher
import Git.Types
import Config
import Utility.ThreadScheduler

import Data.Bits.Utils
import Data.Typeable
import qualified Data.ByteString.Lazy as L
import qualified Control.Exception as E
import Data.Time.Clock

checkCanWatch :: Annex ()
checkCanWatch
	| canWatch = do
		liftIO setupLsof
		unlessM (liftIO (inPath "lsof") <||> Annex.getState Annex.force)
			needLsof
	| otherwise = error "watch mode is not available on this system"

needLsof :: Annex ()
needLsof = error $ unlines
	[ "The lsof command is needed for watch mode to be safe, and is not in PATH."
	, "To override lsof checks to ensure that files are not open for writing"
	, "when added to the annex, you can use --force"
	, "Be warned: This can corrupt data in the annex, and make fsck complain."
	]

{- A special exception that can be thrown to pause or resume the watcher. -}
data WatcherException = PauseWatcher | ResumeWatcher
        deriving (Show, Eq, Typeable)

instance E.Exception WatcherException

watchThread :: NamedThread
watchThread = namedThread "Watcher" $
	ifM (liftAnnex $ annexAutoCommit <$> Annex.getGitConfig)
		( runWatcher
		, waitFor ResumeWatcher runWatcher
		)

runWatcher :: Assistant ()
runWatcher = do
	startup <- asIO1 startupScan
	matcher <- liftAnnex $ largeFilesMatcher
	direct <- liftAnnex isDirect
	addhook <- hook $ if direct then onAddDirect matcher else onAdd matcher
	delhook <- hook onDel
	addsymlinkhook <- hook $ onAddSymlink direct
	deldirhook <- hook onDelDir
	errhook <- hook onErr
	let hooks = mkWatchHooks
		{ addHook = addhook
		, delHook = delhook
		, addSymlinkHook = addsymlinkhook
		, delDirHook = deldirhook
		, errHook = errhook
		}
	handle <- liftIO $ watchDir "." ignored hooks startup
	debug [ "watching", "."]
	
	{- Let the DirWatcher thread run until signalled to pause it,
	 - then wait for a resume signal, and restart. -}
	waitFor PauseWatcher $ do
		liftIO $ stopWatchDir handle
		waitFor ResumeWatcher runWatcher
  where
	hook a = Just <$> asIO2 (runHandler a)

waitFor :: WatcherException -> Assistant () -> Assistant ()
waitFor sig next = do
	r <- liftIO $ (E.try pause :: IO (Either E.SomeException ()))
	case r of
		Left e -> case E.fromException e of
			Just s
				| s == sig -> next
			_ -> noop
		_ -> noop
  where
	pause = runEvery (Seconds 86400) noop

{- Initial scartup scan. The action should return once the scan is complete. -}
startupScan :: IO a -> Assistant a
startupScan scanner = do
	liftAnnex $ showAction "scanning"
	alertWhile' startupScanAlert $ do
		r <- liftIO $ scanner

		-- Notice any files that were deleted before
		-- watching was started.
		top <- liftAnnex $ fromRepo Git.repoPath
		(fs, cleanup) <- liftAnnex $ inRepo $ LsFiles.deleted [top]
		forM_ fs $ \f -> do
			liftAnnex $ Annex.Queue.addUpdateIndex =<<
				inRepo (Git.UpdateIndex.unstageFile f)
			maybe noop recordChange =<< madeChange f RmChange
		void $ liftIO $ cleanup
		
		liftAnnex $ showAction "started"
		liftIO $ putStrLn ""
		
		modifyDaemonStatus_ $ \s -> s { scanComplete = True }

		return (True, r)

ignored :: FilePath -> Bool
ignored = ig . takeFileName
  where
	ig ".git" = True
	ig ".gitignore" = True
	ig ".gitattributes" = True
#ifdef darwin_HOST_OS
	ig ".DS_Store" = True
#endif
	ig _ = False

type Handler = FilePath -> Maybe FileStatus -> Assistant (Maybe Change)

{- Runs an action handler, and if there was a change, adds it to the ChangeChan.
 -
 - Exceptions are ignored, otherwise a whole watcher thread could be crashed.
 -}
runHandler :: Handler -> FilePath -> Maybe FileStatus -> Assistant ()
runHandler handler file filestatus = void $ do
	r <- tryIO <~> handler file filestatus
	case r of
		Left e -> liftIO $ print e
		Right Nothing -> noop
		Right (Just change) -> do
			-- Just in case the commit thread is not
			-- flushing the queue fast enough.
			liftAnnex $ Annex.Queue.flushWhenFull
			recordChange change

{- Small files are added to git as-is, while large ones go into the annex. -}
add :: FileMatcher -> FilePath -> Assistant (Maybe Change)
add bigfilematcher file = ifM (liftAnnex $ checkFileMatcher bigfilematcher file)
	( pendingAddChange file
	, do
		liftAnnex $ Annex.Queue.addCommand "add"
			[Params "--force --"] [file]
		madeChange file AddFileChange
	)

onAdd :: FileMatcher -> Handler
onAdd matcher file filestatus
	| maybe False isRegularFile filestatus = add matcher file
	| otherwise = noChange

{- In direct mode, add events are received for both new files, and
 - modified existing files. -}
onAddDirect :: FileMatcher -> Handler
onAddDirect matcher file fs = do
	v <- liftAnnex $ catKeyFile file
	case (v, fs) of
		(Just key, Just filestatus) ->
			ifM (liftAnnex $ sameFileStatus key filestatus)
				{- It's possible to get an add event for
				 - an existing file that is not
				 - really modified, but it might have
				 - just been deleted and been put back,
				 - so it symlink is restaged to make sure. -}
				( do
					link <- liftAnnex $ calcGitLink file key
					addLink file link (Just key)
				, do
					debug ["changed direct", file]
					liftAnnex $ changedDirect key file
					add matcher file
				)
		_ -> do
			debug ["add direct", file]
			add matcher file

{- A symlink might be an arbitrary symlink, which is just added.
 - Or, if it is a git-annex symlink, ensure it points to the content
 - before adding it.
 -}
onAddSymlink :: Bool -> Handler
onAddSymlink isdirect file filestatus = go =<< liftAnnex (Backend.lookupFile file)
  where
	go (Just (key, _)) = do
		when isdirect $
			liftAnnex $ void $ addAssociatedFile key file
		link <- liftAnnex $ calcGitLink file key
		ifM ((==) (Just link) <$> liftIO (catchMaybeIO $ readSymbolicLink file))
			( ensurestaged (Just link) (Just key) =<< getDaemonStatus
			, do
				unless isdirect $
					liftAnnex $ replaceFile file $
						makeAnnexLink link
				addLink file link (Just key)
			)
	go Nothing = do -- other symlink
		mlink <- liftIO (catchMaybeIO $ readSymbolicLink file)
		ensurestaged mlink Nothing =<< getDaemonStatus

	{- This is often called on symlinks that are already
	 - staged correctly. A symlink may have been deleted
	 - and being re-added, or added when the watcher was
	 - not running. So they're normally restaged to make sure.
	 -
	 - As an optimisation, during the startup scan, avoid
	 - restaging everything. Only links that were created since
	 - the last time the daemon was running are staged.
	 - (If the daemon has never ran before, avoid staging
	 - links too.)
	 -}
	ensurestaged (Just link) mk daemonstatus
		| scanComplete daemonstatus = addLink file link mk
		| otherwise = case filestatus of
			Just s
				| not (afterLastDaemonRun (statusChangeTime s) daemonstatus) -> noChange
			_ -> addLink file link mk
	ensurestaged Nothing _ _ = noChange

{- For speed, tries to reuse the existing blob for symlink target. -}
addLink :: FilePath -> FilePath -> Maybe Key -> Assistant (Maybe Change)
addLink file link mk = do
	debug ["add symlink", file]
	liftAnnex $ do
		v <- catObjectDetails $ Ref $ ':':file
		case v of
			Just (currlink, sha)
				| s2w8 link == L.unpack currlink ->
					stageSymlink file sha
			_ -> stageSymlink file =<< hashSymlink link
	madeChange file $ LinkChange mk

onDel :: Handler
onDel file _ = do
	debug ["file deleted", file]
	liftAnnex $ 
		Annex.Queue.addUpdateIndex =<<
			inRepo (Git.UpdateIndex.unstageFile file)
	madeChange file RmChange

{- A directory has been deleted, or moved, so tell git to remove anything
 - that was inside it from its cache. Since it could reappear at any time,
 - use --cached to only delete it from the index.
 -
 - This queues up a lot of RmChanges, which assists the Committer in
 - pairing up renamed files when the directory was renamed. -}
onDelDir :: Handler
onDelDir dir _ = do
	debug ["directory deleted", dir]
	(fs, clean) <- liftAnnex $ inRepo $ LsFiles.deleted [dir]

	liftAnnex $ forM_ fs $ \f -> Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.unstageFile f)

	-- Get the events queued up as fast as possible, so the
	-- committer sees them all in one block.
	now <- liftIO getCurrentTime
	forM_ fs $ \f -> recordChange $ Change now f RmChange

	void $ liftIO $ clean
	liftAnnex $ Annex.Queue.flushWhenFull
	noChange

{- Called when there's an error with inotify or kqueue. -}
onErr :: Handler
onErr msg _ = do
	liftAnnex $ warning msg
	void $ addAlert $ warningAlert "watcher" msg
	noChange
