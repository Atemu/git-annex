{- git-annex assistant commit thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Threads.Committer where

import Assistant.Common
import Assistant.Changes
import Assistant.Types.Changes
import Assistant.Commits
import Assistant.Alert
import Assistant.DaemonStatus
import Assistant.TransferQueue
import Assistant.Drop
import Logs.Transfer
import Logs.Location
import qualified Annex.Queue
import qualified Git.Command
import qualified Git.LsFiles
import qualified Git.Version
import qualified Command.Add
import Utility.ThreadScheduler
import qualified Utility.Lsof as Lsof
import qualified Utility.DirWatcher as DirWatcher
import Types.KeySource
import Config
import Annex.Exception
import Annex.Content
import Annex.Link
import Annex.CatFile
import qualified Annex
import Utility.InodeCache
import Annex.Content.Direct

import Data.Time.Clock
import Data.Tuple.Utils
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Either
import Control.Concurrent

{- This thread makes git commits at appropriate times. -}
commitThread :: NamedThread
commitThread = namedThread "Committer" $ do
	delayadd <- liftAnnex $
		maybe delayaddDefault (return . Just . Seconds)
			=<< annexDelayAdd <$> Annex.getGitConfig
	waitChangeTime $ \(changes, time) -> do
		readychanges <- handleAdds delayadd changes
		if shouldCommit time (length readychanges) readychanges
			then do
				debug
					[ "committing"
					, show (length readychanges)
					, "changes"
					]
				void $ alertWhile commitAlert $
					liftAnnex commitStaged
				recordCommit
				let numchanges = length readychanges
				mapM_ checkChangeContent readychanges
				return numchanges
			else do
				refill readychanges
				return 0

refill :: [Change] -> Assistant ()	
refill [] = noop
refill cs = do
	debug ["delaying commit of", show (length cs), "changes"]
	refillChanges cs

{- Wait for one or more changes to arrive to be committed, and then
 - runs an action to commit them. If more changes arrive while this is
 - going on, they're handled intelligently, batching up changes into
 - large commits where possible, doing rename detection, and
 - commiting immediately otherwise. -}
waitChangeTime :: (([Change], UTCTime) -> Assistant Int) -> Assistant ()
waitChangeTime a = waitchanges 0
  where
	waitchanges lastcommitsize = do
		-- Wait one one second as a simple rate limiter.
		liftIO $ threadDelaySeconds (Seconds 1)
		-- Now, wait until at least one change is available for
		-- processing.
		cs <- getChanges
		handlechanges cs lastcommitsize
	handlechanges changes lastcommitsize = do
		let len = length changes
		-- See if now's a good time to commit.
		now <- liftIO getCurrentTime
		case (lastcommitsize >= maxCommitSize, shouldCommit now len changes, possiblyrename changes) of
			(True, True, _)
				| len > maxCommitSize -> 
					waitchanges =<< a (changes, now)
				| otherwise -> aftermaxcommit changes
			(_, True, False) ->
				waitchanges =<< a (changes, now)
			(_, True, True) -> do
				morechanges <- getrelatedchanges changes
				waitchanges =<< a (changes ++ morechanges, now)
			_ -> do
				refill changes
				waitchanges lastcommitsize
	
	{- Did we perhaps only get one of the AddChange and RmChange pair
	 - that make up a file rename? Or some of the pairs that make up 
	 - a directory rename?
	 -}
	possiblyrename cs = all renamepart cs

	renamepart (PendingAddChange _ _) = True
	renamepart c = isRmChange c

	{- Gets changes related to the passed changes, without blocking
	 - very long.
	 -
	 - If there are multiple RmChanges, this is probably a directory
	 - rename, in which case it may be necessary to wait longer to get
	 - all the Changes involved.
	 -}
	getrelatedchanges oldchanges
		| length (filter isRmChange oldchanges) > 1 =
			concat <$> getbatchchanges []
		| otherwise = do
			liftIO humanImperceptibleDelay
			getAnyChanges
	getbatchchanges cs = do
		liftIO $ threadDelay $ fromIntegral $ oneSecond `div` 10
		cs' <- getAnyChanges
		if null cs'
			then return cs
			else getbatchchanges (cs':cs)

	{- The last commit was maximum size, so it's very likely there
	 - are more changes and we'd like to ensure we make another commit
	 - of maximum size if possible.
	 -
	 - But, it can take a while for the Watcher to wake back up
	 - after a commit. It can get blocked by another thread
	 - that is using the Annex state, such as a git-annex branch
	 - commit. Especially after such a large commit, this can
	 - take several seconds. When this happens, it defeats the
	 - normal commit batching, which sees some old changes the
	 - Watcher found while the commit was being prepared, and sees
	 - no recent ones, and wants to commit immediately.
	 -
	 - All that we need to do, then, is wait for the Watcher to
	 - wake up, and queue up one more change.
	 -
	 - However, it's also possible that we're at the end of changes for
	 - now. So to avoid waiting a really long time before committing
	 - those changes we have, poll for up to 30 seconds, and then
	 - commit them.
	 -
	 - Also, try to run something in Annex, to ensure we block
	 - longer if the Annex state is indeed blocked.
	 -}
	aftermaxcommit oldchanges = loop (30 :: Int)
	  where
	  	loop 0 = continue oldchanges
	  	loop n = do
			liftAnnex noop -- ensure Annex state is free
			liftIO $ threadDelaySeconds (Seconds 1)
			changes <- getAnyChanges
			if null changes
				then loop (n - 1)
				else continue (oldchanges ++ changes)
		continue cs
			| null cs = waitchanges 0
			| otherwise = handlechanges cs 0

isRmChange :: Change -> Bool
isRmChange (Change { changeInfo = i }) | i == RmChange = True
isRmChange _ = False

{- An amount of time that is hopefully imperceptably short for humans,
 - while long enough for a computer to get some work done. 
 - Note that 0.001 is a little too short for rename change batching to
 - work. -}
humanImperceptibleInterval :: NominalDiffTime
humanImperceptibleInterval = 0.01

humanImperceptibleDelay :: IO ()
humanImperceptibleDelay = threadDelay $
	truncate $ humanImperceptibleInterval * fromIntegral oneSecond

maxCommitSize :: Int
maxCommitSize = 5000

{- Decide if now is a good time to make a commit.
 - Note that the list of changes has an undefined order.
 -
 - Current strategy: If there have been 10 changes within the past second,
 - a batch activity is taking place, so wait for later.
 -}
shouldCommit :: UTCTime -> Int -> [Change] -> Bool
shouldCommit now len changes
	| len == 0 = False
	| len >= maxCommitSize = True
	| length recentchanges < 10 = True
	| otherwise = False -- batch activity
  where
	thissecond c = timeDelta c <= 1
	recentchanges = filter thissecond changes
	timeDelta c = now `diffUTCTime` changeTime c

commitStaged :: Annex Bool
commitStaged = do
	{- This could fail if there's another commit being made by
	 - something else. -}
	v <- tryAnnex Annex.Queue.flush
	case v of
		Left _ -> return False
		Right _ -> do
			{- Empty commits may be made if tree changes cancel
			 - each other out, etc. Git returns nonzero on those,
			 - so don't propigate out commit failures. -}
			void $ inRepo $ catchMaybeIO . 
				Git.Command.runQuiet
					(Param "commit" : nomessage params)
			return True
  where
	params =
		[ Param "--quiet"
		{- Avoid running the usual pre-commit hook;
		 - the Watcher does the same symlink fixing,
		 - and direct mode bookkeeping updating. -}
		, Param "--no-verify"
		]
	nomessage ps
		| Git.Version.older "1.7.2" =
			Param "-m" : Param "autocommit" : ps
		| Git.Version.older "1.7.8" =
			Param "--allow-empty-message" :
			Param "-m" : Param "" : ps
		| otherwise =
			Param "--allow-empty-message" :
			Param "--no-edit" : Param "-m" : Param "" : ps

{- OSX needs a short delay after a file is added before locking it down,
 - when using a non-direct mode repository, as pasting a file seems to
 - try to set file permissions or otherwise access the file after closing
 - it. -}
delayaddDefault :: Annex (Maybe Seconds)
#ifdef darwin_HOST_OS
delayaddDefault = ifM isDirect
	( return Nothing
	, return $ Just $ Seconds 1
	)
#else
delayaddDefault = return Nothing
#endif

{- If there are PendingAddChanges, or InProcessAddChanges, the files
 - have not yet actually been added to the annex, and that has to be done
 - now, before committing.
 -
 - Deferring the adds to this point causes batches to be bundled together,
 - which allows faster checking with lsof that the files are not still open
 - for write by some other process, and faster checking with git-ls-files
 - that the files are not already checked into git.
 -
 - When a file is added, Inotify will notice the new symlink. So this waits
 - for additional Changes to arrive, so that the symlink has hopefully been
 - staged before returning, and will be committed immediately.
 -
 - OTOH, for kqueue, eventsCoalesce, so instead the symlink is directly
 - created and staged.
 -
 - Returns a list of all changes that are ready to be committed.
 - Any pending adds that are not ready yet are put back into the ChangeChan,
 - where they will be retried later.
 -}
handleAdds :: Maybe Seconds -> [Change] -> Assistant [Change]
handleAdds delayadd cs = returnWhen (null incomplete) $ do
	let (pending, inprocess) = partition isPendingAddChange incomplete
	direct <- liftAnnex isDirect
	(pending', cleanup) <- if direct
		then return (pending, noop)
		else findnew pending
	(postponed, toadd) <- partitionEithers <$> safeToAdd delayadd pending' inprocess
	cleanup

	unless (null postponed) $
		refillChanges postponed

	returnWhen (null toadd) $ do
		added <- addaction toadd $
			catMaybes <$> if direct
				then adddirect toadd
				else forM toadd add
		if DirWatcher.eventsCoalesce || null added || direct
			then return $ added ++ otherchanges
			else do
				r <- handleAdds delayadd =<< getChanges
				return $ r ++ added ++ otherchanges
  where
	(incomplete, otherchanges) = partition (\c -> isPendingAddChange c || isInProcessAddChange c) cs
		
	findnew [] = return ([], noop)
	findnew pending@(exemplar:_) = do
		(newfiles, cleanup) <- liftAnnex $
			inRepo (Git.LsFiles.notInRepo False $ map changeFile pending)
		-- note: timestamp info is lost here
		let ts = changeTime exemplar
		return (map (PendingAddChange ts) newfiles, void $ liftIO $ cleanup)

	returnWhen c a
		| c = return otherchanges
		| otherwise = a

	add :: Change -> Assistant (Maybe Change)
	add change@(InProcessAddChange { keySource = ks }) = 
		catchDefaultIO Nothing <~> do
			sanitycheck ks $ do
				key <- liftAnnex $ do
					showStart "add" $ keyFilename ks
					Command.Add.ingest $ Just ks
				maybe (failedingest change) (done change $ keyFilename ks) key
	add _ = return Nothing

	{- In direct mode, avoid overhead of re-injesting a renamed
	 - file, by examining the other Changes to see if a removed
	 - file has the same InodeCache as the new file. If so,
	 - we can just update bookkeeping, and stage the file in git.
	 -}
	adddirect :: [Change] -> Assistant [Maybe Change]
	adddirect toadd = do
		ct <- liftAnnex compareInodeCachesWith
		m <- liftAnnex $ removedKeysMap ct cs
		if M.null m
			then forM toadd add
			else forM toadd $ \c -> do
				mcache <- liftIO $ genInodeCache $ changeFile c
				case mcache of
					Nothing -> add c
					Just cache ->
						case M.lookup (inodeCacheToKey ct cache) m of
							Nothing -> add c
							Just k -> fastadd c k

	fastadd :: Change -> Key -> Assistant (Maybe Change)
	fastadd change key = do
		let source = keySource change
		liftAnnex $ Command.Add.finishIngestDirect key source
		done change (keyFilename source) key

	removedKeysMap :: InodeComparisonType -> [Change] -> Annex (M.Map InodeCacheKey Key)
	removedKeysMap ct l = do
		mks <- forM (filter isRmChange l) $ \c ->
			catKeyFile $ changeFile c
		M.fromList . concat <$> mapM mkpairs (catMaybes mks)
	  where
		mkpairs k = map (\c -> (inodeCacheToKey ct c, k)) <$>
			recordedInodeCache k

	failedingest change = do
		refill [retryChange change]
		liftAnnex showEndFail
		return Nothing

	done change file key = liftAnnex $ do
		logStatus key InfoPresent
		link <- ifM isDirect
			( inRepo $ gitAnnexLink file key
			, Command.Add.link file key True
			)
		whenM (pure DirWatcher.eventsCoalesce <||> isDirect) $ do
			stageSymlink file =<< hashSymlink link
		showEndOk
		return $ Just $ finishedChange change key

	{- Check that the keysource's keyFilename still exists,
	 - and is still a hard link to its contentLocation,
	 - before ingesting it. -}
	sanitycheck keysource a = do
		fs <- liftIO $ getSymbolicLinkStatus $ keyFilename keysource
		ks <- liftIO $ getSymbolicLinkStatus $ contentLocation keysource
		if deviceID ks == deviceID fs && fileID ks == fileID fs
			then a
			else do
				-- remove the hard link
				when (contentLocation keysource /= keyFilename keysource) $
					void $ liftIO $ tryIO $ removeFile $ contentLocation keysource
				return Nothing

	{- Shown an alert while performing an action to add a file or
	 - files. When only a few files are added, their names are shown
	 - in the alert. When it's a batch add, the number of files added
	 - is shown.
	 -
	 - Add errors tend to be transient and will be
	 - automatically dealt with, so the alert is always told
	 - the add succeeded.
	 -}
	addaction [] a = a
	addaction toadd a = alertWhile' (addFileAlert $ map changeFile toadd) $
		(,) 
			<$> pure True
			<*> a

{- Files can Either be Right to be added now,
 - or are unsafe, and must be Left for later.
 -
 - Check by running lsof on the repository.
 -}
safeToAdd :: Maybe Seconds -> [Change] -> [Change] -> Assistant [Either Change Change]
safeToAdd _ [] [] = return []
safeToAdd delayadd pending inprocess = do
	maybe noop (liftIO . threadDelaySeconds) delayadd
	liftAnnex $ do
		keysources <- mapM Command.Add.lockDown (map changeFile pending)
		let inprocess' = inprocess ++ catMaybes (map mkinprocess $ zip pending keysources)
		openfiles <- S.fromList . map fst3 . filter openwrite <$>
			findopenfiles (map keySource inprocess')
		let checked = map (check openfiles) inprocess'

		{- If new events are received when files are closed,
		 - there's no need to retry any changes that cannot
		 - be done now. -}
		if DirWatcher.closingTracked
			then do
				mapM_ canceladd $ lefts checked
				allRight $ rights checked
			else return checked
  where
	check openfiles change@(InProcessAddChange { keySource = ks })
		| S.member (contentLocation ks) openfiles = Left change
	check _ change = Right change

	mkinprocess (c, Just ks) = Just $ InProcessAddChange
		{ changeTime = changeTime c
		, keySource = ks
		}
	mkinprocess (_, Nothing) = Nothing

	canceladd (InProcessAddChange { keySource = ks }) = do
		warning $ keyFilename ks
			++ " still has writers, not adding"
		-- remove the hard link
		when (contentLocation ks /= keyFilename ks) $
			void $ liftIO $ tryIO $ removeFile $ contentLocation ks
	canceladd _ = noop

	openwrite (_file, mode, _pid)
		| mode == Lsof.OpenWriteOnly = True
		| mode == Lsof.OpenReadWrite = True
		| mode == Lsof.OpenUnknown = True
		| otherwise = False

	allRight = return . map Right

	{- Normally the KeySources are locked down inside the temp directory,
	 - so can just lsof that, which is quite efficient.
	 -
	 - In crippled filesystem mode, there is no lock down, so must run lsof
	 - on each individual file.
	 -}
	findopenfiles keysources = ifM crippledFileSystem
		( liftIO $ do
			let segments = segmentXargs $ map keyFilename keysources
			concat <$> forM segments (\fs -> Lsof.query $ "--" : fs)
		, do
			tmpdir <- fromRepo gitAnnexTmpDir
			liftIO $ Lsof.queryDir tmpdir
		)

{- After a Change is committed, queue any necessary transfers or drops
 - of the content of the key.
 -
 - This is not done during the startup scan, because the expensive
 - transfer scan does the same thing then.
 -}
checkChangeContent :: Change -> Assistant ()
checkChangeContent change@(Change { changeInfo = i }) =
	case changeInfoKey i of
		Nothing -> noop
		Just k -> whenM (scanComplete <$> getDaemonStatus) $ do
			present <- liftAnnex $ inAnnex k
			if present
				then queueTransfers "new file created" Next k (Just f) Upload
				else queueTransfers "new or renamed file wanted" Next k (Just f) Download
			handleDrops "file renamed" present k (Just f) Nothing
  where
	f = changeFile change
checkChangeContent _ = noop
