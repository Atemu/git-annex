{- git-annex file content managing
 -
 - Copyright 2010,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Content (
	inAnnex,
	inAnnexSafe,
	lockContent,
	getViaTmp,
	getViaTmpChecked,
	getViaTmpUnchecked,
	withTmp,
	checkDiskSpace,
	moveAnnex,
	sendAnnex,
	prepSendAnnex,
	removeAnnex,
	fromAnnex,
	moveBad,
	getKeysPresent,
	saveState,
	downloadUrl,
	preseedTmp,
	freezeContent,
	thawContent,
	replaceFile,
	cleanObjectLoc,
) where

import System.IO.Unsafe (unsafeInterleaveIO)

import Common.Annex
import Logs.Location
import qualified Git
import qualified Annex
import qualified Annex.Queue
import qualified Annex.Branch
import Utility.DiskFree
import Utility.FileMode
import qualified Utility.Url as Url
import Types.Key
import Utility.DataUnits
import Utility.CopyFile
import Config
import Annex.Exception
import Git.SharedRepository
import Annex.Perms
import Annex.Link
import Annex.Content.Direct

{- Checks if a given key's content is currently present. -}
inAnnex :: Key -> Annex Bool
inAnnex = inAnnex' id False $ liftIO . doesFileExist

{- Generic inAnnex, handling both indirect and direct mode.
 -
 - In direct mode, at least one of the associated files must pass the
 - check. Additionally, the file must be unmodified.
 -}
inAnnex' :: (a -> Bool) -> a -> (FilePath -> Annex a) -> Key -> Annex a
inAnnex' isgood bad check key = withObjectLoc key checkindirect checkdirect
  where
	checkindirect loc = do
		whenM (fromRepo Git.repoIsUrl) $
			error "inAnnex cannot check remote repo"
		check loc
	checkdirect [] = return bad
	checkdirect (loc:locs) = do
		r <- check loc
		if isgood r
			then ifM (goodContent key loc)
				( return r
				, checkdirect locs
				)
			else checkdirect locs

{- A safer check; the key's content must not only be present, but
 - is not in the process of being removed. -}
inAnnexSafe :: Key -> Annex (Maybe Bool)
inAnnexSafe = inAnnex' (fromMaybe False) (Just False) go
  where
	go f = liftIO $ openforlock f >>= check
	openforlock f = catchMaybeIO $
		openFd f ReadOnly Nothing defaultFileFlags
	check Nothing = return is_missing
	check (Just h) = do
		v <- getLock h (ReadLock, AbsoluteSeek, 0, 0)
		closeFd h
		return $ case v of
			Just _ -> is_locked
			Nothing -> is_unlocked
	is_locked = Nothing
	is_unlocked = Just True
	is_missing = Just False

{- Content is exclusively locked while running an action that might remove
 - it. (If the content is not present, no locking is done.) -}
lockContent :: Key -> Annex a -> Annex a
lockContent key a = do
	file <- calcRepo $ gitAnnexLocation key
	bracketIO (openforlock file >>= lock) unlock a
  where
	{- Since files are stored with the write bit disabled, have
	 - to fiddle with permissions to open for an exclusive lock. -}
	openforlock f = catchMaybeIO $ ifM (doesFileExist f)
		( withModifiedFileMode f
			(`unionFileModes` ownerWriteMode)
			open
		, open
		)
	  where
		open = openFd f ReadWrite Nothing defaultFileFlags
	lock Nothing = return Nothing
	lock (Just fd) = do
		v <- tryIO $ setLock fd (WriteLock, AbsoluteSeek, 0, 0)
		case v of
			Left _ -> error "content is locked"
			Right _ -> return $ Just fd
	unlock Nothing = noop
	unlock (Just l) = closeFd l

{- Runs an action, passing it a temporary filename to get,
 - and if the action succeeds, moves the temp file into 
 - the annex as a key's content. -}
getViaTmp :: Key -> (FilePath -> Annex Bool) -> Annex Bool
getViaTmp = getViaTmpChecked (return True)

{- Like getViaTmp, but does not check that there is enough disk space
 - for the incoming key. For use when the key content is already on disk
 - and not being copied into place. -}
getViaTmpUnchecked :: Key -> (FilePath -> Annex Bool) -> Annex Bool
getViaTmpUnchecked = finishGetViaTmp (return True)

getViaTmpChecked :: Annex Bool -> Key -> (FilePath -> Annex Bool) -> Annex Bool
getViaTmpChecked check key action = do
	tmp <- fromRepo $ gitAnnexTmpLocation key

	-- Check that there is enough free disk space.
	-- When the temp file already exists, count the space
	-- it is using as free.
	e <- liftIO $ doesFileExist tmp
	alreadythere <- if e
		then fromIntegral . fileSize <$> liftIO (getFileStatus tmp)
		else return 0
	ifM (checkDiskSpace Nothing key alreadythere)
		( do
			when e $ thawContent tmp
			finishGetViaTmp check key action
		, return False
		)

finishGetViaTmp :: Annex Bool -> Key -> (FilePath -> Annex Bool) -> Annex Bool
finishGetViaTmp check key action = do
	tmpfile <- prepTmp key
	ifM (action tmpfile <&&> check)
		( do
			moveAnnex key tmpfile
			logStatus key InfoPresent
			return True
		, do
			-- the tmp file is left behind, in case caller wants
			-- to resume its transfer
			return False
		)

prepTmp :: Key -> Annex FilePath
prepTmp key = do
	tmp <- fromRepo $ gitAnnexTmpLocation key
	createAnnexDirectory (parentDir tmp)
	return tmp

{- Creates a temp file, runs an action on it, and cleans up the temp file. -}
withTmp :: Key -> (FilePath -> Annex a) -> Annex a
withTmp key action = do
	tmp <- prepTmp key
	res <- action tmp
	liftIO $ nukeFile tmp
	return res

{- Checks that there is disk space available to store a given key,
 - in a destination (or the annex) printing a warning if not. -}
checkDiskSpace :: Maybe FilePath -> Key -> Integer -> Annex Bool
checkDiskSpace destination key alreadythere = do
	reserve <- annexDiskReserve <$> Annex.getGitConfig
	free <- liftIO . getDiskFree =<< dir
	force <- Annex.getState Annex.force
	case (free, keySize key) of
		(Just have, Just need) -> do
			let ok = (need + reserve <= have + alreadythere) || force
			unless ok $ do
				liftIO $ print (need, reserve, have, alreadythere)
				needmorespace (need + reserve - have - alreadythere)
			return ok
		_ -> return True
  where
	dir = maybe (fromRepo gitAnnexDir) return destination
	needmorespace n =
		warning $ "not enough free space, need " ++ 
			roughSize storageUnits True n ++
			" more" ++ forcemsg
	forcemsg = " (use --force to override this check or adjust annex.diskreserve)"

{- Moves a key's content into .git/annex/objects/
 -
 - In direct mode, moves it to the associated file, or files.
 -
 - What if the key there already has content? This could happen for
 - various reasons; perhaps the same content is being annexed again.
 - Perhaps there has been a hash collision generating the keys.
 -
 - The current strategy is to assume that in this case it's safe to delete
 - one of the two copies of the content; and the one already in the annex
 - is left there, assuming it's the original, canonical copy.
 -
 - I considered being more paranoid, and checking that both files had
 - the same content. Decided against it because A) users explicitly choose
 - a backend based on its hashing properties and so if they're dealing
 - with colliding files it's their own fault and B) adding such a check
 - would not catch all cases of colliding keys. For example, perhaps 
 - a remote has a key; if it's then added again with different content then
 - the overall system now has two different peices of content for that
 - key, and one of them will probably get deleted later. So, adding the
 - check here would only raise expectations that git-annex cannot truely
 - meet.
 -}
moveAnnex :: Key -> FilePath -> Annex ()
moveAnnex key src = withObjectLoc key storeobject storedirect
  where
	storeobject dest = ifM (liftIO $ doesFileExist dest)
		( liftIO $ removeFile src
		, do
			createContentDir dest
			liftIO $ moveFile src dest
			freezeContent dest
			freezeContentDir dest
		)
	storedirect fs = storedirect' =<< filterM validsymlink fs
	validsymlink f = (==) (Just key) <$> isAnnexLink f

	storedirect' [] = storeobject =<< calcRepo (gitAnnexLocation key)
	storedirect' (dest:fs) = do
		updateInodeCache key src
		thawContent src
		replaceFile dest $ liftIO . moveFile src
		{- Copy to any other locations. -}
		forM_ fs $ \f -> replaceFile f $
			liftIO . void . copyFileExternal dest

{- Replaces a possibly already existing file with a new version, 
 - atomically, by running an action.

 - The action is passed a temp file, which it can write to, and once
 - done the temp file is moved into place.
 -}
replaceFile :: FilePath -> (FilePath -> Annex ()) -> Annex ()
replaceFile file a = do
	tmpdir <- fromRepo gitAnnexTmpDir
	createAnnexDirectory tmpdir
	tmpfile <- liftIO $ do
		(tmpfile, h) <- openTempFileWithDefaultPermissions tmpdir $
			takeFileName file
		hClose h
		return tmpfile
	a tmpfile
	liftIO $ do
		r <- tryIO $ rename tmpfile file
		case r of
			Left _ -> do
				createDirectoryIfMissing True $ parentDir file
				rename tmpfile file
			_ -> noop

{- Runs an action to transfer an object's content.
 -
 - In direct mode, it's possible for the file to change as it's being sent.
 - If this happens, runs the rollback action and returns False. The
 - rollback action should remove the data that was transferred.
 -}
sendAnnex :: Key -> Annex () -> (FilePath -> Annex Bool) -> Annex Bool
sendAnnex key rollback sendobject = go =<< prepSendAnnex key
  where
	go Nothing = return False
	go (Just (f, checksuccess)) = do
		r <- sendobject f
		ifM checksuccess
			( return r
			, do
				rollback
				return False
			)

{- Returns a file that contains an object's content,
 - and an check to run after the transfer is complete.
 -
 - In direct mode, it's possible for the file to change as it's being sent,
 - and the check detects this case and returns False.
 -
 - Note that the returned check action is, in some cases, run in the
 - Annex monad of the remote that is receiving the object, rather than
 - the sender. So it cannot rely on Annex state.
 -}
prepSendAnnex :: Key -> Annex (Maybe (FilePath, Annex Bool))
prepSendAnnex key = withObjectLoc key indirect direct
  where
	indirect f = return $ Just (f, return True)
	direct [] = return Nothing
	direct (f:fs) = do
		cache <- recordedInodeCache key
		-- check that we have a good file
		ifM (sameInodeCache f cache)
			( return $ Just (f, sameInodeCache f cache)
			, direct fs
			)

{- Performs an action, passing it the location to use for a key's content.
 -
 - In direct mode, the associated files will be passed. But, if there are
 - no associated files for a key, the indirect mode action will be
 - performed instead. -}
withObjectLoc :: Key -> (FilePath -> Annex a) -> ([FilePath] -> Annex a) -> Annex a
withObjectLoc key indirect direct = ifM isDirect
	( do
		fs <- associatedFiles key
		if null fs
			then goindirect
			else direct fs
	, goindirect
	)
  where
	goindirect = indirect =<< calcRepo (gitAnnexLocation key)

cleanObjectLoc :: Key -> Annex ()
cleanObjectLoc key = do
	file <- calcRepo $ gitAnnexLocation key
	unlessM crippledFileSystem $
		void $ liftIO $ catchMaybeIO $ allowWrite $ parentDir file
	liftIO $ removeparents file (3 :: Int)
  where
	removeparents _ 0 = noop
	removeparents file n = do
		let dir = parentDir file
		maybe noop (const $ removeparents dir (n-1))
			<=< catchMaybeIO $ removeDirectory dir

{- Removes a key's file from .git/annex/objects/
 -
 - In direct mode, deletes the associated files or files, and replaces
 - them with symlinks. -}
removeAnnex :: Key -> Annex ()
removeAnnex key = withObjectLoc key remove removedirect
  where
	remove file = do
		unlessM crippledFileSystem $
			liftIO $ allowWrite $ parentDir file
		liftIO $ nukeFile file
		removeInodeCache key
		cleanObjectLoc key
	removedirect fs = do
		cache <- recordedInodeCache key
		removeInodeCache key
		mapM_ (resetfile cache) fs
	resetfile cache f = whenM (sameInodeCache f cache) $ do
		l <- inRepo $ gitAnnexLink f key
		top <- fromRepo Git.repoPath
		cwd <- liftIO getCurrentDirectory
		let top' = fromMaybe top $ absNormPath cwd top
		let l' = relPathDirToFile top' (fromMaybe l $ absNormPath top' l)
		replaceFile f $ makeAnnexLink l'

{- Moves a key's file out of .git/annex/objects/ -}
fromAnnex :: Key -> FilePath -> Annex ()
fromAnnex key dest = do
	file <- calcRepo $ gitAnnexLocation key
	unlessM crippledFileSystem $
		liftIO $ allowWrite $ parentDir file
	thawContent file
	liftIO $ moveFile file dest
	cleanObjectLoc key

{- Moves a key out of .git/annex/objects/ into .git/annex/bad, and
 - returns the file it was moved to. -}
moveBad :: Key -> Annex FilePath
moveBad key = do
	src <- calcRepo $ gitAnnexLocation key
	bad <- fromRepo gitAnnexBadDir
	let dest = bad </> takeFileName src
	createAnnexDirectory (parentDir dest)
	unlessM crippledFileSystem $
		liftIO $ allowWrite (parentDir src)
	liftIO $ moveFile src dest
	cleanObjectLoc key
	logStatus key InfoMissing
	return dest

{- List of keys whose content exists in the annex. -}
getKeysPresent :: Annex [Key]
getKeysPresent = do
	direct <- isDirect
	dir <- fromRepo gitAnnexObjectDir
	liftIO $ traverse direct (2 :: Int) dir
  where
	traverse direct depth dir = do
		contents <- catchDefaultIO [] (dirContents dir)
		if depth == 0
			then do
				contents' <- filterM (present direct) contents
				let keys = mapMaybe (fileKey . takeFileName) contents'
				continue keys []
			else do
				let deeper = traverse direct (depth - 1)
				continue [] (map deeper contents)
	continue keys [] = return keys
	continue keys (a:as) = do
		{- Force lazy traversal with unsafeInterleaveIO. -}
		morekeys <- unsafeInterleaveIO a
		continue (morekeys++keys) as

	{- In indirect mode, look for the key. In direct mode,
	 - the inode cache file is only present when a key's content
	 - is present. -}
	present False d = doesFileExist $ contentfile d
	present True d = doesFileExist $ contentfile d ++ ".cache"
	contentfile d = d </> takeFileName d

{- Things to do to record changes to content when shutting down.
 -
 - It's acceptable to avoid committing changes to the branch,
 - especially if performing a short-lived action.
 -}
saveState :: Bool -> Annex ()
saveState nocommit = doSideAction $ do
	Annex.Queue.flush
	unless nocommit $
		whenM (annexAlwaysCommit <$> Annex.getGitConfig) $
			Annex.Branch.commit "update"

{- Downloads content from any of a list of urls. -}
downloadUrl :: [Url.URLString] -> FilePath -> Annex Bool
downloadUrl urls file = do
	o <- map Param . annexWebOptions <$> Annex.getGitConfig
	headers <- getHttpHeaders
	liftIO $ anyM (\u -> Url.download u headers o file) urls

{- Copies a key's content, when present, to a temp file.
 - This is used to speed up some rsyncs. -}
preseedTmp :: Key -> FilePath -> Annex Bool
preseedTmp key file = go =<< inAnnex key
  where
	go False = return False
	go True = do
		ok <- copy
		when ok $ thawContent file
		return ok
	copy = ifM (liftIO $ doesFileExist file)
		( return True
		, do
			s <- calcRepo $ gitAnnexLocation key
			liftIO $ copyFileExternal s file
		)

{- Blocks writing to an annexed file. The file is made unwritable
 - to avoid accidental edits. core.sharedRepository may change
 - who can read it. -}
freezeContent :: FilePath -> Annex ()
freezeContent file = unlessM crippledFileSystem $
	liftIO . go =<< fromRepo getSharedRepository
  where
	go GroupShared = modifyFileMode file $
		removeModes writeModes .
		addModes [ownerReadMode, groupReadMode]
	go AllShared = modifyFileMode file $
		removeModes writeModes .
		addModes readModes
	go _ = preventWrite file

{- Allows writing to an annexed file that freezeContent was called on
 - before. -}
thawContent :: FilePath -> Annex ()
thawContent file = unlessM crippledFileSystem $
	liftIO . go =<< fromRepo getSharedRepository
  where
	go GroupShared = groupWriteRead file
	go AllShared = groupWriteRead file
	go _ = allowWrite file
