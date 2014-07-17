{- git-annex file content managing for direct mode
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Content.Direct (
	associatedFiles,
	associatedFilesRelative,
	removeAssociatedFile,
	removeAssociatedFileUnchecked,
	removeAssociatedFiles,
	addAssociatedFile,
	goodContent,
	recordedInodeCache,
	updateInodeCache,
	addInodeCache,
	writeInodeCache,
	compareInodeCaches,
	compareInodeCachesWith,
	sameInodeCache,
	elemInodeCaches,
	sameFileStatus,
	removeInodeCache,
	toInodeCache,
	inodesChanged,
	createInodeSentinalFile,
	addContentWhenNotPresent,
	withTSDelta,
	getTSDelta,
) where

import Common.Annex
import qualified Annex
import Annex.Perms
import qualified Git
import Utility.Tmp
import Logs.Location
import Utility.InodeCache
import Utility.CopyFile
import Annex.ReplaceFile
import Annex.Link

{- Absolute FilePaths of Files in the tree that are associated with a key. -}
associatedFiles :: Key -> Annex [FilePath]
associatedFiles key = do
	files <- associatedFilesRelative key
	top <- fromRepo Git.repoPath
	return $ map (top </>) files

{- List of files in the tree that are associated with a key, relative to
 - the top of the repo. -}
associatedFilesRelative :: Key -> Annex [FilePath] 
associatedFilesRelative key = do
	mapping <- calcRepo $ gitAnnexMapping key
	liftIO $ catchDefaultIO [] $ withFile mapping ReadMode $ \h -> do
		fileEncoding h
		-- Read strictly to ensure the file is closed
		-- before changeAssociatedFiles tries to write to it.
		-- (Especially needed on Windows.)
		lines <$> hGetContentsStrict h

{- Changes the associated files information for a key, applying a
 - transformation to the list. Returns new associatedFiles value. -}
changeAssociatedFiles :: Key -> ([FilePath] -> [FilePath]) -> Annex [FilePath]
changeAssociatedFiles key transform = do
	mapping <- calcRepo $ gitAnnexMapping key
	files <- associatedFilesRelative key
	let files' = transform files
	when (files /= files') $
		modifyContent mapping $
			liftIO $ viaTmp writeFileAnyEncoding mapping $
				unlines files'
	top <- fromRepo Git.repoPath
	return $ map (top </>) files'

{- Removes the list of associated files. -}
removeAssociatedFiles :: Key -> Annex ()
removeAssociatedFiles key = do
	mapping <- calcRepo $ gitAnnexMapping key
	modifyContent mapping $
		liftIO $ nukeFile mapping

{- Removes an associated file. Returns new associatedFiles value.
 - Checks if this was the last copy of the object, and updates location
 - log. -}
removeAssociatedFile :: Key -> FilePath -> Annex [FilePath]
removeAssociatedFile key file = do
	fs <- removeAssociatedFileUnchecked key file
	when (null fs) $
		logStatus key InfoMissing
	return fs

{- Removes an associated file. Returns new associatedFiles value. -}
removeAssociatedFileUnchecked :: Key -> FilePath -> Annex [FilePath]
removeAssociatedFileUnchecked key file = do
	file' <- normaliseAssociatedFile file
	changeAssociatedFiles key $ filter (/= file')

{- Adds an associated file. Returns new associatedFiles value. -}
addAssociatedFile :: Key -> FilePath -> Annex [FilePath]
addAssociatedFile key file = do
	file' <- normaliseAssociatedFile file
	changeAssociatedFiles key $ \files ->
		if file' `elem` files
			then files
			else file':files

{- Associated files are always stored relative to the top of the repository.
 - The input FilePath is relative to the CWD, or is absolute. -}
normaliseAssociatedFile :: FilePath -> Annex FilePath
normaliseAssociatedFile file = do
	top <- fromRepo Git.repoPath
	liftIO $ relPathDirToFile top <$> absPath file

{- Checks if a file in the tree, associated with a key, has not been modified.
 -
 - To avoid needing to fsck the file's content, which can involve an
 - expensive checksum, this relies on a cache that contains the file's
 - expected mtime and inode.
 -}
goodContent :: Key -> FilePath -> Annex Bool
goodContent key file = sameInodeCache file =<< recordedInodeCache key

{- Gets the recorded inode cache for a key. 
 -
 - A key can be associated with multiple files, so may return more than
 - one. -}
recordedInodeCache :: Key -> Annex [InodeCache]
recordedInodeCache key = withInodeCacheFile key $ \f ->
	liftIO $ catchDefaultIO [] $
		mapMaybe readInodeCache . lines <$> readFileStrict f

{- Caches an inode for a file.
 -
 - Anything else already cached is preserved.
 -}
updateInodeCache :: Key -> FilePath -> Annex ()
updateInodeCache key file = maybe noop (addInodeCache key)
	=<< withTSDelta (liftIO . genInodeCache file)

{- Adds another inode to the cache for a key. -}
addInodeCache :: Key -> InodeCache -> Annex ()
addInodeCache key cache = do
	oldcaches <- recordedInodeCache key
	unlessM (elemInodeCaches cache oldcaches) $
		writeInodeCache key (cache:oldcaches)

{- Writes inode cache for a key. -}
writeInodeCache :: Key -> [InodeCache] -> Annex ()
writeInodeCache key caches = withInodeCacheFile key $ \f -> 
	modifyContent f $
		liftIO $ writeFile f $
			unlines $ map showInodeCache caches

{- Removes an inode cache. -}
removeInodeCache :: Key -> Annex ()
removeInodeCache key = withInodeCacheFile key $ \f ->
	modifyContent f $
		liftIO $ nukeFile f

withInodeCacheFile :: Key -> (FilePath -> Annex a) -> Annex a
withInodeCacheFile key a = a =<< calcRepo (gitAnnexInodeCache key)

{- Checks if a InodeCache matches the current version of a file. -}
sameInodeCache :: FilePath -> [InodeCache] -> Annex Bool
sameInodeCache _ [] = return False
sameInodeCache file old = go =<< withTSDelta (liftIO . genInodeCache file)
  where
	go Nothing = return False
	go (Just curr) = elemInodeCaches curr old

{- Checks if a FileStatus matches the recorded InodeCache of a file. -}
sameFileStatus :: Key -> FileStatus -> Annex Bool
sameFileStatus key status = do
	old <- recordedInodeCache key
	curr <- withTSDelta $ \delta -> liftIO $ toInodeCache delta status
	case (old, curr) of
		(_, Just c) -> elemInodeCaches c old
		([], Nothing) -> return True
		_ -> return False

{- If the inodes have changed, only the size and mtime are compared. -}
compareInodeCaches :: InodeCache -> InodeCache -> Annex Bool
compareInodeCaches x y
	| compareStrong x y = return True
	| otherwise = ifM inodesChanged
		( return $ compareWeak x y
		, return False
		)

elemInodeCaches :: InodeCache -> [InodeCache] -> Annex Bool
elemInodeCaches _ [] = return False
elemInodeCaches c (l:ls) = ifM (compareInodeCaches c l)
	( return True
	, elemInodeCaches c ls
	)

compareInodeCachesWith :: Annex InodeComparisonType
compareInodeCachesWith = ifM inodesChanged ( return Weakly, return Strongly )

{- Copies the contentfile to the associated file, if the associated
 - file has no content. If the associated file does have content,
 - even if the content differs, it's left unchanged. -}
addContentWhenNotPresent :: Key -> FilePath -> FilePath -> Annex ()
addContentWhenNotPresent key contentfile associatedfile = do
	v <- isAnnexLink associatedfile
	when (Just key == v) $
		replaceFile associatedfile $
			liftIO . void . copyFileExternal contentfile
	updateInodeCache key associatedfile	

{- Some filesystems get new inodes each time they are mounted.
 - In order to work on such a filesystem, a sentinal file is used to detect
 - when the inodes have changed.
 -
 - If the sentinal file does not exist, we have to assume that the
 - inodes have changed.
 -}
inodesChanged :: Annex Bool
inodesChanged = sentinalInodesChanged <$> sentinalStatus

withTSDelta :: (TSDelta -> Annex a) -> Annex a
withTSDelta a = a =<< getTSDelta

getTSDelta :: Annex TSDelta
#ifdef mingw32_HOST_OS
getTSDelta = sentinalTSDelta <$> sentinalStatus
#else
getTSDelta = pure noTSDelta -- optimisation
#endif

sentinalStatus :: Annex SentinalStatus
sentinalStatus = maybe check return =<< Annex.getState Annex.sentinalstatus
  where
	check = do
		sc <- liftIO . checkSentinalFile =<< annexSentinalFile
		Annex.changeState $ \s -> s { Annex.sentinalstatus = Just sc }
		return sc

{- The sentinal file is only created when first initializing a repository.
 - If there are any annexed objects in the repository already, creating
 - the file would invalidate their inode caches. -}
createInodeSentinalFile :: Annex ()
createInodeSentinalFile = unlessM (alreadyexists <||> hasobjects) $ do
	s <- annexSentinalFile
	createAnnexDirectory (parentDir (sentinalFile s))
	liftIO $ writeSentinalFile s
  where
	alreadyexists = liftIO. sentinalFileExists =<< annexSentinalFile
	hasobjects = liftIO . doesDirectoryExist =<< fromRepo gitAnnexObjectDir

annexSentinalFile :: Annex SentinalFile
annexSentinalFile = do
	sentinalfile <- fromRepo gitAnnexInodeSentinal
	sentinalcachefile <- fromRepo gitAnnexInodeSentinalCache
	return $ SentinalFile
		{ sentinalFile = sentinalfile
		, sentinalCacheFile = sentinalcachefile
		}
