{- git-annex command
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Fsck where

import Common.Annex
import Command
import qualified Annex
import qualified Remote
import qualified Types.Backend
import qualified Types.Key
import qualified Backend
import Annex.Content
import Annex.Content.Direct
import Annex.Direct
import Annex.Perms
import Annex.Link
import Logs.Location
import Logs.Trust
import Logs.Activity
import Logs.TimeStamp
import Annex.NumCopies
import Annex.UUID
import Utility.DataUnits
import Config
import Types.Key
import Types.CleanupActions
import Utility.HumanTime
import Utility.CopyFile
import Git.FilePath
import Utility.PID
import qualified Database.Fsck as FsckDb

import Data.Time.Clock.POSIX
import System.Posix.Types (EpochTime)

cmd :: Command
cmd = withGlobalOptions annexedMatchingOptions $
	command "fsck" SectionMaintenance
		"find and fix problems"
		paramPaths (seek <$$> optParser)

data FsckOptions = FsckOptions
	{ fsckFiles :: CmdParams
	, fsckFromOption :: Maybe (DeferredParse Remote)
	, incrementalOpt :: Maybe IncrementalOpt
	, keyOptions :: Maybe KeyOptions
	}

data IncrementalOpt
	= StartIncrementalO
	| MoreIncrementalO
	| ScheduleIncrementalO Duration

optParser :: CmdParamsDesc -> Parser FsckOptions
optParser desc = FsckOptions
	<$> cmdParams desc
	<*> optional (parseRemoteOption $ strOption 
		( long "from" <> short 'f' <> metavar paramRemote 
		<> help "check remote"
		))
	<*> optional parseincremental
	<*> optional (parseKeyOptions False)
  where
	parseincremental =
		flag' StartIncrementalO
			( long "incremental" <> short 'S'
			<> help "start an incremental fsck"
			)
		<|> flag' MoreIncrementalO
			( long "more" <> short 'm'
			<> help "continue an incremental fsck"
			)
		<|> (ScheduleIncrementalO <$> option (str >>= parseDuration)
			( long "incremental-schedule" <> metavar paramTime
			<> help "schedule incremental fscking"
			))

seek :: FsckOptions -> CommandSeek
seek o = do
	from <- maybe (pure Nothing) (Just <$$> getParsed) (fsckFromOption o)
	u <- maybe getUUID (pure . Remote.uuid) from
	i <- prepIncremental u (incrementalOpt o)
	withKeyOptions (keyOptions o) False
		(\k -> startKey i k =<< getNumCopies)
		(withFilesInGit $ whenAnnexed $ start from i)
		(fsckFiles o)
	withFsckDb i FsckDb.closeDb
	void $ tryIO $ recordActivity Fsck u

start :: Maybe Remote -> Incremental -> FilePath -> Key -> CommandStart
start from inc file key = do
	v <- Backend.getBackend file key
	case v of
		Nothing -> stop
		Just backend -> do
			numcopies <- getFileNumCopies file
			case from of
				Nothing -> go $ perform key file backend numcopies
				Just r -> go $ performRemote key file backend numcopies r
  where
	go = runFsck inc file key

perform :: Key -> FilePath -> Backend -> NumCopies -> Annex Bool
perform key file backend numcopies = check
	-- order matters
	[ fixLink key file
	, verifyLocationLog key file
	, verifyDirectMapping key file
	, verifyDirectMode key file
	, checkKeySize key
	, checkBackend backend key (Just file)
	, checkKeyNumCopies key (Just file) numcopies
	]

{- To fsck a remote, the content is retrieved to a tmp file,
 - and checked locally. -}
performRemote :: Key -> FilePath -> Backend -> NumCopies -> Remote -> Annex Bool
performRemote key file backend numcopies remote =
	dispatch =<< Remote.hasKey remote key
  where
	dispatch (Left err) = do
		showNote err
		return False
	dispatch (Right True) = withtmp $ \tmpfile -> do
		r <- getfile tmpfile
		case r of
			Nothing -> go True Nothing
			Just True -> go True (Just tmpfile)
			Just False -> do
				warning "failed to download file from remote"
				void $ go True Nothing
				return False
	dispatch (Right False) = go False Nothing
	go present localcopy = check
		[ verifyLocationLogRemote key file remote present
		, checkKeySizeRemote key remote localcopy
		, checkBackendRemote backend key remote localcopy
		, checkKeyNumCopies key (Just file) numcopies
		]
	withtmp a = do
		pid <- liftIO getPID
		t <- fromRepo gitAnnexTmpObjectDir
		createAnnexDirectory t
		let tmp = t </> "fsck" ++ show pid ++ "." ++ keyFile key
		let cleanup = liftIO $ catchIO (removeFile tmp) (const noop)
		cleanup
		cleanup `after` a tmp
	getfile tmp = ifM (checkDiskSpace (Just tmp) key 0 True)
		( ifM (Remote.retrieveKeyFileCheap remote key (Just file) tmp)
			( return (Just True)
			, ifM (Annex.getState Annex.fast)
				( return Nothing
				, Just <$>
					Remote.retrieveKeyFile remote key Nothing tmp dummymeter
				)
			)
		, return (Just False)
		)
	dummymeter _ = noop

startKey :: Incremental -> Key -> NumCopies -> CommandStart
startKey inc key numcopies =
	case Backend.maybeLookupBackendName (Types.Key.keyBackendName key) of
		Nothing -> stop
		Just backend -> runFsck inc (key2file key) key $
			performKey key backend numcopies

performKey :: Key -> Backend -> NumCopies -> Annex Bool
performKey key backend numcopies = check
	[ verifyLocationLog key (key2file key)
	, checkKeySize key
	, checkBackend backend key Nothing
	, checkKeyNumCopies key Nothing numcopies
	]

check :: [Annex Bool] -> Annex Bool
check cs = and <$> sequence cs

{- Checks that the file's link points correctly to the content.
 -
 - In direct mode, there is only a link when the content is not present.
 -}
fixLink :: Key -> FilePath -> Annex Bool
fixLink key file = do
	want <- calcRepo $ gitAnnexLink file key
	have <- getAnnexLinkTarget file
	maybe noop (go want) have
	return True
  where
	go want have
		| want /= fromInternalGitPath have = do
			showNote "fixing link"
			liftIO $ createDirectoryIfMissing True (parentDir file)
			liftIO $ removeFile file
			addAnnexLink want file
		| otherwise = noop

{- Checks that the location log reflects the current status of the key,
 - in this repository only. -}
verifyLocationLog :: Key -> String -> Annex Bool
verifyLocationLog key desc = do
	present <- inAnnex key
	direct <- isDirect
	u <- getUUID
	
	{- Since we're checking that a key's file is present, throw
	 - in a permission fixup here too. -}
	file <- calcRepo $ gitAnnexLocation key
	when (present && not direct) $
		freezeContent file
	whenM (liftIO $ doesDirectoryExist $ parentDir file) $
		freezeContentDir file

	{- In direct mode, modified files will show up as not present,
	 - but that is expected and not something to do anything about. -}
	if direct && not present
		then return True
		else verifyLocationLog' key desc present u (logChange key u)

verifyLocationLogRemote :: Key -> String -> Remote -> Bool -> Annex Bool
verifyLocationLogRemote key desc remote present =
	verifyLocationLog' key desc present (Remote.uuid remote)
		(Remote.logStatus remote key)

verifyLocationLog' :: Key -> String -> Bool -> UUID -> (LogStatus -> Annex ()) -> Annex Bool
verifyLocationLog' key desc present u updatestatus = do
	uuids <- Remote.keyLocations key
	case (present, u `elem` uuids) of
		(True, False) -> do
				fix InfoPresent
				-- There is no data loss, so do not fail.
				return True
		(False, True) -> do
				fix InfoMissing
				warning $
					"** Based on the location log, " ++ desc
					++ "\n** was expected to be present, " ++
					"but its content is missing."
				return False
		_ -> return True
  where
	fix s = do
		showNote "fixing location log"
		updatestatus s

{- Ensures the direct mode mapping file is consistent. Each file
 - it lists for the key should exist, and the specified file should be
 - included in it.
 -}
verifyDirectMapping :: Key -> FilePath -> Annex Bool
verifyDirectMapping key file = do
	whenM isDirect $ do
		fs <- addAssociatedFile key file
		forM_ fs $ \f -> 
			unlessM (liftIO $ doesFileExist f) $
				void $ removeAssociatedFile key f
	return True

{- Ensures that files whose content is available are in direct mode. -}
verifyDirectMode :: Key -> FilePath -> Annex Bool
verifyDirectMode key file = do
	whenM (isDirect <&&> isJust <$> isAnnexLink file) $ do
		v <- toDirectGen key file
		case v of
			Nothing -> noop
			Just a -> do
				showNote "fixing direct mode"
				a
	return True

{- The size of the data for a key is checked against the size encoded in
 - the key's metadata, if available.
 -
 - Not checked in direct mode, because files can be changed directly.
 -}
checkKeySize :: Key -> Annex Bool
checkKeySize key = ifM isDirect
	( return True
	, do
		file <- calcRepo $ gitAnnexLocation key
		ifM (liftIO $ doesFileExist file)
			( checkKeySizeOr badContent key file
			, return True
			)
	)

checkKeySizeRemote :: Key -> Remote -> Maybe FilePath -> Annex Bool
checkKeySizeRemote _ _ Nothing = return True
checkKeySizeRemote key remote (Just file) =
	checkKeySizeOr (badContentRemote remote file) key file

checkKeySizeOr :: (Key -> Annex String) -> Key -> FilePath -> Annex Bool
checkKeySizeOr bad key file = case Types.Key.keySize key of
	Nothing -> return True
	Just size -> do
		size' <- liftIO $ getFileSize file
		comparesizes size size'
  where
	comparesizes a b = do
		let same = a == b
		unless same $ badsize a b
		return same
	badsize a b = do
		msg <- bad key
		warning $ concat
			[ "Bad file size ("
			, compareSizes storageUnits True a b
			, "); "
			, msg
			]

{- Runs the backend specific check on a key's content.
 -
 - In direct mode this is not done if the file has clearly been modified,
 - because modification of direct mode files is allowed. It's still done
 - if the file does not appear modified, to catch disk corruption, etc.
 -}
checkBackend :: Backend -> Key -> Maybe FilePath -> Annex Bool
checkBackend backend key mfile = go =<< isDirect
  where
	go False = do
		content <- calcRepo $ gitAnnexLocation key
		checkBackendOr badContent backend key content
	go True = maybe nocheck checkdirect mfile
	checkdirect file = ifM (goodContent key file)
		( checkBackendOr' (badContentDirect file) backend key file
			(goodContent key file)
		, nocheck
		)
	nocheck = return True

checkBackendRemote :: Backend -> Key -> Remote -> Maybe FilePath -> Annex Bool
checkBackendRemote backend key remote = maybe (return True) go
  where
	go file = checkBackendOr (badContentRemote remote file) backend key file

checkBackendOr :: (Key -> Annex String) -> Backend -> Key -> FilePath -> Annex Bool
checkBackendOr bad backend key file =
	checkBackendOr' bad backend key file (return True)

checkBackendOr' :: (Key -> Annex String) -> Backend -> Key -> FilePath -> Annex Bool -> Annex Bool
checkBackendOr' bad backend key file postcheck =
	case Types.Backend.fsckKey backend of
		Nothing -> return True
		Just a -> do
			ok <- a key file
			ifM postcheck
				( do
					unless ok $ do
						msg <- bad key
						warning $ "Bad file content; " ++ msg
					return ok
				, return True
				)

checkKeyNumCopies :: Key -> AssociatedFile -> NumCopies -> Annex Bool
checkKeyNumCopies key afile numcopies = do
	let file = fromMaybe (key2file key) afile
	(untrustedlocations, safelocations) <- trustPartition UnTrusted =<< Remote.keyLocations key
	let present = NumCopies (length safelocations)
	if present < numcopies
		then ifM (pure (isNothing afile) <&&> checkDead key)
			( do
				showLongNote $ "This key is dead, skipping."
				return True
			, do
				ppuuids <- Remote.prettyPrintUUIDs "untrusted" untrustedlocations
				warning $ missingNote file present numcopies ppuuids
				when (fromNumCopies present == 0 && isNothing afile) $
					showLongNote "(Avoid this check by running: git annex dead --key )"
				return False
			)
		else return True

missingNote :: String -> NumCopies -> NumCopies -> String -> String
missingNote file (NumCopies 0) _ [] = 
		"** No known copies exist of " ++ file
missingNote file (NumCopies 0) _ untrusted =
		"Only these untrusted locations may have copies of " ++ file ++
		"\n" ++ untrusted ++
		"Back it up to trusted locations with git-annex copy."
missingNote file present needed [] =
		"Only " ++ show (fromNumCopies present) ++ " of " ++ show (fromNumCopies needed) ++ 
		" trustworthy copies exist of " ++ file ++
		"\nBack it up with git-annex copy."
missingNote file present needed untrusted = 
		missingNote file present needed [] ++
		"\nThe following untrusted locations may also have copies: " ++
		"\n" ++ untrusted

{- Bad content is moved aside. -}
badContent :: Key -> Annex String
badContent key = do
	dest <- moveBad key
	return $ "moved to " ++ dest

{- Bad content is left where it is, but we touch the file, so it'll be
 - committed to a new key. -}
badContentDirect :: FilePath -> Key -> Annex String
badContentDirect file key = do
	void $ liftIO $ catchMaybeIO $ touchFile file
	logStatus key InfoMissing
	return "left in place for you to examine"

{- Bad content is dropped from the remote. We have downloaded a copy
 - from the remote to a temp file already (in some cases, it's just a
 - symlink to a file in the remote). To avoid any further data loss,
 - that temp file is moved to the bad content directory unless 
 - the local annex has a copy of the content. -}
badContentRemote :: Remote -> FilePath -> Key -> Annex String
badContentRemote remote localcopy key = do
	bad <- fromRepo gitAnnexBadDir
	let destbad = bad </> key2file key
	movedbad <- ifM (inAnnex key <||> liftIO (doesFileExist destbad))
		( return False
		, do
			createAnnexDirectory (parentDir destbad)
			liftIO $ catchDefaultIO False $
				ifM (isSymbolicLink <$> getSymbolicLinkStatus localcopy)
					( copyFileExternal CopyTimeStamps localcopy destbad
					, do
						moveFile localcopy destbad
						return True
					)
		)

	dropped <- Remote.removeKey remote key
	when dropped $
		Remote.logStatus remote key InfoMissing
	return $ case (movedbad, dropped) of
		(True, True) -> "moved from " ++ Remote.name remote ++
			" to " ++ destbad
		(False, True) -> "dropped from " ++ Remote.name remote
		(_, False) -> "failed to drop from" ++ Remote.name remote

runFsck :: Incremental -> FilePath -> Key -> Annex Bool -> CommandStart
runFsck inc file key a = ifM (needFsck inc key)
	( do
		showStart "fsck" file
		next $ do
			ok <- a
			when ok $
				recordFsckTime inc key
			next $ return ok
	, stop
	)

{- Check if a key needs to be fscked, with support for incremental fscks. -}
needFsck :: Incremental -> Key -> Annex Bool
needFsck (ContIncremental h) key = liftIO $ not <$> FsckDb.inDb h key
needFsck _ _ = return True

withFsckDb :: Incremental -> (FsckDb.FsckHandle -> Annex ()) -> Annex ()
withFsckDb (ContIncremental h) a = a h
withFsckDb (StartIncremental h) a = a h
withFsckDb NonIncremental _ = noop

recordFsckTime :: Incremental -> Key -> Annex ()
recordFsckTime inc key = withFsckDb inc $ \h -> liftIO $ FsckDb.addDb h key

{- Records the start time of an incremental fsck.
 -
 - To guard against time stamp damange (for example, if an annex directory
 - is copied without -a), the fsckstate file contains a time that should
 - be identical to its modification time.
 - (This is not possible to do on Windows, and so the timestamp in
 - the file will only be equal or greater than the modification time.)
 -}
recordStartTime :: UUID -> Annex ()
recordStartTime u = do
	f <- fromRepo (gitAnnexFsckState u)
	createAnnexDirectory $ parentDir f
	liftIO $ do
		nukeFile f
		withFile f WriteMode $ \h -> do
#ifndef mingw32_HOST_OS
			t <- modificationTime <$> getFileStatus f
#else
			t <- getPOSIXTime
#endif
			hPutStr h $ showTime $ realToFrac t
  where
	showTime :: POSIXTime -> String
	showTime = show

resetStartTime :: UUID -> Annex ()
resetStartTime u = liftIO . nukeFile =<< fromRepo (gitAnnexFsckState u)

{- Gets the incremental fsck start time. -}
getStartTime :: UUID -> Annex (Maybe EpochTime)
getStartTime u = do
	f <- fromRepo (gitAnnexFsckState u)
	liftIO $ catchDefaultIO Nothing $ do
		timestamp <- modificationTime <$> getFileStatus f
		let fromstatus = Just (realToFrac timestamp)
		fromfile <- parsePOSIXTime <$> readFile f
		return $ if matchingtimestamp fromfile fromstatus
			then Just timestamp
			else Nothing
  where
	matchingtimestamp fromfile fromstatus =
#ifndef mingw32_HOST_OS
		fromfile == fromstatus
#else
		fromfile >= fromstatus
#endif

data Incremental = StartIncremental FsckDb.FsckHandle | ContIncremental FsckDb.FsckHandle | NonIncremental

prepIncremental :: UUID -> Maybe IncrementalOpt -> Annex Incremental
prepIncremental _ Nothing = pure NonIncremental
prepIncremental u (Just StartIncrementalO) = do
	recordStartTime u
	ifM (FsckDb.newPass u)
		( StartIncremental <$> FsckDb.openDb u
		, error "Cannot start a new --incremental fsck pass; another fsck process is already running."
		)
prepIncremental u (Just MoreIncrementalO) =
	ContIncremental <$> FsckDb.openDb u
prepIncremental u (Just (ScheduleIncrementalO delta)) = do
	Annex.addCleanup FsckCleanup $ do
		v <- getStartTime u
		case v of
			Nothing -> noop
			Just started -> do
				now <- liftIO getPOSIXTime
				when (now - realToFrac started >= durationToPOSIXTime delta) $
					resetStartTime u
	started <- getStartTime u
	prepIncremental u $ Just $ case started of
		Nothing -> StartIncrementalO
		Just _ -> MoreIncrementalO
