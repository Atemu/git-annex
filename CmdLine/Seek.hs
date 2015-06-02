{- git-annex command seeking
 - 
 - These functions find appropriate files or other things based on
 - the values a user passes to a command, and prepare actions operating
 - on them.
 -
 - Copyright 2010-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine.Seek where

import Common.Annex
import Types.Command
import Types.Key
import Types.FileMatcher
import qualified Annex
import qualified Git
import qualified Git.Command
import qualified Git.LsFiles as LsFiles
import qualified Git.LsTree as LsTree
import Git.FilePath
import qualified Limit
import CmdLine.Option
import CmdLine.Action
import Logs.Location
import Logs.Unused
import Annex.CatFile
import Annex.Content

withFilesInGit :: (FilePath -> CommandStart) -> CommandSeek
withFilesInGit a params = seekActions $ prepFiltered a $
	seekHelper LsFiles.inRepo params

withFilesInGitNonRecursive :: (FilePath -> CommandStart) -> CommandSeek
withFilesInGitNonRecursive a params = ifM (Annex.getState Annex.force)
	( withFilesInGit a params
	, if null params
		then needforce
		else seekActions $ prepFiltered a (getfiles [] params)
	)
  where
	getfiles c [] = return (reverse c)
	getfiles c (p:ps) = do
		(fs, cleanup) <- inRepo $ LsFiles.inRepo [p]
		case fs of
			[f] -> do
				void $ liftIO $ cleanup
				getfiles (f:c) ps
			[] -> do
				void $ liftIO $ cleanup
				getfiles c ps
			_ -> needforce
	needforce = error "Not recursively setting metadata. Use --force to do that."

withFilesNotInGit :: Bool -> (FilePath -> CommandStart) -> CommandSeek
withFilesNotInGit skipdotfiles a params
	| skipdotfiles = do
		{- dotfiles are not acted on unless explicitly listed -}
		files <- filter (not . dotfile) <$>
			seekunless (null ps && not (null params)) ps
		dotfiles <- seekunless (null dotps) dotps
		go (files++dotfiles)
	| otherwise = go =<< seekunless False params
  where
	(dotps, ps) = partition dotfile params
	seekunless True _ = return []
	seekunless _ l = do
		force <- Annex.getState Annex.force
		g <- gitRepo
		liftIO $ Git.Command.leaveZombie <$> LsFiles.notInRepo force l g
	go l = seekActions $ prepFiltered a $
		return $ concat $ segmentPaths params l

withFilesInRefs :: (FilePath -> Key -> CommandStart) -> CommandSeek
withFilesInRefs a = mapM_ go
  where
	go r = do	
		matcher <- Limit.getMatcher
		l <- inRepo $ LsTree.lsTree (Git.Ref r)
		forM_ l $ \i -> do
			let f = getTopFilePath $ LsTree.file i
			v <- catKey (Git.Ref $ LsTree.sha i) (LsTree.mode i)
			case v of
				Nothing -> noop
				Just k -> whenM (matcher $ MatchingKey k) $
					commandAction $ a f k

withPathContents :: ((FilePath, FilePath) -> CommandStart) -> CommandSeek
withPathContents a params = do
	matcher <- Limit.getMatcher
	seekActions $ map a <$> (filterM (checkmatch matcher) =<< ps)
  where
	ps = concat <$> liftIO (mapM get params)
	get p = ifM (isDirectory <$> getFileStatus p)
		( map (\f -> (f, makeRelative (parentDir p) f))
			<$> dirContentsRecursiveSkipping (".git" `isSuffixOf`) True p
		, return [(p, takeFileName p)]
		)
	checkmatch matcher (f, relf) = matcher $ MatchingFile $ FileInfo
		{ currFile = f
		, matchFile = relf
		}

withWords :: ([String] -> CommandStart) -> CommandSeek
withWords a params = seekActions $ return [a params]

withStrings :: (String -> CommandStart) -> CommandSeek
withStrings a params = seekActions $ return $ map a params

withPairs :: ((String, String) -> CommandStart) -> CommandSeek
withPairs a params = seekActions $ return $ map a $ pairs [] params
  where
	pairs c [] = reverse c
	pairs c (x:y:xs) = pairs ((x,y):c) xs
	pairs _ _ = error "expected pairs"

withFilesToBeCommitted :: (String -> CommandStart) -> CommandSeek
withFilesToBeCommitted a params = seekActions $ prepFiltered a $
	seekHelper LsFiles.stagedNotDeleted params

withFilesUnlocked :: (FilePath -> CommandStart) -> CommandSeek
withFilesUnlocked = withFilesUnlocked' LsFiles.typeChanged

withFilesUnlockedToBeCommitted :: (FilePath -> CommandStart) -> CommandSeek
withFilesUnlockedToBeCommitted = withFilesUnlocked' LsFiles.typeChangedStaged

{- Unlocked files have changed type from a symlink to a regular file.
 -
 - Furthermore, unlocked files used to be a git-annex symlink,
 - not some other sort of symlink.
 -}
withFilesUnlocked' :: ([FilePath] -> Git.Repo -> IO ([FilePath], IO Bool)) -> (FilePath -> CommandStart) -> CommandSeek
withFilesUnlocked' typechanged a params = seekActions $
	prepFiltered a unlockedfiles
  where
	unlockedfiles = filterM isUnlocked =<< seekHelper typechanged params

isUnlocked :: FilePath -> Annex Bool
isUnlocked f = liftIO (notSymlink f) <&&> 
	(isJust <$> catKeyFile f <||> isJust <$> catKeyFileHEAD f)

{- Finds files that may be modified. -}
withFilesMaybeModified :: (FilePath -> CommandStart) -> CommandSeek
withFilesMaybeModified a params = seekActions $
	prepFiltered a $ seekHelper LsFiles.modified params

withKeys :: (Key -> CommandStart) -> CommandSeek
withKeys a params = seekActions $ return $ map (a . parse) params
  where
	parse p = fromMaybe (error "bad key") $ file2key p

{- Gets the value of a field options, which is fed into
 - a conversion function.
 -}
getOptionField :: Option -> (Maybe String -> Annex a) -> Annex a
getOptionField option converter = converter <=< Annex.getField $ optionName option

getOptionFlag :: Option -> Annex Bool
getOptionFlag option = Annex.getFlag (optionName option)

withNothing :: CommandStart -> CommandSeek
withNothing a [] = seekActions $ return [a]
withNothing _ _ = error "This command takes no parameters."

{- Handles the --all, --unused, --key, and --incomplete options,
 - which specify particular keys to run an action on.
 -
 - In a bare repo, --all is the default.
 -
 - Otherwise falls back to a regular CommandSeek action on
 - whatever params were passed. -}
withKeyOptions :: Bool -> (Key -> CommandStart) -> CommandSeek -> CommandSeek
withKeyOptions auto keyop fallbackop params = do
	bare <- fromRepo Git.repoIsLocalBare
	allkeys <- Annex.getFlag "all"
	unused <- Annex.getFlag "unused"
	incomplete <- Annex.getFlag "incomplete"
	specifickey <- Annex.getField "key"
	when (auto && bare) $
		error "Cannot use --auto in a bare repository"
	case	(allkeys, unused, incomplete, null params, specifickey) of
		(False  , False , False     , True       , Nothing)
			| bare -> go auto loggedKeys
			| otherwise -> fallbackop params
		(False  , False , False     , _          , Nothing) -> fallbackop params
		(True   , False , False     , True       , Nothing) -> go auto loggedKeys
		(False  , True  , False     , True       , Nothing) -> go auto unusedKeys'
		(False  , False , True      , True       , Nothing) -> go auto incompletekeys
		(False  , False , False     , True       , Just ks) -> case file2key ks of
			Nothing -> error "Invalid key"
			Just k -> go auto $ return [k]
		_ -> error "Can only specify one of file names, --all, --unused, --key, or --incomplete"
  where
	go True _ = error "Cannot use --auto with --all or --unused or --key or --incomplete"
	go False a = do
		matcher <- Limit.getMatcher
		seekActions $ map (process matcher) <$> a
	process matcher k = ifM (matcher $ MatchingKey k)
		( keyop k , return Nothing)
	incompletekeys = staleKeysPrune gitAnnexTmpObjectDir True

prepFiltered :: (FilePath -> CommandStart) -> Annex [FilePath] -> Annex [CommandStart]
prepFiltered a fs = do
	matcher <- Limit.getMatcher
	map (process matcher) <$> fs
  where
	process matcher f = ifM (matcher $ MatchingFile $ FileInfo f f)
		( a f , return Nothing )

seekActions :: Annex [CommandStart] -> Annex ()
seekActions gen = do
	as <- gen
	mapM_ commandAction as

seekHelper :: ([FilePath] -> Git.Repo -> IO ([FilePath], IO Bool)) -> [FilePath] -> Annex [FilePath]
seekHelper a params = do
	ll <- inRepo $ \g -> concat <$> forM (segmentXargsOrdered params)
		(runSegmentPaths (\fs -> Git.Command.leaveZombie <$> a fs g))
	forM_ (map fst $ filter (null . snd) $ zip params ll) $ \p ->
		unlessM (isJust <$> liftIO (catchMaybeIO $ getSymbolicLinkStatus p)) $ do
			toplevelWarning False (p ++ " not found")
			Annex.incError
	return $ concat ll

notSymlink :: FilePath -> IO Bool
notSymlink f = liftIO $ not . isSymbolicLink <$> getSymbolicLinkStatus f
