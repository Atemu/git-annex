{- git-annex key/value backends
 -
 - Copyright 2010,2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend (
	list,
	orderedList,
	genKey,
	lookupFile,
	isAnnexLink,
	makeAnnexLink,
	chooseBackend,
	lookupBackendName,
	maybeLookupBackendName
) where

import System.Posix.Files

import Common.Annex
import qualified Annex
import Annex.CheckAttr
import Annex.CatFile
import Types.Key
import Types.KeySource
import qualified Types.Backend as B
import Config

-- When adding a new backend, import it here and add it to the list.
import qualified Backend.SHA
import qualified Backend.WORM
import qualified Backend.URL

list :: [Backend]
list = Backend.SHA.backends ++ Backend.WORM.backends ++ Backend.URL.backends

{- List of backends in the order to try them when storing a new key. -}
orderedList :: Annex [Backend]
orderedList = do
	l <- Annex.getState Annex.backends -- list is cached here
	if not $ null l
		then return l
		else do
			f <- Annex.getState Annex.forcebackend
			case f of
				Just name | not (null name) ->
					return [lookupBackendName name]
				_ -> do
					l' <- gen . annexBackends <$> Annex.getGitConfig
					Annex.changeState $ \s -> s { Annex.backends = l' }
					return l'
  where
	gen [] = list
	gen l = map lookupBackendName l

{- Generates a key for a file, trying each backend in turn until one
 - accepts it. -}
genKey :: KeySource -> Maybe Backend -> Annex (Maybe (Key, Backend))
genKey source trybackend = do
	bs <- orderedList
	let bs' = maybe bs (: bs) trybackend
	genKey' bs' source
genKey' :: [Backend] -> KeySource -> Annex (Maybe (Key, Backend))
genKey' [] _ = return Nothing
genKey' (b:bs) source = do
	r <- B.getKey b source
	case r of
		Nothing -> genKey' bs source
		Just k -> return $ Just (makesane k, b)
  where
	-- keyNames should not contain newline characters.
	makesane k = k { keyName = map fixbadchar (keyName k) }
	fixbadchar c
		| c == '\n' = '_'
		| otherwise = c

{- Looks up the key and backend corresponding to an annexed file,
 - by examining what the file symlinks to.
 -
 - In direct mode, there is often no symlink on disk, in which case
 - the symlink is looked up in git instead. However, a real symlink
 - on disk still takes precedence over what was committed to git in direct
 - mode.
 -
 - On a filesystem that does not support symlinks, git will instead store
 - the symlink target in a regular file.
 -}
lookupFile :: FilePath -> Annex (Maybe (Key, Backend))
lookupFile file = do
	mkey <- isAnnexLink file
	case mkey of
		Just key -> makeret key
		Nothing -> ifM isDirect
			( maybe (return Nothing) makeret =<< catKeyFile file
			, return Nothing
			)
  where
	makeret k = let bname = keyBackendName k in
		case maybeLookupBackendName bname of
			Just backend -> do
				return $ Just (k, backend)
			Nothing -> do
				warning $
					"skipping " ++ file ++
					" (unknown backend " ++ bname ++ ")"
				return Nothing

{- Checks if a file is a symlink to a key.
 -
 - On a filesystem that does not support symlinks, git will instead store
 - the symlink target in a regular file. (Only look at first 8k of file,
 - more than enough for any symlink target.)
 -}
isAnnexLink :: FilePath -> Annex (Maybe Key)
isAnnexLink file = maybe Nothing makekey <$> gettarget
  where
	gettarget = ifM (coreSymlinks <$> Annex.getGitConfig)
		( liftIO $ catchMaybeIO $ readSymbolicLink file
		, liftIO $ catchMaybeIO $ take 8192 <$> readFile file
		)
	makekey l
		| isLinkToAnnex l = fileKey $ takeFileName l
		| otherwise = Nothing

{- Creates a symlink on disk.
 -
 - On a filesystem that does not support symlinks, writes the link target
 - to a file. Note that git will only treat the file as a symlink if
 - it's staged as such.
 -}
makeAnnexLink :: String -> FilePath -> Annex ()
makeAnnexLink linktarget file = ifM (coreSymlinks <$> Annex.getGitConfig)
	( liftIO $ createSymbolicLink linktarget file
	, liftIO $ writeFile file linktarget
	)

{- Looks up the backend that should be used for a file.
 - That can be configured on a per-file basis in the gitattributes file. -}
chooseBackend :: FilePath -> Annex (Maybe Backend)
chooseBackend f = Annex.getState Annex.forcebackend >>= go
  where
	go Nothing =  maybeLookupBackendName <$> checkAttr "annex.backend" f
	go (Just _) = Just . Prelude.head <$> orderedList

{- Looks up a backend by name. May fail if unknown. -}
lookupBackendName :: String -> Backend
lookupBackendName s = fromMaybe unknown $ maybeLookupBackendName s
  where
	unknown = error $ "unknown backend " ++ s
maybeLookupBackendName :: String -> Maybe Backend
maybeLookupBackendName s = headMaybe matches
  where
	matches = filter (\b -> s == B.name b) list
