{- git-annex key/value backends
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend (
	BackendFile,
	list,
	orderedList,
	genKey,
	lookupFile,
	chooseBackends,
	lookupBackendName,
	maybeLookupBackendName
) where

import System.IO.Error (try)
import System.Posix.Files

import Common.Annex
import qualified Git
import qualified Annex
import Types.Key
import qualified Types.Backend as B

-- When adding a new backend, import it here and add it to the list.
import qualified Backend.WORM
import qualified Backend.SHA
import qualified Backend.URL

list :: [Backend Annex]
list = Backend.WORM.backends ++ Backend.SHA.backends ++ Backend.URL.backends

{- List of backends in the order to try them when storing a new key. -}
orderedList :: Annex [Backend Annex]
orderedList = do
	l <- Annex.getState Annex.backends -- list is cached here
	if not $ null l
		then return l
		else do
			s <- getstandard
			d <- Annex.getState Annex.forcebackend
			handle d s
	where
		parseBackendList [] = list
		parseBackendList s = map lookupBackendName $ words s
		handle Nothing s = return s
		handle (Just "") s = return s
		handle (Just name) s = do
			let l' = lookupBackendName name : s
			Annex.changeState $ \state -> state { Annex.backends = l' }
			return l'
		getstandard = do
			g <- gitRepo
			return $ parseBackendList $
				Git.configGet g "annex.backends" ""

{- Generates a key for a file, trying each backend in turn until one
 - accepts it. -}
genKey :: FilePath -> Maybe (Backend Annex) -> Annex (Maybe (Key, Backend Annex))
genKey file trybackend = do
	bs <- orderedList
	let bs' = maybe bs (: bs) trybackend
	genKey' bs' file
genKey' :: [Backend Annex] -> FilePath -> Annex (Maybe (Key, Backend Annex))
genKey' [] _ = return Nothing
genKey' (b:bs) file = do
	r <- (B.getKey b) file
	case r of
		Nothing -> genKey' bs file
		Just k -> return $ Just (k, b)

{- Looks up the key and backend corresponding to an annexed file,
 - by examining what the file symlinks to. -}
lookupFile :: FilePath -> Annex (Maybe (Key, Backend Annex))
lookupFile file = do
	tl <- liftIO $ try getsymlink
	case tl of
		Left _ -> return Nothing
		Right l -> makekey l
	where
		getsymlink = takeFileName <$> readSymbolicLink file
		makekey l = maybe (return Nothing) (makeret l) (fileKey l)
		makeret l k =
			case maybeLookupBackendName bname of
					Just backend -> return $ Just (k, backend)
					Nothing -> do
						when (isLinkToAnnex l) $
							warning skip
						return Nothing
			where
				bname = keyBackendName k
				skip = "skipping " ++ file ++ 
					" (unknown backend " ++ bname ++ ")"

type BackendFile = (Maybe (Backend Annex), FilePath)

{- Looks up the backends that should be used for each file in a list.
 - That can be configured on a per-file basis in the gitattributes file.
 -}
chooseBackends :: [FilePath] -> Annex [BackendFile]
chooseBackends fs = do
	g <- gitRepo
	forced <- Annex.getState Annex.forcebackend
	if isJust forced
		then do
			l <- orderedList
			return $ map (\f -> (Just $ head l, f)) fs
		else do
			pairs <- liftIO $ Git.checkAttr g "annex.backend" fs
			return $ map (\(f,b) -> (maybeLookupBackendName b, f)) pairs

{- Looks up a backend by name. May fail if unknown. -}
lookupBackendName :: String -> Backend Annex
lookupBackendName s = fromMaybe unknown $ maybeLookupBackendName s
	where
		unknown = error $ "unknown backend " ++ s
maybeLookupBackendName :: String -> Maybe (Backend Annex)
maybeLookupBackendName s
	| length matches == 1 = Just $ head matches
	| otherwise = Nothing
	where matches = filter (\b -> s == B.name b) list
