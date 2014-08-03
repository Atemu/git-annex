{- WebDAV remotes.
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ScopedTypeVariables #-}

module Remote.WebDAV (remote, davCreds, configUrl) where

import Network.Protocol.HTTP.DAV
import qualified Data.Map as M
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString.Lazy.UTF8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Control.Exception as E
import qualified Control.Exception.Lifted as EL
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Types
import System.Log.Logger (debugM)
import System.IO.Error

import Common.Annex
import Types.Remote
import qualified Git
import Config
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.Encryptable
import qualified Remote.Helper.Chunked.Legacy as Legacy
import Crypto
import Creds
import Utility.Metered
import Annex.Content
import Annex.UUID
import Remote.WebDAV.DavUrl

type DavUser = B8.ByteString
type DavPass = B8.ByteString

remote :: RemoteType
remote = RemoteType {
	typename = "webdav",
	enumerate = findSpecialRemotes "webdav",
	generate = gen,
	setup = webdavSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = new <$> remoteCost gc expensiveRemoteCost
  where
	new cst = Just $ encryptableRemote c
		(storeEncrypted this)
		(retrieveEncrypted this)
		this
	  where
		this = Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
			storeKey = store this,
			retrieveKeyFile = retrieve this,
			retrieveKeyFileCheap = retrieveCheap this,
			removeKey = remove this,
			hasKey = checkPresent this,
			hasKeyCheap = False,
			whereisKey = Nothing,
			remoteFsck = Nothing,
			repairRepo = Nothing,
			config = c,
			repo = r,
			gitconfig = gc,
			localpath = Nothing,
			readonly = False,
			availability = GloballyAvailable,
			remotetype = remote
		}

webdavSetup :: Maybe UUID -> Maybe CredPair -> RemoteConfig -> Annex (RemoteConfig, UUID)
webdavSetup mu mcreds c = do
	u <- maybe (liftIO genUUID) return mu
	let url = fromMaybe (error "Specify url=") $
		M.lookup "url" c
	c' <- encryptionSetup c
	creds <- maybe (getCreds c' u) (return . Just) mcreds
	testDav url creds
	gitConfigSpecialRemote u c' "webdav" "true"
	c'' <- setRemoteCredPair c' (davCreds u) creds
	return (c'', u)

store :: Remote -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store r k _f p = metered (Just p) k $ \meterupdate ->
	davAction r False $ \(baseurl, user, pass) -> 
		sendAnnex k (void $ remove r k) $ \src ->
			liftIO $ withMeteredFile src meterupdate $
				storeHelper r k baseurl user pass

storeEncrypted :: Remote -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted r (cipher, enck) k p = metered (Just p) k $ \meterupdate ->
	davAction r False $ \(baseurl, user, pass) ->
		sendAnnex k (void $ remove r enck) $ \src ->
			liftIO $ encrypt (getGpgEncParams r) cipher
				(streamMeteredFile src meterupdate) $
				readBytes $ storeHelper r enck baseurl user pass

storeHelper :: Remote -> Key -> DavUrl -> DavUser -> DavPass -> L.ByteString -> IO Bool
storeHelper r k baseurl user pass b = catchBoolIO $ do
	mkdirRecursiveDAV tmpurl user pass
	case chunkconfig of
		NoChunks -> flip catchNonAsync (\e -> warningIO (show e) >> return False) $ do
			storehttp tmpurl b
			finalizer tmpurl keyurl
			return True
		UnpaddedChunks _ -> error "TODO: storeHelper with UnpaddedChunks"
		LegacyChunks chunksize -> do
			let storer urls = Legacy.storeChunked chunksize urls storehttp b
			let recorder url s = storehttp url (L8.fromString s)
			Legacy.storeChunks k tmpurl keyurl storer recorder finalizer

  where
	tmpurl = tmpLocation baseurl k
	keyurl = davLocation baseurl k
	chunkconfig = getChunkConfig $ config r
	finalizer srcurl desturl = do
		void $ tryNonAsync (deleteDAV desturl user pass)
		mkdirRecursiveDAV (urlParent desturl) user pass
		moveDAV srcurl desturl user pass
	storehttp url = putDAV url user pass

retrieveCheap :: Remote -> Key -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

retrieve :: Remote -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
retrieve r k _f d p = metered (Just p) k $ \meterupdate ->
	davAction r False $ \(baseurl, user, pass) -> liftIO $ catchBoolIO $
		withStoredFiles r k baseurl user pass onerr $ \urls -> do
			Legacy.meteredWriteFileChunks meterupdate d urls $ \url -> do
				mb <- getDAV url user pass
				case mb of
					Nothing -> throwIO "download failed"
					Just b -> return b
			return True
  where
	onerr _ = return False

retrieveEncrypted :: Remote -> (Cipher, Key) -> Key -> FilePath -> MeterUpdate -> Annex Bool
retrieveEncrypted r (cipher, enck) k d p = metered (Just p) k $ \meterupdate ->
	davAction r False $ \(baseurl, user, pass) -> liftIO $ catchBoolIO $
		withStoredFiles r enck baseurl user pass onerr $ \urls -> do
			decrypt cipher (feeder user pass urls) $
				readBytes $ meteredWriteFile meterupdate d
			return True
  where
	onerr _ = return False

	feeder _ _ [] _ = noop
	feeder user pass (url:urls) h = do
		mb <- getDAV url user pass
		case mb of
			Nothing -> throwIO "download failed"
			Just b -> do
				L.hPut h b
				feeder user pass urls h

remove :: Remote -> Key -> Annex Bool
remove r k = davAction r False $ \(baseurl, user, pass) -> liftIO $ do
	-- Delete the key's whole directory, including any chunked
	-- files, etc, in a single action.
	let url = davLocation baseurl k
	isJust . eitherToMaybe <$> tryNonAsync (deleteDAV url user pass)

checkPresent :: Remote -> Key -> Annex (Either String Bool)
checkPresent r k = davAction r noconn go
  where
	noconn = Left $ error $ name r ++ " not configured"

	go (baseurl, user, pass) = do
		showAction $ "checking " ++ name r
		liftIO $ withStoredFiles r k baseurl user pass onerr check
	  where
		check [] = return $ Right True
		check (url:urls) = do
			v <- existsDAV url user pass
			if v == Right True
				then check urls
				else return v

		{- Failed to read the chunkcount file; see if it's missing,
		 - or if there's a problem accessing it,
		 - or perhaps this was an intermittent error. -}
		onerr url = do
			v <- existsDAV url user pass
			return $ if v == Right True
				then Left $ "failed to read " ++ url
				else v

withStoredFiles
	:: Remote
	-> Key
	-> DavUrl
	-> DavUser
	-> DavPass
	-> (DavUrl -> IO a)
	-> ([DavUrl] -> IO a)
	-> IO a
withStoredFiles r k baseurl user pass onerr a = case chunkconfig of
	NoChunks -> a [keyurl]
	UnpaddedChunks _ -> error "TODO: withStoredFiles with UnpaddedChunks"
	LegacyChunks _ -> do
		let chunkcount = keyurl ++ Legacy.chunkCount
		v <- getDAV chunkcount user pass
		case v of
			Just s -> a $ Legacy.listChunks keyurl $ L8.toString s
			Nothing -> do
				chunks <- Legacy.probeChunks keyurl $ \u -> (== Right True) <$> existsDAV u user pass
				if null chunks
					then onerr chunkcount
					else a chunks
  where
	keyurl = davLocation baseurl k ++ keyFile k
	chunkconfig = getChunkConfig $ config r

davAction :: Remote -> a -> ((DavUrl, DavUser, DavPass) -> Annex a) -> Annex a
davAction r unconfigured action = do
	mcreds <- getCreds (config r) (uuid r)
	case (mcreds, configUrl r) of
		(Just (user, pass), Just url) ->
			action (url, toDavUser user, toDavPass pass)
		_ -> return unconfigured

configUrl :: Remote -> Maybe DavUrl
configUrl r = fixup <$> M.lookup "url" (config r)
  where
	-- box.com DAV url changed
	fixup = replace "https://www.box.com/dav/" "https://dav.box.com/dav/"

toDavUser :: String -> DavUser
toDavUser = B8.fromString

toDavPass :: String -> DavPass
toDavPass = B8.fromString

{- Creates a directory in WebDAV, if not already present; also creating
 - any missing parent directories. -}
mkdirRecursiveDAV :: DavUrl -> DavUser -> DavPass -> IO ()
mkdirRecursiveDAV url user pass = go url
  where
	make u = mkdirDAV u user pass

	go u = do
		r <- E.try (make u) :: IO (Either E.SomeException Bool)
		case r of
			{- Parent directory is missing. Recurse to create
			 - it, and try once more to create the directory. -}
			Right False -> do
				go (urlParent u)
				void $ make u
			{- Directory created successfully -}
			Right True -> return ()
			{- Directory already exists, or some other error
			 - occurred. In the latter case, whatever wanted
			 - to use this directory will fail. -}
			Left _ -> return ()

{- Test if a WebDAV store is usable, by writing to a test file, and then
 - deleting the file. Exits with an IO error if not. -}
testDav :: String -> Maybe CredPair -> Annex ()
testDav baseurl (Just (u, p)) = do
	showSideAction "testing WebDAV server"
	test "make directory" $ mkdirRecursiveDAV baseurl user pass
	test "write file" $ putDAV testurl user pass L.empty
	test "delete file" $ deleteDAV testurl user pass
  where
	test desc a = liftIO $
		either (\e -> throwIO $ "WebDAV failed to " ++ desc ++ ": " ++ show e)
			(const noop)
			=<< tryNonAsync a

	user = toDavUser u
	pass = toDavPass p
	testurl = davUrl baseurl "git-annex-test"
testDav _ Nothing = error "Need to configure webdav username and password."

getCreds :: RemoteConfig -> UUID -> Annex (Maybe CredPair)
getCreds c u = getRemoteCredPairFor "webdav" c (davCreds u)

davCreds :: UUID -> CredPairStorage
davCreds u = CredPairStorage
	{ credPairFile = fromUUID u
	, credPairEnvironment = ("WEBDAV_USERNAME", "WEBDAV_PASSWORD")
	, credPairRemoteKey = Just "davcreds"
	}

{- Content-Type to use for files uploaded to WebDAV. -}
contentType :: Maybe B8.ByteString
contentType = Just $ B8.fromString "application/octet-stream"

throwIO :: String -> IO a
throwIO msg = ioError $ mkIOError userErrorType msg Nothing Nothing

debugDAV :: DavUrl -> String -> IO ()
debugDAV msg url = debugM "DAV" $ msg ++ " " ++ url

{---------------------------------------------------------------------
 - Low-level DAV operations.
 ---------------------------------------------------------------------}

putDAV :: DavUrl -> DavUser -> DavPass -> L.ByteString -> IO ()
putDAV url user pass b = do
	debugDAV "PUT" url
	goDAV url user pass $ putContentM (contentType, b)

getDAV :: DavUrl -> DavUser -> DavPass -> IO (Maybe L.ByteString)
getDAV url user pass = do
	debugDAV "GET" url
	eitherToMaybe <$> tryNonAsync go
  where
	go = goDAV url user pass $ snd <$> getContentM

deleteDAV :: DavUrl -> DavUser -> DavPass -> IO ()
deleteDAV url user pass = do
	debugDAV "DELETE" url
	goDAV url user pass delContentM

moveDAV :: DavUrl -> DavUrl -> DavUser -> DavPass -> IO ()
moveDAV url newurl user pass = do
	debugDAV ("MOVE to " ++ newurl ++ " from ") url
	goDAV url user pass $ moveContentM newurl'
  where
	newurl' = B8.fromString newurl

mkdirDAV :: DavUrl -> DavUser -> DavPass -> IO Bool
mkdirDAV url user pass = do
	debugDAV "MKDIR" url
	goDAV url user pass mkCol

existsDAV :: DavUrl -> DavUser -> DavPass -> IO (Either String Bool)
existsDAV url user pass = do
	debugDAV "EXISTS" url
	either (Left . show) id <$> tryNonAsync check
  where
	ispresent = return . Right
	check = goDAV url user pass $ do
		setDepth Nothing
		EL.catchJust
			(matchStatusCodeException notFound404)
			(getPropsM >> ispresent True)
			(const $ ispresent False)

matchStatusCodeException :: Status -> HttpException -> Maybe ()
matchStatusCodeException want (StatusCodeException s _ _)
	| s == want = Just ()
	| otherwise = Nothing
matchStatusCodeException _ _ = Nothing

goDAV :: DavUrl -> DavUser -> DavPass -> DAVT IO a -> IO a
goDAV url user pass a = choke $ evalDAVT url $ do
	setResponseTimeout Nothing -- disable default (5 second!) timeout
	setCreds user pass
	a
  where
	choke :: IO (Either String a) -> IO a
	choke f = do
		x <- f
		case x of
			Left e -> error e
			Right r -> return r
