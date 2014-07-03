{- git-annex command
 -
 - Copyright 2011-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.AddUrl where

import Network.URI

import Common.Annex
import Command
import Backend
import qualified Command.Add
import qualified Annex
import qualified Annex.Queue
import qualified Annex.Url as Url
import qualified Backend.URL
import Annex.Content
import Logs.Web
import Types.Key
import Types.KeySource
import Config
import Annex.Content.Direct
import Logs.Location
import qualified Annex.Transfer as Transfer
#ifdef WITH_QUVI
import Annex.Quvi
import qualified Utility.Quvi as Quvi
#endif

def :: [Command]
def = [notBareRepo $ withOptions [fileOption, pathdepthOption, relaxedOption] $
	command "addurl" (paramRepeating paramUrl) seek
		SectionCommon "add urls to annex"]

fileOption :: Option
fileOption = fieldOption [] "file" paramFile "specify what file the url is added to"

pathdepthOption :: Option
pathdepthOption = fieldOption [] "pathdepth" paramNumber "path components to use in filename"

relaxedOption :: Option
relaxedOption = flagOption [] "relaxed" "skip size check"

seek :: CommandSeek
seek ps = do
	f <- getOptionField fileOption return
	relaxed <- getOptionFlag relaxedOption
	d <- getOptionField pathdepthOption (return . maybe Nothing readish)
	withStrings (start relaxed f d) ps

start :: Bool -> Maybe FilePath -> Maybe Int -> String -> CommandStart
start relaxed optfile pathdepth s = go $ fromMaybe bad $ parseURI s
  where
  	(s', downloader) = getDownloader s
	bad = fromMaybe (error $ "bad url " ++ s') $
		parseURI $ escapeURIString isUnescapedInURI s'
	choosefile = flip fromMaybe optfile
	go url = case downloader of
		QuviDownloader -> usequvi
		DefaultDownloader -> 
#ifdef WITH_QUVI
			ifM (quviSupported s')
				( usequvi
				, regulardownload url
				)
#else
			regulardownload url
#endif
	regulardownload url = do
		pathmax <- liftIO $ fileNameLengthLimit "."
		let file = choosefile $ url2file url pathdepth pathmax
		showStart "addurl" file
		next $ perform relaxed s' file
#ifdef WITH_QUVI
	badquvi = error $ "quvi does not know how to download url " ++ s'
	usequvi = do
		page <- fromMaybe badquvi
			<$> withQuviOptions Quvi.forceQuery [Quvi.quiet, Quvi.httponly] s'
		let link = fromMaybe badquvi $ headMaybe $ Quvi.pageLinks page
		pathmax <- liftIO $ fileNameLengthLimit "."
		let file = choosefile $ truncateFilePath pathmax $ sanitizeFilePath $
			Quvi.pageTitle page ++ "." ++ Quvi.linkSuffix link
		showStart "addurl" file
		next $ performQuvi relaxed s' (Quvi.linkUrl link) file
#else
	usequvi = error "not built with quvi support"
#endif

#ifdef WITH_QUVI
performQuvi :: Bool -> URLString -> URLString -> FilePath -> CommandPerform
performQuvi relaxed pageurl videourl file = ifAnnexed file addurl geturl
  where
  	quviurl = setDownloader pageurl QuviDownloader
  	addurl key = next $ cleanup quviurl file key Nothing
	geturl = next $ isJust <$> addUrlFileQuvi relaxed quviurl videourl file
#endif

#ifdef WITH_QUVI
addUrlFileQuvi :: Bool -> URLString -> URLString -> FilePath -> Annex (Maybe Key)
addUrlFileQuvi relaxed quviurl videourl file = do
	key <- Backend.URL.fromUrl quviurl Nothing
	ifM (pure relaxed <||> Annex.getState Annex.fast)
		( do
			cleanup' quviurl file key Nothing
			return (Just key)
		, do
			{- Get the size, and use that to check
			 - disk space. However, the size info is not
			 - retained, because the size of a video stream
			 - might change and we want to be able to download
			 - it later. -}
			sizedkey <- addSizeUrlKey videourl key
			prepGetViaTmpChecked sizedkey Nothing $ do
				tmp <- fromRepo $ gitAnnexTmpObjectLocation key
				showOutput
				ok <- Transfer.notifyTransfer Transfer.Download (Just file) $
					Transfer.download webUUID key (Just file) Transfer.forwardRetry $ const $ do
						liftIO $ createDirectoryIfMissing True (parentDir tmp)
						downloadUrl [videourl] tmp
				if ok
					then do
						cleanup' quviurl file key (Just tmp)
						return (Just key)
					else return Nothing
		)
#endif

perform :: Bool -> URLString -> FilePath -> CommandPerform
perform relaxed url file = ifAnnexed file addurl geturl
  where
	geturl = next $ isJust <$> addUrlFile relaxed url file
	addurl key
		| relaxed = do
			setUrlPresent key url
			next $ return True
		| otherwise = ifM (elem url <$> getUrls key)
			( stop
			, do
				(exists, samesize) <- Url.withUrlOptions $ Url.check url (keySize key)
				if exists && samesize
					then do
						setUrlPresent key url
						next $ return True
					else do
						warning $ "while adding a new url to an already annexed file, " ++ if exists
							then "url does not have expected file size (use --relaxed to bypass this check) " ++ url
							else "failed to verify url exists: " ++ url
						stop
			)

addUrlFile :: Bool -> URLString -> FilePath -> Annex (Maybe Key)
addUrlFile relaxed url file = do
	liftIO $ createDirectoryIfMissing True (parentDir file)
	ifM (Annex.getState Annex.fast <||> pure relaxed)
		( nodownload relaxed url file
		, do
			showAction $ "downloading " ++ url ++ " "
			download url file
		)

download :: URLString -> FilePath -> Annex (Maybe Key)
download url file = do
	{- Generate a dummy key to use for this download, before we can
	 - examine the file and find its real key. This allows resuming
	 - downloads, as the dummy key for a given url is stable. -}
	dummykey <- addSizeUrlKey url =<< Backend.URL.fromUrl url Nothing
	prepGetViaTmpChecked dummykey Nothing $ do
		tmp <- fromRepo $ gitAnnexTmpObjectLocation dummykey
		showOutput
		ifM (runtransfer dummykey tmp)
			( do
				backend <- chooseBackend file
				let source = KeySource
					{ keyFilename = file
					, contentLocation = tmp
					, inodeCache = Nothing
					}
				k <- genKey source backend
				case k of
					Nothing -> return Nothing
					Just (key, _) -> do
						cleanup' url file key (Just tmp)
						return (Just key)
			, return Nothing
			)
  where
  	runtransfer dummykey tmp =  Transfer.notifyTransfer Transfer.Download (Just file) $
		Transfer.download webUUID dummykey (Just file) Transfer.forwardRetry $ const $ do
			liftIO $ createDirectoryIfMissing True (parentDir tmp)
			downloadUrl [url] tmp

{- Hits the url to get the size, if available.
 -
 - This is needed to avoid exceeding the diskreserve when downloading,
 - and so the assistant can display a pretty progress bar.
 -}
addSizeUrlKey :: URLString -> Key -> Annex Key
addSizeUrlKey url key = do
	size <- snd <$> Url.withUrlOptions (Url.exists url)
	return $ key { keySize = size }

cleanup :: URLString -> FilePath -> Key -> Maybe FilePath -> Annex Bool
cleanup url file key mtmp = do
	cleanup' url file key mtmp
	return True

cleanup' :: URLString -> FilePath -> Key -> Maybe FilePath -> Annex ()
cleanup' url file key mtmp = do
	when (isJust mtmp) $
		logStatus key InfoPresent
	setUrlPresent key url
	Command.Add.addLink file key Nothing
	whenM isDirect $ do
		void $ addAssociatedFile key file
		{- For moveAnnex to work in direct mode, the symlink
		 - must already exist, so flush the queue. -}
		Annex.Queue.flush
	maybe noop (moveAnnex key) mtmp

nodownload :: Bool -> URLString -> FilePath -> Annex (Maybe Key)
nodownload relaxed url file = do
	(exists, size) <- if relaxed
		then pure (True, Nothing)
		else Url.withUrlOptions (Url.exists url)
	if exists
		then do
			key <- Backend.URL.fromUrl url size
			cleanup' url file key Nothing
			return (Just key)
		else do
			warning $ "unable to access url: " ++ url
			return Nothing

url2file :: URI -> Maybe Int -> Int -> FilePath
url2file url pathdepth pathmax = case pathdepth of
	Nothing -> truncateFilePath pathmax $ sanitizeFilePath fullurl
	Just depth
		| depth >= length urlbits -> frombits id
		| depth > 0 -> frombits $ drop depth
		| depth < 0 -> frombits $ reverse . take (negate depth) . reverse
		| otherwise -> error "bad --pathdepth"
  where
	fullurl = uriRegName auth ++ uriPath url ++ uriQuery url
	frombits a = intercalate "/" $ a urlbits
	urlbits = map (truncateFilePath pathmax . sanitizeFilePath) $
		filter (not . null) $ split "/" fullurl
	auth = fromMaybe (error $ "bad url " ++ show url) $ uriAuthority url
