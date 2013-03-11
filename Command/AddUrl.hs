{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.AddUrl where

import Network.URI

import Common.Annex
import Command
import Backend
import qualified Command.Add
import qualified Annex
import qualified Backend.URL
import qualified Utility.Url as Url
import Annex.Content
import Logs.Web
import qualified Option
import Types.Key
import Types.KeySource
import Config
import Annex.Content.Direct

def :: [Command]
def = [notBareRepo $ withOptions [fileOption, pathdepthOption, relaxedOption] $
	command "addurl" (paramRepeating paramUrl) seek "add urls to annex"]

fileOption :: Option
fileOption = Option.field [] "file" paramFile "specify what file the url is added to"

pathdepthOption :: Option
pathdepthOption = Option.field [] "pathdepth" paramNumber "path components to use in filename"

relaxedOption :: Option
relaxedOption = Option.flag [] "relaxed" "skip size check"

seek :: [CommandSeek]
seek = [withField fileOption return $ \f ->
	withFlag relaxedOption $ \relaxed ->
	withField pathdepthOption (return . maybe Nothing readish) $ \d ->
	withStrings $ start relaxed f d]

start :: Bool -> Maybe FilePath -> Maybe Int -> String -> CommandStart
start relaxed optfile pathdepth s = go $ fromMaybe bad $ parseURI s
  where
	bad = fromMaybe (error $ "bad url " ++ s) $
		parseURI $ escapeURIString isUnescapedInURI s
	go url = do
		let file = fromMaybe (url2file url pathdepth) optfile
		showStart "addurl" file
		next $ perform relaxed s file

perform :: Bool -> String -> FilePath -> CommandPerform
perform relaxed url file = ifAnnexed file addurl geturl
  where
	geturl = do
		liftIO $ createDirectoryIfMissing True (parentDir file)
		ifM (Annex.getState Annex.fast <||> pure relaxed)
			( nodownload relaxed url file , download url file )
	addurl (key, _backend) = do
		headers <- getHttpHeaders
		ifM (liftIO $ Url.check url headers $ keySize key)
			( do
				setUrlPresent key url
				next $ return True
			, do
				warning $ "failed to verify url: " ++ url
				stop
			)

download :: String -> FilePath -> CommandPerform
download url file = do
	showAction $ "downloading " ++ url ++ " "
	let dummykey = Backend.URL.fromUrl url Nothing
	tmp <- fromRepo $ gitAnnexTmpLocation dummykey
	liftIO $ createDirectoryIfMissing True (parentDir tmp)
	stopUnless (downloadUrl [url] tmp) $ do
		backend <- chooseBackend file
		let source = KeySource
			{ keyFilename = file
			, contentLocation = tmp
			, inodeCache = Nothing
			}
		k <- genKey source backend
		case k of
			Nothing -> stop
			Just (key, _) -> do
				whenM isDirect $
					void $ addAssociatedFile key file
				moveAnnex key tmp
				setUrlPresent key url
				next $ Command.Add.cleanup file key True

nodownload :: Bool -> String -> FilePath -> CommandPerform
nodownload relaxed url file = do
	headers <- getHttpHeaders
	(exists, size) <- if relaxed
		then pure (True, Nothing)
		else liftIO $ Url.exists url headers
	if exists
		then do
			let key = Backend.URL.fromUrl url size
			whenM isDirect $
				void $ addAssociatedFile key file
			setUrlPresent key url
			next $ Command.Add.cleanup file key False
		else do
			warning $ "unable to access url: " ++ url
			stop

url2file :: URI -> Maybe Int -> FilePath
url2file url pathdepth = case pathdepth of
	Nothing -> filesize $ escape fullurl
	Just depth
		| depth > 0 -> frombits $ drop depth
		| depth < 0 -> frombits $ reverse . take (negate depth) . reverse
		| otherwise -> error "bad --pathdepth"
  where
	fullurl = uriRegName auth ++ uriPath url ++ uriQuery url
	frombits a = join "/" $ a urlbits
	urlbits = map (filesize . escape) $ filter (not . null) $ split "/" fullurl
	auth = fromMaybe (error $ "bad url " ++ show url) $ uriAuthority url
	filesize = take 255
	escape = replace "/" "_" . replace "?" "_"
