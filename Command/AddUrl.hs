{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.AddUrl where

import Control.Monad.State (liftIO, when)
import Network.URI
import Data.String.Utils
import System.Directory

import Command
import qualified Backend
import qualified Utility.Url as Url
import qualified Remote.Web
import qualified Command.Add
import qualified Annex
import qualified Backend.URL
import Messages
import Content
import PresenceLog
import Locations
import Utility.Path

command :: [Command]
command = [repoCommand "addurl" paramPath seek "add urls to annex"]

seek :: [CommandSeek]
seek = [withStrings start]

start :: CommandStartString
start s = do
	let u = parseURI s
	case u of
		Nothing -> error $ "bad url " ++ s
		Just url -> do
			file <- liftIO $ url2file url
			showStart "addurl" file
			next $ perform s file
			
perform :: String -> FilePath -> CommandPerform
perform url file = do
	fast <- Annex.getState Annex.fast
	if fast then nodownload url file else download url file

download :: String -> FilePath -> CommandPerform
download url file = do
	g <- Annex.gitRepo
	showAction $ "downloading " ++ url ++ " "
	let dummykey = Backend.URL.fromUrl url
	let tmp = gitAnnexTmpLocation g dummykey
	liftIO $ createDirectoryIfMissing True (parentDir tmp)
	ok <- Url.download url tmp
	if ok
		then do
			[(_, backend)] <- Backend.chooseBackends [file]
			k <- Backend.genKey tmp backend
			case k of
				Nothing -> stop
				Just (key, _) -> do
					moveAnnex key tmp
					Remote.Web.setUrl key url InfoPresent
					next $ Command.Add.cleanup file key True
		else stop

nodownload :: String -> FilePath -> CommandPerform
nodownload url file = do
	let key = Backend.URL.fromUrl url
	Remote.Web.setUrl key url InfoPresent
	
	next $ Command.Add.cleanup file key False

url2file :: URI -> IO FilePath
url2file url = do
	let parts = filter safe $ split "/" $ uriPath url
	if null parts
		then fallback
		else do
			let file = last parts
			e <- doesFileExist file
			if e then fallback else return file
	where
		fallback = do
			let file = replace "/" "_" $ show url
			e <- doesFileExist file
			when e $ error "already have this url"
			return file
		safe "" = False
		safe "." = False
		safe ".." = False
		safe _ = True
