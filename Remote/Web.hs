{- Web remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Web (remote) where

import Common.Annex
import Types.Remote
import qualified Git
import qualified Git.Construct
import Annex.Content
import Config
import Logs.Web
import qualified Utility.Url as Url
import Types.Key

import qualified Data.Map as M

remote :: RemoteType
remote = RemoteType {
	typename = "web",
	enumerate = list,
	generate = gen,
	setup = error "not supported"
}

-- There is only one web remote, and it always exists.
-- (If the web should cease to exist, remove this module and redistribute
-- a new release to the survivors by carrier pigeon.)
list :: Annex [Git.Repo]
list = do
	r <- liftIO $ Git.Construct.remoteNamed "web" Git.Construct.fromUnknown
	return [r]

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex Remote
gen r _ _ gc = 
	return Remote {
		uuid = webUUID,
		cost = expensiveRemoteCost,
		name = Git.repoDescribe r,
		storeKey = uploadKey,
		retrieveKeyFile = downloadKey,
		retrieveKeyFileCheap = downloadKeyCheap,
		removeKey = dropKey,
		hasKey = checkKey,
		hasKeyCheap = False,
		whereisKey = Just getUrls,
		config = M.empty,
		gitconfig = gc,
		localpath = Nothing,
		repo = r,
		readonly = True,
		remotetype = remote
	}

downloadKey :: Key -> AssociatedFile -> FilePath -> Annex Bool
downloadKey key _file dest = get =<< getUrls key
  where
	get [] = do
		warning "no known url"
		return False
	get urls = do
		showOutput -- make way for download progress bar
		downloadUrl urls dest

downloadKeyCheap :: Key -> FilePath -> Annex Bool
downloadKeyCheap _ _ = return False

uploadKey :: Key -> AssociatedFile -> MeterUpdate -> Annex Bool
uploadKey _ _ _ = do
	warning "upload to web not supported"
	return False

dropKey :: Key -> Annex Bool
dropKey k = do
	mapM_ (setUrlMissing k) =<< getUrls k
	return True

checkKey :: Key -> Annex (Either String Bool)
checkKey key = do
	us <- getUrls key
	if null us
		then return $ Right False
		else return . Right =<< checkKey' key us
checkKey' :: Key -> [URLString] -> Annex Bool
checkKey' key us = untilTrue us $ \u -> do
	showAction $ "checking " ++ u
	headers <- getHttpHeaders
	liftIO $ Url.check u headers (keySize key)
