{-# LANGUAGE CPP #-}

{- git-annex remote list
 -
 - Copyright 2011,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.List where

import qualified Data.Map as M

import Common.Annex
import qualified Annex
import Logs.Remote
import Types.Remote
import Types.GitConfig
import Annex.UUID
import Remote.Helper.Hooks
import qualified Git
import qualified Git.Config

import qualified Remote.Git
#ifdef WITH_S3
import qualified Remote.S3
#endif
import qualified Remote.Bup
import qualified Remote.Directory
import qualified Remote.Rsync
import qualified Remote.Web
#ifdef WITH_WEBDAV
import qualified Remote.WebDAV
#endif
import qualified Remote.Glacier
import qualified Remote.Hook

remoteTypes :: [RemoteType]
remoteTypes =
	[ Remote.Git.remote
#ifdef WITH_S3
	, Remote.S3.remote
#endif
	, Remote.Bup.remote
	, Remote.Directory.remote
	, Remote.Rsync.remote
	, Remote.Web.remote
#ifdef WITH_WEBDAV
	, Remote.WebDAV.remote
#endif
	, Remote.Glacier.remote
	, Remote.Hook.remote
	]

{- Builds a list of all available Remotes.
 - Since doing so can be expensive, the list is cached. -}
remoteList :: Annex [Remote]
remoteList = do
	rs <- Annex.getState Annex.remotes
	if null rs
		then do
			m <- readRemoteLog
			rs' <- concat <$> mapM (process m) remoteTypes
			Annex.changeState $ \s -> s { Annex.remotes = rs' }
			return rs'
		else return rs
  where
	process m t = enumerate t >>= mapM (remoteGen m t)

{- Forces the remoteList to be re-generated, re-reading the git config. -}
remoteListRefresh :: Annex [Remote]
remoteListRefresh = do
	newg <- inRepo Git.Config.reRead
	Annex.changeState $ \s -> s 
		{ Annex.remotes = []
		, Annex.repo = newg
		}
	remoteList

{- Generates a Remote. -}
remoteGen :: (M.Map UUID RemoteConfig) -> RemoteType -> Git.Repo -> Annex Remote
remoteGen m t r = do
	u <- getRepoUUID r
	g <- fromRepo id
	let gc = extractRemoteGitConfig g (Git.repoDescribe r)
	let c = fromMaybe M.empty $ M.lookup u m
	addHooks <$> generate t r u c gc

{- Updates a local git Remote, re-reading its git config. -}
updateRemote :: Remote -> Annex Remote
updateRemote remote = do
	m <- readRemoteLog
	remote' <- updaterepo $ repo remote
	remoteGen m (remotetype remote) remote'
  where
	updaterepo r
		| Git.repoIsLocal r || Git.repoIsLocalUnknown r =
			Remote.Git.configRead r
		| otherwise = return r

{- All remotes that are not ignored. -}
enabledRemoteList :: Annex [Remote]
enabledRemoteList = filter (not . remoteAnnexIgnore . gitconfig) <$> remoteList

{- Checks if a remote is a special remote -}
specialRemote :: Remote -> Bool
specialRemote r = remotetype r /= Remote.Git.remote
