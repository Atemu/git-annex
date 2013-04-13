{- git-annex configuration
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.GitConfig ( 
	GitConfig(..),
	extractGitConfig,
	RemoteGitConfig(..),
	extractRemoteGitConfig,
) where

import Common
import qualified Git
import qualified Git.Config
import Utility.DataUnits
import Config.Cost

{- Main git-annex settings. Each setting corresponds to a git-config key
 - such as annex.foo -}
data GitConfig = GitConfig
	{ annexVersion :: Maybe String
	, annexNumCopies :: Int
	, annexDiskReserve :: Integer
	, annexDirect :: Bool
	, annexBackends :: [String]
	, annexQueueSize :: Maybe Int
	, annexBloomCapacity :: Maybe Int
	, annexBloomAccuracy :: Maybe Int
	, annexSshCaching :: Maybe Bool
	, annexAlwaysCommit :: Bool
	, annexDelayAdd :: Maybe Int
	, annexHttpHeaders :: [String]
	, annexHttpHeadersCommand :: Maybe String
	, annexAutoCommit :: Bool
	, annexWebOptions :: [String]
	, annexWebDownloadCommand :: Maybe String
	, annexCrippledFileSystem :: Bool
	, annexLargeFiles :: Maybe String
	, coreSymlinks :: Bool
	}

extractGitConfig :: Git.Repo -> GitConfig
extractGitConfig r = GitConfig
	{ annexVersion = notempty $ getmaybe (annex "version")
	, annexNumCopies = get (annex "numcopies") 1
	, annexDiskReserve = fromMaybe onemegabyte $
		readSize dataUnits =<< getmaybe (annex "diskreserve")
	, annexDirect = getbool (annex "direct") False
	, annexBackends = getwords (annex "backends")
	, annexQueueSize = getmayberead (annex "queuesize")
	, annexBloomCapacity = getmayberead (annex "bloomcapacity")
	, annexBloomAccuracy = getmayberead (annex "bloomaccuracy")
	, annexSshCaching = getmaybebool (annex "sshcaching")
	, annexAlwaysCommit = getbool (annex "alwayscommit") True
	, annexDelayAdd = getmayberead (annex "delayadd")
	, annexHttpHeaders = getlist (annex "http-headers")
	, annexHttpHeadersCommand = getmaybe (annex "http-headers-command")
	, annexAutoCommit = getbool (annex "autocommit") True
	, annexWebOptions = getwords (annex "web-options")
	, annexWebDownloadCommand = getmaybe (annex "web-download-command")
	, annexCrippledFileSystem = getbool (annex "crippledfilesystem") False
	, annexLargeFiles = getmaybe (annex "largefiles")
	, coreSymlinks = getbool "core.symlinks" True
	}
  where
	get k def = fromMaybe def $ getmayberead k
	getbool k def = fromMaybe def $ getmaybebool k
	getmaybebool k = Git.Config.isTrue =<< getmaybe k
	getmayberead k = readish =<< getmaybe k
	getmaybe k = Git.Config.getMaybe k r
	getlist k = Git.Config.getList k r
	getwords k = fromMaybe [] $ words <$> getmaybe k

	annex k = "annex." ++ k
			
	onemegabyte = 1000000

{- Per-remote git-annex settings. Each setting corresponds to a git-config
 - key such as <remote>.annex-foo, or if that is not set, a default from
 - annex.foo -}
data RemoteGitConfig = RemoteGitConfig
	{ remoteAnnexCost :: Maybe Cost
	, remoteAnnexCostCommand :: Maybe String
	, remoteAnnexIgnore :: Bool
	, remoteAnnexSync :: Bool
	, remoteAnnexTrustLevel :: Maybe String
	, remoteAnnexStartCommand :: Maybe String
	, remoteAnnexStopCommand :: Maybe String

	{- These settings are specific to particular types of remotes
	 - including special remotes. -}
	, remoteAnnexSshOptions :: [String]
	, remoteAnnexRsyncOptions :: [String]
	, remoteAnnexRsyncTransport :: [String]
	, remoteAnnexGnupgOptions :: [String]
	, remoteAnnexRsyncUrl :: Maybe String
	, remoteAnnexBupRepo :: Maybe String
	, remoteAnnexBupSplitOptions :: [String]
	, remoteAnnexDirectory :: Maybe FilePath
	, remoteAnnexHookType :: Maybe String
	{- A regular git remote's git repository config. -}
	, remoteGitConfig :: Maybe GitConfig
	}

extractRemoteGitConfig :: Git.Repo -> String -> RemoteGitConfig
extractRemoteGitConfig r remotename = RemoteGitConfig
	{ remoteAnnexCost = getmayberead "cost"
	, remoteAnnexCostCommand = notempty $ getmaybe "cost-command"
	, remoteAnnexIgnore = getbool "ignore" False
	, remoteAnnexSync = getbool "sync" True
	, remoteAnnexTrustLevel = notempty $ getmaybe "trustlevel"
	, remoteAnnexStartCommand = notempty $ getmaybe "start-command"
	, remoteAnnexStopCommand = notempty $ getmaybe "stop-command"

	, remoteAnnexSshOptions = getoptions "ssh-options"
	, remoteAnnexRsyncOptions = getoptions "rsync-options"
	, remoteAnnexRsyncTransport = getoptions "rsync-transport"
	, remoteAnnexGnupgOptions = getoptions "gnupg-options"
	, remoteAnnexRsyncUrl = notempty $ getmaybe "rsyncurl"
	, remoteAnnexBupRepo = getmaybe "buprepo"
	, remoteAnnexBupSplitOptions = getoptions "bup-split-options"
	, remoteAnnexDirectory = notempty $ getmaybe "directory"
	, remoteAnnexHookType = notempty $ getmaybe "hooktype"
	, remoteGitConfig = Nothing
	}
  where
	getbool k def = fromMaybe def $ getmaybebool k
	getmaybebool k = Git.Config.isTrue =<< getmaybe k
	getmayberead k = readish =<< getmaybe k
	getmaybe k = mplus (Git.Config.getMaybe (key k) r)
		(Git.Config.getMaybe (remotekey k) r)
	getoptions k = fromMaybe [] $ words <$> getmaybe k

	key k = "annex." ++ k
	remotekey k = "remote." ++ remotename ++ ".annex-" ++ k

notempty :: Maybe String -> Maybe String	
notempty Nothing = Nothing
notempty (Just "") = Nothing
notempty (Just s) = Just s

