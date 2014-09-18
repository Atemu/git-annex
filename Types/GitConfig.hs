{- git-annex configuration
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
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
import Types.Distribution
import Types.Availability
import Types.NumCopies
import Utility.HumanTime

{- Main git-annex settings. Each setting corresponds to a git-config key
 - such as annex.foo -}
data GitConfig = GitConfig
	{ annexVersion :: Maybe String
	, annexNumCopies :: Maybe NumCopies
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
	, annexDebug :: Bool
	, annexWebOptions :: [String]
	, annexQuviOptions :: [String]
	, annexWebDownloadCommand :: Maybe String
	, annexCrippledFileSystem :: Bool
	, annexLargeFiles :: Maybe String
	, annexFsckNudge :: Bool
	, annexAutoUpgrade :: AutoUpgrade
	, annexExpireUnused :: Maybe (Maybe Duration)
	, annexSecureEraseCommand :: Maybe String
	, annexGenMetaData :: Bool
	, annexListen :: Maybe String
	, annexStartupScan :: Bool
	, annexHardLink :: Bool
	, coreSymlinks :: Bool
	, gcryptId :: Maybe String
	}

extractGitConfig :: Git.Repo -> GitConfig
extractGitConfig r = GitConfig
	{ annexVersion = notempty $ getmaybe (annex "version")
	, annexNumCopies = NumCopies <$> getmayberead (annex "numcopies")
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
	, annexDebug = getbool (annex "debug") False
	, annexWebOptions = getwords (annex "web-options")
	, annexQuviOptions = getwords (annex "quvi-options")
	, annexWebDownloadCommand = getmaybe (annex "web-download-command")
	, annexCrippledFileSystem = getbool (annex "crippledfilesystem") False
	, annexLargeFiles = getmaybe (annex "largefiles")
	, annexFsckNudge = getbool (annex "fscknudge") True
	, annexAutoUpgrade = toAutoUpgrade $ getmaybe (annex "autoupgrade")
	, annexExpireUnused = maybe Nothing Just . parseDuration
		<$> getmaybe (annex "expireunused")
	, annexSecureEraseCommand = getmaybe (annex "secure-erase-command")
	, annexGenMetaData = getbool (annex "genmetadata") False
	, annexListen = getmaybe (annex "listen")
	, annexStartupScan = getbool (annex "startupscan") True
	, annexHardLink = getbool (annex "hardlink") False
	, coreSymlinks = getbool "core.symlinks" True
	, gcryptId = getmaybe "core.gcrypt-id"
	}
  where
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
	, remoteAnnexReadOnly :: Bool
	, remoteAnnexTrustLevel :: Maybe String
	, remoteAnnexStartCommand :: Maybe String
	, remoteAnnexStopCommand :: Maybe String
	, remoteAnnexAvailability :: Maybe Availability
	, remoteAnnexBare :: Maybe Bool

	{- These settings are specific to particular types of remotes
	 - including special remotes. -}
	, remoteAnnexShell :: Maybe String
	, remoteAnnexSshOptions :: [String]
	, remoteAnnexRsyncOptions :: [String]
	, remoteAnnexRsyncUploadOptions :: [String]
	, remoteAnnexRsyncDownloadOptions :: [String]
	, remoteAnnexRsyncTransport :: [String]
	, remoteAnnexGnupgOptions :: [String]
	, remoteAnnexRsyncUrl :: Maybe String
	, remoteAnnexBupRepo :: Maybe String
	, remoteAnnexTahoe :: Maybe FilePath
	, remoteAnnexBupSplitOptions :: [String]
	, remoteAnnexDirectory :: Maybe FilePath
	, remoteAnnexGCrypt :: Maybe String
	, remoteAnnexDdarRepo :: Maybe String
	, remoteAnnexHookType :: Maybe String
	, remoteAnnexExternalType :: Maybe String
	{- A regular git remote's git repository config. -}
	, remoteGitConfig :: Maybe GitConfig
	}

extractRemoteGitConfig :: Git.Repo -> String -> RemoteGitConfig
extractRemoteGitConfig r remotename = RemoteGitConfig
	{ remoteAnnexCost = getmayberead "cost"
	, remoteAnnexCostCommand = notempty $ getmaybe "cost-command"
	, remoteAnnexIgnore = getbool "ignore" False
	, remoteAnnexSync = getbool "sync" True
	, remoteAnnexReadOnly = getbool "readonly" False
	, remoteAnnexTrustLevel = notempty $ getmaybe "trustlevel"
	, remoteAnnexStartCommand = notempty $ getmaybe "start-command"
	, remoteAnnexStopCommand = notempty $ getmaybe "stop-command"
	, remoteAnnexAvailability = getmayberead "availability"
	, remoteAnnexBare = getmaybebool "bare"

	, remoteAnnexShell = getmaybe "shell"
	, remoteAnnexSshOptions = getoptions "ssh-options"
	, remoteAnnexRsyncOptions = getoptions "rsync-options"
	, remoteAnnexRsyncDownloadOptions = getoptions "rsync-download-options"
	, remoteAnnexRsyncUploadOptions = getoptions "rsync-upload-options"
	, remoteAnnexRsyncTransport = getoptions "rsync-transport"
	, remoteAnnexGnupgOptions = getoptions "gnupg-options"
	, remoteAnnexRsyncUrl = notempty $ getmaybe "rsyncurl"
	, remoteAnnexBupRepo = getmaybe "buprepo"
	, remoteAnnexTahoe = getmaybe "tahoe"
	, remoteAnnexBupSplitOptions = getoptions "bup-split-options"
	, remoteAnnexDirectory = notempty $ getmaybe "directory"
	, remoteAnnexGCrypt = notempty $ getmaybe "gcrypt"
	, remoteAnnexDdarRepo = getmaybe "ddarrepo"
	, remoteAnnexHookType = notempty $ getmaybe "hooktype"
	, remoteAnnexExternalType = notempty $ getmaybe "externaltype"
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

