{- git-annex repository versioning
 -
 - Copyright 2010-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Version where

import Annex.Common
import Config
import Types.RepoVersion
import qualified Annex

import qualified Data.Map as M

defaultVersion :: RepoVersion
defaultVersion = RepoVersion 5

latestVersion :: RepoVersion
latestVersion = RepoVersion 7

supportedVersions :: [RepoVersion]
supportedVersions = map RepoVersion [5, 7]

versionForAdjustedClone :: RepoVersion
versionForAdjustedClone = RepoVersion 7

versionForCrippledFilesystem :: RepoVersion
versionForCrippledFilesystem = RepoVersion 7

upgradableVersions :: [RepoVersion]
#ifndef mingw32_HOST_OS
upgradableVersions = map RepoVersion [0..6]
#else
upgradableVersions = map RepoVersion [2..6]
#endif

autoUpgradeableVersions :: M.Map RepoVersion RepoVersion
autoUpgradeableVersions = M.fromList
	[ (RepoVersion 3, RepoVersion 5)
	, (RepoVersion 4, RepoVersion 5)
	, (RepoVersion 6, RepoVersion 7)
	]

versionField :: ConfigKey
versionField = annexConfig "version"

getVersion :: Annex (Maybe RepoVersion)
getVersion = annexVersion <$> Annex.getGitConfig

versionSupportsDirectMode :: Annex Bool
versionSupportsDirectMode = go <$> getVersion
  where
	go (Just v) | v >= RepoVersion 6 = False
	go _ = True

versionSupportsUnlockedPointers :: Annex Bool
versionSupportsUnlockedPointers = go <$> getVersion
  where
	go (Just v) | v >= RepoVersion 6 = True
	go _ = False

versionSupportsAdjustedBranch :: Annex Bool
versionSupportsAdjustedBranch = versionSupportsUnlockedPointers

versionUsesKeysDatabase :: Annex Bool
versionUsesKeysDatabase = versionSupportsUnlockedPointers

setVersion :: RepoVersion -> Annex ()
setVersion (RepoVersion v) = setConfig versionField (show v)

removeVersion :: Annex ()
removeVersion = unsetConfig versionField
