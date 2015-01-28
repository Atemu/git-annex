{-# LANGUAGE BangPatterns #-}

{- git-annex location log
 -
 - git-annex keeps track of which repositories have the contents of annexed
 - files.
 -
 - Repositories record their UUID and the date when they --get or --drop
 - a value.
 - 
 - Copyright 2010-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Location (
	LogStatus(..),
	logStatus,
	logChange,
	loggedLocations,
	loggedLocationsHistorical,
	loggedKeys,
	loggedKeysFor,
) where

import Common.Annex
import qualified Annex.Branch
import Logs
import Logs.Presence
import Annex.UUID
import Git.Types (RefDate)
import qualified Annex

{- Log a change in the presence of a key's value in current repository. -}
logStatus :: Key -> LogStatus -> Annex ()
logStatus key s = do
	u <- getUUID
	logChange key u s

{- Log a change in the presence of a key's value in a repository. -}
logChange :: Key -> UUID -> LogStatus -> Annex ()
logChange key (UUID u) s = do
	config <- Annex.getGitConfig
	addLog (locationLogFile config key) =<< logNow s u
logChange _ NoUUID _ = noop

{- Returns a list of repository UUIDs that, according to the log, have
 - the value of a key. -}
loggedLocations :: Key -> Annex [UUID]
loggedLocations = getLoggedLocations currentLog

{- Gets the location log on a particular date. -}
loggedLocationsHistorical :: RefDate -> Key -> Annex [UUID]
loggedLocationsHistorical = getLoggedLocations . historicalLog

getLoggedLocations :: (FilePath -> Annex [String]) -> Key -> Annex [UUID]
getLoggedLocations getter key = do
	config <- Annex.getGitConfig
	map toUUID <$> (getter . locationLogFile config) key

{- Finds all keys that have location log information.
 - (There may be duplicate keys in the list.) -}
loggedKeys :: Annex [Key]
loggedKeys = mapMaybe locationLogFileKey <$> Annex.Branch.files

{- Finds all keys that have location log information indicating
 - they are present for the specified repository. -}
loggedKeysFor :: UUID -> Annex [Key]
loggedKeysFor u = filterM isthere =<< loggedKeys
  where
	{- This should run strictly to avoid the filterM
	 - building many thunks containing keyLocations data. -}
	isthere k = do
		us <- loggedLocations k
		let !there = u `elem` us
		return there
