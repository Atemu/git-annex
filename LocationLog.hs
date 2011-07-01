{- git-annex location log
 -
 - git-annex keeps track of which repositories have the contents of annexed
 - files.
 -
 - Repositories record their UUID and the date when they --get or --drop
 - a value.
 - 
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module LocationLog (
	LogStatus(..),
	logChange,
	readLog,
	writeLog,
	keyLocations,
	loggedKeys
) where

import System.FilePath
import Control.Monad (when)
import Data.Maybe

import qualified Git
import qualified Branch
import UUID
import Types
import Locations
import PresenceLog

{- Log a change in the presence of a key's value in a repository. -}
logChange :: Git.Repo -> Key -> UUID -> LogStatus -> Annex ()
logChange repo key u s = do
	when (null u) $
		error $ "unknown UUID for " ++ Git.repoDescribe repo ++ 
			" (have you run git annex init there?)"
	line <- logNow s u
	let f = logFile key
	ls <- readLog f
	writeLog f (compactLog $ line:ls)

{- Returns a list of repository UUIDs that, according to the log, have
 - the value of a key. -}
keyLocations :: Key -> Annex [UUID]
keyLocations key = currentLog $ logFile key

{- Finds all keys that have location log information.
 - (There may be duplicate keys in the list.) -}
loggedKeys :: Annex [Key]
loggedKeys =
	return . catMaybes . map (logFileKey . takeFileName) =<< Branch.files
