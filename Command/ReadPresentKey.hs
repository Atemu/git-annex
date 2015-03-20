{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.ReadPresentKey where

import Common.Annex
import Command
import Logs.Location
import Types.Key

cmd :: [Command]
cmd = [noCommit $ command "readpresentkey" (paramPair paramKey paramUUID) seek
	SectionPlumbing "read records of where key is present"] 

seek :: CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start (ks:us:[]) = do
	ls <- loggedLocations k
	if toUUID us `elem` ls
		then liftIO exitSuccess
		else liftIO exitFailure
  where
	k = fromMaybe (error "bad key") (file2key ks)
start _ = error "Wrong number of parameters"
