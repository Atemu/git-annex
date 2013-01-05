{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.PreCommit where

import Common.Annex
import Command
import qualified Command.Add
import qualified Command.Fix
import Config

def :: [Command]
def = [command "pre-commit" paramPaths seek "run by git pre-commit hook"]

{- The pre-commit hook needs to fix symlinks to all files being committed.
 - And, it needs to inject unlocked files into the annex. -}
seek :: [CommandSeek]
seek =
	[ withFilesToBeCommitted $ whenNotDirect $ whenAnnexed $ Command.Fix.start
	, withFilesUnlockedToBeCommitted start]

whenNotDirect :: (FilePath -> Annex (Maybe CommandPerform)) -> FilePath -> Annex (Maybe CommandPerform)
whenNotDirect a f = ifM isDirect ( stop , a f )

start :: FilePath -> CommandStart
start file = next $ perform file

perform :: FilePath -> CommandPerform
perform file = do
	unlessM (doCommand $ Command.Add.start file) $
		error $ "failed to add " ++ file ++ "; canceling commit"
	next $ return True
