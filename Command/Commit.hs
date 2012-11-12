{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Commit where

import Common.Annex
import Command
import qualified Annex.Branch
import qualified Git

def :: [Command]
def = [command "commit" paramNothing seek
	"commits any staged changes to the git-annex branch"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = next $ next $ do
	Annex.Branch.commit "update"
	_ <- runhook <=< inRepo $ Git.hookPath "annex-content"
	return True
  where
	runhook (Just hook) = liftIO $ boolSystem hook []
	runhook Nothing = return True
