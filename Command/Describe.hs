{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Describe where

import Common.Annex
import Command
import qualified Remote
import Logs.UUID

def :: [Command]
def = [command "describe" (paramPair paramRemote paramDesc) seek
	"change description of a repository"]

seek :: [CommandSeek]
seek = [withWords start]

start :: [String] -> CommandStart
start (name:description) = do
	showStart "describe" name
	u <- Remote.nameToUUID name
	next $ perform u $ unwords description
start _ = error "Specify a repository and a description."	

perform :: UUID -> String -> CommandPerform
perform u description = do
	describeUUID u description
	next $ return True
