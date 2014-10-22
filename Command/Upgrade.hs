{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Upgrade where

import Common.Annex
import Command
import Upgrade

cmd :: [Command]
cmd = [dontCheck repoExists $ -- because an old version may not seem to exist
	command "upgrade" paramNothing seek
		SectionMaintenance "upgrade repository layout"]

seek :: CommandSeek
seek = withNothing start

start :: CommandStart
start = do
	showStart "upgrade" "."
	r <- upgrade False
	next $ next $ return r
