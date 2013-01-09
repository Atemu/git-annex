{- git-annex command
 -
 - Copyright 2010,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.SendKey where

import Common.Annex
import Command
import Annex.Content
import Utility.Rsync
import Logs.Transfer
import qualified Fields

def :: [Command]
def = [noCommit $ command "sendkey" paramKey seek
	"runs rsync in server mode to send content"]

seek :: [CommandSeek]
seek = [withKeys start]

start :: Key -> CommandStart
start key = ifM (inAnnex key)
	( fieldTransfer Upload key $ \_p ->
		sendAnnex key rollback $ liftIO . rsyncServerSend
	, do
		warning "requested key is not present"
		liftIO exitFailure
	)
  where
	{- No need to do any rollback; when sendAnnex fails, a nonzero
	 - exit will be propigated, and the remote will know the transfer
	 - failed. -}
	rollback = noop

fieldTransfer :: Direction -> Key -> (MeterUpdate -> Annex Bool) -> CommandStart
fieldTransfer direction key a = do
	afile <- Fields.getField Fields.associatedFile
	ok <- maybe (a $ const noop)
		(\u -> runTransfer (Transfer direction (toUUID u) key) afile noRetry a)
		=<< Fields.getField Fields.remoteUUID
	if ok
		then liftIO exitSuccess
		else liftIO exitFailure
