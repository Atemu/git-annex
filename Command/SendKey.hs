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
import Annex
import Utility.Rsync
import Annex.Transfer
import qualified CmdLine.GitAnnexShell.Fields as Fields
import Utility.Metered

cmd :: [Command]
cmd = [noCommit $ command "sendkey" paramKey seek
	SectionPlumbing "runs rsync in server mode to send content"]

seek :: CommandSeek
seek = withKeys start

start :: Key -> CommandStart
start key = do
	opts <- filterRsyncSafeOptions . maybe [] words
		<$> getField "RsyncOptions"
	ifM (inAnnex key)
		( fieldTransfer Upload key $ \_p ->
			sendAnnex key rollback $ liftIO . rsyncServerSend (map Param opts)
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
		(\u -> runner (Transfer direction (toUUID u) key) afile noRetry a)
		=<< Fields.getField Fields.remoteUUID
	liftIO $ exitBool ok
  where
	{- Allow the key to be sent to the remote even if there seems to be
	 - another transfer of that key going on to that remote.
	 - That one may be stale, etc.
	 -}
	runner
		| direction == Upload = alwaysRunTransfer
		| otherwise = runTransfer
