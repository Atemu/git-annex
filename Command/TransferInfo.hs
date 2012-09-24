{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.TransferInfo where

import Common.Annex
import Command
import Annex.Content
import Logs.Transfer
import Types.Key
import qualified Fields

def :: [Command]
def = [noCommit $ command "transferinfo" paramKey seek
	"updates sender on number of bytes of content received"]

seek :: [CommandSeek]
seek = [withWords start]

{- Security:
 - 
 - The transfer info file contains the user-supplied key, but
 - the built-in guards prevent slashes in it from showing up in the filename.
 - It also contains the UUID of the remote. But slashes are also filtered
 - out of that when generating the filename.
 - 
 - Checks that the key being transferred is inAnnex, to prevent
 - malicious spamming of bogus keys. Does not check that a transfer
 - of the key is actually in progress, because this could be started
 - concurrently with sendkey, and win the race.
 -}
start :: [String] -> CommandStart
start (k:[]) = do
	case (file2key k) of
		Nothing -> error "bad key"
		(Just key) -> whenM (inAnnex key) $ do
			file <- Fields.getField Fields.associatedFile
			u <- maybe (error "missing remoteuuid") toUUID
				<$> Fields.getField Fields.remoteUUID
			let t = Transfer
				{ transferDirection = Upload
				, transferUUID = u
				, transferKey = key
				}
			info <- liftIO $ startTransferInfo file
			(update, tfile, _) <- mkProgressUpdater t info
			liftIO $ mapM_ void
				[ tryIO $ forever $ do
					bytes <- readish <$> getLine
					maybe (error "transferinfo protocol error") update bytes
				, tryIO $ removeFile tfile
				, exitSuccess
				]
	stop
start _ = error "wrong number of parameters"
