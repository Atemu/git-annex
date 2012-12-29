{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Reinject where

import Common.Annex
import Command
import Logs.Location
import Annex.Content
import qualified Command.Fsck

def :: [Command]
def = [notDirect $ command "reinject" (paramPair "SRC" "DEST") seek
	"sets content of annexed file"]

seek :: [CommandSeek]
seek = [withWords start]

start :: [FilePath] -> CommandStart
start (src:dest:[])
	| src == dest = stop
	| otherwise =
		ifAnnexed src
			(error $ "cannot used annexed file as src: " ++ src)
			go
  where
	go = do
		showStart "reinject" dest
		next $ whenAnnexed (perform src) dest
start _ = error "specify a src file and a dest file"

perform :: FilePath -> FilePath -> (Key, Backend) -> CommandPerform
perform src _dest (key, backend) = do
	{- Check the content before accepting it. -}
	ifM (Command.Fsck.checkKeySizeOr reject key src
		<&&> Command.Fsck.checkBackendOr reject backend key src)
		( do
			unlessM move $ error "mv failed!"
			next $ cleanup key
		, error "not reinjecting"
		)
  where
	-- the file might be on a different filesystem,
	-- so mv is used rather than simply calling
	-- moveToObjectDir; disk space is also
	-- checked this way.
	move = getViaTmp key $ \tmp ->
		liftIO $ boolSystem "mv" [File src, File tmp]
	reject = const $ return "wrong file?"

cleanup :: Key -> CommandCleanup
cleanup key = do
	logStatus key InfoPresent
	return True
