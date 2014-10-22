{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.ReKey where

import Common.Annex
import Command
import qualified Annex
import Types.Key
import Annex.Content
import qualified Command.Add
import Logs.Web
import Logs.Location
import Utility.CopyFile

cmd :: [Command]
cmd = [notDirect $ command "rekey"
	(paramOptional $ paramRepeating $ paramPair paramPath paramKey)
	seek SectionPlumbing "change keys used for files"]

seek :: CommandSeek
seek = withPairs start

start :: (FilePath, String) -> CommandStart
start (file, keyname) = ifAnnexed file go stop
  where
	newkey = fromMaybe (error "bad key") $ file2key keyname
	go oldkey
		| oldkey == newkey = stop
		| otherwise = do
			showStart "rekey" file
			next $ perform file oldkey newkey

perform :: FilePath -> Key -> Key -> CommandPerform
perform file oldkey newkey = do
	present <- inAnnex oldkey
	_ <- if present
		then linkKey oldkey newkey
		else do
			unlessM (Annex.getState Annex.force) $
				error $ file ++ " is not available (use --force to override)"
			return True
	next $ cleanup file oldkey newkey

{- Make a hard link to the old key content (when supported),
 - to avoid wasting disk space. -}
linkKey :: Key -> Key -> Annex Bool
linkKey oldkey newkey = getViaTmpUnchecked newkey $ \tmp -> do
	src <- calcRepo $ gitAnnexLocation oldkey
	liftIO $ ifM (doesFileExist tmp)
		( return True
		, createLinkOrCopy src tmp
		)

cleanup :: FilePath -> Key -> Key -> CommandCleanup
cleanup file oldkey newkey = do
	-- If the old key had some associated urls, record them for
	-- the new key as well.
	urls <- getUrls oldkey
	unless (null urls) $
		mapM_ (setUrlPresent newkey) urls

	-- Update symlink to use the new key.
	liftIO $ removeFile file
	Command.Add.addLink file newkey Nothing
	logStatus newkey InfoPresent
	return True
