{- git-annex links to content
 -
 - On file systems that support them, symlinks are used.
 -
 - On other filesystems, git instead stores the symlink target in a regular
 - file.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Link where

import Common.Annex
import qualified Annex
import qualified Git.HashObject
import qualified Git.UpdateIndex
import qualified Annex.Queue
import Git.Types

{- Checks if a file is a link to a key. -}
isAnnexLink :: FilePath -> Annex (Maybe Key)
isAnnexLink file = maybe Nothing (fileKey . takeFileName) <$> getAnnexLinkTarget file

{- Gets the link target of a symlink.
 -
 - On a filesystem that does not support symlinks, get the link
 - target by looking inside the file. (Only return at first 8k of the file,
 - more than enough for any symlink target.)
 -
 - Returns Nothing if the file is not a symlink, or not a link to annex
 - content.
 -}
getAnnexLinkTarget :: FilePath -> Annex (Maybe String)
getAnnexLinkTarget file = do
	v <- ifM (coreSymlinks <$> Annex.getGitConfig)
		( liftIO $ catchMaybeIO $ readSymbolicLink file
		, liftIO $ catchMaybeIO $ take 8192 <$> readFile file
		)
	case v of
		Nothing -> return Nothing
		Just l
			| isLinkToAnnex l -> return v
			| otherwise -> return Nothing

{- Creates a link on disk.
 -
 - On a filesystem that does not support symlinks, writes the link target
 - to a file. Note that git will only treat the file as a symlink if
 - it's staged as such, so use addAnnexLink when adding a new file or
 - modified link to git.
 -}
makeAnnexLink :: String -> FilePath -> Annex ()
makeAnnexLink linktarget file = ifM (coreSymlinks <$> Annex.getGitConfig)
	( liftIO $ createSymbolicLink linktarget file
	, liftIO $ writeFile file linktarget
	)

{- Creates a link on disk, and additionally stages it in git. -}
addAnnexLink :: String -> FilePath -> Annex ()
addAnnexLink linktarget file = do
	makeAnnexLink linktarget file
	stageSymlink file =<< hashSymlink linktarget

{- Injects a symlink target into git, returning its Sha. -}
hashSymlink :: String -> Annex Sha
hashSymlink linktarget = inRepo $ Git.HashObject.hashObject BlobObject linktarget

{- Stages a symlink to the annex, using a Sha of its target. -}
stageSymlink :: FilePath -> Sha -> Annex ()
stageSymlink file sha =
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.stageSymlink file sha)
