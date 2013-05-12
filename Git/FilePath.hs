{- git FilePath library
 -
 - Different git commands use different types of FilePaths to refer to
 - files in the repository. Some commands use paths relative to the
 - top of the repository even when run in a subdirectory. Adding some
 - types helps keep that straight.
 -
 - Copyright 2012-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Git.FilePath (
	TopFilePath,
	getTopFilePath,
	toTopFilePath,
	asTopFilePath,
	InternalGitPath,
	toInternalGitPath,
	fromInternalGitPath
) where

import Common
import Git

{- A FilePath, relative to the top of the git repository. -}
newtype TopFilePath = TopFilePath { getTopFilePath :: FilePath }

{- The input FilePath can be absolute, or relative to the CWD. -}
toTopFilePath :: FilePath -> Git.Repo -> IO TopFilePath
toTopFilePath file repo = TopFilePath <$>
	relPathDirToFile (repoPath repo) <$> absPath file

{- The input FilePath must already be relative to the top of the git
 - repository -}
asTopFilePath :: FilePath -> TopFilePath
asTopFilePath file = TopFilePath file

{- Git may use a different representation of a path when storing
 - it internally. For example, on Windows, git uses '/' to separate paths
 - stored in the repository, despite Windows using '\' -}
type InternalGitPath = String

toInternalGitPath :: FilePath -> InternalGitPath
#ifndef __WINDOWS__
toInternalGitPath = id
#else
toInternalGitPath = replace "\\" "/"
#endif

fromInternalGitPath :: InternalGitPath -> FilePath
#ifndef __WINDOWS__
fromInternalGitPath = id
#else
fromInternalGitPath = replace "/" "\\"
#endif
