{- git ls-tree interface
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.LsTree (
	TreeItem(..),
	lsTree,
	lsTreeFiles,
	parseLsTree
) where

import Numeric
import Control.Applicative
import System.Posix.Types

import Common
import Git
import Git.Command
import Git.Sha
import qualified Git.Filename

data TreeItem = TreeItem
	{ mode :: FileMode
	, typeobj :: String
	, sha :: String
	, file :: FilePath
	} deriving Show

{- Lists the complete contents of a tree. -}
lsTree :: Ref -> Repo -> IO [TreeItem]
lsTree t repo = map parseLsTree <$>
	pipeNullSplitZombie [Params "ls-tree --full-tree -z -r --", File $ show t] repo

{- Lists specified files in a tree. -}
lsTreeFiles :: Ref -> [FilePath] -> Repo -> IO [TreeItem]
lsTreeFiles t fs repo = map parseLsTree <$>
	pipeNullSplitZombie ([Params "ls-tree -z --", File $ show t] ++ map File fs) repo

{- Parses a line of ls-tree output.
 - (The --long format is not currently supported.) -}
parseLsTree :: String -> TreeItem
parseLsTree l = TreeItem 
	{ mode = fst $ Prelude.head $ readOct m
	, typeobj = t
	, sha = s
	, file = Git.Filename.decode f
	}
  where
	-- l = <mode> SP <type> SP <sha> TAB <file>
	-- All fields are fixed, so we can pull them out of
	-- specific positions in the line.
	(m, past_m) = splitAt 7 l
	(t, past_t) = splitAt 4 past_m
	(s, past_s) = splitAt shaSize $ Prelude.tail past_t
	f = Prelude.tail past_s
