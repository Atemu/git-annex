{- git diff-tree interface
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.DiffTree (
	DiffTreeItem(..),
	isDiffOf,
	diffTree,
	diffTreeRecursive,
	diffIndex,
	diffWorkTree,
	diffLog,
) where

import Numeric

import Common
import Git
import Git.Sha
import Git.Command
import Git.FilePath
import Git.DiffTreeItem
import qualified Git.Filename
import qualified Git.Ref

{- Checks if the DiffTreeItem modifies a file with a given name
 - or under a directory by that name. -}
isDiffOf :: DiffTreeItem -> TopFilePath -> Bool
isDiffOf diff f = case getTopFilePath f of
	"" -> True -- top of repo contains all
	d -> d `dirContains` getTopFilePath (file diff)

{- Diffs two tree Refs. -}
diffTree :: Ref -> Ref -> Repo -> IO ([DiffTreeItem], IO Bool)
diffTree src dst = getdiff (Param "diff-tree")
	[Param (fromRef src), Param (fromRef dst), Param "--"]

{- Diffs two tree Refs, recursing into sub-trees -}
diffTreeRecursive :: Ref -> Ref -> Repo -> IO ([DiffTreeItem], IO Bool)
diffTreeRecursive src dst = getdiff (Param "diff-tree")
	[Param "-r", Param (fromRef src), Param (fromRef dst), Param "--"]

{- Diffs between a tree and the index. Does nothing if there is not yet a
 - commit in the repository. -}
diffIndex :: Ref -> Repo -> IO ([DiffTreeItem], IO Bool)
diffIndex ref = diffIndex' ref [Param "--cached"]

{- Diffs between a tree and the working tree. Does nothing if there is not
 - yet a commit in the repository, or if the repository is bare. -}
diffWorkTree :: Ref -> Repo -> IO ([DiffTreeItem], IO Bool)
diffWorkTree ref repo =
	ifM (Git.Ref.headExists repo)
		( diffIndex' ref [] repo
		, return ([], return True)
		)

diffIndex' :: Ref -> [CommandParam] -> Repo -> IO ([DiffTreeItem], IO Bool)
diffIndex' ref params repo =
	ifM (Git.Ref.headExists repo)
		( getdiff (Param "diff-index")
			( params ++ [Param $ fromRef ref] ++ [Param "--"] )
			repo
		, return ([], return True)
		)

{- Runs git log in --raw mode to get the changes that were made in
 - a particular commit. The output format is adjusted to be the same
 - as diff-tree --raw._-}
diffLog :: [CommandParam] -> Repo -> IO ([DiffTreeItem], IO Bool)
diffLog params = getdiff (Param "log")
	(Param "-n1" : Param "--abbrev=40" : Param "--pretty=format:" : params)

getdiff :: CommandParam -> [CommandParam] -> Repo -> IO ([DiffTreeItem], IO Bool)
getdiff command params repo = do
	(diff, cleanup) <- pipeNullSplit ps repo
	return (parseDiffRaw diff, cleanup)
  where
	ps = command : Params "-z --raw --no-renames -l0" : params

{- Parses --raw output used by diff-tree and git-log. -}
parseDiffRaw :: [String] -> [DiffTreeItem]
parseDiffRaw l = go l []
  where
	go [] c = c
	go (info:f:rest) c = go rest (mk info f : c)
	go (s:[]) _ = error $ "diff-tree parse error " ++ s

	mk info f = DiffTreeItem 
		{ srcmode = readmode srcm
		, dstmode = readmode dstm
		, srcsha = fromMaybe (error "bad srcsha") $ extractSha ssha
		, dstsha = fromMaybe (error "bad dstsha") $ extractSha dsha
		, status = s
		, file = asTopFilePath $ fromInternalGitPath $ Git.Filename.decode f
		}
	  where
		readmode = fst . Prelude.head . readOct

		-- info = :<srcmode> SP <dstmode> SP <srcsha> SP <dstsha> SP <status>
		-- All fields are fixed, so we can pull them out of
		-- specific positions in the line.
		(srcm, past_srcm) = splitAt 7 $ drop 1 info
		(dstm, past_dstm) = splitAt 7 past_srcm
		(ssha, past_ssha) = splitAt shaSize past_dstm
		(dsha, past_dsha) = splitAt shaSize $ drop 1 past_ssha
		s = drop 1 past_dsha
