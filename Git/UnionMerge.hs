{- git-union-merge library
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.UnionMerge (
	merge,
	mergeIndex
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Set as S

import Common
import Git
import Git.Sha
import Git.CatFile
import Git.Command
import Git.UpdateIndex
import Git.HashObject
import Git.Types
import Git.FilePath

{- Performs a union merge between two branches, staging it in the index.
 - Any previously staged changes in the index will be lost.
 -
 - Should be run with a temporary index file configured by useIndex.
 -}
merge :: Ref -> Ref -> Repo -> IO ()
merge x y repo = do
	h <- catFileStart repo
	streamUpdateIndex repo
		[ lsTree x repo
		, mergeTrees x y h repo
		]
	catFileStop h

{- Merges a list of branches into the index. Previously staged changes in
 - the index are preserved (and participate in the merge). -}
mergeIndex :: CatFileHandle -> Repo -> [Ref] -> IO ()
mergeIndex h repo bs =
	streamUpdateIndex repo $ map (\b -> mergeTreeIndex b h repo) bs

{- For merging two trees. -}
mergeTrees :: Ref -> Ref -> CatFileHandle -> Repo -> Streamer
mergeTrees (Ref x) (Ref y) h = doMerge h $ "diff-tree":diffOpts ++ [x, y]

{- For merging a single tree into the index. -}
mergeTreeIndex :: Ref -> CatFileHandle -> Repo -> Streamer
mergeTreeIndex (Ref x) h = doMerge h $
	"diff-index" : diffOpts ++ ["--cached", x]

diffOpts :: [String]
diffOpts = ["--raw", "-z", "-r", "--no-renames", "-l0"]

{- Streams update-index changes to perform a merge,
 - using git to get a raw diff. -}
doMerge :: CatFileHandle -> [String] -> Repo -> Streamer
doMerge ch differ repo streamer = do
	(diff, cleanup) <- pipeNullSplit (map Param differ) repo
	go diff
	void $ cleanup
  where
	go [] = noop
	go (info:file:rest) = mergeFile info file ch repo >>=
		maybe (go rest) (\l -> streamer l >> go rest)
	go (_:[]) = error $ "parse error " ++ show differ

{- Given an info line from a git raw diff, and the filename, generates
 - a line suitable for update-index that union merges the two sides of the
 - diff. -}
mergeFile :: String -> FilePath -> CatFileHandle -> Repo -> IO (Maybe String)
mergeFile info file h repo = case filter (/= nullSha) [Ref asha, Ref bsha] of
	[] -> return Nothing
	(sha:[]) -> use sha
	shas -> use
		=<< either return (\s -> hashObject BlobObject (unlines s) repo)
		=<< calcMerge . zip shas <$> mapM getcontents shas
  where
	[_colonmode, _bmode, asha, bsha, _status] = words info
	use sha = return $ Just $
		updateIndexLine sha FileBlob $ asTopFilePath file
	-- We don't know how the file is encoded, but need to
	-- split it into lines to union merge. Using the
	-- FileSystemEncoding for this is a hack, but ensures there
	-- are no decoding errors. Note that this works because
	-- hashObject sets fileEncoding on its write handle.
	getcontents s = lines . encodeW8 . L.unpack <$> catObject h s

{- Calculates a union merge between a list of refs, with contents.
 -
 - When possible, reuses the content of an existing ref, rather than
 - generating new content.
 -}
calcMerge :: [(Ref, [String])] -> Either Ref [String]
calcMerge shacontents
	| null reuseable = Right $ new
	| otherwise = Left $ fst $ Prelude.head reuseable
  where
	reuseable = filter (\c -> sorteduniq (snd c) == new) shacontents
	new = sorteduniq $ concat $ map snd shacontents
	sorteduniq = S.toList . S.fromList
