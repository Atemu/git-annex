{- management of the git-annex branch
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.Branch (
	fullname,
	name,
	hasOrigin,
	hasSibling,
	siblingBranches,
	create,
	UpdateMade(..),
	update,
	forceUpdate,
	updateTo,
	get,
	getHistorical,
	change,
	maybeChange,
	commitMessage,
	commit,
	forceCommit,
	getBranch,
	files,
	rememberTreeish,
	performTransitions,
	withIndex,
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B8
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Function
import Data.Char
import Data.ByteString.Builder
import Control.Concurrent (threadDelay)
import qualified System.FilePath.ByteString as P

import Annex.Common
import Types.BranchState
import Annex.BranchState
import Annex.Journal
import Annex.GitOverlay
import Annex.Tmp
import qualified Git
import qualified Git.Command
import qualified Git.Ref
import qualified Git.RefLog
import qualified Git.Sha
import qualified Git.Branch
import qualified Git.UnionMerge
import qualified Git.UpdateIndex
import qualified Git.Tree
import qualified Git.LsTree
import Git.LsTree (lsTreeParams)
import qualified Git.HashObject
import Annex.HashObject
import Git.Types (Ref(..), fromRef, fromRef', RefDate, TreeItemType(..))
import Git.FilePath
import Annex.CatFile
import Annex.Perms
import Logs
import Logs.Transitions
import Logs.File
import Logs.Trust.Pure
import Logs.Remote.Pure
import Logs.Difference.Pure
import qualified Annex.Queue
import Annex.Branch.Transitions
import qualified Annex
import Annex.Hook
import Utility.Directory.Stream
import qualified Utility.RawFilePath as R

{- Name of the branch that is used to store git-annex's information. -}
name :: Git.Ref
name = Git.Ref "git-annex"

{- Fully qualified name of the branch. -}
fullname :: Git.Ref
fullname = Git.Ref $ "refs/heads/" <> fromRef' name

{- Branch's name in origin. -}
originname :: Git.Ref
originname = Git.Ref $ "origin/" <> fromRef' name

{- Does origin/git-annex exist? -}
hasOrigin :: Annex Bool
hasOrigin = inRepo $ Git.Ref.exists originname

{- Does the git-annex branch or a sibling foo/git-annex branch exist? -}
hasSibling :: Annex Bool
hasSibling = not . null <$> siblingBranches

{- List of git-annex (shas, branches), including the main one and any
 - from remotes. Duplicates are filtered out. -}
siblingBranches :: Annex [(Git.Sha, Git.Branch)]
siblingBranches = inRepo $ Git.Ref.matchingUniq [name]

{- Creates the branch, if it does not already exist. -}
create :: Annex ()
create = void getBranch

{- Returns the ref of the branch, creating it first if necessary. -}
getBranch :: Annex Git.Ref
getBranch = maybe (hasOrigin >>= go >>= use) return =<< branchsha
  where
	go True = do
		inRepo $ Git.Command.run
			[Param "branch", Param $ fromRef name, Param $ fromRef originname]
		fromMaybe (error $ "failed to create " ++ fromRef name)
			<$> branchsha
	go False = withIndex' True $ do
		cmode <- annexCommitMode <$> Annex.getGitConfig
		inRepo $ Git.Branch.commitAlways cmode "branch created" fullname []
	use sha = do
		setIndexSha sha
		return sha
	branchsha = inRepo $ Git.Ref.sha fullname

{- Ensures that the branch and index are up-to-date; should be
 - called before data is read from it. Runs only once per git-annex run. -}
update :: Annex BranchState
update = runUpdateOnce $ journalClean <$$> updateTo =<< siblingBranches

{- Forces an update even if one has already been run. -}
forceUpdate :: Annex UpdateMade
forceUpdate = updateTo =<< siblingBranches

data UpdateMade = UpdateMade
	{ refsWereMerged :: Bool
	, journalClean :: Bool
	}

{- Merges the specified Refs into the index, if they have any changes not
 - already in it. The Branch names are only used in the commit message;
 - it's even possible that the provided Branches have not been updated to
 - point to the Refs yet.
 - 
 - The branch is fast-forwarded if possible, otherwise a merge commit is
 - made.
 -
 - Before Refs are merged into the index, it's important to first stage the
 - journal into the index. Otherwise, any changes in the journal would
 - later get staged, and might overwrite changes made during the merge.
 - This is only done if some of the Refs do need to be merged.
 -
 - Also handles performing any Transitions that have not yet been
 - performed, in either the local branch, or the Refs.
 -
 - Returns True if any refs were merged in, False otherwise.
 -}
updateTo :: [(Git.Sha, Git.Branch)] -> Annex UpdateMade
updateTo pairs = ifM (annexMergeAnnexBranches <$> Annex.getGitConfig)
	( updateTo' pairs
	, return (UpdateMade False False)
	)

updateTo' :: [(Git.Sha, Git.Branch)] -> Annex UpdateMade
updateTo' pairs = do
	-- ensure branch exists, and get its current ref
	branchref <- getBranch
	dirty <- journalDirty
	ignoredrefs <- getIgnoredRefs
	let unignoredrefs = excludeset ignoredrefs pairs
	tomerge <- if null unignoredrefs
		then return []
		else do
			mergedrefs <- getMergedRefs
			filterM isnewer (excludeset mergedrefs unignoredrefs)
	journalclean <- if null tomerge
		{- Even when no refs need to be merged, the index
		 - may still be updated if the branch has gotten ahead 
		 - of the index, or just if the journal is dirty. -}
		then ifM (needUpdateIndex branchref)
			( lockJournal $ \jl -> do
				forceUpdateIndex jl branchref
				{- When there are journalled changes
				 - as well as the branch being updated,
				 - a commit needs to be done. -}
				when dirty $
					go branchref dirty [] jl
				return True
			, if dirty
				then ifM (annexAlwaysCommit <$> Annex.getGitConfig)
					( do
						lockJournal $ go branchref dirty []
						return True
					, return False
					)
				else return True
			)
		else do
			lockJournal $ go branchref dirty tomerge
			return True
	return $ UpdateMade
		{ refsWereMerged = not (null tomerge)
		, journalClean = journalclean
		}
  where
	excludeset s = filter (\(r, _) -> S.notMember r s)
	isnewer (r, _) = inRepo $ Git.Branch.changed fullname r
	go branchref dirty tomerge jl = stagejournalwhen dirty jl $ do
		let (refs, branches) = unzip tomerge
		merge_desc <- if null tomerge
			then commitMessage
			else return $ "merging " ++
				unwords (map Git.Ref.describe branches) ++ 
				" into " ++ fromRef name
		localtransitions <- parseTransitionsStrictly "local"
			<$> getLocal transitionsLog
		unless (null tomerge) $ do
			showSideAction merge_desc
			mapM_ checkBranchDifferences refs
			mergeIndex jl refs
		let commitrefs = nub $ fullname:refs
		ifM (handleTransitions jl localtransitions commitrefs)
			( runAnnexHook postUpdateAnnexHook
			, do
				ff <- if dirty
					then return False
					else inRepo $ Git.Branch.fastForward fullname refs
				if ff
					then updateIndex jl branchref
					else commitIndex jl branchref merge_desc commitrefs
			)
		addMergedRefs tomerge
		invalidateCache
	stagejournalwhen dirty jl a
		| dirty = stageJournal jl a
		| otherwise = withIndex a

{- Gets the content of a file, which may be in the journal, or in the index
 - (and committed to the branch).
 - 
 - Updates the branch if necessary, to ensure the most up-to-date available
 - content is returned.
 -
 - Returns an empty string if the file doesn't exist yet. -}
get :: RawFilePath -> Annex L.ByteString
get file = getCache file >>= \case
	Just content -> return content
	Nothing -> do
		st <- update
		content <- if journalIgnorable st
			then getRef fullname file
			else getLocal file
		setCache file content
		return content

{- Like get, but does not merge the branch, so the info returned may not
 - reflect changes in remotes.
 - (Changing the value this returns, and then merging is always the
 - same as using get, and then changing its value.) -}
getLocal :: RawFilePath -> Annex L.ByteString
getLocal file = go =<< getJournalFileStale file
  where
	go (Just journalcontent) = return journalcontent
	go Nothing = getRef fullname file

{- Gets the content of a file as staged in the branch's index. -}
getStaged :: RawFilePath -> Annex L.ByteString
getStaged = getRef indexref
  where
	-- This makes git cat-file be run with ":file",
	-- so it looks at the index.
	indexref = Ref ""

getHistorical :: RefDate -> RawFilePath -> Annex L.ByteString
getHistorical date file =
	-- This check avoids some ugly error messages when the reflog
	-- is empty.
	ifM (null <$> inRepo (Git.RefLog.get' [Param (fromRef fullname), Param "-n1"]))
		( giveup ("No reflog for " ++ fromRef fullname)
		, getRef (Git.Ref.dateRef fullname date) file
		)

getRef :: Ref -> RawFilePath -> Annex L.ByteString
getRef ref file = withIndex $ catFile ref file

{- Applies a function to modify the content of a file.
 -
 - Note that this does not cause the branch to be merged, it only
 - modifes the current content of the file on the branch.
 -}
change :: Journalable content => RawFilePath -> (L.ByteString -> content) -> Annex ()
change file f = lockJournal $ \jl -> f <$> getLocal file >>= set jl file

{- Applies a function which can modify the content of a file, or not. -}
maybeChange :: Journalable content => RawFilePath -> (L.ByteString -> Maybe content) -> Annex ()
maybeChange file f = lockJournal $ \jl -> do
	v <- getLocal file
	case f v of
		Just jv ->
			let b = journalableByteString jv
			in when (v /= b) $ set jl file b
		_ -> noop

{- Records new content of a file into the journal -}
set :: Journalable content => JournalLocked -> RawFilePath -> content -> Annex ()
set jl f c = do
	journalChanged
	setJournalFile jl f c
	-- Could cache the new content, but it would involve
	-- evaluating a Journalable Builder twice, which is not very
	-- efficient. Instead, assume that it's not common to need to read
	-- a log file immediately after writing it.
	invalidateCache

{- Commit message used when making a commit of whatever data has changed
 - to the git-annex brach. -}
commitMessage :: Annex String
commitMessage = fromMaybe "update" . annexCommitMessage <$> Annex.getGitConfig

{- Stages the journal, and commits staged changes to the branch. -}
commit :: String -> Annex ()
commit = whenM journalDirty . forceCommit

{- Commits the current index to the branch even without any journalled
 - changes. -}
forceCommit :: String -> Annex ()
forceCommit message = lockJournal $ \jl ->
	stageJournal jl $ do
		ref <- getBranch
		commitIndex jl ref message [fullname]

{- Commits the staged changes in the index to the branch.
 - 
 - Ensures that the branch's index file is first updated to merge the state
 - of the branch at branchref, before running the commit action. This
 - is needed because the branch may have had changes pushed to it, that
 - are not yet reflected in the index.
 - 
 - The branchref value can have been obtained using getBranch at any
 - previous point, though getting it a long time ago makes the race
 - more likely to occur.
 -
 - Note that changes may be pushed to the branch at any point in time!
 - So, there's a race. If the commit is made using the newly pushed tip of
 - the branch as its parent, and that ref has not yet been merged into the
 - index, then the result is that the commit will revert the pushed
 - changes, since they have not been merged into the index. This race
 - is detected and another commit made to fix it.
 -
 - (It's also possible for the branch to be overwritten,
 - losing the commit made here. But that's ok; the data is still in the
 - index and will get committed again later.)
 -}
commitIndex :: JournalLocked -> Git.Ref -> String -> [Git.Ref] -> Annex ()
commitIndex jl branchref message parents = do
	showStoringStateAction
	commitIndex' jl branchref message message 0 parents
commitIndex' :: JournalLocked -> Git.Ref -> String -> String -> Integer -> [Git.Ref] -> Annex ()
commitIndex' jl branchref message basemessage retrynum parents = do
	updateIndex jl branchref
	cmode <- annexCommitMode <$> Annex.getGitConfig
	committedref <- inRepo $ Git.Branch.commitAlways cmode message fullname parents
	setIndexSha committedref
	parentrefs <- commitparents <$> catObject committedref
	when (racedetected branchref parentrefs) $
		fixrace committedref parentrefs
  where
	-- look for "parent ref" lines and return the refs
	commitparents = map (Git.Ref . snd) . filter isparent .
		map (toassoc . L.toStrict) . L.split newline
	newline = fromIntegral (ord '\n')
	toassoc = separate' (== (fromIntegral (ord ' ')))
	isparent (k,_) = k == "parent"
		
	{- The race can be detected by checking the commit's
	 - parent, which will be the newly pushed branch,
	 - instead of the expected ref that the index was updated to. -}
	racedetected expectedref parentrefs
		| expectedref `elem` parentrefs = False -- good parent
		| otherwise = True -- race!
		
	{- To recover from the race, union merge the lost refs
	 - into the index. -}
	fixrace committedref lostrefs = do
		showSideAction "recovering from race"
		let retrynum' = retrynum+1
		-- small sleep to let any activity that caused
		-- the race settle down
		liftIO $ threadDelay (100000 + fromInteger retrynum')
		mergeIndex jl lostrefs
		let racemessage = basemessage ++ " (recovery from race #" ++ show retrynum' ++ "; expected commit parent " ++ show branchref ++ " but found " ++ show lostrefs ++ " )"
		commitIndex' jl committedref racemessage basemessage retrynum' [committedref]

{- Lists all files on the branch. including ones in the journal
 - that have not been committed yet. There may be duplicates in the list. -}
files :: Annex ([RawFilePath], IO Bool)
files = do
	_  <- update
	(bfs, cleanup) <- branchFiles
	-- ++ forces the content of the first list to be buffered in memory,
	-- so use getJournalledFilesStale which should be much smaller most
	-- of the time. branchFiles will stream as the list is consumed.
	l <- (++)
		<$> (map toRawFilePath <$> getJournalledFilesStale)
		<*> pure bfs
	return (l, cleanup)

{- Files in the branch, not including any from journalled changes,
 - and without updating the branch. -}
branchFiles :: Annex ([RawFilePath], IO Bool)
branchFiles = withIndex $ inRepo branchFiles'

branchFiles' :: Git.Repo -> IO ([RawFilePath], IO Bool)
branchFiles' = Git.Command.pipeNullSplit' $
	lsTreeParams Git.LsTree.LsTreeRecursive (Git.LsTree.LsTreeLong False)
		fullname
		[Param "--name-only"]

{- Populates the branch's index file with the current branch contents.
 - 
 - This is only done when the index doesn't yet exist, and the index 
 - is used to build up changes to be commited to the branch, and merge
 - in changes from other branches.
 -}
genIndex :: Git.Repo -> IO ()
genIndex g = Git.UpdateIndex.streamUpdateIndex g
	[Git.UpdateIndex.lsTree fullname g]

{- Merges the specified refs into the index.
 - Any changes staged in the index will be preserved. -}
mergeIndex :: JournalLocked -> [Git.Ref] -> Annex ()
mergeIndex jl branches = do
	prepareModifyIndex jl
	hashhandle <- hashObjectHandle
	withCatFileHandle $ \ch ->
		inRepo $ \g -> Git.UnionMerge.mergeIndex hashhandle ch g branches

{- Removes any stale git lock file, to avoid git falling over when
 - updating the index.
 -
 - Since all modifications of the index are performed inside this module,
 - and only when the journal is locked, the fact that the journal has to be
 - locked when this is called ensures that no other process is currently
 - modifying the index. So any index.lock file must be stale, caused
 - by git running when the system crashed, or the repository's disk was
 - removed, etc.
 -}
prepareModifyIndex :: JournalLocked -> Annex ()
prepareModifyIndex _jl = do
	index <- fromRepo gitAnnexIndex
	void $ liftIO $ tryIO $ R.removeLink (index <> ".lock")

{- Runs an action using the branch's index file. -}
withIndex :: Annex a -> Annex a
withIndex = withIndex' False
withIndex' :: Bool -> Annex a -> Annex a
withIndex' bootstrapping a = withIndexFile AnnexIndexFile $ \f -> do
	checkIndexOnce $ unlessM (liftIO $ doesFileExist f) $ do
		unless bootstrapping create
		createAnnexDirectory $ toRawFilePath $ takeDirectory f
		unless bootstrapping $ inRepo genIndex
	a

{- Updates the branch's index to reflect the current contents of the branch.
 - Any changes staged in the index will be preserved.
 -
 - Compares the ref stored in the lock file with the current
 - ref of the branch to see if an update is needed.
 -}
updateIndex :: JournalLocked -> Git.Ref -> Annex ()
updateIndex jl branchref = whenM (needUpdateIndex branchref) $
	forceUpdateIndex jl branchref

forceUpdateIndex :: JournalLocked -> Git.Ref -> Annex ()
forceUpdateIndex jl branchref = do
	withIndex $ mergeIndex jl [fullname]
	setIndexSha branchref

{- Checks if the index needs to be updated. -}
needUpdateIndex :: Git.Ref -> Annex Bool
needUpdateIndex branchref = do
	f <- fromRawFilePath <$> fromRepo gitAnnexIndexStatus
	committedref <- Git.Ref . firstLine' <$>
		liftIO (catchDefaultIO mempty $ B.readFile f)
	return (committedref /= branchref)

{- Record that the branch's index has been updated to correspond to a
 - given ref of the branch. -}
setIndexSha :: Git.Ref -> Annex ()
setIndexSha ref = do
	f <- fromRepo gitAnnexIndexStatus
	writeLogFile f $ fromRef ref ++ "\n"
	runAnnexHook postUpdateAnnexHook

{- Stages the journal into the index, and runs an action that
 - commits the index to the branch. Note that the action is run
 - inside withIndex so will automatically use the branch's index.
 -
 - Before staging, this removes any existing git index file lock.
 - This is safe to do because stageJournal is the only thing that
 - modifies this index file, and only one can run at a time, because
 - the journal is locked. So any existing git index file lock must be
 - stale, and the journal must contain any data that was in the process
 - of being written to the index file when it crashed.
 -}
stageJournal :: JournalLocked -> Annex () -> Annex ()
stageJournal jl commitindex = withIndex $ withOtherTmp $ \tmpdir -> do
	prepareModifyIndex jl
	g <- gitRepo
	let dir = gitAnnexJournalDir g
	(jlogf, jlogh) <- openjlog (fromRawFilePath tmpdir)
	h <- hashObjectHandle
	withJournalHandle $ \jh ->
		Git.UpdateIndex.streamUpdateIndex g
			[genstream dir h jh jlogh]
	commitindex
	liftIO $ cleanup (fromRawFilePath dir) jlogh jlogf
  where
	genstream dir h jh jlogh streamer = readDirectory jh >>= \case
		Nothing -> return ()
		Just file -> do
			unless (dirCruft file) $ do
				let path = dir P.</> toRawFilePath file
				sha <- Git.HashObject.hashFile h path
				hPutStrLn jlogh file
				streamer $ Git.UpdateIndex.updateIndexLine
					sha TreeFile (asTopFilePath $ fileJournal $ toRawFilePath file)
			genstream dir h jh jlogh streamer
	-- Clean up the staged files, as listed in the temp log file.
	-- The temp file is used to avoid needing to buffer all the
	-- filenames in memory.
	cleanup dir jlogh jlogf = do
		hFlush jlogh
		hSeek jlogh AbsoluteSeek 0
		stagedfs <- lines <$> hGetContents jlogh
		mapM_ (removeFile . (dir </>)) stagedfs
		hClose jlogh
		removeWhenExistsWith (R.removeLink) (toRawFilePath jlogf)
	openjlog tmpdir = liftIO $ openTempFile tmpdir "jlog"

{- This is run after the refs have been merged into the index,
 - but before the result is committed to the branch.
 - (Which is why it's passed the contents of the local branches's
 - transition log before that merge took place.)
 -
 - When the refs contain transitions that have not yet been done locally,
 - the transitions are performed on the index, and a new branch
 - is created from the result.
 -
 - When there are transitions recorded locally that have not been done
 - to the remote refs, the transitions are performed in the index,
 - and committed to the existing branch. In this case, the untransitioned
 - remote refs cannot be merged into the branch (since transitions
 - throw away history), so they are added to the list of refs to ignore,
 - to avoid re-merging content from them again.
 -}
handleTransitions :: JournalLocked -> Transitions -> [Git.Ref] -> Annex Bool
handleTransitions jl localts refs = do
	m <- M.fromList <$> mapM getreftransition refs
	let remotets = M.elems m
	if all (localts ==) remotets
		then return False
		else do
			let allts = combineTransitions (localts:remotets)
			let (transitionedrefs, untransitionedrefs) =
				partition (\r -> M.lookup r m == Just allts) refs
			performTransitionsLocked jl allts (localts /= allts) transitionedrefs
			ignoreRefs untransitionedrefs
			return True
  where
	getreftransition ref = do
		ts <- parseTransitionsStrictly "remote"
			<$> catFile ref transitionsLog
		return (ref, ts)

{- Performs the specified transitions on the contents of the index file,
 - commits it to the branch, or creates a new branch.
 -}
performTransitions :: Transitions -> Bool -> [Ref] -> Annex ()
performTransitions ts neednewlocalbranch transitionedrefs = lockJournal $ \jl ->
	performTransitionsLocked jl ts neednewlocalbranch transitionedrefs
performTransitionsLocked :: JournalLocked -> Transitions -> Bool -> [Ref] -> Annex ()
performTransitionsLocked jl ts neednewlocalbranch transitionedrefs = do
	-- For simplicity & speed, we're going to use the Annex.Queue to
	-- update the git-annex branch, while it usually holds changes
	-- for the head branch. Flush any such changes.
	Annex.Queue.flush
	-- Stop any running git cat-files, to ensure that the
	-- getStaged calls below use the current index, and not some older
	-- one.
	catFileStop
	withIndex $ do
		prepareModifyIndex jl
		run $ mapMaybe getTransitionCalculator tlist
		Annex.Queue.flush
		if neednewlocalbranch
			then do
				cmode <- annexCommitMode <$> Annex.getGitConfig
				committedref <- inRepo $ Git.Branch.commitAlways cmode message fullname transitionedrefs
				setIndexSha committedref
			else do
				ref <- getBranch
				commitIndex jl ref message (nub $ fullname:transitionedrefs)
  where
	message
		| neednewlocalbranch && null transitionedrefs = "new branch for transition " ++ tdesc
		| otherwise = "continuing transition " ++ tdesc
	tdesc = show $ map describeTransition tlist
	tlist = knownTransitionList ts

	{- The changes to make to the branch are calculated and applied to
	 - the branch directly, rather than going through the journal,
	 - which would be innefficient. (And the journal is not designed
	 - to hold changes to every file in the branch at once.)
	 -
	 - When a file in the branch is changed by transition code,
	 - its new content is remembered and fed into the code for subsequent
	 - transitions.
	 -}
	run [] = noop
	run changers = do
		config <- Annex.getGitConfig
		trustmap <- calcTrustMap <$> getStaged trustLog
		remoteconfigmap <- calcRemoteConfigMap <$> getStaged remoteLog
		-- partially apply, improves performance
		let changers' = map (\c -> c config trustmap remoteconfigmap) changers
		(fs, cleanup) <- branchFiles
		forM_ fs $ \f -> do
			content <- getStaged f
			apply changers' f content
		liftIO $ void cleanup
	apply [] _ _ = return ()
	apply (changer:rest) file content = case changer file content of
		PreserveFile -> apply rest file content
		ChangeFile builder -> do
			let content' = toLazyByteString builder
			if L.null content'
				then do
					Annex.Queue.addUpdateIndex
						=<< inRepo (Git.UpdateIndex.unstageFile (fromRawFilePath file))
					-- File is deleted; can't run any other
					-- transitions on it.
					return ()
				else do
					sha <- hashBlob content'
					Annex.Queue.addUpdateIndex $ Git.UpdateIndex.pureStreamer $
						Git.UpdateIndex.updateIndexLine sha TreeFile (asTopFilePath file)
					apply rest file content'

checkBranchDifferences :: Git.Ref -> Annex ()
checkBranchDifferences ref = do
	theirdiffs <- allDifferences . parseDifferencesLog
		<$> catFile ref differenceLog
	mydiffs <- annexDifferences <$> Annex.getGitConfig
	when (theirdiffs /= mydiffs) $
		giveup "Remote repository is tuned in incompatible way; cannot be merged with local repository."

ignoreRefs :: [Git.Sha] -> Annex ()
ignoreRefs rs = do
	old <- getIgnoredRefs
	let s = S.unions [old, S.fromList rs]
	f <- fromRepo gitAnnexIgnoredRefs
	writeLogFile f $
		unlines $ map fromRef $ S.elems s

getIgnoredRefs :: Annex (S.Set Git.Sha)
getIgnoredRefs = 
	S.fromList . mapMaybe Git.Sha.extractSha . B8.lines <$> content
  where
	content = do
		f <- fromRawFilePath <$> fromRepo gitAnnexIgnoredRefs
		liftIO $ catchDefaultIO mempty $ B.readFile f

addMergedRefs :: [(Git.Sha, Git.Branch)] -> Annex ()
addMergedRefs [] = return ()
addMergedRefs new = do
	old <- getMergedRefs'
	-- Keep only the newest sha for each branch.
	let l = nubBy ((==) `on` snd) (new ++ old)
	f <- fromRepo gitAnnexMergedRefs
	writeLogFile f $
		unlines $ map (\(s, b) -> fromRef s ++ '\t' : fromRef b) l

getMergedRefs :: Annex (S.Set Git.Sha)
getMergedRefs = S.fromList . map fst <$> getMergedRefs'

getMergedRefs' :: Annex [(Git.Sha, Git.Branch)]
getMergedRefs' = do
	f <- fromRawFilePath <$> fromRepo gitAnnexMergedRefs
	s <- liftIO $ catchDefaultIO mempty $ B.readFile f
	return $ map parse $ B8.lines s
  where
	parse l = 
		let (s, b) = separate' (== (fromIntegral (ord '\t'))) l
		in (Ref s, Ref b)

{- Grafts a treeish into the branch at the specified location,
 - and then removes it. This ensures that the treeish won't get garbage
 - collected, and will always be available as long as the git-annex branch
 - is available. -}
rememberTreeish :: Git.Ref -> TopFilePath -> Annex ()
rememberTreeish treeish graftpoint = lockJournal $ \jl -> do
	branchref <- getBranch
	updateIndex jl branchref
	origtree <- fromMaybe (giveup "unable to determine git-annex branch tree") <$>
		inRepo (Git.Ref.tree branchref)
	addedt <- inRepo $ Git.Tree.graftTree treeish graftpoint origtree
	cmode <- annexCommitMode <$> Annex.getGitConfig
	c <- inRepo $ Git.Branch.commitTree cmode
		"graft" [branchref] addedt
	c' <- inRepo $ Git.Branch.commitTree cmode
		"graft cleanup" [c] origtree
	inRepo $ Git.Branch.update' fullname c'
	-- The tree in c' is the same as the tree in branchref,
	-- and the index was updated to that above, so it's safe to
	-- say that the index contains c'.
	setIndexSha c'

