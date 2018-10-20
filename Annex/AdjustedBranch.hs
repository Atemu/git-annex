{- adjusted branch
 -
 - Copyright 2016-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Annex.AdjustedBranch (
	Adjustment(..),
	LinkAdjustment(..),
	PresenceAdjustment(..),
	adjustmentHidesFiles,
	OrigBranch,
	AdjBranch(..),
	originalToAdjusted,
	adjustedToOriginal,
	fromAdjustedBranch,
	getAdjustment,
	enterAdjustedBranch,
	updateAdjustedBranch,
	adjustBranch,
	adjustToCrippledFileSystem,
	mergeToAdjustedBranch,
	propigateAdjustedCommits,
	AdjustedClone(..),
	checkAdjustedClone,
	isGitVersionSupported,
	checkVersionSupported,
) where

import Annex.Common
import Types.AdjustedBranch
import Annex.AdjustedBranch.Name
import qualified Annex
import Git
import Git.Types
import qualified Git.Branch
import qualified Git.Ref
import qualified Git.Command
import qualified Git.Tree
import qualified Git.DiffTree
import qualified Git.Merge
import Git.Tree (TreeItem(..))
import Git.Sha
import Git.Env
import Git.Index
import Git.FilePath
import qualified Git.LockFile
import qualified Git.Version
import Annex.Version
import Annex.CatFile
import Annex.Link
import Annex.AutoMerge
import Annex.Content
import Annex.Perms
import Annex.GitOverlay
import Utility.Tmp.Dir
import Utility.CopyFile
import qualified Database.Keys
import Config

import qualified Data.Map as M

-- How to perform various adjustments to a TreeItem.
class AdjustTreeItem t where
	adjustTreeItem :: t -> TreeItem -> Annex (Maybe TreeItem)

instance AdjustTreeItem Adjustment where
	adjustTreeItem (LinkAdjustment l) t = adjustTreeItem l t
	adjustTreeItem (PresenceAdjustment p Nothing) t = adjustTreeItem p t
	adjustTreeItem (PresenceAdjustment p (Just l)) t =
		adjustTreeItem p t >>= \case
			Nothing -> return Nothing
			Just t' -> adjustTreeItem l t'

instance AdjustTreeItem LinkAdjustment where
	adjustTreeItem UnlockAdjustment = ifSymlink adjustToPointer noAdjust
	adjustTreeItem LockAdjustment = ifSymlink noAdjust adjustToSymlink
	adjustTreeItem FixAdjustment = ifSymlink adjustToSymlink noAdjust
	adjustTreeItem UnFixAdjustment = ifSymlink (adjustToSymlink' gitAnnexLinkCanonical) noAdjust

instance AdjustTreeItem PresenceAdjustment where
	adjustTreeItem HideMissingAdjustment = \ti@(TreeItem _ _ s) ->
		catKey s >>= \case
			Just k -> ifM (inAnnex k)
				( return (Just ti)
				, return Nothing
				)
			Nothing -> return (Just ti)
	adjustTreeItem ShowMissingAdjustment = noAdjust

ifSymlink :: (TreeItem -> Annex a) -> (TreeItem -> Annex a) -> TreeItem -> Annex a
ifSymlink issymlink notsymlink ti@(TreeItem _f m _s)
	| toTreeItemType m == Just TreeSymlink = issymlink ti
	| otherwise = notsymlink ti

noAdjust :: TreeItem -> Annex (Maybe TreeItem)
noAdjust = return . Just

adjustToPointer :: TreeItem -> Annex (Maybe TreeItem)
adjustToPointer ti@(TreeItem f _m s) = catKey s >>= \case
	Just k -> do
		Database.Keys.addAssociatedFile k f
		Just . TreeItem f (fromTreeItemType TreeFile)
			<$> hashPointerFile k
	Nothing -> return (Just ti)

adjustToSymlink :: TreeItem -> Annex (Maybe TreeItem)
adjustToSymlink = adjustToSymlink' gitAnnexLink

adjustToSymlink' :: (FilePath -> Key -> Git.Repo -> GitConfig -> IO FilePath) -> TreeItem -> Annex (Maybe TreeItem)
adjustToSymlink' gitannexlink ti@(TreeItem f _m s) = catKey s >>= \case
	Just k -> do
		absf <- inRepo $ \r -> absPath $
			fromTopFilePath f r
		linktarget <- calcRepo $ gitannexlink absf k
		Just . TreeItem f (fromTreeItemType TreeSymlink)
			<$> hashSymlink linktarget
	Nothing -> return (Just ti)

-- This is a hidden branch ref, that's used as the basis for the AdjBranch,
-- since pushes can overwrite the OrigBranch at any time. So, changes
-- are propigated from the AdjBranch to the head of the BasisBranch.
newtype BasisBranch = BasisBranch Ref

-- The basis for refs/heads/adjusted/master(unlocked) is
-- refs/basis/adjusted/master(unlocked).
basisBranch :: AdjBranch -> BasisBranch
basisBranch (AdjBranch adjbranch) = BasisBranch $
	Ref ("refs/basis/" ++ fromRef (Git.Ref.base adjbranch))

getAdjustment :: Branch -> Maybe Adjustment
getAdjustment = fmap fst . adjustedToOriginal

fromAdjustedBranch :: Branch -> OrigBranch
fromAdjustedBranch b = maybe b snd (adjustedToOriginal b)

originalBranch :: Annex (Maybe OrigBranch)
originalBranch = fmap fromAdjustedBranch <$> inRepo Git.Branch.current

{- Enter an adjusted version of current branch (or, if already in an
 - adjusted version of a branch, changes the adjustment of the original
 - branch).
 -
 - Can fail, if no branch is checked out, or if the adjusted branch already
 - exists, or if staged changes prevent a checkout.
 -}
enterAdjustedBranch :: Adjustment -> Annex Bool
enterAdjustedBranch adj = inRepo Git.Branch.current >>= \case
	Just currbranch -> case getAdjustment currbranch of
		Just curradj | curradj == adj ->
			updateAdjustedBranch adj (AdjBranch currbranch)
				(fromAdjustedBranch currbranch)
		_ -> go currbranch
	Nothing -> do
		warning "not on any branch!"
		return False
  where
	go currbranch = do
		let origbranch = fromAdjustedBranch currbranch
		let adjbranch = adjBranch $ originalToAdjusted origbranch adj
		ifM (inRepo (Git.Ref.exists adjbranch) <&&> (not <$> Annex.getState Annex.force))
			( do
				mapM_ (warning . unwords)
					[ [ "adjusted branch"
					  , Git.Ref.describe adjbranch
					  , "already exists."
					  ]
					, [ "Aborting because that branch may have changes that have not yet reached"
					  , Git.Ref.describe origbranch
					  ]
					, [ "You can check out the adjusted branch manually to enter it,"
					  , "or add the --force option to overwrite the old branch."
					  ]
					]
				return False
			, do
				b <- preventCommits $ const $ 
					adjustBranch adj origbranch
				checkoutAdjustedBranch b []
			)

checkoutAdjustedBranch :: AdjBranch -> [CommandParam] -> Annex Bool
checkoutAdjustedBranch (AdjBranch b) checkoutparams = do
	showOutput -- checkout can have output in large repos
	inRepo $ Git.Command.runBool $
		[ Param "checkout"
		, Param $ fromRef $ Git.Ref.base b
		-- always show checkout progress, even if --quiet is used
		-- to suppress other messages
		, Param "--progress"
		] ++ checkoutparams

{- Already in a branch with this adjustment, but the user asked to enter it
 - again. This should have the same result as checking out the original branch,
 - deleting and rebuilding the adjusted branch, and then checking it out.
 - But, it can be implemented more efficiently than that.
 -}
updateAdjustedBranch :: Adjustment -> AdjBranch -> OrigBranch -> Annex Bool
updateAdjustedBranch adj@(PresenceAdjustment _ _) (AdjBranch currbranch) origbranch = do
	b <- preventCommits $ \commitlck -> do
		-- Avoid losing any commits that the adjusted branch has that
		-- have not yet been propigated back to the origbranch.
		_ <- propigateAdjustedCommits' origbranch adj commitlck

		-- Git normally won't do anything when asked to check out the
		-- currently checked out branch, even when its ref has
		-- changed. Work around this by writing a raw sha to .git/HEAD.
		inRepo (Git.Ref.sha currbranch) >>= \case
			Just headsha -> inRepo $ \r ->
				writeFile (Git.Ref.headFile r) (fromRef headsha)
			_ -> noop
	
		adjustBranch adj origbranch
	
	-- Make git checkout quiet to avoid warnings about disconnected
	-- branch tips being lost.
	checkoutAdjustedBranch b [Param "--quiet"]
reenterAdjustedBranch adj@(LinkAdjustment _) _ origbranch = preventCommits $ \commitlck -> do
	-- Not really needed here, but done for consistency.
	_ <- propigateAdjustedCommits' origbranch adj commitlck
	-- No need to do anything else, because link adjustments are stable.
	return True

adjustToCrippledFileSystem :: Annex ()
adjustToCrippledFileSystem = do
	warning "Entering an adjusted branch where files are unlocked as this filesystem does not support locked files."
	whenM (isNothing <$> originalBranch) $
		void $ inRepo $ Git.Branch.commitCommand Git.Branch.AutomaticCommit
			[ Param "--quiet"
			, Param "--allow-empty"
			, Param "-m"
			, Param "commit before entering adjusted unlocked branch"
			]
	unlessM (enterAdjustedBranch (LinkAdjustment UnlockAdjustment)) $
		warning "Failed to enter adjusted branch!"

setBasisBranch :: BasisBranch -> Ref -> Annex ()
setBasisBranch (BasisBranch basis) new = 
	inRepo $ Git.Branch.update' basis new

setAdjustedBranch :: String -> AdjBranch -> Ref -> Annex ()
setAdjustedBranch msg (AdjBranch b) r = inRepo $ Git.Branch.update msg b r

adjustBranch :: Adjustment -> OrigBranch -> Annex AdjBranch
adjustBranch adj origbranch = do
	-- Start basis off with the current value of the origbranch.
	setBasisBranch basis origbranch
	sha <- adjustCommit adj basis
	setAdjustedBranch "entering adjusted branch" adjbranch sha
	return adjbranch
  where
	adjbranch = originalToAdjusted origbranch adj
	basis = basisBranch adjbranch

adjustCommit :: Adjustment -> BasisBranch -> Annex Sha
adjustCommit adj basis = do
	treesha <- adjustTree adj basis
	commitAdjustedTree treesha basis

adjustTree :: Adjustment -> BasisBranch -> Annex Sha
adjustTree adj (BasisBranch basis) = do
	let toadj = adjustTreeItem adj
	treesha <- Git.Tree.adjustTree toadj [] [] basis =<< Annex.gitRepo
	return treesha

type CommitsPrevented = Git.LockFile.LockHandle

{- Locks git's index file, preventing git from making a commit, merge, 
 - or otherwise changing the HEAD ref while the action is run.
 -
 - Throws an IO exception if the index file is already locked.
 -}
preventCommits :: (CommitsPrevented -> Annex a) -> Annex a
preventCommits = bracket setup cleanup
  where
	setup = do
		lck <- fromRepo $ indexFileLock . indexFile
		liftIO $ Git.LockFile.openLock lck
	cleanup = liftIO . Git.LockFile.closeLock

{- Commits a given adjusted tree, with the provided parent ref.
 -
 - This should always yield the same value, even if performed in different 
 - clones of a repo, at different times. The commit message and other
 - metadata is based on the parent.
 -}
commitAdjustedTree :: Sha -> BasisBranch -> Annex Sha
commitAdjustedTree treesha parent@(BasisBranch b) =
	commitAdjustedTree' treesha parent [b]

commitAdjustedTree' :: Sha -> BasisBranch -> [Ref] -> Annex Sha
commitAdjustedTree' treesha (BasisBranch basis) parents =
	go =<< catCommit basis
  where
	go Nothing = inRepo mkcommit
	go (Just basiscommit) = inRepo $ commitWithMetaData
		(commitAuthorMetaData basiscommit)
		(commitCommitterMetaData basiscommit)
		mkcommit
	mkcommit = Git.Branch.commitTree Git.Branch.AutomaticCommit
		adjustedBranchCommitMessage parents treesha

{- This message should never be changed. -}
adjustedBranchCommitMessage :: String
adjustedBranchCommitMessage = "git-annex adjusted branch"

findAdjustingCommit :: AdjBranch -> Annex (Maybe Commit)
findAdjustingCommit (AdjBranch b) = go =<< catCommit b
  where
	go Nothing = return Nothing
	go (Just c)
		| commitMessage c == adjustedBranchCommitMessage = return (Just c)
		| otherwise = case commitParent c of
			[p] -> go =<< catCommit p
			_ -> return Nothing

{- Update the currently checked out adjusted branch, merging the provided
 - branch into it. Note that the provided branch should be a non-adjusted
 - branch. -}
mergeToAdjustedBranch :: Branch -> (OrigBranch, Adjustment) -> [Git.Merge.MergeConfig] -> Annex Bool -> Git.Branch.CommitMode -> Annex Bool
mergeToAdjustedBranch tomerge (origbranch, adj) mergeconfig canresolvemerge commitmode = catchBoolIO $
	join $ preventCommits go
  where
	adjbranch@(AdjBranch currbranch) = originalToAdjusted origbranch adj
	basis = basisBranch adjbranch

	go commitsprevented =
		ifM (inRepo $ Git.Branch.changed currbranch tomerge)
			( do
				(updatedorig, _) <- propigateAdjustedCommits'
					origbranch adj commitsprevented
				changestomerge updatedorig
			, nochangestomerge
			)

	nochangestomerge = return $ return True

	{- Since the adjusted branch changes files, merging tomerge
	 - directly into it would likely result in unncessary merge
	 - conflicts. To avoid those conflicts, instead merge tomerge into
	 - updatedorig. The result of the merge can the be
	 - adjusted to yield the final adjusted branch.
	 -
	 - In order to do a merge into a ref that is not checked out,
	 - set the work tree to a temp directory, and set GIT_DIR
	 - to another temp directory, in which HEAD contains the
	 - updatedorig sha. GIT_COMMON_DIR is set to point to the real
	 - git directory, and so git can read and write objects from there,
	 - but will use GIT_DIR for HEAD and index.
	 -
	 - (Doing the merge this way also lets it run even though the main
	 - index file is currently locked.)
	 -}
	changestomerge (Just updatedorig) = do
		misctmpdir <- fromRepo gitAnnexTmpMiscDir
		void $ createAnnexDirectory misctmpdir
		tmpwt <- fromRepo gitAnnexMergeDir
		git_dir <- fromRepo Git.localGitDir
		withTmpDirIn misctmpdir "git" $ \tmpgit -> withWorkTreeRelated tmpgit $
			withemptydir tmpwt $ withWorkTree tmpwt $ do
				liftIO $ writeFile (tmpgit </> "HEAD") (fromRef updatedorig)
				-- Copy in refs and packed-refs, to work
				-- around bug in git 2.13.0, which
				-- causes it not to look in GIT_DIR for refs.
				refs <- liftIO $ dirContentsRecursive $
					git_dir </> "refs"
				let refs' = (git_dir </> "packed-refs") : refs
				liftIO $ forM_ refs' $ \src ->
					whenM (doesFileExist src) $ do
						dest <- relPathDirToFile git_dir src
						let dest' = tmpgit </> dest
						createDirectoryIfMissing True (takeDirectory dest')
						void $ createLinkOrCopy src dest'
				-- This reset makes git merge not care
				-- that the work tree is empty; otherwise
				-- it will think that all the files have
				-- been staged for deletion, and sometimes
				-- the merge includes these deletions
				-- (for an unknown reason).
				-- http://thread.gmane.org/gmane.comp.version-control.git/297237
				inRepo $ Git.Command.run [Param "reset", Param "HEAD", Param "--quiet"]
				showAction $ "Merging into " ++ fromRef (Git.Ref.base origbranch)
				merged <- inRepo (Git.Merge.merge' [] tomerge mergeconfig commitmode)
					<||> (resolveMerge (Just updatedorig) tomerge True <&&> commitResolvedMerge commitmode)
				if merged
					then do
						!mergecommit <- liftIO $ extractSha <$> readFile (tmpgit </> "HEAD")
						-- This is run after the commit lock is dropped.
						return $ postmerge mergecommit
					else return $ return False
	changestomerge Nothing = return $ return False
	
	withemptydir d a = bracketIO setup cleanup (const a)
	  where
		setup = do
			whenM (doesDirectoryExist d) $
				removeDirectoryRecursive d
			createDirectoryIfMissing True d
		cleanup _ = removeDirectoryRecursive d

	{- A merge commit has been made between the basisbranch and 
	 - tomerge. Update the basisbranch and origbranch to point
	 - to that commit, adjust it to get the new adjusted branch,
	 - and check it out.
	 -
	 - But, there may be unstaged work tree changes that conflict, 
	 - so the check out is done by making a normal merge of
	 - the new adjusted branch.
	 -}
	postmerge (Just mergecommit) = do
		setBasisBranch basis mergecommit
		inRepo $ Git.Branch.update' origbranch mergecommit
		adjtree <- adjustTree adj (BasisBranch mergecommit)
		adjmergecommit <- commitAdjustedTree adjtree (BasisBranch mergecommit)
		-- Make currbranch be the parent, so that merging
		-- this commit will be a fast-forward.
		adjmergecommitff <- commitAdjustedTree' adjtree (BasisBranch mergecommit) [currbranch]
		showAction "Merging into adjusted branch"
		ifM (autoMergeFrom adjmergecommitff (Just currbranch) mergeconfig canresolvemerge commitmode)
			( reparent adjtree adjmergecommit =<< getcurrentcommit
			, return False
			)
	postmerge Nothing = return False

	-- Now that the merge into the adjusted branch is complete,
	-- take the tree from that merge, and attach it on top of the
	-- adjmergecommit, if it's different.
	reparent adjtree adjmergecommit (Just currentcommit) = do
		if (commitTree currentcommit /= adjtree)
			then do
				c <- inRepo $ Git.Branch.commitTree Git.Branch.AutomaticCommit
					("Merged " ++ fromRef tomerge) [adjmergecommit]
					(commitTree currentcommit)
				inRepo $ Git.Branch.update "updating adjusted branch" currbranch c
				propigateAdjustedCommits origbranch adj
			else inRepo $ Git.Branch.update "updating adjusted branch" currbranch adjmergecommit
		return True
	reparent _ _ Nothing = return False

	getcurrentcommit = inRepo Git.Branch.currentUnsafe >>= \case
		Nothing -> return Nothing
		Just c -> catCommit c

{- Check for any commits present on the adjusted branch that have not yet
 - been propigated to the basis branch, and propigate them to the basis
 - branch and from there on to the orig branch.
 -
 - After propigating the commits back to the basis banch,
 - rebase the adjusted branch on top of the updated basis branch.
 -}
propigateAdjustedCommits :: OrigBranch -> Adjustment -> Annex ()
propigateAdjustedCommits origbranch adj = 
	preventCommits $ \commitsprevented ->
		join $ snd <$> propigateAdjustedCommits' origbranch adj commitsprevented
		
{- Returns sha of updated basis branch, and action which will rebase
 - the adjusted branch on top of the updated basis branch. -}
propigateAdjustedCommits'
	:: OrigBranch
	-> Adjustment
	-> CommitsPrevented
	-> Annex (Maybe Sha, Annex ())
propigateAdjustedCommits' origbranch adj _commitsprevented =
	inRepo (Git.Ref.sha basis) >>= \case
		Just origsha -> catCommit currbranch >>= \case
			Just currcommit ->
				newcommits >>= go origsha False >>= \case
					Left e -> do
						warning e
						return (Nothing, return ())
					Right newparent -> return
						( Just newparent
						, rebase currcommit newparent
						)
			Nothing -> return (Nothing, return ())
		Nothing -> return (Nothing, return ())
  where
	(BasisBranch basis) = basisBranch adjbranch
	adjbranch@(AdjBranch currbranch) = originalToAdjusted origbranch adj
	newcommits = inRepo $ Git.Branch.changedCommits basis currbranch
		-- Get commits oldest first, so they can be processed
		-- in order made.
		[Param "--reverse"]
	go parent _ [] = do
		setBasisBranch (BasisBranch basis) parent
		inRepo $ Git.Branch.update' origbranch parent
		return (Right parent)
	go parent pastadjcommit (sha:l) = catCommit sha >>= \case
		Just c
			| commitMessage c == adjustedBranchCommitMessage ->
				go parent True l
			| pastadjcommit ->
				reverseAdjustedCommit parent adj (sha, c) origbranch
					>>= \case
						Left e -> return (Left e)
						Right commit -> go commit pastadjcommit l
		_ -> go parent pastadjcommit l
	rebase currcommit newparent = do
		-- Reuse the current adjusted tree, and reparent it
		-- on top of the newparent.
		commitAdjustedTree (commitTree currcommit) (BasisBranch newparent)
			>>= inRepo . Git.Branch.update rebaseOnTopMsg currbranch

rebaseOnTopMsg :: String
rebaseOnTopMsg = "rebasing adjusted branch on top of updated original branch"

{- Reverses an adjusted commit, and commit with provided commitparent,
 - yielding a commit sha.
 -
 - Adjusts the tree of the commitparent, changing only the files that the
 - commit changed, and reverse adjusting those changes.
 -
 - The commit message, and the author and committer metadata are
 - copied over from the basiscommit. However, any gpg signature
 - will be lost, and any other headers are not copied either. -}
reverseAdjustedCommit :: Sha -> Adjustment -> (Sha, Commit) -> OrigBranch -> Annex (Either String Sha)
reverseAdjustedCommit commitparent adj (csha, basiscommit) origbranch
	| length (commitParent basiscommit) > 1 = return $
		Left $ "unable to propigate merge commit " ++ show csha ++ " back to " ++ show origbranch
	| otherwise = do
		treesha <- reverseAdjustedTree commitparent adj csha
		revadjcommit <- inRepo $ commitWithMetaData
			(commitAuthorMetaData basiscommit)
			(commitCommitterMetaData basiscommit) $
				Git.Branch.commitTree Git.Branch.AutomaticCommit
					(commitMessage basiscommit) [commitparent] treesha
		return (Right revadjcommit)

{- Adjusts the tree of the basis, changing only the files that the
 - commit changed, and reverse adjusting those changes.
 -
 - commitDiff does not support merge commits, so the csha must not be a
 - merge commit. -}
reverseAdjustedTree :: Sha -> Adjustment -> Sha -> Annex Sha
reverseAdjustedTree basis adj csha = do
	(diff, cleanup) <- inRepo (Git.DiffTree.commitDiff csha)
	let (adds, others) = partition (\dti -> Git.DiffTree.srcsha dti == nullSha) diff
	let (removes, changes) = partition (\dti -> Git.DiffTree.dstsha dti == nullSha) others
	adds' <- catMaybes <$>
		mapM (adjustTreeItem reverseadj) (map diffTreeToTreeItem adds)
	treesha <- Git.Tree.adjustTree
		(propchanges changes)
		adds'
		(map Git.DiffTree.file removes)
		basis
		=<< Annex.gitRepo
	void $ liftIO cleanup
	return treesha
  where
	reverseadj = reverseAdjustment adj
	propchanges changes ti@(TreeItem f _ _) =
		case M.lookup (norm f) m of
			Nothing -> return (Just ti) -- not changed
			Just change -> adjustTreeItem reverseadj change
	  where
		m = M.fromList $ map (\i@(TreeItem f' _ _) -> (norm f', i)) $
			map diffTreeToTreeItem changes
		norm = normalise . getTopFilePath

diffTreeToTreeItem :: Git.DiffTree.DiffTreeItem -> TreeItem
diffTreeToTreeItem dti = TreeItem
	(Git.DiffTree.file dti)
	(Git.DiffTree.dstmode dti)
	(Git.DiffTree.dstsha dti)

data AdjustedClone = InAdjustedClone | NotInAdjustedClone | NeedUpgradeForAdjustedClone

{- Cloning a repository that has an adjusted branch checked out will
 - result in the clone having the same adjusted branch checked out -- but
 - the origbranch won't exist in the clone, nor will the basis. So
 - to properly set up the adjusted branch, the origbranch and basis need
 - to be set.
 - 
 - We can't trust that the origin's origbranch matches up with the currently
 - checked out adjusted branch; the origin could have the two branches
 - out of sync (eg, due to another branch having been pushed to the origin's
 - origbranch), or due to a commit on its adjusted branch not having been
 - propigated back to origbranch.
 -
 - So, find the adjusting commit on the currently checked out adjusted
 - branch, and use the parent of that commit as the basis, and set the
 - origbranch to it.
 -
 - The repository may also need to be upgraded to a new version, if the
 - current version is too old to support adjusted branches. -}
checkAdjustedClone :: Annex AdjustedClone
checkAdjustedClone = ifM isBareRepo
	( return NotInAdjustedClone
	, go =<< inRepo Git.Branch.current
	)
  where
	go Nothing = return NotInAdjustedClone
	go (Just currbranch) = case adjustedToOriginal currbranch of
		Nothing -> return NotInAdjustedClone
		Just (adj, origbranch) -> do
			let basis@(BasisBranch bb) = basisBranch (originalToAdjusted origbranch adj)
			unlessM (inRepo $ Git.Ref.exists bb) $ do
				unlessM (inRepo $ Git.Ref.exists origbranch) $ do
					let remotebranch = Git.Ref.underBase "refs/remotes/origin" origbranch
					inRepo $ Git.Branch.update' origbranch remotebranch
				aps <- fmap commitParent <$> findAdjustingCommit (AdjBranch currbranch)
				case aps of
					Just [p] -> setBasisBranch basis p
					_ -> giveup $ "Unable to clean up from clone of adjusted branch; perhaps you should check out " ++ Git.Ref.describe origbranch
			ifM versionSupportsUnlockedPointers
				( return InAdjustedClone
				, return NeedUpgradeForAdjustedClone
				)

-- git 2.2.0 needed for GIT_COMMON_DIR which is needed
-- by updateAdjustedBranch to use withWorkTreeRelated.
isGitVersionSupported :: IO Bool
isGitVersionSupported = not <$> Git.Version.older "2.2.0"

checkVersionSupported :: Annex ()
checkVersionSupported = do
	unlessM versionSupportsAdjustedBranch $
		giveup "Adjusted branches are only supported in v6 or newer repositories."
	unlessM (liftIO isGitVersionSupported) $
		giveup "Your version of git is too old; upgrade it to 2.2.0 or newer to use adjusted branches."
