{- management of the git-annex branch
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Branch (
	create,
	update,
	get,
	change,
	commit,
	files,
	refExists,
	hasOrigin,
	name	
) where

import Control.Monad (unless, when, liftM)
import Control.Monad.State (liftIO)
import System.FilePath
import System.Directory
import Data.String.Utils
import System.Cmd.Utils
import Data.Maybe
import Data.List
import System.IO
import System.Posix.IO
import System.Posix.Process
import System.Log.Logger

import Types.BranchState
import qualified GitRepo as Git
import qualified GitUnionMerge
import qualified Annex
import Utility
import Types
import Messages
import Locations

{- Name of the branch that is used to store git-annex's information. -}
name :: String
name = "git-annex"

{- Fully qualified name of the branch. -}
fullname :: String
fullname = "refs/heads/" ++ name

{- Branch's name in origin. -}
originname :: String
originname = "origin/" ++ name

{- Converts a fully qualified git ref into a short version for human
 - consumptiom. -}
shortref :: String -> String
shortref = remove "refs/heads/" . remove "refs/remotes/"
	where
		remove prefix s
			| prefix `isPrefixOf` s = drop (length prefix) s
			| otherwise = s

{- A separate index file for the branch. -}
index :: Git.Repo -> FilePath
index g = gitAnnexDir g </> "index"

{- Populates the branch's index file with the current branch contents.
 - 
 - Usually, this is only done when the index doesn't yet exist, and
 - the index is used to build up changes to be commited to the branch,
 - and merge in changes from other branches.
 -}
genIndex :: Git.Repo -> IO ()
genIndex g = GitUnionMerge.ls_tree g fullname >>= GitUnionMerge.update_index g

{- Runs an action using the branch's index file. -}
withIndex :: Annex a -> Annex a
withIndex = withIndex' False
withIndex' :: Bool -> Annex a -> Annex a
withIndex' bootstrapping a = do
	g <- Annex.gitRepo
	let f = index g
	reset <- liftIO $ Git.useIndex f

	e <- liftIO $ doesFileExist f
	unless e $ liftIO $ do
		createDirectoryIfMissing True $ takeDirectory f
		unless bootstrapping $ genIndex g

	r <- a
	liftIO reset
	return r

withIndexUpdate :: Annex a -> Annex a
withIndexUpdate a = update >> withIndex a

getState :: Annex BranchState
getState = Annex.getState Annex.branchstate

setState :: BranchState -> Annex ()
setState state = Annex.changeState $ \s -> s { Annex.branchstate = state }

setCache :: FilePath -> String -> Annex ()
setCache file content = do
	state <- getState
	setState state { cachedFile = Just file, cachedContent = content }

invalidateCache :: Annex ()
invalidateCache = do
	state <- getState
	setState state { cachedFile = Nothing, cachedContent = "" }

getCache :: FilePath -> Annex (Maybe String)
getCache file = getState >>= handle
	where
		handle state
			| cachedFile state == Just file =
				return $ Just $ cachedContent state
			| otherwise = return Nothing

{- Creates the branch, if it does not already exist. -}
create :: Annex ()
create = do
	exists <- refExists fullname
	unless exists $ do
		g <- Annex.gitRepo
		e <- hasOrigin
		if e
			then liftIO $ Git.run g "branch" [Param name, Param originname]
			else withIndex' True $
				liftIO $ GitUnionMerge.commit g "branch created" fullname []

{- Stages the journal, and commits staged changes to the branch. -}
commit :: String -> Annex ()
commit message = do
	staged <- stageJournalFiles
	when staged $ do
		g <- Annex.gitRepo
		withIndex $ liftIO $
			GitUnionMerge.commit g message fullname [fullname]

{- Ensures that the branch is up-to-date; should be called before
 - data is read from it. Runs only once per git-annex run. -}
update :: Annex ()
update = do
	state <- Annex.getState Annex.branchstate
	unless (branchUpdated state) $ withIndex $ do
		{- Since branches get merged into the index, it's important to
		 - first stage the journal into the index. Otherwise, any
		 - changes in the journal would later get staged, and might
		 - overwrite changes made during the merge.
		 -
		 - It would be cleaner to handle the merge by updating the
		 - journal, not the index, with changes from the branches.
		 -}
		staged <- stageJournalFiles

		g <- Annex.gitRepo
		r <- liftIO $ Git.pipeRead g [Param "show-ref", Param name]
		let refs = map (last . words) (lines r)
		updated <- catMaybes `liftM` mapM updateRef refs
		unless (null updated && not staged) $ liftIO $
			GitUnionMerge.commit g "update" fullname
				(fullname:updated)
		Annex.changeState $ \s -> s { Annex.branchstate = state { branchUpdated = True } }
		invalidateCache

{- Does origin/git-annex exist? -}
hasOrigin :: Annex Bool
hasOrigin = refExists originname

{- Checks if a git ref exists. -}
refExists :: String -> Annex Bool
refExists ref = do
	g <- Annex.gitRepo
	liftIO $ Git.runBool g "show-ref"
		[Param "--verify", Param "-q", Param ref]

{- Ensures that a given ref has been merged into the index. -}
updateRef :: String -> Annex (Maybe String)
updateRef ref
	| ref == fullname = return Nothing
	| otherwise = do
		g <- Annex.gitRepo
		-- checking with log to see if there have been changes
		-- is less expensive than always merging
		diffs <- liftIO $ Git.pipeRead g [
			Param "log",
			Param (name++".."++ref),
			Params "--oneline -n1"
			]
		if (null diffs)
			then return Nothing
			else do
				showSideAction $ "merging " ++ shortref ref ++ " into " ++ name ++ "..."
				-- By passing only one ref, it is actually
				-- merged into the index, preserving any
				-- changes that may already be staged.
				--
				-- However, any changes in the git-annex
				-- branch that are *not* reflected in the
				-- index will be removed. So, documentation
				-- advises users not to directly modify the
				-- branch.
				liftIO $ GitUnionMerge.merge g [ref]
				return $ Just ref

{- Records changed content of a file into the journal. -}
change :: FilePath -> String -> Annex ()
change file content = do
	setJournalFile file content
	setCache file content

{- Gets the content of a file on the branch, or content from the journal, or
 - staged in the index.
 -
 - Returns an empty string if the file doesn't exist yet. -}
get :: FilePath -> Annex String
get file = do
	cached <- getCache file
	case cached of
		Just content -> return content
		Nothing -> do
			j <- getJournalFile file
			case j of
				Just content -> do
					setCache file content
					return content
				Nothing -> withIndexUpdate $ do
					g <- Annex.gitRepo
					content <- liftIO $ catch (cat g) (const $ return "")
					setCache file content
					return content
	where
		cat g = cmdOutput "git" $ toCommand $ Git.gitCommandLine g
			[Param "cat-file", Param "blob", Param $ ':':file]

{- Runs a command, returning its output, ignoring nonzero exit
 - status, and discarding stderr. -}
cmdOutput :: FilePath -> [String] -> IO String
cmdOutput cmd params = do
	pipepair <- createPipe
	let callfunc _ = do
		closeFd (snd pipepair)
		h <- fdToHandle (fst pipepair)
		x <- hGetContentsStrict h
		hClose h
		return $! x
	let child = do
		closeFd (fst pipepair)
 		-- disable stderr output by this child,
		-- and since the logger uses it, also disable it
		liftIO $ updateGlobalLogger rootLoggerName $ setLevel EMERGENCY
		closeFd stdError

	debugM "Utility.executeFile" $ cmd ++ " " ++ show params
	
	pid <- pOpen3Raw Nothing (Just (snd pipepair)) Nothing cmd params child
	retval <- callfunc $! pid
	let rv = seq retval retval
	_ <- getProcessStatus True False pid
	return rv

{- Lists all files on the branch. There may be duplicates in the list. -}
files :: Annex [FilePath]
files = withIndexUpdate $ do
	g <- Annex.gitRepo
	bfiles <- liftIO $ Git.pipeNullSplit g
		[Params "ls-tree --name-only -r -z", Param fullname]
	jfiles <- getJournalFiles
	return $ jfiles ++ bfiles

{- Records content for a file in the branch to the journal.
 -
 - Using the journal, rather than immediatly staging content to the index
 - avoids git needing to rewrite the index after every change. -}
setJournalFile :: FilePath -> String -> Annex ()
setJournalFile file content = do
	g <- Annex.gitRepo
	liftIO $ catch (write g) $ const $ do
		createDirectoryIfMissing True $ gitAnnexJournalDir g
		createDirectoryIfMissing True $ gitAnnexTmpDir g
		write g
	where
		-- journal file is written atomically
		write g = do
			let jfile = journalFile g file
			let tmpfile = gitAnnexTmpDir g </> takeFileName jfile
			writeFile tmpfile content
			renameFile tmpfile jfile

{- Gets journalled content for a file in the branch. -}
getJournalFile :: FilePath -> Annex (Maybe String)
getJournalFile file = do
	g <- Annex.gitRepo
	liftIO $ catch (liftM Just . readFileStrict $ journalFile g file)
		(const $ return Nothing)

{- List of journal files. -}
getJournalFiles :: Annex [FilePath]
getJournalFiles = getJournalFilesRaw >>= return . map fileJournal

getJournalFilesRaw :: Annex [FilePath]
getJournalFilesRaw = do
	g <- Annex.gitRepo
	fs <- liftIO $ catch (getDirectoryContents $ gitAnnexJournalDir g)
		(const $ return [])
	return $ filter (\f -> f /= "." && f /= "..") fs

{- Stages all journal files into the index, and returns True if the index
 - was modified. -}
stageJournalFiles :: Annex Bool
stageJournalFiles = do
	l <- getJournalFilesRaw
	if null l
		then return False
		else do
			g <- Annex.gitRepo
			withIndex $ liftIO $ stage g l
			return True
	where
		stage g fs = do
			let dir = gitAnnexJournalDir g
			let paths = map (dir </>) fs
			-- inject all the journal files directly into git
			-- in one quick command
			(h, s) <- Git.pipeWriteRead g [Param "hash-object",
				Param "-w", Param "--stdin-paths"] $ unlines paths
			-- update the index, also in just one command
			GitUnionMerge.update_index g $
				index_lines (lines s) $ map fileJournal fs
			forceSuccess h
			mapM_ removeFile paths
		index_lines shas fs = map genline $ zip shas fs
		genline (sha, file) = GitUnionMerge.update_index_line sha file

{- Produces a filename to use in the journal for a file on the branch.
 -
 - The journal typically won't have a lot of files in it, so the hashing
 - used in the branch is not necessary, and all the files are put directly
 - in the journal directory.
 -}
journalFile :: Git.Repo -> FilePath -> FilePath
journalFile repo file = gitAnnexJournalDir repo </> concatMap mangle file
	where
		mangle '/' = "_"
		mangle '_' = "__"
		mangle c = [c]

{- Converts a journal file (relative to the journal dir) back to the
 - filename on the branch. -}
fileJournal :: FilePath -> FilePath
fileJournal = replace "//" "_" . replace "_" "/"
