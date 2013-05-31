{- running git commands
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Command where

import System.Process (std_out, env)

import Common
import Git
import Git.Types
import qualified Utility.CoProcess as CoProcess

{- Constructs a git command line operating on the specified repo. -}
gitCommandLine :: [CommandParam] -> Repo -> [CommandParam]
gitCommandLine params Repo { location = l@(Local _ _ ) } = setdir : settree ++ params
  where
	setdir = Param $ "--git-dir=" ++ gitdir l
	settree = case worktree l of
		Nothing -> []
		Just t -> [Param $ "--work-tree=" ++ t]
gitCommandLine _ repo = assertLocal repo $ error "internal"

{- Runs git in the specified repo. -}
runBool :: [CommandParam] -> Repo -> IO Bool
runBool params repo = assertLocal repo $
	boolSystemEnv "git"
		(gitCommandLine params repo)
		(gitEnv repo)

{- Runs git in the specified repo, throwing an error if it fails. -}
run :: [CommandParam] -> Repo -> IO ()
run params repo = assertLocal repo $
	unlessM (runBool params repo) $
		error $ "git " ++ show params ++ " failed"

{- Runs git and forces it to be quiet, throwing an error if it fails. -}
runQuiet :: [CommandParam] -> Repo -> IO ()
runQuiet params repo = withQuietOutput createProcessSuccess $
	(proc "git" $ toCommand $ gitCommandLine (params) repo)
		{ env = gitEnv repo }

{- Runs a git command and returns its output, lazily.
 -
 - Also returns an action that should be used when the output is all
 - read (or no more is needed), that will wait on the command, and
 - return True if it succeeded. Failure to wait will result in zombies.
 -}
pipeReadLazy :: [CommandParam] -> Repo -> IO (String, IO Bool)
pipeReadLazy params repo = assertLocal repo $ do
	(_, Just h, _, pid) <- createProcess p { std_out = CreatePipe }
	fileEncoding h
	c <- hGetContents h
	return (c, checkSuccessProcess pid)
  where
	p  = gitCreateProcess params repo

{- Runs a git command, and returns its output, strictly.
 -
 - Nonzero exit status is ignored.
 -}
pipeReadStrict :: [CommandParam] -> Repo -> IO String
pipeReadStrict params repo = assertLocal repo $
	withHandle StdoutHandle (createProcessChecked ignoreFailureProcess) p $ \h -> do
		fileEncoding h
		output <- hGetContentsStrict h
		hClose h
		return output
  where
	p  = gitCreateProcess params repo

{- Runs a git command, feeding it input, and returning its output,
 - which is expected to be fairly small, since it's all read into memory
 - strictly. -}
pipeWriteRead :: [CommandParam] -> String -> Repo -> IO String
pipeWriteRead params s repo = assertLocal repo $
	writeReadProcessEnv "git" (toCommand $ gitCommandLine params repo) 
		(gitEnv repo) s (Just fileEncoding)

{- Runs a git command, feeding it input on a handle with an action. -}
pipeWrite :: [CommandParam] -> Repo -> (Handle -> IO ()) -> IO ()
pipeWrite params repo = withHandle StdinHandle createProcessSuccess $
	gitCreateProcess params repo

{- Reads null terminated output of a git command (as enabled by the -z 
 - parameter), and splits it. -}
pipeNullSplit :: [CommandParam] -> Repo -> IO ([String], IO Bool)
pipeNullSplit params repo = do
	(s, cleanup) <- pipeReadLazy params repo
	return (filter (not . null) $ split sep s, cleanup)
  where
	sep = "\0"

pipeNullSplitStrict :: [CommandParam] -> Repo -> IO [String]
pipeNullSplitStrict params repo = do
	s <- pipeReadStrict params repo
	return $ filter (not . null) $ split sep s
  where
	sep = "\0"

pipeNullSplitZombie :: [CommandParam] -> Repo -> IO [String]
pipeNullSplitZombie params repo = leaveZombie <$> pipeNullSplit params repo

{- Doesn't run the cleanup action. A zombie results. -}
leaveZombie :: (a, IO Bool) -> a
leaveZombie = fst

{- Runs a git command as a coprocess. -}
gitCoProcessStart :: Bool -> [CommandParam] -> Repo -> IO CoProcess.CoProcessHandle
gitCoProcessStart restartable params repo = CoProcess.start restartable "git"
	(toCommand $ gitCommandLine params repo)
	(gitEnv repo)

gitCreateProcess :: [CommandParam] -> Repo -> CreateProcess
gitCreateProcess params repo =
	(proc "git" $ toCommand $ gitCommandLine params repo)
			{ env = gitEnv repo }
