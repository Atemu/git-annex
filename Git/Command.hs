{- running git commands
 -
 - Copyright 2010, 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Command where

import qualified Data.ByteString.Lazy.Char8 as L

import Common
import Git
import Git.Types

{- Constructs a git command line operating on the specified repo. -}
gitCommandLine :: [CommandParam] -> Repo -> [CommandParam]
gitCommandLine params repo@(Repo { location = Dir _ } ) =
	-- force use of specified repo via --git-dir and --work-tree
	[ Param ("--git-dir=" ++ gitDir repo)
	, Param ("--work-tree=" ++ workTree repo)
	] ++ params
gitCommandLine _ repo = assertLocal repo $ error "internal"

{- Runs git in the specified repo. -}
runBool :: String -> [CommandParam] -> Repo -> IO Bool
runBool subcommand params repo = assertLocal repo $
	boolSystem "git" $ gitCommandLine (Param subcommand : params) repo

{- Runs git in the specified repo, throwing an error if it fails. -}
run :: String -> [CommandParam] -> Repo -> IO ()
run subcommand params repo = assertLocal repo $
	unlessM (runBool subcommand params repo) $
		error $ "git " ++ show params ++ " failed"

{- Runs a git subcommand and returns its output, lazily. 
 -
 - Note that this leaves the git process running, and so zombies will
 - result unless reap is called.
 -}
pipeRead :: [CommandParam] -> Repo -> IO L.ByteString
pipeRead params repo = assertLocal repo $ do
	(_, h) <- hPipeFrom "git" $ toCommand $ gitCommandLine params repo
	hSetBinaryMode h True
	L.hGetContents h

{- Runs a git subcommand, feeding it input.
 - You should call either getProcessStatus or forceSuccess on the PipeHandle. -}
pipeWrite :: [CommandParam] -> L.ByteString -> Repo -> IO PipeHandle
pipeWrite params s repo = assertLocal repo $ do
	(p, h) <- hPipeTo "git" (toCommand $ gitCommandLine params repo)
	L.hPut h s
	hClose h
	return p

{- Runs a git subcommand, feeding it input, and returning its output.
 - You should call either getProcessStatus or forceSuccess on the PipeHandle. -}
pipeWriteRead :: [CommandParam] -> L.ByteString -> Repo -> IO (PipeHandle, L.ByteString)
pipeWriteRead params s repo = assertLocal repo $ do
	(p, from, to) <- hPipeBoth "git" (toCommand $ gitCommandLine params repo)
	hSetBinaryMode from True
	L.hPut to s
	hClose to
	c <- L.hGetContents from
	return (p, c)

{- Reads null terminated output of a git command (as enabled by the -z 
 - parameter), and splits it. -}
pipeNullSplit :: [CommandParam] -> Repo -> IO [String]
pipeNullSplit params repo = map L.unpack <$> pipeNullSplitB params repo

{- For when Strings are not needed. -}
pipeNullSplitB ::[CommandParam] -> Repo -> IO [L.ByteString]
pipeNullSplitB params repo = filter (not . L.null) . L.split '\0' <$>
	pipeRead params repo

{- Reaps any zombie git processes. -}
reap :: IO ()
reap = do
	-- throws an exception when there are no child processes
	r <- catchDefaultIO (getAnyProcessStatus False True) Nothing
	maybe (return ()) (const reap) r
