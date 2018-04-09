{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Copy where

import Command
import qualified Command.Move
import qualified Remote
import Annex.Wanted
import Annex.NumCopies

cmd :: Command
cmd = withGlobalOptions [jobsOption, jsonOptions, jsonProgressOption, annexedMatchingOptions] $
	command "copy" SectionCommon
		"copy content of files to/from another repository"
		paramPaths (seek <--< optParser)

data CopyOptions = CopyOptions
	{ copyFiles :: CmdParams
	, fromToOptions :: FromToHereOptions
	, keyOptions :: Maybe KeyOptions
	, autoMode :: Bool
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser CopyOptions
optParser desc = CopyOptions
	<$> cmdParams desc
	<*> parseFromToHereOptions
	<*> optional (parseKeyOptions <|> parseFailedTransfersOption)
	<*> parseAutoOption
	<*> parseBatchOption

instance DeferredParseClass CopyOptions where
	finishParse v = CopyOptions
		<$> pure (copyFiles v)
		<*> finishParse (fromToOptions v)
		<*> pure (keyOptions v)
		<*> pure (autoMode v)
		<*> pure (batchOption v)

seek :: CopyOptions -> CommandSeek
seek o = allowConcurrentOutput $ do
	let go = whenAnnexed $ start o
	case batchOption o of
		Batch -> batchInput Right (batchCommandAction . go)
		NoBatch -> withKeyOptions
			(keyOptions o) (autoMode o)
			(Command.Move.startKey (fromToOptions o) False)
			(withFilesInGit go)
			=<< workTreeItems (copyFiles o)

{- A copy is just a move that does not delete the source file.
 - However, auto mode avoids unnecessary copies, and avoids getting or
 - sending non-preferred content. -}
start :: CopyOptions -> FilePath -> Key -> CommandStart
start o file key = stopUnless shouldCopy $ 
	Command.Move.start (fromToOptions o) False file key
  where
	shouldCopy
		| autoMode o = want <||> numCopiesCheck file key (<)
		| otherwise = return True
	want = case fromToOptions o of
		Right (ToRemote dest) ->
			(Remote.uuid <$> getParsed dest) >>= checkwantsend
		Right (FromRemote _) -> checkwantget
		Left ToHere -> checkwantget
			
	checkwantsend = wantSend False (Just key) (AssociatedFile (Just file))
	checkwantget = wantGet False (Just key) (AssociatedFile (Just file))
