{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Help where

import Common.Annex
import Command
import qualified Command.Init
import qualified Command.Add
import qualified Command.Drop
import qualified Command.Get
import qualified Command.Move
import qualified Command.Copy
import qualified Command.Sync
import qualified Command.Whereis
import qualified Command.Fsck

import System.Console.GetOpt

cmd :: [Command]
cmd = [noCommit $ noRepo startNoRepo $ dontCheck repoExists $
	command "help" paramNothing seek SectionQuery "display help"]

seek :: CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start params = do
	liftIO $ start' params
	stop

startNoRepo :: CmdParams -> IO ()
startNoRepo = start'

start' :: [String] -> IO ()
start' ["options"] = showCommonOptions
start' _ = showGeneralHelp

showCommonOptions :: IO ()
showCommonOptions = putStrLn $ usageInfo "Common options:" gitAnnexOptions

showGeneralHelp :: IO ()
showGeneralHelp = putStrLn $ unlines
	[ "The most frequently used git-annex commands are:"
	, unlines $ map cmdline $ concat
		[ Command.Init.cmd
		, Command.Add.cmd
		, Command.Drop.cmd
		, Command.Get.cmd
		, Command.Move.cmd
		, Command.Copy.cmd
		, Command.Sync.cmd
		, Command.Whereis.cmd
		, Command.Fsck.cmd
		]
	, "Run 'git-annex' for a complete command list."
	, "Run 'git-annex command --help' for help on a specific command."
	, "Run `git annex help options' for a list of common options."
	]
  where
	cmdline c = "\t" ++ cmdname c ++ "\t" ++ cmddesc c
