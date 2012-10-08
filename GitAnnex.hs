{- git-annex main program
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module GitAnnex where

import System.Console.GetOpt

import Common.Annex
import qualified Git.Config
import qualified Git.CurrentRepo
import CmdLine
import Command
import Types.TrustLevel
import qualified Annex
import qualified Remote
import qualified Limit
import qualified Option

import qualified Command.Add
import qualified Command.Unannex
import qualified Command.Drop
import qualified Command.Move
import qualified Command.Copy
import qualified Command.Get
import qualified Command.FromKey
import qualified Command.DropKey
import qualified Command.TransferKey
import qualified Command.ReKey
import qualified Command.Reinject
import qualified Command.Fix
import qualified Command.Init
import qualified Command.Describe
import qualified Command.InitRemote
import qualified Command.Fsck
import qualified Command.Unused
import qualified Command.DropUnused
import qualified Command.AddUnused
import qualified Command.Unlock
import qualified Command.Lock
import qualified Command.PreCommit
import qualified Command.Find
import qualified Command.Whereis
import qualified Command.Log
import qualified Command.Merge
import qualified Command.Status
import qualified Command.Migrate
import qualified Command.Uninit
import qualified Command.Trust
import qualified Command.Untrust
import qualified Command.Semitrust
import qualified Command.Dead
import qualified Command.Group
import qualified Command.Ungroup
import qualified Command.Vicfg
import qualified Command.Sync
import qualified Command.AddUrl
import qualified Command.Import
import qualified Command.Map
import qualified Command.Upgrade
import qualified Command.Version
#ifdef WITH_ASSISTANT
import qualified Command.Watch
import qualified Command.Assistant
#ifdef WITH_WEBAPP
import qualified Command.WebApp
#endif
#endif

cmds :: [Command]
cmds = concat
	[ Command.Add.def
	, Command.Get.def
	, Command.Drop.def
	, Command.Move.def
	, Command.Copy.def
	, Command.Unlock.def
	, Command.Lock.def
	, Command.Sync.def
	, Command.AddUrl.def
	, Command.Import.def
	, Command.Init.def
	, Command.Describe.def
	, Command.InitRemote.def
	, Command.Reinject.def
	, Command.Unannex.def
	, Command.Uninit.def
	, Command.PreCommit.def
	, Command.Trust.def
	, Command.Untrust.def
	, Command.Semitrust.def
	, Command.Dead.def
	, Command.Group.def
	, Command.Ungroup.def
	, Command.Vicfg.def
	, Command.FromKey.def
	, Command.DropKey.def
	, Command.TransferKey.def
	, Command.ReKey.def
	, Command.Fix.def
	, Command.Fsck.def
	, Command.Unused.def
	, Command.DropUnused.def
	, Command.AddUnused.def
	, Command.Find.def
	, Command.Whereis.def
	, Command.Log.def
	, Command.Merge.def
	, Command.Status.def
	, Command.Migrate.def
	, Command.Map.def
	, Command.Upgrade.def
	, Command.Version.def
#ifdef WITH_ASSISTANT
	, Command.Watch.def
	, Command.Assistant.def
#ifdef WITH_WEBAPP
	, Command.WebApp.def
#endif
#endif
	]

options :: [Option]
options = Option.common ++
	[ Option ['N'] ["numcopies"] (ReqArg setnumcopies paramNumber)
		"override default number of copies"
	, Option [] ["trust"] (ReqArg (Remote.forceTrust Trusted) paramRemote)
		"override trust setting"
	, Option [] ["semitrust"] (ReqArg (Remote.forceTrust SemiTrusted) paramRemote)
		"override trust setting back to default"
	, Option [] ["untrust"] (ReqArg (Remote.forceTrust UnTrusted) paramRemote)
		"override trust setting to untrusted"
	, Option ['c'] ["config"] (ReqArg setgitconfig "NAME=VALUE")
		"override git configuration setting"
	, Option ['x'] ["exclude"] (ReqArg Limit.addExclude paramGlob)
		"skip files matching the glob pattern"
	, Option ['I'] ["include"] (ReqArg Limit.addInclude paramGlob)
		"don't skip files matching the glob pattern"
	, Option ['i'] ["in"] (ReqArg Limit.addIn paramRemote)
		"skip files not present in a remote"
	, Option ['C'] ["copies"] (ReqArg Limit.addCopies paramNumber)
		"skip files with fewer copies"
	, Option ['B'] ["inbackend"] (ReqArg Limit.addInBackend paramName)
		"skip files not using a key-value backend"
	, Option [] ["ingroup"] (ReqArg Limit.addInGroup paramGroup)
		"skip files not present in all remotes in a group"
	, Option [] ["largerthan"] (ReqArg Limit.addLargerThan paramSize)
		"skip files larger than a size"
	, Option [] ["smallerthan"] (ReqArg Limit.addSmallerThan paramSize)
		"skip files smaller than a size"
	, Option ['T'] ["time-limit"] (ReqArg Limit.addTimeLimit paramTime)
		"stop after the specified amount of time"
	] ++ Option.matcher
	where
		setnumcopies v = Annex.changeState $ \s -> s { Annex.forcenumcopies = readish v }
		setgitconfig :: String -> Annex ()
		setgitconfig v = do
			newg <- inRepo $ Git.Config.store v
			Annex.changeState $ \s -> s { Annex.repo = newg }

header :: String
header = "Usage: git-annex command [option ..]"

run :: [String] -> IO ()
run args = dispatch True args cmds options [] header Git.CurrentRepo.get
