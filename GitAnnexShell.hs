{- git-annex-shell main program
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module GitAnnexShell where

import System.Posix.Env
import System.Console.GetOpt

import Common.Annex
import qualified Git.Construct
import CmdLine
import Command
import Annex.UUID
import qualified Option
import Fields
import Utility.UserInfo

import qualified Command.ConfigList
import qualified Command.InAnnex
import qualified Command.DropKey
import qualified Command.RecvKey
import qualified Command.SendKey
import qualified Command.TransferInfo
import qualified Command.Commit

cmds_readonly :: [Command]
cmds_readonly = concat
	[ Command.ConfigList.def
	, Command.InAnnex.def
	, Command.SendKey.def
	, Command.TransferInfo.def
	]

cmds_notreadonly :: [Command]
cmds_notreadonly = concat
	[ Command.RecvKey.def
	, Command.DropKey.def
	, Command.Commit.def
	]

cmds :: [Command]
cmds = map adddirparam $ cmds_readonly ++ cmds_notreadonly
  where
	adddirparam c = c { cmdparamdesc = "DIRECTORY " ++ cmdparamdesc c }

options :: [OptDescr (Annex ())]
options = Option.common ++
	[ Option [] ["uuid"] (ReqArg checkuuid paramUUID) "local repository uuid"
	]
  where
	checkuuid expected = getUUID >>= check
	  where
		check u | u == toUUID expected = noop
		check NoUUID = unexpected "uninitialized repository"
		check u = unexpected $ "UUID " ++ fromUUID u
		unexpected s = error $
			"expected repository UUID " ++
			expected ++ " but found " ++ s

header :: String
header = "Usage: git-annex-shell [-c] command [parameters ...] [option ..]"

run :: [String] -> IO ()
run [] = failure
-- skip leading -c options, passed by eg, ssh
run ("-c":p) = run p
-- a command can be either a builtin or something to pass to git-shell
run c@(cmd:dir:params)
	| cmd `elem` builtins = builtin cmd dir params
	| otherwise = external c
run c@(cmd:_)
	-- Handle the case of being the user's login shell. It will be passed
	-- a single string containing all the real parameters.
	| "git-annex-shell " `isPrefixOf` cmd = run $ drop 1 $ shellUnEscape cmd
	| cmd `elem` builtins = failure
	| otherwise = external c

builtins :: [String]
builtins = map cmdname cmds

builtin :: String -> String -> [String] -> IO ()
builtin cmd dir params = do
	checkNotReadOnly cmd
	checkDirectory $ Just dir
	let (params', fieldparams) = partitionParams params
	let fields = filter checkField $ parseFields fieldparams
	dispatch False (cmd : params') cmds options fields header $
		Git.Construct.repoAbsPath dir >>= Git.Construct.fromAbsPath

external :: [String] -> IO ()
external params = do
	{- Normal git-shell commands all have the directory as their last
	 - parameter. -}
	let lastparam = lastMaybe =<< shellUnEscape <$> lastMaybe params
	checkDirectory lastparam
	checkNotLimited
	unlessM (boolSystem "git-shell" $ map Param $ "-c":fst (partitionParams params)) $
		error "git-shell failed"

{- Parameters between two -- markers are field settings, in the form:
 - field=value field=value
 -
 - Parameters after the last -- are ignored, these tend to be passed by
 - rsync and not be useful.
 -}
partitionParams :: [String] -> ([String], [String])
partitionParams ps = case segment (== "--") ps of
	params:fieldparams:_ -> (params, fieldparams)
	[params] -> (params, [])
	_ -> ([], [])

parseFields :: [String] -> [(String, String)]
parseFields = map (separate (== '='))

{- Only allow known fields to be set, ignore others.
 - Make sure that field values make sense. -}
checkField :: (String, String) -> Bool
checkField (field, value)
	| field == fieldName remoteUUID = fieldCheck remoteUUID value
	| field == fieldName associatedFile = fieldCheck associatedFile value
	| field == fieldName direct = fieldCheck direct value
	| otherwise = False

failure :: IO ()
failure = error $ "bad parameters\n\n" ++ usage header cmds options

checkNotLimited :: IO ()
checkNotLimited = checkEnv "GIT_ANNEX_SHELL_LIMITED"

checkNotReadOnly :: String -> IO ()
checkNotReadOnly cmd
	| cmd `elem` map cmdname cmds_readonly = noop
	| otherwise = checkEnv "GIT_ANNEX_SHELL_READONLY"

checkDirectory :: Maybe FilePath -> IO ()
checkDirectory mdir = do
	v <- getEnv "GIT_ANNEX_SHELL_DIRECTORY"
	case (v, mdir) of
		(Nothing, _) -> noop
		(Just d, Nothing) -> req d Nothing
		(Just d, Just dir)
			|  d `equalFilePath` dir -> noop
			| otherwise -> do
				home <- myHomeDir
				d' <- canondir home d
				dir' <- canondir home dir
				if d' `equalFilePath` dir'
					then noop
					else req d' (Just dir')
  where
	req d mdir' = error $ unwords 
		[ "Only allowed to access"
		, d
		, maybe "and could not determine directory from command line" ("not " ++) mdir'
		]

	{- A directory may start with ~/ or in some cases, even /~/,
	 - or could just be relative to home, or of course could
	 - be absolute. -}
	canondir home d
		| "~/" `isPrefixOf` d = return d
		| "/~/" `isPrefixOf` d = return $ drop 1 d
		| otherwise = relHome $ absPathFrom home d

checkEnv :: String -> IO ()
checkEnv var = do
	v <- getEnv var
	case v of
		Nothing -> noop
		Just "" -> noop
		Just _ -> error $ "Action blocked by " ++ var
