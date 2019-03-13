{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Init where

import Command
import Annex.Init
import Annex.Version
import Types.RepoVersion
import qualified Annex.SpecialRemote

import qualified Data.Map as M
	
cmd :: Command
cmd = dontCheck repoExists $
	command "init" SectionSetup "initialize git-annex"
		paramDesc (seek <$$> optParser)

data InitOptions = InitOptions
	{ initDesc :: String
	, initVersion :: Maybe RepoVersion
	}

optParser :: CmdParamsDesc -> Parser InitOptions
optParser desc = InitOptions
	<$> (unwords <$> cmdParams desc)
	<*> optional (option (str >>= parseRepoVersion)
		( long "version" <> metavar paramValue
		<> help "Override default annex.version"
		))

parseRepoVersion :: Monad m => String -> m RepoVersion
parseRepoVersion s = case RepoVersion <$> readish s of
	Nothing -> fail $ "version parse error"
	Just v
		| v `elem` supportedVersions -> return v
		| otherwise -> case M.lookup v autoUpgradeableVersions of
			Just v' -> return v'
			Nothing -> fail $ s ++ " is not a currently supported repository version"

seek :: InitOptions -> CommandSeek
seek = commandAction . start

start :: InitOptions -> CommandStart
start os = do
	showStart' "init" (Just $ initDesc os)
	next $ perform os

perform :: InitOptions -> CommandPerform
perform os = do
	initialize
		(if null (initDesc os) then Nothing else Just (initDesc os))
		(initVersion os)
	Annex.SpecialRemote.autoEnable
	next $ return True
