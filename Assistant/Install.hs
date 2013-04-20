{- Assistant installation
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Install where

import Assistant.Common
import Assistant.Install.AutoStart
import Assistant.Install.Menu
import Assistant.Ssh
import Locations.UserConfig
import Utility.FileMode
import Utility.Shell
import Utility.TempFile

#ifdef darwin_HOST_OS
import Utility.OSX
#else
import Utility.FreeDesktop
#endif

import System.Posix.Env

standaloneAppBase :: IO (Maybe FilePath)
standaloneAppBase = getEnv "GIT_ANNEX_APP_BASE"

{- The standalone app does not have an installation process.
 - So when it's run, it needs to set up autostarting of the assistant
 - daemon, as well as writing the programFile, and putting a
 - git-annex-shell wrapper into ~/.ssh
 -
 - Note that this is done every time it's started, so if the user moves
 - it around, the paths this sets up won't break.
 -}
ensureInstalled :: IO ()
ensureInstalled = go =<< standaloneAppBase
  where
	go Nothing = noop
	go (Just base) = do
		let program = base </> "git-annex"
		programfile <- programFile
		createDirectoryIfMissing True (parentDir programfile)
		writeFile programfile program

#ifdef darwin_HOST_OS
		autostartfile <- userAutoStart osxAutoStartLabel
#else
		installMenu program
			=<< desktopMenuFilePath "git-annex" <$> userConfigDir
		autostartfile <- autoStartPath "git-annex" <$> userConfigDir
#endif
		installAutoStart program autostartfile

		{- This shim is only updated if it doesn't
		 - already exist with the right content. -}
		sshdir <- sshDir
		let shim = sshdir </> "git-annex-shell"
		let runshell var = "exec " ++ base </> "runshell" ++
			" git-annex-shell -c \"" ++ var ++ "\""
		let content = unlines
			[ shebang
			, "set -e"
			, "if [ \"x$SSH_ORIGINAL_COMMAND\" != \"x\" ]; then"
			,   runshell "$SSH_ORIGINAL_COMMAND"
			, "else"
			,   runshell "$@"
			, "fi"
			]

		curr <- catchDefaultIO "" $ readFileStrict shim
		when (curr /= content) $ do
			createDirectoryIfMissing True (parentDir shim)
			viaTmp writeFile shim content
			modifyFileMode shim $ addModes [ownerExecuteMode]

{- Returns a cleaned up environment that lacks settings used to make the
 - standalone builds use their bundled libraries and programs.
 - Useful when calling programs not included in the standalone builds.
 -
 - For a non-standalone build, returns Nothing.
 -}
cleanEnvironment :: IO (Maybe [(String, String)])
cleanEnvironment = clean <$> getEnvironment
  where
	clean env
		| null vars = Nothing
		| otherwise = Just $ catMaybes $ map (restoreorig env) env
		| otherwise = Nothing
	  where
		vars = words $ fromMaybe "" $
			lookup "GIT_ANNEX_STANDLONE_ENV" env
		restoreorig oldenv p@(k, _v)
			| k `elem` vars = case lookup ("ORIG_" ++ k) oldenv of
				Nothing -> Nothing
				(Just v') -> Just (k, v')
			| otherwise = Just p
