{- git-annex webapp launcher
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.WebApp where

import Common.Annex
import Command
import Assistant
import Assistant.Common
import Assistant.NamedThread
import Assistant.Threads.WebApp
import Assistant.WebApp
import Assistant.Install
import Annex.Environment
import Utility.WebApp
import Utility.Daemon (checkDaemon)
#ifdef __ANDROID__
import Utility.Env
#endif
import Annex.Init
import qualified Git
import qualified Git.Config
import qualified Git.CurrentRepo
import qualified Annex
import Config.Files
import Upgrade
import Annex.Version

import Control.Concurrent
import Control.Concurrent.STM
import System.Process (env, std_out, std_err)
import Network.Socket (HostName)
import System.Environment (getArgs)

def :: [Command]
def = [ withOptions [listenOption] $
	noCommit $ noRepo startNoRepo $ dontCheck repoExists $ notBareRepo $
	command "webapp" paramNothing seek SectionCommon "launch webapp"]

listenOption :: Option
listenOption = fieldOption [] "listen" paramAddress
	"accept connections to this address"

seek :: CommandSeek
seek ps = do
	listenhost <- getOptionField listenOption return
	withNothing (start listenhost) ps

start :: Maybe HostName -> CommandStart
start = start' True

start' :: Bool -> Maybe HostName -> CommandStart
start' allowauto listenhost = do
	liftIO ensureInstalled
	ifM isInitialized 
		( maybe notinitialized (go <=< needsUpgrade) =<< getVersion
		, if allowauto
			then liftIO $ startNoRepo []
			else notinitialized
		)
	stop
  where
	go cannotrun = do
		browser <- fromRepo webBrowser
		f <- liftIO . absPath =<< fromRepo gitAnnexHtmlShim
		listenhost' <- if isJust listenhost
			then pure listenhost
			else annexListen <$> Annex.getGitConfig
		ifM (checkpid <&&> checkshim f)
			( if isJust listenhost
				then error "The assistant is already running, so --listen cannot be used."
				else do
					url <- liftIO . readFile
						=<< fromRepo gitAnnexUrlFile
					liftIO $ if isJust listenhost'
						then putStrLn url
						else liftIO $ openBrowser browser f url Nothing Nothing
			, do
				startDaemon True True Nothing cannotrun listenhost' $ Just $ 
					\origout origerr url htmlshim ->
						if isJust listenhost'
							then maybe noop (`hPutStrLn` url) origout
							else openBrowser browser htmlshim url origout origerr
			)
	checkpid = do
		pidfile <- fromRepo gitAnnexPidFile
		liftIO $ isJust <$> checkDaemon pidfile
	checkshim f = liftIO $ doesFileExist f
	notinitialized = do
		g <- Annex.gitRepo
		liftIO $ cannotStartIn (Git.repoLocation g) "repository has not been initialized by git-annex"
		liftIO $ firstRun listenhost

{- When run without a repo, start the first available listed repository in
 - the autostart file. If none, it's our first time being run! -}
startNoRepo :: CmdParams -> IO ()
startNoRepo _ = do
	-- FIXME should be able to reuse regular getopt, but 
	-- it currently runs in the Annex monad.
	args <- getArgs
	let listenhost = headMaybe $ map (snd . separate (== '=')) $ 
		filter ("--listen=" `isPrefixOf`) args

	go listenhost =<< liftIO (filterM doesDirectoryExist =<< readAutoStartFile)
  where
	go listenhost [] = firstRun listenhost
	go listenhost (d:ds) = do
		v <- tryNonAsync $ do
			setCurrentDirectory d
			Annex.new =<< Git.CurrentRepo.get
		case v of
			Left e -> do
				cannotStartIn d (show e)
				go listenhost ds
			Right state -> void $ Annex.eval state $ do
				whenM (fromRepo Git.repoIsLocalBare) $
					error $ d ++ " is a bare git repository, cannot run the webapp in it"
				callCommandAction $
					start' False listenhost

cannotStartIn :: FilePath -> String -> IO ()
cannotStartIn d reason = warningIO $ "unable to start webapp in repository " ++ d ++ ": " ++ reason

{- Run the webapp without a repository, which prompts the user, makes one,
 - changes to it, starts the regular assistant, and redirects the
 - browser to its url.
 -
 - This is a very tricky dance -- The first webapp calls the signaler,
 - which signals the main thread when it's ok to continue by writing to a
 - MVar. The main thread starts the second webapp, and uses its callback
 - to write its url back to the MVar, from where the signaler retrieves it,
 - returning it to the first webapp, which does the redirect.
 -
 - Note that it's important that mainthread never terminates! Much
 - of this complication is due to needing to keep the mainthread running.
 -}
firstRun :: Maybe HostName -> IO ()
firstRun listenhost = do
	checkEnvironmentIO
	{- Without a repository, we cannot have an Annex monad, so cannot
	 - get a ThreadState. Using undefined is only safe because the
	 - webapp checks its noAnnex field before accessing the
	 - threadstate. -}
	let st = undefined
	{- Get a DaemonStatus without running in the Annex monad. -}
	dstatus <- atomically . newTMVar =<< newDaemonStatus
	d <- newAssistantData st dstatus
	urlrenderer <- newUrlRenderer
	v <- newEmptyMVar
	let callback a = Just $ a v
	runAssistant d $ do
		startNamedThread urlrenderer $
			webAppThread d urlrenderer True Nothing
				(callback signaler)
				listenhost
				(callback mainthread)
		waitNamedThreads
  where
	signaler v = do
		putMVar v ""
		takeMVar v
	mainthread v url htmlshim
		| isJust listenhost = do
			putStrLn url
			hFlush stdout
			go
		| otherwise = do
			browser <- maybe Nothing webBrowser
				<$> catchDefaultIO Nothing Git.Config.global
			openBrowser browser htmlshim url Nothing Nothing
			go
	  where
		go = do
			_wait <- takeMVar v
			state <- Annex.new =<< Git.CurrentRepo.get
			Annex.eval state $
				startDaemon True True Nothing Nothing listenhost $ Just $
					sendurlback v
	sendurlback v _origout _origerr url _htmlshim = do
		recordUrl url
		putMVar v url

recordUrl :: String -> IO ()
#ifdef __ANDROID__
{- The Android app has a menu item that opens the url recorded
 - in this file. -}
recordUrl url = writeFile "/sdcard/git-annex.home/.git-annex-url" url
#else
recordUrl _ = noop
#endif

openBrowser :: Maybe FilePath -> FilePath -> String -> Maybe Handle -> Maybe Handle -> IO ()
#ifndef __ANDROID__
openBrowser mcmd htmlshim _realurl outh errh = runbrowser
#else
openBrowser mcmd htmlshim realurl outh errh = do
	recordUrl url
	{- Android's `am` command does not work reliably across the
	 - wide range of Android devices. Intead, FIFO should be set to 
	 - the filename of a fifo that we can write the URL to. -}
	v <- getEnv "FIFO"
	case v of
		Nothing -> runbrowser
		Just f -> void $ forkIO $ do
			fd <- openFd f WriteOnly Nothing defaultFileFlags
			void $ fdWrite fd url
			closeFd fd
#endif
  where
	p = case mcmd of
		Just cmd -> proc cmd [htmlshim]
		Nothing -> browserProc url
#ifdef __ANDROID__
	{- Android does not support file:// urls, but neither is
	 - the security of the url in the process table important
	 - there, so just use the real url. -}
	url = realurl
#else
	url = fileUrl htmlshim
#endif
	runbrowser = do
		hPutStrLn (fromMaybe stdout outh) $ "Launching web browser on " ++ url
		hFlush stdout
		environ <- cleanEnvironment
		(_, _, _, pid) <- createProcess p
			{ env = environ
			, std_out = maybe Inherit UseHandle outh
			, std_err = maybe Inherit UseHandle errh
			}
		exitcode <- waitForProcess pid
		unless (exitcode == ExitSuccess) $
			hPutStrLn (fromMaybe stderr errh) "failed to start web browser"

{- web.browser is a generic git config setting for a web browser program -}
webBrowser :: Git.Repo -> Maybe FilePath
webBrowser = Git.Config.getMaybe "web.browser"

fileUrl :: FilePath -> String
fileUrl file = "file://" ++ file
