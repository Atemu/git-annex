{- git-annex assistant webapp control
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Control where

import Assistant.WebApp.Common
import Locations.UserConfig
import Utility.LogFile
import Assistant.DaemonStatus

import Control.Concurrent
import System.Posix (getProcessID, signalProcess, sigTERM)
import qualified Data.Map as M

getShutdownR :: Handler RepHtml
getShutdownR = page "Shutdown" Nothing $
	$(widgetFile "control/shutdown")

getShutdownConfirmedR :: Handler RepHtml
getShutdownConfirmedR = page "Shutdown" Nothing $ do
	{- Wait 2 seconds before shutting down, to give the web page time
	 - to display. -}
	void $ liftIO $ forkIO $ do
		threadDelay 2000000
		signalProcess sigTERM =<< getProcessID
	$(widgetFile "control/shutdownconfirmed")

{- Quite a hack, and doesn't redirect the browser window. -}
getRestartR :: Handler RepHtml
getRestartR = page "Restarting" Nothing $ do
	void $ liftIO $ forkIO $ do
		threadDelay 2000000
		program <- readProgramFile
		unlessM (boolSystem "sh" [Param "-c", Param $ restartcommand program]) $
			error "restart failed"
	$(widgetFile "control/restarting")
  where
	restartcommand program = program ++ " assistant --stop; " ++
		program ++ " webapp"

getRestartThreadR :: ThreadName -> Handler ()
getRestartThreadR name = do
	m <- liftAssistant $ startedThreads <$> getDaemonStatus
	liftIO $ maybe noop snd $ M.lookup name m
	redirectBack

getLogR :: Handler RepHtml
getLogR = page "Logs" Nothing $ do
	logfile <- lift $ runAnnex undefined $ fromRepo gitAnnexLogFile
	logs <- liftIO $ listLogs logfile
	logcontent <- liftIO $ concat <$> mapM readFile logs
	$(widgetFile "control/log")
