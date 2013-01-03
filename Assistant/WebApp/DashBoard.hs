{- git-annex assistant webapp dashboard
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.DashBoard where

import Assistant.WebApp.Common
import Assistant.WebApp.Utility
import Assistant.WebApp.Notifications
import Assistant.WebApp.Configurators
import Assistant.TransferQueue
import Utility.NotificationBroadcaster
import Logs.Transfer
import Utility.Percentage
import Utility.DataUnits
import Types.Key
import qualified Remote
import qualified Git

import Text.Hamlet
import qualified Data.Map as M
import Control.Concurrent

{- A display of currently running and queued transfers.
 -
 - Or, if there have never been any this run, an intro display. -}
transfersDisplay :: Bool -> Widget
transfersDisplay warnNoScript = do
	webapp <- lift getYesod
	current <- lift $ M.toList <$> getCurrentTransfers
	queued <- lift $ liftAssistant getTransferQueue
	autoUpdate ident NotifierTransfersR (10 :: Int) (10 :: Int)
	let transfers = simplifyTransfers $ current ++ queued
	if null transfers
		then ifM (lift $ showIntro <$> getWebAppState)
			( introDisplay ident
			, $(widgetFile "dashboard/transfers")
			)
		else $(widgetFile "dashboard/transfers")
  where
	ident = "transfers"
	isrunning info = not $
		transferPaused info || isNothing (startedTime info)

{- Simplifies a list of transfers, avoiding display of redundant
 - equivilant transfers. -}
simplifyTransfers :: [(Transfer, TransferInfo)] -> [(Transfer, TransferInfo)]
simplifyTransfers [] = []
simplifyTransfers (x:[]) = [x]
simplifyTransfers (v@(t1, _):r@((t2, _):l))
	| equivilantTransfer t1 t2 = simplifyTransfers (v:l)
	| otherwise = v : (simplifyTransfers r)

{- Called by client to get a display of currently in process transfers.
 -
 - Returns a div, which will be inserted into the calling page.
 -
 - Note that the head of the widget is not included, only its
 - body is. To get the widget head content, the widget is also 
 - inserted onto the getHomeR page.
 -}
getTransfersR :: NotificationId -> Handler RepHtml
getTransfersR nid = do
	waitNotifier getTransferBroadcaster nid

	p <- widgetToPageContent $ transfersDisplay False
	hamletToRepHtml $ [hamlet|^{pageBody p}|]

{- The main dashboard. -}
dashboard :: Bool -> Widget
dashboard warnNoScript = do
	let content = transfersDisplay warnNoScript
	$(widgetFile "dashboard/main")

getHomeR :: Handler RepHtml
getHomeR = ifM (inFirstRun)
	( redirect ConfigurationR
	, page "" (Just DashBoard) $ dashboard True
	)

{- Used to test if the webapp is running. -}
headHomeR :: Handler ()
headHomeR = noop

{- Same as HomeR, except no autorefresh at all (and no noscript warning). -}
getNoScriptR :: Handler RepHtml
getNoScriptR = page "" (Just DashBoard) $ dashboard False

{- Same as HomeR, except with autorefreshing via meta refresh. -}
getNoScriptAutoR :: Handler RepHtml
getNoScriptAutoR = page "" (Just DashBoard) $ do
	let ident = NoScriptR
	let delayseconds = 3 :: Int
	let this = NoScriptAutoR
	toWidgetHead $(hamletFile $ hamletTemplate "dashboard/metarefresh")
	dashboard False

{- The javascript code does a post. -}
postFileBrowserR :: Handler ()
postFileBrowserR = void openFileBrowser

{- Used by non-javascript browsers, where clicking on the link actually
 - opens this page, so we redirect back to the referrer. -}
getFileBrowserR :: Handler ()
getFileBrowserR = whenM openFileBrowser $ redirectBack

{- Opens the system file browser on the repo, or, as a fallback,
 - goes to a file:// url. Returns True if it's ok to redirect away
 - from the page (ie, the system file browser was opened). 
 -
 - Note that the command is opened using a different thread, to avoid
 - blocking the response to the browser on it. -}
openFileBrowser :: Handler Bool
openFileBrowser = do
	path <- runAnnex (error "no configured repository") $
		fromRepo Git.repoPath
	ifM (liftIO $ inPath cmd <&&> inPath cmd)
		( do
			void $ liftIO $ forkIO $ void $
				boolSystem cmd [Param path]
			return True
		, do
			void $ redirect $ "file://" ++ path
			return False
		)
  where
#ifdef darwin_HOST_OS
	cmd = "open"
#else
	cmd = "xdg-open"
#endif

{- Transfer controls. The GET is done in noscript mode and redirects back
 - to the referring page. The POST is called by javascript. -}
getPauseTransferR :: Transfer -> Handler ()
getPauseTransferR t = pauseTransfer t >> redirectBack
postPauseTransferR :: Transfer -> Handler ()
postPauseTransferR t = pauseTransfer t
getStartTransferR :: Transfer -> Handler ()
getStartTransferR t = startTransfer t >> redirectBack
postStartTransferR :: Transfer -> Handler ()
postStartTransferR t = startTransfer t
getCancelTransferR :: Transfer -> Handler ()
getCancelTransferR t = cancelTransfer False t >> redirectBack
postCancelTransferR :: Transfer -> Handler ()
postCancelTransferR t = cancelTransfer False t
