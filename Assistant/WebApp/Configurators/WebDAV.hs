{- git-annex assistant webapp configurators for WebDAV remotes
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.WebDAV where

import Assistant.WebApp.Common
import Assistant.MakeRemote
import Assistant.Sync
#ifdef WITH_WEBDAV
import qualified Remote.WebDAV as WebDAV
#endif
import qualified Remote
import Types.Remote (RemoteConfig)
import Types.StandardGroups
import Logs.PreferredContent
import Logs.Remote
import Creds

import qualified Data.Text as T
import qualified Data.Map as M

webDAVConfigurator :: Widget -> Handler RepHtml
webDAVConfigurator = page "Add a WebDAV repository" (Just Configuration)

boxConfigurator :: Widget -> Handler RepHtml
boxConfigurator = page "Add a Box.com repository" (Just Configuration)

data WebDAVInput = WebDAVInput
	{ user :: Text
	, password :: Text
	, embedCreds :: Bool
	, directory :: Text
	, enableEncryption :: EnableEncryption
	}

toCredPair :: WebDAVInput -> CredPair
toCredPair input = (T.unpack $ user input, T.unpack $ password input)

boxComAForm :: AForm WebApp WebApp WebDAVInput
boxComAForm = WebDAVInput
	<$> areq textField "Username or Email" Nothing
	<*> areq passwordField "Box.com Password" Nothing
	<*> areq checkBoxField "Share this account with friends?" (Just True)
	<*> areq textField "Directory" (Just "annex")
	<*> enableEncryptionField

webDAVCredsAForm :: AForm WebApp WebApp WebDAVInput
webDAVCredsAForm = WebDAVInput
	<$> areq textField "Username or Email" Nothing
	<*> areq passwordField "Password" Nothing
	<*> pure False
	<*> pure T.empty
	<*> pure NoEncryption -- not used!

getAddBoxComR :: Handler RepHtml
#ifdef WITH_WEBDAV
getAddBoxComR = boxConfigurator $ do
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap boxComAForm
	case result of
		FormSuccess input -> lift $ 
			makeWebDavRemote "box.com" (toCredPair input) setgroup $ M.fromList
				[ configureEncryption $ enableEncryption input
				, ("embedcreds", if embedCreds input then "yes" else "no")
				, ("type", "webdav")
				, ("url", "https://www.box.com/dav/" ++ T.unpack (directory input))
				-- Box.com has a max file size of 100 mb, but
				-- using smaller chunks has better memory
				-- performance.
				, ("chunksize", "10mb")
				]
		_ -> $(widgetFile "configurators/addbox.com")
  where
	setgroup r = runAnnex () $
		setStandardGroup (Remote.uuid r) TransferGroup
#else
getAddBoxComR = error "WebDAV not supported by this build"
#endif

getEnableWebDAVR :: UUID -> Handler RepHtml
#ifdef WITH_WEBDAV
getEnableWebDAVR uuid = do
	m <- runAnnex M.empty readRemoteLog
	let c = fromJust $ M.lookup uuid m
	let name = fromJust $ M.lookup "name" c
	let url = fromJust $ M.lookup "url" c
	mcreds <- runAnnex Nothing $
		getRemoteCredPairFor "webdav" c (WebDAV.davCreds uuid)
	case mcreds of
		Just creds -> webDAVConfigurator $ lift $
			makeWebDavRemote name creds (const noop) M.empty
		Nothing
			| "box.com/" `isInfixOf` url ->
				boxConfigurator $ showform name url
			| otherwise ->
				webDAVConfigurator $ showform name url
  where
	showform name url = do
		((result, form), enctype) <- lift $
			runFormGet $ renderBootstrap webDAVCredsAForm
		case result of
			FormSuccess input -> lift $
				makeWebDavRemote name (toCredPair input) (const noop) M.empty
			_ -> do
				description <- lift $ runAnnex "" $
					T.pack . concat <$> Remote.prettyListUUIDs [uuid]
				$(widgetFile "configurators/enablewebdav")
#else
getEnableWebDAVR _ = error "WebDAV not supported by this build"
#endif

#ifdef WITH_WEBDAV
makeWebDavRemote :: String -> CredPair -> (Remote -> Handler ()) -> RemoteConfig -> Handler ()
makeWebDavRemote name creds setup config = do
	remotename <- runAnnex name $ fromRepo $ uniqueRemoteName name 0
	liftIO $ WebDAV.setCredsEnv creds
	r <- liftAssistant $ liftAnnex $ addRemote $ do
		makeSpecialRemote name WebDAV.remote config
		return remotename
	setup r
	liftAssistant $ syncNewRemote r
	redirect $ EditNewCloudRepositoryR $ Remote.uuid r
#endif
