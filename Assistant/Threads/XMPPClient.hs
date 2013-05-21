{- git-annex XMPP client
 -
 - Copyright 2012, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.XMPPClient where

import Assistant.Common
import Assistant.XMPP
import Assistant.XMPP.Client
import Assistant.NetMessager
import Assistant.Types.NetMessager
import Assistant.Types.Buddies
import Assistant.XMPP.Buddies
import Assistant.Sync
import Assistant.DaemonStatus
import qualified Remote
import Utility.ThreadScheduler
import Assistant.WebApp (UrlRenderer)
import Assistant.WebApp.Types hiding (liftAssistant)
import Assistant.WebApp.Configurators.XMPP (checkCloudRepos)
import Assistant.Alert
import Assistant.Pairing
import Assistant.XMPP.Git
import Annex.UUID
import Logs.UUID

import Network.Protocol.XMPP
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Git.Branch
import Data.Time.Clock

xmppClientThread :: UrlRenderer -> NamedThread
xmppClientThread urlrenderer = namedThread "XMPPClient" $
	restartableClient . xmppClient urlrenderer =<< getAssistant id

{- Runs the client, handing restart events. -}
restartableClient :: (XMPPCreds -> IO ()) -> Assistant ()
restartableClient a = forever $ go =<< liftAnnex getXMPPCreds
  where
	go Nothing = waitNetMessagerRestart
	go (Just creds) = do
		tid <- liftIO $ forkIO $ a creds
		waitNetMessagerRestart
		liftIO $ killThread tid

xmppClient :: UrlRenderer -> AssistantData -> XMPPCreds -> IO ()
xmppClient urlrenderer d creds =
	retry (runclient creds) =<< getCurrentTime
  where
	liftAssistant = runAssistant d
	inAssistant = liftIO . liftAssistant

	{- When the client exits, it's restarted;
	 - if it keeps failing, back off to wait 5 minutes before
	 - trying it again. -}
	retry client starttime = do
		{- The buddy list starts empty each time
		 - the client connects, so that stale info
		 - is not retained. -}
		liftAssistant $
			updateBuddyList (const noBuddies) <<~ buddyList
		e <- client
		liftAssistant $ modifyDaemonStatus_ $ \s -> s
			{ xmppClientID = Nothing }
		now <- getCurrentTime
		if diffUTCTime now starttime > 300
			then do
				liftAssistant $ debug ["connection lost; reconnecting", show e]
				retry client now
			else do
				liftAssistant $ debug ["connection failed; will retry", show e]
				threadDelaySeconds (Seconds 300)
				retry client =<< getCurrentTime

	runclient c = liftIO $ connectXMPP c $ \jid -> do
		selfjid <- bindJID jid
		putStanza gitAnnexSignature

		inAssistant $ do
			modifyDaemonStatus_ $ \s -> s
				{ xmppClientID = Just $ xmppJID creds }
			debug ["connected", logJid selfjid]

		xmppThread $ receivenotifications selfjid
		forever $ do
			a <- inAssistant $ relayNetMessage selfjid
			a

	receivenotifications selfjid = forever $ do
		l <- decodeStanza selfjid <$> getStanza
		inAssistant $ debug
			["received:", show $ map logXMPPEvent l]
		mapM_ (handle selfjid) l

	handle selfjid (PresenceMessage p) = do
		void $ inAssistant $ 
			updateBuddyList (updateBuddies p) <<~ buddyList
		resendImportantMessages selfjid p
	handle _ (GotNetMessage QueryPresence) = putStanza gitAnnexSignature
	handle _ (GotNetMessage (NotifyPush us)) = void $ inAssistant $ pull us
	handle selfjid (GotNetMessage (PairingNotification stage c u)) =
		maybe noop (inAssistant . pairMsgReceived urlrenderer stage u selfjid) (parseJID c)
	handle _ (GotNetMessage m@(Pushing _ pushstage))
		| isPushNotice pushstage = inAssistant $ handlePushNotice m
		| isPushInitiation pushstage = inAssistant $
			unlessM (queueNetPushMessage m) $ do
				let checker = checkCloudRepos urlrenderer
				void $ forkIO <~> handlePushInitiation checker m
		| otherwise = void $ inAssistant $ queueNetPushMessage m
	handle _ (Ignorable _) = noop
	handle _ (Unknown _) = noop
	handle _ (ProtocolError _) = noop

	resendImportantMessages selfjid (Presence { presenceFrom = Just jid }) = do
		let c = formatJID jid
		(stored, sent) <- inAssistant $
			checkImportantNetMessages (formatJID (baseJID jid), c)
		forM_ (S.toList $ S.difference stored sent) $ \msg -> do
			let msg' = readdressNetMessage msg c
			inAssistant $ debug
				[ "sending to new client:"
				, logJid jid
				, show $ logNetMessage msg'
				]
			a <- inAssistant $ convertNetMsg msg' selfjid
			a
			inAssistant $ sentImportantNetMessage msg c
	resendImportantMessages _ _ = noop

data XMPPEvent
	= GotNetMessage NetMessage
	| PresenceMessage Presence
	| Ignorable ReceivedStanza
	| Unknown ReceivedStanza
	| ProtocolError ReceivedStanza
	deriving Show

logXMPPEvent :: XMPPEvent -> String
logXMPPEvent (GotNetMessage m) = logNetMessage m
logXMPPEvent (PresenceMessage p) = logPresence p
logXMPPEvent (Ignorable (ReceivedPresence p)) = "Ignorable " ++ logPresence p
logXMPPEvent v = show v

logPresence :: Presence -> String
logPresence (p@Presence { presenceFrom = Just jid }) = unwords
	[ "Presence from"
	, logJid jid
	, show $ extractGitAnnexTag p
	]
logPresence _ = "Presence from unknown"

logJid :: JID -> String
logJid jid =
	let name = T.unpack (buddyName jid)
	    resource = maybe "" (T.unpack . strResource) (jidResource jid)
	in take 1 name ++ show (length name) ++ "/" ++ resource

logClient :: Client -> String
logClient (Client jid) = logJid jid

{- Decodes an XMPP stanza into one or more events. -}
decodeStanza :: JID -> ReceivedStanza -> [XMPPEvent]
decodeStanza selfjid s@(ReceivedPresence p)
	| presenceType p == PresenceError = [ProtocolError s]
	| presenceFrom p == Nothing = [Ignorable s]
	| presenceFrom p == Just selfjid = [Ignorable s]
	| otherwise = maybe [PresenceMessage p] decode (gitAnnexTagInfo p)
  where
	decode i
		| tagAttr i == pushAttr = impliedp $ GotNetMessage $ NotifyPush $
			decodePushNotification (tagValue i)
		| tagAttr i == queryAttr = impliedp $ GotNetMessage QueryPresence
		| otherwise = [Unknown s]
	{- Things sent via presence imply a presence message,
	 - along with their real meaning. -}
	impliedp v = [PresenceMessage p, v]
decodeStanza selfjid s@(ReceivedMessage m)
	| messageFrom m == Nothing = [Ignorable s]
	| messageFrom m == Just selfjid = [Ignorable s]
	| messageType m == MessageError = [ProtocolError s]
	| otherwise = [fromMaybe (Unknown s) (GotNetMessage <$> decodeMessage m)]
decodeStanza _ s = [Unknown s]

{- Waits for a NetMessager message to be sent, and relays it to XMPP.
 -
 - Chat messages must be directed to specific clients, not a base
 - account JID, due to git-annex clients using a negative presence priority.
 - PairingNotification messages are always directed at specific
 - clients, but Pushing messages are sometimes not, and need to be exploded
 - out to specific clients.
 -
 - Important messages, not directed at any specific client, 
 - are cached to be sent later when additional clients connect.
 -}
relayNetMessage :: JID -> Assistant (XMPP ())
relayNetMessage selfjid = do
	msg <- waitNetMessage
	debug ["sending:", logNetMessage msg]
	a1 <- handleImportant msg
	a2 <- convert msg
	return (a1 >> a2)
  where
	handleImportant msg = case parseJID =<< isImportantNetMessage msg of
		Just tojid
			| tojid == baseJID tojid -> do
				storeImportantNetMessage msg (formatJID tojid) $
					\c -> (baseJID <$> parseJID c) == Just tojid
				return $ putStanza presenceQuery
		_ -> return noop
	convert (Pushing c pushstage) = withOtherClient selfjid c $ \tojid -> do
		if tojid == baseJID tojid
			then do
				clients <- maybe [] (S.toList . buddyAssistants)
					<$> getBuddy (genBuddyKey tojid) <<~ buddyList
				debug ["exploded undirected message to clients", unwords $ map logClient clients]
				return $ forM_ (clients) $ \(Client jid) ->
					putStanza $ pushMessage pushstage jid selfjid
			else do
				debug ["to client:", logJid tojid]
				return $ putStanza $ pushMessage pushstage tojid selfjid
	convert msg = convertNetMsg msg selfjid

{- Converts a NetMessage to an XMPP action. -}
convertNetMsg :: NetMessage -> JID -> Assistant (XMPP ())
convertNetMsg msg selfjid = convert msg
  where
	convert (NotifyPush us) = return $ putStanza $ pushNotification us
	convert QueryPresence = return $ putStanza presenceQuery
	convert (PairingNotification stage c u) = withOtherClient selfjid c $ \tojid -> do
		changeBuddyPairing tojid True
		return $ putStanza $ pairingNotification stage u tojid selfjid
	convert (Pushing c pushstage) = withOtherClient selfjid c $ \tojid ->
		return $ putStanza $  pushMessage pushstage tojid selfjid

withOtherClient :: JID -> ClientID -> (JID -> Assistant (XMPP ())) -> (Assistant (XMPP ()))
withOtherClient selfjid c a = case parseJID c of
	Nothing -> return noop
	Just tojid
		| tojid == selfjid -> return noop
		| otherwise -> a tojid

withClient :: ClientID -> (JID -> XMPP ()) -> XMPP ()
withClient c a = maybe noop a $ parseJID c

{- Runs a XMPP action in a separate thread, using a session to allow it
 - to access the same XMPP client. -}
xmppThread :: XMPP () -> XMPP ()
xmppThread a = do
	s <- getSession
	void $ liftIO $ forkIO $
		void $ runXMPP s a

{- We only pull from one remote out of the set listed in the push
 - notification, as an optimisation.
 -
 - Note that it might be possible (though very unlikely) for the push
 - notification to take a while to be sent, and multiple pushes happen
 - before it is sent, so it includes multiple remotes that were pushed
 - to at different times. 
 -
 - It could then be the case that the remote we choose had the earlier
 - push sent to it, but then failed to get the later push, and so is not
 - fully up-to-date. If that happens, the pushRetryThread will come along
 - and retry the push, and we'll get another notification once it succeeds,
 - and pull again. -}
pull :: [UUID] -> Assistant ()
pull [] = noop
pull us = do
	rs <- filter matching . syncGitRemotes <$> getDaemonStatus
	debug $ "push notification for" : map (fromUUID . Remote.uuid ) rs
	pullone rs =<< liftAnnex (inRepo Git.Branch.current)
  where
	matching r = Remote.uuid r `S.member` s
	s = S.fromList us

	pullone [] _ = noop
	pullone (r:rs) branch =
		unlessM (null . fst <$> manualPull branch [r]) $
			pullone rs branch

{- PairReq from another client using our JID is automatically
 - accepted. This is so pairing devices all using the same XMPP
 - account works without confirmations.
 -
 - Also, autoaccept PairReq from the same JID of any repo we've
 - already paired with, as long as the UUID in the PairReq is
 - one we know about.
-}
pairMsgReceived :: UrlRenderer -> PairStage -> UUID -> JID -> JID -> Assistant ()
pairMsgReceived urlrenderer PairReq theiruuid selfjid theirjid
	| baseJID selfjid == baseJID theirjid = autoaccept
	| otherwise = do
		knownjids <- catMaybes . map (parseJID . getXMPPClientID)
			. filter isXMPPRemote . syncRemotes <$> getDaemonStatus
		um <- liftAnnex uuidMap
		if any (== baseJID theirjid) knownjids && M.member theiruuid um
			then autoaccept
			else showalert

  where
	autoaccept = do
		selfuuid <- liftAnnex getUUID
		sendNetMessage $
			PairingNotification PairAck (formatJID theirjid) selfuuid
		finishXMPPPairing theirjid theiruuid
	-- Show an alert to let the user decide if they want to pair.
	showalert = do
		button <- mkAlertButton (T.pack "Respond") urlrenderer $
			ConfirmXMPPPairFriendR $
				PairKey theiruuid $ formatJID theirjid
		void $ addAlert $ pairRequestReceivedAlert
			(T.unpack $ buddyName theirjid)
			button

{- PairAck must come from one of the buddies we are pairing with;
 - don't pair with just anyone. -}
pairMsgReceived _ PairAck theiruuid _selfjid theirjid =
	whenM (isBuddyPairing theirjid) $ do
		changeBuddyPairing theirjid False
		selfuuid <- liftAnnex getUUID
		sendNetMessage $
			PairingNotification PairDone (formatJID theirjid) selfuuid
		finishXMPPPairing theirjid theiruuid

pairMsgReceived _ PairDone _theiruuid _selfjid theirjid =
	changeBuddyPairing theirjid False

isBuddyPairing :: JID -> Assistant Bool
isBuddyPairing jid = maybe False buddyPairing <$> 
	getBuddy (genBuddyKey jid) <<~ buddyList

changeBuddyPairing :: JID -> Bool -> Assistant ()
changeBuddyPairing jid ispairing =
	updateBuddyList (M.adjust set key) <<~ buddyList
  where
	key = genBuddyKey jid
	set b = b { buddyPairing = ispairing }
