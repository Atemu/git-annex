{- git over XMPP
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.XMPP.Git where

import Assistant.Common
import Assistant.NetMessager
import Assistant.Types.NetMessager
import Assistant.XMPP
import Assistant.XMPP.Buddies
import Assistant.DaemonStatus
import Assistant.Alert
import Assistant.MakeRemote
import Assistant.Sync
import qualified Command.Sync
import qualified Annex.Branch
import Annex.UUID
import Config
import Git
import qualified Git.Branch
import Locations.UserConfig
import qualified Types.Remote as Remote
import Utility.FileMode

import Network.Protocol.XMPP
import qualified Data.Text as T
import System.Posix.Env
import System.Posix.Types
import System.Process (std_in, std_out, std_err)
import Control.Concurrent
import System.Timeout
import qualified Data.ByteString as B
import qualified Data.Map as M

finishXMPPPairing :: JID -> UUID -> Assistant ()
finishXMPPPairing jid u = void $ alertWhile alert $
	makeXMPPGitRemote buddy (baseJID jid) u
  where
	buddy = T.unpack $ buddyName jid
	alert = pairRequestAcknowledgedAlert buddy Nothing

gitXMPPLocation :: JID -> String
gitXMPPLocation jid = "xmpp::" ++ T.unpack (formatJID $ baseJID jid)

makeXMPPGitRemote :: String -> JID -> UUID -> Assistant Bool
makeXMPPGitRemote buddyname jid u = do
	remote <- liftAnnex $ addRemote $
		makeGitRemote buddyname $ gitXMPPLocation jid
	liftAnnex $ storeUUID (remoteConfig (Remote.repo remote) "uuid") u
	syncNewRemote remote
	return True

{- Pushes over XMPP, communicating with a specific client.
 - Runs an arbitrary IO action to push, which should run git-push with
 - an xmpp:: url.
 -
 - To handle xmpp:: urls, git push will run git-remote-xmpp, which is
 - injected into its PATH, and in turn runs git-annex xmppgit. The
 - dataflow them becomes:
 - 
 - git push <--> git-annex xmppgit <--> xmppPush <-------> xmpp
 -                                                          |
 - git receive-pack <--> xmppReceivePack <---------------> xmpp
 - 
 - The pipe between git-annex xmppgit and us is set up and communicated
 - using two environment variables, relayIn and relayOut, that are set
 - to the file descriptors to use. Another, relayControl, is used to
 - propigate the exit status of git receive-pack.
 -
 - We listen at the other end of the pipe and relay to and from XMPP.
 -}
xmppPush :: ClientID -> (Git.Repo -> IO Bool) -> Assistant Bool
xmppPush cid gitpush = runPush SendPack cid handleDeferred $ do
	sendNetMessage $ Pushing cid StartingPush

	(Fd inf, writepush) <- liftIO createPipe
	(readpush, Fd outf) <- liftIO createPipe
	(Fd controlf, writecontrol) <- liftIO createPipe

	tmp <- liftAnnex $ fromRepo gitAnnexTmpDir
	let tmpdir = tmp </> "xmppgit"
	installwrapper tmpdir

	env <- liftIO getEnvironment
	path <- liftIO getSearchPath
	let myenv = M.fromList
		[ ("PATH", join [searchPathSeparator] $ tmpdir:path)
		, (relayIn, show inf)
		, (relayOut, show outf)
		, (relayControl, show controlf)
		]
		`M.union` M.fromList env

	inh <- liftIO $ fdToHandle readpush
	outh <- liftIO $ fdToHandle writepush
	controlh <- liftIO $ fdToHandle writecontrol
	
	t1 <- forkIO <~> toxmpp inh
	t2 <- forkIO <~> fromxmpp outh controlh

	{- This can take a long time to run, so avoid running it in the
	 - Annex monad. Also, override environment. -}
	g <- liftAnnex gitRepo
	r <- liftIO $ gitpush $ g { gitEnv = Just $ M.toList myenv }

	liftIO $ do
		mapM_ killThread [t1, t2]
		mapM_ hClose [inh, outh, controlh]

	return r
  where
	toxmpp inh = forever $ do
		b <- liftIO $ B.hGetSome inh chunkSize
		if B.null b
			then liftIO $ killThread =<< myThreadId
			else sendNetMessage $ Pushing cid $ SendPackOutput b
	fromxmpp outh controlh = forever $ do
		m <- timeout xmppTimeout <~> waitNetPushMessage SendPack
		case m of
			(Just (Pushing _ (ReceivePackOutput b))) -> 
				liftIO $ writeChunk outh b
			(Just (Pushing _ (ReceivePackDone exitcode))) ->
				liftIO $ do
					hPrint controlh exitcode
					hFlush controlh
			(Just _) -> noop
			Nothing -> do
				debug ["timeout waiting for git receive-pack output via XMPP"]
				-- Send a synthetic exit code to git-annex
				-- xmppgit, which will exit and cause git push
				-- to die.
				liftIO $ do
					hPrint controlh (ExitFailure 1)
					hFlush controlh
	installwrapper tmpdir = liftIO $ do
		createDirectoryIfMissing True tmpdir
		let wrapper = tmpdir </> "git-remote-xmpp"
		program <- readProgramFile
		writeFile wrapper $ unlines
			[ "#!/bin/sh"
			, "exec " ++ program ++ " xmppgit"
			]
		modifyFileMode wrapper $ addModes executeModes

type EnvVar = String

envVar :: String -> EnvVar
envVar s = "GIT_ANNEX_XMPPGIT_" ++ s

relayIn :: EnvVar
relayIn = envVar "IN"

relayOut :: EnvVar
relayOut = envVar "OUT"

relayControl :: EnvVar
relayControl = envVar "CONTROL"

relayHandle :: EnvVar -> IO Handle
relayHandle var = do
	v <- getEnv var
	case readish =<< v of
		Nothing -> error $ var ++ " not set"
		Just n -> fdToHandle $ Fd n

{- Called by git-annex xmppgit.
 -
 - git-push is talking to us on stdin
 - we're talking to git-push on stdout
 - git-receive-pack is talking to us on relayIn (via XMPP)
 - we're talking to git-receive-pack on relayOut (via XMPP)
 - git-receive-pack's exit code will be passed to us on relayControl
 -}
xmppGitRelay :: IO ()
xmppGitRelay = do
	flip relay stdout =<< relayHandle relayIn
	relay stdin =<< relayHandle relayOut
	code <- hGetLine =<< relayHandle relayControl
	exitWith $ fromMaybe (ExitFailure 1) $ readish code
  where
	{- Is it possible to set up pipes and not need to copy the data
	 - ourselves? See splice(2) -}
	relay fromh toh = void $ forkIO $ forever $ do
		b <- B.hGetSome fromh chunkSize
		when (B.null b) $ do
			hClose fromh
			hClose toh
			killThread =<< myThreadId
		writeChunk toh b

{- Relays git receive-pack stdin and stdout via XMPP, as well as propigating
 - its exit status to XMPP. -}
xmppReceivePack :: ClientID -> Assistant Bool
xmppReceivePack cid = runPush ReceivePack cid handleDeferred $ do
	repodir <- liftAnnex $ fromRepo repoPath
	let p = (proc "git" ["receive-pack", repodir])
		{ std_in = CreatePipe
		, std_out = CreatePipe
		, std_err = Inherit
		}
	(Just inh, Just outh, _, pid) <- liftIO $ createProcess p
	readertid <- forkIO <~> relayfromxmpp inh
	relaytoxmpp outh
	code <- liftIO $ waitForProcess pid
	void $ sendNetMessage $ Pushing cid $ ReceivePackDone code
	liftIO $ do
		killThread readertid
		hClose inh
		hClose outh
		return $ code == ExitSuccess
  where
	relaytoxmpp outh = do
		b <- liftIO $ B.hGetSome outh chunkSize
		-- empty is EOF, so exit
		unless (B.null b) $ do
			sendNetMessage $ Pushing cid $ ReceivePackOutput b
			relaytoxmpp outh
	relayfromxmpp inh = forever $ do
		m <- timeout xmppTimeout <~> waitNetPushMessage ReceivePack
		case m of
			(Just (Pushing _ (SendPackOutput b))) ->
				liftIO $ writeChunk inh b
			(Just _) -> noop
			Nothing -> do
				debug ["timeout waiting for git send-pack output via XMPP"]
				-- closing the handle will make
				-- git receive-pack exit
				liftIO $ do
					hClose inh
					killThread =<< myThreadId

xmppRemotes :: ClientID -> Assistant [Remote]
xmppRemotes cid = case baseJID <$> parseJID cid of
	Nothing -> return []
	Just jid -> do
		let loc = gitXMPPLocation jid
		filter (matching loc . Remote.repo) . syncGitRemotes 
			<$> getDaemonStatus
  where
	matching loc r = repoIsUrl r && repoLocation r == loc

whenXMPPRemote :: ClientID -> Assistant () -> Assistant ()
whenXMPPRemote cid = unlessM (null <$> xmppRemotes cid)

handlePushInitiation :: NetMessage -> Assistant ()
handlePushInitiation (Pushing cid CanPush) =
	whenXMPPRemote cid $ 
		sendNetMessage $ Pushing cid PushRequest

handlePushInitiation (Pushing cid PushRequest) =
	go =<< liftAnnex (inRepo Git.Branch.current)
  where
	go Nothing = noop
	go (Just branch) = do
		rs <- xmppRemotes cid
		liftAnnex $ Annex.Branch.commit "update"
		(g, u) <- liftAnnex $ (,)
			<$> gitRepo
			<*> getUUID
		liftIO $ Command.Sync.updateBranch (Command.Sync.syncBranch branch) g
		debug ["pushing to", show rs]
		forM_ rs $ \r -> xmppPush cid $ pushFallback u branch r

handlePushInitiation (Pushing cid StartingPush) =
	whenXMPPRemote cid $
		void $ xmppReceivePack cid
handlePushInitiation _ = noop

handleDeferred :: NetMessage -> Assistant ()
handleDeferred = handlePushInitiation

writeChunk :: Handle -> B.ByteString -> IO ()
writeChunk h b = do
	B.hPut h b
	hFlush h

{- Largest chunk of data to send in a single XMPP message. -}
chunkSize :: Int
chunkSize = 4096

{- How long to wait for an expected message before assuming the other side
 - has gone away and canceling a push. 
 -
 - This needs to be long enough to allow a message of up to 2+ times
 - chunkSize to propigate up to a XMPP server, perhaps across to another
 - server, and back down to us. On the other hand, other XMPP pushes can be
 - delayed for running until the timeout is reached, so it should not be
 - excessive.
 -}
xmppTimeout :: Int
xmppTimeout = 120000000 -- 120 seconds
