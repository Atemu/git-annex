{- git-annex assistant mount watcher, using either dbus or mtab polling
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Assistant.Threads.MountWatcher where

import Assistant.Common
import Assistant.DaemonStatus
import Assistant.Sync
import qualified Annex
import qualified Git
import Utility.ThreadScheduler
import Utility.Mounts
import Remote.List
import qualified Types.Remote as Remote

import qualified Data.Set as S

#if WITH_DBUS
import Utility.DBus
import DBus.Client
import DBus
import Data.Word (Word32)
import Control.Concurrent
import qualified Control.Exception as E
#else
#warning Building without dbus support; will use mtab polling
#endif

thisThread :: ThreadName
thisThread = "MountWatcher"

mountWatcherThread :: NamedThread
mountWatcherThread = NamedThread "MountWatcher" $
#if WITH_DBUS
	dbusThread
#else
	pollingThread
#endif

#if WITH_DBUS

dbusThread :: Assistant ()
dbusThread = do
	runclient <- asIO1 go
	r <- liftIO $ E.try $ runClient getSessionAddress runclient
	either onerr (const noop) r
  where
	go client = ifM (checkMountMonitor client)
		( do
			{- Store the current mount points in an MVar, to be
			 - compared later. We could in theory work out the
			 - mount point from the dbus message, but this is
			 - easier. -}
			mvar <- liftIO $ newMVar =<< currentMountPoints
			handleevent <- asIO1 $ \_event -> do
				nowmounted <- liftIO $ currentMountPoints
				wasmounted <- liftIO $ swapMVar mvar nowmounted
				handleMounts wasmounted nowmounted
			liftIO $ forM_ mountChanged $ \matcher ->
				listen client matcher handleevent
		, do
			liftAnnex $
				warning "No known volume monitor available through dbus; falling back to mtab polling"
			pollingThread
		)
	onerr :: E.SomeException -> Assistant ()
	onerr e = do
		{- If the session dbus fails, the user probably
		 - logged out of their desktop. Even if they log
		 - back in, we won't have access to the dbus
		 - session key, so polling is the best that can be
		 - done in this situation. -}
		liftAnnex $
			warning $ "dbus failed; falling back to mtab polling (" ++ show e ++ ")"
		pollingThread

{- Examine the list of services connected to dbus, to see if there
 - are any we can use to monitor mounts. If not, will attempt to start one. -}
checkMountMonitor :: Client -> Assistant Bool
checkMountMonitor client = do
	running <- filter (`elem` usableservices)
		<$> liftIO (listServiceNames client)
	case running of
		[] -> startOneService client startableservices
		(service:_) -> do
			debug [ "Using running DBUS service"
				, service
				, "to monitor mount events."
				]
			return True
  where
	startableservices = [gvfs, gvfsgdu]
	usableservices = startableservices ++ [kde]
	gvfs = "org.gtk.Private.UDisks2VolumeMonitor"
	gvfsgdu = "org.gtk.Private.GduVolumeMonitor"
	kde = "org.kde.DeviceNotifications"

startOneService :: Client -> [ServiceName] -> Assistant Bool
startOneService _ [] = return False
startOneService client (x:xs) = do
	_ <- liftIO $ tryNonAsync $ callDBus client "StartServiceByName"
		[toVariant x, toVariant (0 :: Word32)]
	ifM (liftIO $ elem x <$> listServiceNames client)
		( do
			debug
				[ "Started DBUS service", x
				, "to monitor mount events."
				]
			return True
		, startOneService client xs
		)

{- Filter matching events recieved when drives are mounted and unmounted. -}	
mountChanged :: [MatchRule]
mountChanged = [gvfs True, gvfs False, kde, kdefallback]
  where
	{- gvfs reliably generates this event whenever a
	 - drive is mounted/unmounted, whether automatically, or manually -}
	gvfs mount = matchAny
		{ matchInterface = Just "org.gtk.Private.RemoteVolumeMonitor"
		, matchMember = Just $ if mount then "MountAdded" else "MountRemoved"
		}
	{- This event fires when KDE prompts the user what to do with a drive,
	 - but maybe not at other times. And it's not received -}
	kde = matchAny
		{ matchInterface = Just "org.kde.Solid.Device"
		, matchMember = Just "setupDone"
		}
	{- This event may not be closely related to mounting a drive, but it's
	 - observed reliably when a drive gets mounted or unmounted. -}
	kdefallback = matchAny
		{ matchInterface = Just "org.kde.KDirNotify"
		, matchMember = Just "enteredDirectory"
		}

#endif

pollingThread :: Assistant ()
pollingThread = go =<< liftIO currentMountPoints
  where
	go wasmounted = do
		liftIO $ threadDelaySeconds (Seconds 10)
		nowmounted <- liftIO currentMountPoints
		handleMounts wasmounted nowmounted
		go nowmounted

handleMounts :: MountPoints -> MountPoints -> Assistant ()
handleMounts wasmounted nowmounted =
	mapM_ (handleMount . mnt_dir) $
		S.toList $ newMountPoints wasmounted nowmounted

handleMount :: FilePath -> Assistant ()
handleMount dir = do
	debug ["detected mount of", dir]
	rs <- filter (Git.repoIsLocal . Remote.repo) <$> remotesUnder dir
	reconnectRemotes True rs

{- Finds remotes located underneath the mount point.
 -
 - Updates state to include the remotes.
 -
 - The config of git remotes is re-read, as it may not have been available
 - at startup time, or may have changed (it could even be a different
 - repository at the same remote location..)
 -}
remotesUnder :: FilePath -> Assistant [Remote]
remotesUnder dir = do
	repotop <- liftAnnex $ fromRepo Git.repoPath
	rs <- liftAnnex remoteList
	pairs <- liftAnnex $ mapM (checkremote repotop) rs
	let (waschanged, rs') = unzip pairs
	when (any id waschanged) $ do
		liftAnnex $ Annex.changeState $ \s -> s { Annex.remotes = rs' }
		updateSyncRemotes
	return $ map snd $ filter fst pairs
  where
	checkremote repotop r = case Remote.localpath r of
		Just p | dirContains dir (absPathFrom repotop p) ->
			(,) <$> pure True <*> updateRemote r
		_ -> return (False, r)

type MountPoints = S.Set Mntent

currentMountPoints :: IO MountPoints
currentMountPoints = S.fromList <$> getMounts

newMountPoints :: MountPoints -> MountPoints -> MountPoints
newMountPoints old new = S.difference new old
