{- git-annex assistant daemon status
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}

module Assistant.Types.DaemonStatus where

import Common.Annex
import Assistant.Alert
import Assistant.Pairing
import Utility.NotificationBroadcaster
import Logs.Transfer

import Control.Concurrent.STM
import Data.Time.Clock.POSIX
import qualified Data.Map as M

data DaemonStatus = DaemonStatus
	-- False when the daemon is performing its startup scan
	{ scanComplete :: Bool
	-- Time when a previous process of the daemon was running ok
	, lastRunning :: Maybe POSIXTime
	-- True when the sanity checker is running
	, sanityCheckRunning :: Bool
	-- Last time the sanity checker ran
	, lastSanityCheck :: Maybe POSIXTime
	-- Currently running file content transfers
	, currentTransfers :: TransferMap
	-- Messages to display to the user.
	, alertMap :: AlertMap
	, lastAlertId :: AlertId
	-- Ordered list of all remotes that can be synced with
	, syncRemotes :: [Remote]
	-- Ordered list of remotes to sync git with
	, syncGitRemotes :: [Remote]
	-- Ordered list of remotes to sync data with
	, syncDataRemotes :: [Remote]
	-- Pairing request that is in progress.
	, pairingInProgress :: Maybe PairingInProgress
	-- Broadcasts notifications about all changes to the DaemonStatus
	, changeNotifier :: NotificationBroadcaster
	-- Broadcasts notifications when queued or current transfers change.
	, transferNotifier :: NotificationBroadcaster
	-- Broadcasts notifications when there's a change to the alerts
	, alertNotifier :: NotificationBroadcaster
	-- Broadcasts notifications when the syncRemotes change
	, syncRemotesNotifier :: NotificationBroadcaster
	}

type TransferMap = M.Map Transfer TransferInfo

{- This TMVar is never left empty, so accessing it will never block. -}
type DaemonStatusHandle = TMVar DaemonStatus

newDaemonStatus :: IO DaemonStatus
newDaemonStatus = DaemonStatus
	<$> pure False
	<*> pure Nothing
	<*> pure False
	<*> pure Nothing
	<*> pure M.empty
	<*> pure M.empty
	<*> pure firstAlertId
	<*> pure []
	<*> pure []
	<*> pure []
	<*> pure Nothing
	<*> newNotificationBroadcaster
	<*> newNotificationBroadcaster
	<*> newNotificationBroadcaster
	<*> newNotificationBroadcaster
