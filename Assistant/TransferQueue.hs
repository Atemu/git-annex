{- git-annex assistant pending transfer queue
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.TransferQueue (
	TransferQueue,
	Schedule(..),
	newTransferQueue,
	getTransferQueue,
	queueTransfers,
	queueTransfer,
	queueTransferAt,
	getNextTransfer,
	dequeueTransfer,
) where

import Common.Annex
import Assistant.DaemonStatus
import Logs.Transfer
import Types.Remote
import qualified Remote

import Control.Concurrent.STM
import qualified Data.Map as M

{- The transfer queue consists of a channel listing the transfers to make;
 - the size of the queue is also tracked, and a list is maintained
 - in parallel to allow for reading. -}
data TransferQueue = TransferQueue
	{ queue :: TChan (Transfer, TransferInfo)
	, queuesize :: TVar Int
	, queuelist :: TVar [(Transfer, TransferInfo)]
	}

data Schedule = Next | Later
	deriving (Eq)

newTransferQueue :: IO TransferQueue
newTransferQueue = atomically $ TransferQueue
	<$> newTChan
	<*> newTVar 0
	<*> newTVar []

{- Reads the queue's content without blocking or changing it. -}
getTransferQueue :: TransferQueue -> IO [(Transfer, TransferInfo)]
getTransferQueue q = atomically $ readTVar $ queuelist q

stubInfo :: AssociatedFile -> Remote -> TransferInfo
stubInfo f r = TransferInfo
	{ startedTime = Nothing
	, transferPid = Nothing
	, transferTid = Nothing
	, transferRemote = Just r
	, bytesComplete = Nothing
	, associatedFile = f
	, transferPaused = False
	}

{- Adds transfers to queue for some of the known remotes. -}
queueTransfers :: Schedule -> TransferQueue -> DaemonStatusHandle -> Key -> AssociatedFile -> Direction -> Annex ()
queueTransfers schedule q dstatus k f direction = do
	rs <- knownRemotes <$> liftIO (getDaemonStatus dstatus)
	mapM_ go =<< sufficientremotes rs
	where
		sufficientremotes rs
			-- Queue downloads from all remotes that
			-- have the key, with the cheapest ones first.
			-- More expensive ones will only be tried if
			-- downloading from a cheap one fails.
			| direction == Download = do
				uuids <- Remote.keyLocations k
				return $ filter (\r -> uuid r `elem` uuids) rs
			-- TODO: Determine a smaller set of remotes that
			-- can be uploaded to, in order to ensure all
			-- remotes can access the content. Currently,
			-- send to every remote we can.
			| otherwise = return rs
		gentransfer r = Transfer
			{ transferDirection = direction
			, transferKey = k
			, transferUUID = Remote.uuid r
			}
		go r = liftIO $
			enqueue schedule q dstatus (gentransfer r) (stubInfo f r)

enqueue :: Schedule -> TransferQueue -> DaemonStatusHandle -> Transfer -> TransferInfo -> IO ()
enqueue schedule q dstatus t info
	| schedule == Next = go unGetTChan (new:)
	| otherwise = go writeTChan (\l -> l++[new])
	where
		new = (t, info)
		go modqueue modlist = do
			atomically $ do
				void $ modqueue (queue q) new
				void $ modifyTVar' (queuesize q) succ
				void $ modifyTVar' (queuelist q) modlist
			void $ notifyTransfer dstatus

{- Adds a transfer to the queue. -}
queueTransfer :: Schedule -> TransferQueue -> DaemonStatusHandle -> AssociatedFile -> Transfer -> Remote -> IO ()
queueTransfer schedule q dstatus f t remote =
	enqueue schedule q dstatus t (stubInfo f remote)

{- Blocks until the queue is no larger than a given size, and then adds a
 - transfer to the queue. -}
queueTransferAt :: Int -> Schedule -> TransferQueue -> DaemonStatusHandle -> AssociatedFile -> Transfer -> Remote -> IO ()
queueTransferAt wantsz schedule q dstatus f t remote = do
	atomically $ do
		sz <- readTVar (queuesize q)
		if sz <= wantsz
			then return ()
			else retry -- blocks until queuesize changes
	enqueue schedule q dstatus t (stubInfo f remote)

{- Blocks until a pending transfer is available from the queue,
 - and removes it.
 -
 - Checks that it's acceptable, before adding it to the
 - the currentTransfers map. If it's not acceptable, it's discarded.
 -
 - This is done in a single STM transaction, so there is no window
 - where an observer sees an inconsistent status. -}
getNextTransfer :: TransferQueue -> DaemonStatusHandle -> (TransferInfo -> Bool) -> IO (Maybe (Transfer, TransferInfo))
getNextTransfer q dstatus acceptable = atomically $ do
	void $ modifyTVar' (queuesize q) pred
	void $ modifyTVar' (queuelist q) (drop 1)
	r@(t, info) <- readTChan (queue q)
	if acceptable info
		then do
			adjustTransfersSTM dstatus $
				M.insertWith' const t info
			return $ Just r
		else return Nothing

{- Removes a transfer from the queue, if present, and returns True if it
 - was present. -}
dequeueTransfer :: TransferQueue -> DaemonStatusHandle -> Transfer -> IO Bool
dequeueTransfer q dstatus t = do
	ok <- atomically $ do
		(l, removed) <- partition (\i -> fst i /= t) <$> readTVar (queuelist q)
		void $ writeTVar (queuesize q) (length l)
		void $ writeTVar (queuelist q) l
		return $ not $ null removed
	when ok $
		notifyTransfer dstatus
	return ok
