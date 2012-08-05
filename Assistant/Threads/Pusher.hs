{- git-annex assistant git pushing thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Pusher where

import Assistant.Common
import Assistant.Commits
import Assistant.Pushes
import Assistant.Alert
import Assistant.ThreadedMonad
import Assistant.Threads.Merger
import Assistant.DaemonStatus
import qualified Command.Sync
import Utility.ThreadScheduler
import Utility.Parallel
import qualified Git.Branch

import Data.Time.Clock
import qualified Data.Map as M

thisThread :: ThreadName
thisThread = "Pusher"

{- This thread retries pushes that failed before. -}
pushRetryThread :: ThreadState -> DaemonStatusHandle -> FailedPushMap -> IO ()
pushRetryThread st dstatus pushmap = runEvery (Seconds halfhour) $ do
	-- We already waited half an hour, now wait until there are failed
	-- pushes to retry.
	topush <- getFailedPushesBefore pushmap (fromIntegral halfhour)
	unless (null topush) $ do
		debug thisThread
			[ "retrying"
			, show (length topush)
			, "failed pushes"
			]
		now <- getCurrentTime
		void $ alertWhile dstatus (pushRetryAlert topush) $
			pushToRemotes thisThread now st (Just pushmap) topush
	where
		halfhour = 1800

{- This thread pushes git commits out to remotes soon after they are made. -}
pushThread :: ThreadState -> DaemonStatusHandle -> CommitChan -> FailedPushMap -> IO ()
pushThread st dstatus commitchan pushmap = do
	runEvery (Seconds 2) $ do
		-- We already waited two seconds as a simple rate limiter.
		-- Next, wait until at least one commit has been made
		commits <- getCommits commitchan
		-- Now see if now's a good time to push.
		now <- getCurrentTime
		if shouldPush now commits
			then do
				remotes <- knownRemotes <$> getDaemonStatus dstatus
				unless (null remotes) $ 
					void $ alertWhile dstatus (pushAlert remotes) $
						pushToRemotes thisThread now st (Just pushmap) remotes
			else do
				debug thisThread
					[ "delaying push of"
					, show (length commits)
					, "commits"
					]
				refillCommits commitchan commits

{- Decide if now is a good time to push to remotes.
 -
 - Current strategy: Immediately push all commits. The commit machinery
 - already determines batches of changes, so we can't easily determine
 - batches better.
 -}
shouldPush :: UTCTime -> [Commit] -> Bool
shouldPush _now commits
	| not (null commits) = True
	| otherwise = False

{- Updates the local sync branch, then pushes it to all remotes, in
 - parallel.
 -
 - Avoids running possibly long-duration commands in the Annex monad, so
 - as not to block other threads. -}
pushToRemotes :: ThreadName -> UTCTime -> ThreadState -> (Maybe FailedPushMap) -> [Remote] -> IO Bool
pushToRemotes threadname now st mpushmap remotes = do
	(g, branch) <- runThreadState st $
		(,) <$> fromRepo id <*> inRepo Git.Branch.current
	go True branch g remotes
	where
		go _ Nothing _ _ = return True -- no branch, so nothing to do
		go shouldretry (Just branch) g rs = do
			debug threadname
				[ "pushing to"
				, show rs
				]
			Command.Sync.updateBranch (Command.Sync.syncBranch branch) g
			(succeeded, failed) <- inParallel (push g branch) rs
			let ok = null failed
			case mpushmap of
				Nothing -> noop
				Just pushmap -> 
					changeFailedPushMap pushmap $ \m ->
						M.union (makemap failed) $
							M.difference m (makemap succeeded)
			unless (ok) $
				debug threadname
					[ "failed to push to"
					, show failed
					]
			if (ok || not shouldretry)
				then return ok
				else retry branch g failed

		makemap l = M.fromList $ zip l (repeat now)

		push g branch remote = Command.Sync.pushBranch remote branch g

		retry branch g rs = do
			debug threadname [ "trying manual pull to resolve failed pushes" ]
			runThreadState st $ manualPull (Just branch) rs
			go False (Just branch) g rs
