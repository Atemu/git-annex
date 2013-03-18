{- git-annex assistant git pushing thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Pusher where

import Assistant.Common
import Assistant.Commits
import Assistant.Types.Commits
import Assistant.Pushes
import Assistant.DaemonStatus
import Assistant.Sync
import Utility.ThreadScheduler
import qualified Types.Remote as Remote

{- This thread retries pushes that failed before. -}
pushRetryThread :: NamedThread
pushRetryThread = namedThread "PushRetrier" $ runEvery (Seconds halfhour) <~> do
	-- We already waited half an hour, now wait until there are failed
	-- pushes to retry.
	topush <- getFailedPushesBefore (fromIntegral halfhour)
	unless (null topush) $ do
		debug ["retrying", show (length topush), "failed pushes"]
		void $ pushToRemotes True topush
  where
	halfhour = 1800

{- This thread pushes git commits out to remotes soon after they are made. -}
pushThread :: NamedThread
pushThread = namedThread "Pusher" $ runEvery (Seconds 2) <~> do
	-- We already waited two seconds as a simple rate limiter.
	-- Next, wait until at least one commit has been made
	commits <- getCommits
	-- Now see if now's a good time to push.
	if shouldPush commits
		then void $ pushToRemotes True =<< pushTargets
		else do
			debug ["delaying push of", show (length commits), "commits"]
			refillCommits commits

{- We want to avoid pushing to remotes that are marked readonly.
 -
 - Also, avoid pushing to local remotes we can easily tell are not available,
 - to avoid ugly messages when a removable drive is not attached.
 -}
pushTargets :: Assistant [Remote]
pushTargets = liftIO . filterM available =<< candidates <$> getDaemonStatus
  where
	candidates = filter (not . Remote.readonly) . syncGitRemotes
	available = maybe (return True) doesDirectoryExist . Remote.localpath

{- Decide if now is a good time to push to remotes.
 -
 - Current strategy: Immediately push all commits. The commit machinery
 - already determines batches of changes, so we can't easily determine
 - batches better.
 -}
shouldPush :: [Commit] -> Bool
shouldPush commits
	| not (null commits) = True
	| otherwise = False
