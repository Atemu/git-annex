{- git-annex command queue
 -
 - Copyright 2011, 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Queue (
	addCommand,
	addUpdateIndex,
	flush,
	flushWhenFull,
	size
) where

import Common.Annex
import Annex hiding (new)
import qualified Git.Queue
import qualified Git.UpdateIndex

{- Adds a git command to the queue. -}
addCommand :: String -> [CommandParam] -> [FilePath] -> Annex ()
addCommand command params files = do
	q <- get
	store <=< inRepo $ Git.Queue.addCommand command params files q

{- Adds an update-index stream to the queue. -}
addUpdateIndex :: Git.UpdateIndex.Streamer -> Annex ()
addUpdateIndex streamer = do
	q <- get
	store <=< inRepo $ Git.Queue.addUpdateIndex streamer q

{- Runs the queue if it is full. Should be called periodically. -}
flushWhenFull :: Annex ()
flushWhenFull = do
	q <- get
	when (Git.Queue.full q) flush

{- Runs (and empties) the queue. -}
flush :: Annex ()
flush = do
	q <- get
	unless (0 == Git.Queue.size q) $ do
		showStoringStateAction
		q' <- inRepo $ Git.Queue.flush q
		store q'

{- Gets the size of the queue. -}
size :: Annex Int
size = Git.Queue.size <$> get

get :: Annex Git.Queue.Queue
get = maybe new return =<< getState repoqueue

new :: Annex Git.Queue.Queue
new = do
	q <- Git.Queue.new . annexQueueSize <$> getGitConfig
	store q
	return q

store :: Git.Queue.Queue -> Annex ()
store q = changeState $ \s -> s { repoqueue = Just q }
