{- git-annex command queue
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Queue (
	addCommand,
	flush,
	flushWhenFull
) where

import Common.Annex
import Annex hiding (new)
import qualified Git.Queue
import Config

{- Adds a git command to the queue. -}
addCommand :: String -> [CommandParam] -> [FilePath] -> Annex ()
addCommand command params files = do
	q <- get
	store =<< inRepo (Git.Queue.addCommand command params files q)

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

get :: Annex Git.Queue.Queue
get = maybe new return =<< getState repoqueue

new :: Annex Git.Queue.Queue
new = do
	q <- Git.Queue.new <$> queuesize
	store q
	return q
	where
		queuesize = readish <$> getConfig (annexConfig "queuesize") ""

store :: Git.Queue.Queue -> Annex ()
store q = changeState $ \s -> s { repoqueue = Just q }
