{- git-annex command-line actions
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module CmdLine.Action where

import Common.Annex
import qualified Annex
import Types.Command
import qualified Annex.Queue
import Messages.Internal
import Types.Messages

import Control.Concurrent.Async
import Control.Exception (throwIO)
import qualified Data.Map as M
import Data.Either

type CommandActionRunner = CommandStart -> CommandCleanup

{- Runs a command, starting with the check stage, and then
 - the seek stage. Finishes by running the continutation, and 
 - then showing a count of any failures. -}
performCommandAction :: Command -> CmdParams -> Annex () -> Annex ()
performCommandAction Command { cmdseek = seek, cmdcheck = c, cmdname = name } params cont = do
	mapM_ runCheck c
	Annex.changeState $ \s -> s { Annex.errcounter = 0 }
	seek params
	finishCommandActions
	cont
	showerrcount =<< Annex.getState Annex.errcounter
  where
	showerrcount 0 = noop
	showerrcount cnt = error $ name ++ ": " ++ show cnt ++ " failed"

{- Runs one of the actions needed to perform a command.
 - Individual actions can fail without stopping the whole command,
 - including by throwing IO errors (but other errors terminate the whole
 - command).
 - 
 - When concurrency is enabled, a thread is forked off to run the action
 - in the background, as soon as a free slot is available.
 
 - This should only be run in the seek stage.
 -}
commandAction :: CommandStart -> Annex ()
commandAction a = withOutputType go 
  where
	go (ParallelOutput n) = do
		ws <- Annex.getState Annex.workers
		(st, ws') <- if null ws
			then do
				st <- newWorkerState
				return (st, replicate (n-1) (Left st))
			else do
				l <- liftIO $ drainTo (n-1) ws
				findFreeSlot l
		w <- liftIO $ async $ snd <$> Annex.run st run
		Annex.changeState $ \s -> s { Annex.workers = Right w:ws' }
	go _  =	run
	run = void $ includeCommandAction a

{- Waits for any forked off command actions to finish.
 -
 - Merge together the cleanup actions of all the AnnexStates used by
 - threads, into the current Annex's state, so they'll run at shutdown.
 -
 - Also merge together the errcounters of the AnnexStates.
 -}
finishCommandActions :: Annex ()
finishCommandActions = do
	l <- liftIO . drainTo 0 =<< Annex.getState Annex.workers
	forM_ (lefts l) $ \st -> do
		forM_ (M.toList $ Annex.cleanup st) $
			uncurry Annex.addCleanup
		Annex.changeState $ \s ->
			s { Annex.errcounter = Annex.errcounter s + Annex.errcounter st }

{- Wait for Asyncs from the list to finish, replacing them with their
 - final AnnexStates, until the list of remaining Asyncs is not larger
 - than the specified size, then returns the new list.
 -
 - If the action throws an exception, it is propigated, but first
 - all other actions are waited for, to allow for a clean shutdown.
 -}
drainTo
	:: Int
	-> [Either Annex.AnnexState (Async Annex.AnnexState)]
	-> IO [Either Annex.AnnexState (Async Annex.AnnexState)]
drainTo sz l
	| null as || sz >= length as = return l
	| otherwise = do
		(done, ret) <- waitAnyCatch as
		let as' = filter (/= done) as
		case ret of
			Left e -> do
				void $ drainTo 0 (map Left sts ++ map Right as')
				throwIO e
			Right st -> do
				drainTo sz $ map Left (st:sts) ++ map Right as'
  where
	(sts, as) = partitionEithers l

findFreeSlot :: [Either Annex.AnnexState (Async Annex.AnnexState)] -> Annex (Annex.AnnexState, [Either Annex.AnnexState (Async Annex.AnnexState)])
findFreeSlot = go []
  where
	go c [] = do
		st <- newWorkerState
		return (st, c)
	go c (Left st:rest) = return (st, c ++ rest)
	go c (v:rest) = go (v:c) rest

{- From the current Annex state, get a state that is suitable for being
 - used for a worker thread. Avoid sharing eg, open file handles. -}
newWorkerState :: Annex Annex.AnnexState
newWorkerState = do
	st <- Annex.getState id
	return $ st
		{ Annex.workers = []
		, Annex.catfilehandles = M.empty
		, Annex.checkattrhandle = Nothing
		, Annex.checkignorehandle = Nothing
		} 

{- Like commandAction, but without the concurrency. -}
includeCommandAction :: CommandStart -> CommandCleanup
includeCommandAction a = account =<< tryIO go
  where
	go = do
		Annex.Queue.flushWhenFull
		callCommandAction a
	account (Right True) = return True
	account (Right False) = incerr
	account (Left err) = do
		showErr err
		showEndFail
		incerr
	incerr = do
		Annex.changeState $ \s -> 
			let ! c = Annex.errcounter s + 1 
			    ! s' = s { Annex.errcounter = c }
			in s'
		return False

{- Runs a single command action through the start, perform and cleanup
 - stages, without catching errors. Useful if one command wants to run
 - part of another command. -}
callCommandAction :: CommandStart -> CommandCleanup
callCommandAction = start
  where
	start   = stage $ maybe skip perform
	perform = stage $ maybe failure cleanup
	cleanup = stage $ status
	stage = (=<<)
	skip = return True
	failure = showEndFail >> return False
	status r = showEndResult r >> return r
