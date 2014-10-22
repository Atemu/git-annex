{- git-annex transitions log
 -
 - This is used to record transitions that have been performed on the
 - git-annex branch, and when the transition was first started.
 -
 - We can quickly detect when the local branch has already had an transition
 - done that is listed in the remote branch by checking that the local
 - branch contains the same transition, with the same or newer start time.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Transitions where

import Data.Time.Clock.POSIX
import Data.Time
import System.Locale
import qualified Data.Set as S

import Common.Annex

transitionsLog :: FilePath
transitionsLog = "transitions.log"

data Transition
	= ForgetGitHistory
	| ForgetDeadRemotes
	deriving (Show, Ord, Eq, Read)

data TransitionLine = TransitionLine
	{ transitionStarted :: POSIXTime
	, transition :: Transition
	} deriving (Show, Ord, Eq)

type Transitions = S.Set TransitionLine

describeTransition :: Transition -> String
describeTransition ForgetGitHistory = "forget git history"
describeTransition ForgetDeadRemotes = "forget dead remotes"

noTransitions :: Transitions
noTransitions = S.empty

addTransition :: POSIXTime -> Transition -> Transitions -> Transitions
addTransition ts t = S.insert $ TransitionLine ts t

showTransitions :: Transitions -> String
showTransitions = unlines . map showTransitionLine . S.elems

{- If the log contains new transitions we don't support, returns Nothing. -}
parseTransitions :: String -> Maybe Transitions
parseTransitions = check . map parseTransitionLine . lines
  where
	check l
		| all isJust l = Just $ S.fromList $ catMaybes l
		| otherwise = Nothing

parseTransitionsStrictly :: String -> String -> Transitions
parseTransitionsStrictly source = fromMaybe badsource . parseTransitions
  where
	badsource = error $ "unknown transitions listed in " ++ source ++ "; upgrade git-annex!"

showTransitionLine :: TransitionLine -> String
showTransitionLine (TransitionLine ts t) = unwords [show t, show ts]

parseTransitionLine :: String -> Maybe TransitionLine
parseTransitionLine s = TransitionLine <$> pdate ds <*> readish ts
  where
	ws = words s
	ts = Prelude.head ws
	ds = unwords $ Prelude.tail ws
	pdate = utcTimeToPOSIXSeconds <$$> parseTime defaultTimeLocale "%s%Qs"

combineTransitions :: [Transitions] -> Transitions
combineTransitions = S.unions

transitionList :: Transitions -> [Transition]
transitionList = map transition . S.elems

{- Typically ran with Annex.Branch.change, but we can't import Annex.Branch
 - here since it depends on this module. -}
recordTransitions :: (FilePath -> (String -> String) -> Annex ()) -> Transitions -> Annex ()
recordTransitions changer t = changer transitionsLog $
	showTransitions . S.union t . parseTransitionsStrictly "local"
