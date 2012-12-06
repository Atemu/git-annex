{- user-specified limits on files to act on
 -
 - Copyright 2011,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Limit where

import Text.Regex.PCRE.Light.Char8
import System.Path.WildMatch
import Data.Time.Clock.POSIX
import qualified Data.Set as S
import qualified Data.Map as M

import Common.Annex
import qualified Annex
import qualified Utility.Matcher
import qualified Remote
import qualified Backend
import Annex.Content
import Annex.UUID
import Logs.Trust
import Types.TrustLevel
import Types.Key
import Types.Group
import Logs.Group
import Utility.HumanTime
import Utility.DataUnits

type MatchFiles = AssumeNotPresent -> Annex.FileInfo -> Annex Bool
type MkLimit = String -> Either String MatchFiles
type AssumeNotPresent = S.Set UUID

{- Checks if there are user-specified limits. -}
limited :: Annex Bool
limited = (not . Utility.Matcher.isEmpty) <$> getMatcher'

{- Gets a matcher for the user-specified limits. The matcher is cached for
 - speed; once it's obtained the user-specified limits can't change. -}
getMatcher :: Annex (Annex.FileInfo -> Annex Bool)
getMatcher = Utility.Matcher.matchM <$> getMatcher'

getMatcher' :: Annex (Utility.Matcher.Matcher (Annex.FileInfo -> Annex Bool))
getMatcher' = do
	m <- Annex.getState Annex.limit
	case m of
		Right r -> return r
		Left l -> do
			let matcher = Utility.Matcher.generate (reverse l)
			Annex.changeState $ \s -> s { Annex.limit = Right matcher }
			return matcher

{- Adds something to the limit list, which is built up reversed. -}
add :: Utility.Matcher.Token (Annex.FileInfo -> Annex Bool) -> Annex ()
add l = Annex.changeState $ \s -> s { Annex.limit = prepend $ Annex.limit s }
  where
	prepend (Left ls) = Left $ l:ls
	prepend _ = error "internal"

{- Adds a new token. -}
addToken :: String -> Annex ()
addToken = add . Utility.Matcher.token

{- Adds a new limit. -}
addLimit :: Either String MatchFiles -> Annex ()
addLimit = either error (\l -> add $ Utility.Matcher.Operation $ l S.empty)

{- Add a limit to skip files that do not match the glob. -}
addInclude :: String -> Annex ()
addInclude = addLimit . limitInclude

limitInclude :: MkLimit
limitInclude glob = Right $ const $ return . matchglob glob

{- Add a limit to skip files that match the glob. -}
addExclude :: String -> Annex ()
addExclude = addLimit . limitExclude

limitExclude :: MkLimit
limitExclude glob = Right $ const $ return . not . matchglob glob

matchglob :: String -> Annex.FileInfo -> Bool
matchglob glob (Annex.FileInfo { Annex.matchFile = f }) =
	isJust $ match cregex f []
  where
	cregex = compile regex []
	regex = '^':wildToRegex glob

{- Adds a limit to skip files not believed to be present
 - in a specfied repository. -}
addIn :: String -> Annex ()
addIn = addLimit . limitIn

limitIn :: MkLimit
limitIn name = Right $ \notpresent -> check $
	if name == "."
		then inhere notpresent
		else inremote notpresent
  where
	check a = lookupFile >=> handle a
	handle _ Nothing = return False
	handle a (Just (key, _)) = a key
	inremote notpresent key = do
		u <- Remote.nameToUUID name
		us <- Remote.keyLocations key
		return $ u `elem` us && u `S.notMember` notpresent
	inhere notpresent key
		| S.null notpresent = inAnnex key
		| otherwise = do
			u <- getUUID
			if u `S.member` notpresent
				then return False
				else inAnnex key

{- Limit to content that is currently present on a uuid. -}
limitPresent :: Maybe UUID -> MkLimit
limitPresent u _ = Right $ const $ check $ \key -> do
	hereu <- getUUID
	if u == Just hereu || u == Nothing
		then inAnnex key
		else do
			us <- Remote.keyLocations key
			return $ maybe False (`elem` us) u
  where
	check a = lookupFile >=> handle a
	handle _ Nothing = return False
	handle a (Just (key, _)) = a key

{- Adds a limit to skip files not believed to have the specified number
 - of copies. -}
addCopies :: String -> Annex ()
addCopies = addLimit . limitCopies

limitCopies :: MkLimit
limitCopies want = case split ":" want of
	[v, n] -> case readTrustLevel v of
		Just trust -> go n $ checktrust trust
		Nothing -> go n $ checkgroup v
	[n] -> go n $ const $ return True
	_ -> Left "bad value for copies"
  where
	go num good = case readish num of
		Nothing -> Left "bad number for copies"
		Just n -> Right $ \notpresent f ->
			lookupFile f >>= handle n good notpresent
	handle _ _ _ Nothing = return False
	handle n good notpresent (Just (key, _)) = do
		us <- filter (`S.notMember` notpresent)
			<$> (filterM good =<< Remote.keyLocations key)
		return $ length us >= n
	checktrust t u = (== t) <$> lookupTrust u
	checkgroup g u = S.member g <$> lookupGroups u

{- Adds a limit to skip files not believed to be present in all
 - repositories in the specified group. -}
addInAllGroup :: String -> Annex ()
addInAllGroup groupname = do
	m <- groupMap
	addLimit $ limitInAllGroup m groupname

limitInAllGroup :: GroupMap -> MkLimit
limitInAllGroup m groupname
	| S.null want = Right $ const $ const $ return True
	| otherwise = Right $ \notpresent -> lookupFile >=> check notpresent
  where
	want = fromMaybe S.empty $ M.lookup groupname $ uuidsByGroup m
	check _ Nothing = return False
	check notpresent (Just (key, _))
		-- optimisation: Check if a wanted uuid is notpresent.
		| not (S.null (S.intersection want notpresent))	= return False
		| otherwise = do
			present <- S.fromList <$> Remote.keyLocations key
			return $ S.null $ want `S.difference` present

{- Adds a limit to skip files not using a specified key-value backend. -}
addInBackend :: String -> Annex ()
addInBackend = addLimit . limitInBackend

limitInBackend :: MkLimit
limitInBackend name = Right $ const $ lookupFile >=> check
  where
	wanted = Backend.lookupBackendName name
	check = return . maybe False ((==) wanted . snd)

{- Adds a limit to skip files that are too large or too small -}
addLargerThan :: String -> Annex ()
addLargerThan = addLimit . limitSize (>)

addSmallerThan :: String -> Annex ()
addSmallerThan = addLimit . limitSize (<)

limitSize :: (Maybe Integer -> Maybe Integer -> Bool) -> MkLimit
limitSize vs s = case readSize dataUnits s of
	Nothing -> Left "bad size"
	Just sz -> Right $ const $ lookupFile >=> check sz
  where
	check _ Nothing = return False
	check sz (Just (key, _)) = return $ keySize key `vs` Just sz

addTimeLimit :: String -> Annex ()
addTimeLimit s = do
	let seconds = fromMaybe (error "bad time-limit") $ parseDuration s
	start <- liftIO getPOSIXTime
	let cutoff = start + seconds
	addLimit $ Right $ const $ const $ do
		now <- liftIO getPOSIXTime
		if now > cutoff
			then do
				warning $ "Time limit (" ++ s ++ ") reached!"
				liftIO $ exitWith $ ExitFailure 101
			else return True

lookupFile :: Annex.FileInfo -> Annex (Maybe (Key, Backend))
lookupFile = Backend.lookupFile . Annex.relFile
