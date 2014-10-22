{- git-annex file matching
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.FileMatcher where

import qualified Data.Map as M

import Common.Annex
import Limit
import Utility.Matcher
import Types.Group
import Logs.Group
import Logs.Remote
import Annex.UUID
import qualified Annex
import Types.FileMatcher
import Git.FilePath
import Types.Remote (RemoteConfig)

import Data.Either
import qualified Data.Set as S

checkFileMatcher :: (FileMatcher Annex) -> FilePath -> Annex Bool
checkFileMatcher matcher file = checkMatcher matcher Nothing (Just file) S.empty True

checkMatcher :: (FileMatcher Annex) -> Maybe Key -> AssociatedFile -> AssumeNotPresent -> Bool -> Annex Bool
checkMatcher matcher mkey afile notpresent def
	| isEmpty matcher = return def
	| otherwise = case (mkey, afile) of
		(_, Just file) -> go =<< fileMatchInfo file
		(Just key, _) -> go (MatchingKey key)
		_ -> return def
  where
	go mi = matchMrun matcher $ \a -> a notpresent mi

fileMatchInfo :: FilePath -> Annex MatchInfo
fileMatchInfo file = do
	matchfile <- getTopFilePath <$> inRepo (toTopFilePath file)
	return $ MatchingFile FileInfo
		{ matchFile = matchfile
		, relFile = file
		}

matchAll :: FileMatcher Annex
matchAll = generate []

parsedToMatcher :: [Either String (Token (MatchFiles Annex))] -> Either String (FileMatcher Annex)
parsedToMatcher parsed = case partitionEithers parsed of
	([], vs) -> Right $ generate vs
	(es, _) -> Left $ unwords $ map ("Parse failure: " ++) es

exprParser :: FileMatcher Annex -> FileMatcher Annex -> GroupMap -> M.Map UUID RemoteConfig -> Maybe UUID -> String -> [Either String (Token (MatchFiles Annex))]
exprParser matchstandard matchgroupwanted groupmap configmap mu expr =
	map parse $ tokenizeMatcher expr
  where
	parse = parseToken
		matchstandard
		matchgroupwanted
		(limitPresent mu)
		(limitInDir preferreddir)
		groupmap
	preferreddir = fromMaybe "public" $
		M.lookup "preferreddir" =<< (`M.lookup` configmap) =<< mu

parseToken :: FileMatcher Annex -> FileMatcher Annex -> MkLimit Annex -> MkLimit Annex -> GroupMap -> String -> Either String (Token (MatchFiles Annex))
parseToken matchstandard matchgroupwanted checkpresent checkpreferreddir groupmap t
	| t `elem` tokens = Right $ token t
	| t == "standard" = call matchstandard
	| t == "groupwanted" = call matchgroupwanted
	| t == "present" = use checkpresent
	| t == "inpreferreddir" = use checkpreferreddir
	| t == "unused" = Right $ Operation limitUnused
	| otherwise = maybe (Left $ "near " ++ show t) use $ M.lookup k $
		M.fromList
			[ ("include", limitInclude)
			, ("exclude", limitExclude)
			, ("copies", limitCopies)
			, ("lackingcopies", limitLackingCopies False)
			, ("approxlackingcopies", limitLackingCopies True)
			, ("inbackend", limitInBackend)
			, ("largerthan", limitSize (>))
			, ("smallerthan", limitSize (<))
			, ("metadata", limitMetaData)
			, ("inallgroup", limitInAllGroup groupmap)
			]
  where
	(k, v) = separate (== '=') t
	use a = Operation <$> a v
	call sub = Right $ Operation $ \notpresent mi ->
		matchMrun sub $ \a -> a notpresent mi

{- This is really dumb tokenization; there's no support for quoted values.
 - Open and close parens are always treated as standalone tokens;
 - otherwise tokens must be separated by whitespace. -}
tokenizeMatcher :: String -> [String]
tokenizeMatcher = filter (not . null ) . concatMap splitparens . words
  where
	splitparens = segmentDelim (`elem` "()")

{- Generates a matcher for files large enough (or meeting other criteria)
 - to be added to the annex, rather than directly to git. -}
largeFilesMatcher :: Annex (FileMatcher Annex)
largeFilesMatcher = go =<< annexLargeFiles <$> Annex.getGitConfig
  where
	go Nothing = return matchAll
	go (Just expr) = do
		gm <- groupMap
		rc <- readRemoteLog
		u <- getUUID
		either badexpr return $
			parsedToMatcher $ exprParser matchAll matchAll gm rc (Just u) expr
	badexpr e = error $ "bad annex.largefiles configuration: " ++ e
