{- git-annex command
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.MatchExpression where

import Command
import Annex.FileMatcher
import Types.FileMatcher
import Utility.DataUnits
import Utility.Matcher
import Annex.UUID
import Logs.Group

import qualified Data.Map as M
import qualified Data.Set as S

cmd :: Command
cmd = noCommit $
	command "matchexpression" SectionPlumbing 
		"checks if a preferred content expression matches"
		paramExpression
		(seek <$$> optParser)

data MatchExpressionOptions = MatchExpressionOptions
	{ matchexpr :: String
	, largeFilesExpression :: Bool
	, matchinfo :: MatchInfo
	}

optParser :: CmdParamsDesc -> Parser MatchExpressionOptions
optParser desc = MatchExpressionOptions
	<$> argument str (metavar desc)
	<*> switch
		( long "largefiles"
		<> help "parse as annex.largefiles expression"
		)
	<*> (MatchingInfo . addkeysize <$> dataparser)
  where
	dataparser = ProvidedInfo
		<$> optinfo "file" (strOption
			( long "file" <> metavar paramFile
			<> help "specify filename to match against"
			))
		<*> optinfo "key" (option (str >>= parseKey)
			( long "key" <> metavar paramKey
			<> help "specify key to match against"
			))
		<*> optinfo "size" (option (str >>= maybe (fail "parse error") return . readSize dataUnits)
			( long "size" <> metavar paramSize
			<> help "specify size to match against"
			))
		<*> optinfo "mimetype" (strOption
			( long "mimetype" <> metavar paramValue
			<> help "specify mime type to match against"
			))
		<*> optinfo "mimeencoding" (strOption
			( long "mimeencoding" <> metavar paramValue
			<> help "specify mime encoding to match against"
			))

	optinfo datadesc mk = (Right <$> mk)
		<|> (pure $ Left $ missingdata datadesc)
	missingdata datadesc = bail $ "cannot match this expression without " ++ datadesc ++ " data"
	-- When a key is provided, make its size also be provided.
	addkeysize p = case providedKey p of
		Right k -> case keySize k of
			Just sz -> p { providedFileSize = Right sz }
			Nothing -> p
		Left _ -> p

seek :: MatchExpressionOptions -> CommandSeek
seek o = do
	parser <- if largeFilesExpression o
		then mkLargeFilesParser
		else do
			u <- getUUID
			pure $ preferredContentParser $ preferredContentTokens $ PCD
				{ matchStandard = matchAll
				, matchGroupWanted = matchAll
				, getGroupMap = groupMap
				, configMap = M.empty
				, repoUUID = Just u
				}
	case parsedToMatcher $ parser ((matchexpr o)) of
		Left e -> liftIO $ bail $ "bad expression: " ++ e
		Right matcher -> ifM (checkmatcher matcher)
			( liftIO exitSuccess
			, liftIO exitFailure
			)
  where
	checkmatcher matcher = matchMrun matcher $ \a -> a S.empty (matchinfo o)

bail :: String -> IO a
bail s = do
	hPutStrLn stderr s
	exitWith $ ExitFailure 42
