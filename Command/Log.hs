{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Log where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Time.Clock.POSIX
import Data.Time
import System.Locale
import Data.Char

import Common.Annex
import Command
import Logs
import qualified Logs.Presence
import Annex.CatFile
import qualified Annex.Branch
import qualified Git
import Git.Command
import qualified Remote
import qualified Annex

data RefChange = RefChange 
	{ changetime :: POSIXTime
	, oldref :: Git.Ref
	, newref :: Git.Ref
	}

type Outputter = Bool -> POSIXTime -> [UUID] -> Annex ()

cmd :: [Command]
cmd = [withOptions options $
	command "log" paramPaths seek SectionQuery "shows location log"]

options :: [Option]
options = passthruOptions ++ [gourceOption]

passthruOptions :: [Option]
passthruOptions = map odate ["since", "after", "until", "before"] ++
	[ fieldOption ['n'] "max-count" paramNumber
		"limit number of logs displayed"
	]
  where
	odate n = fieldOption [] n paramDate $ "show log " ++ n ++ " date"

gourceOption :: Option
gourceOption = flagOption [] "gource" "format output for gource"

seek :: CommandSeek
seek ps = do
	m <- Remote.uuidDescriptions
	zone <- liftIO getCurrentTimeZone
	os <- concat <$> mapM getoption passthruOptions
	gource <- getOptionFlag gourceOption
	withFilesInGit (whenAnnexed $ start m zone os gource) ps
  where
	getoption o = maybe [] (use o) <$>
		Annex.getField (optionName o)
	use o v = [Param ("--" ++ optionName o), Param v]

start
	:: M.Map UUID String
	-> TimeZone
	-> [CommandParam]
	-> Bool
	-> FilePath
	-> Key
	-> CommandStart
start m zone os gource file key = do
	showLog output =<< readLog <$> getLog key os
	-- getLog produces a zombie; reap it
	liftIO reapZombies
	stop
  where
	output
		| gource = gourceOutput lookupdescription file
		| otherwise = normalOutput lookupdescription file zone
	lookupdescription u = fromMaybe (fromUUID u) $ M.lookup u m

showLog :: Outputter -> [RefChange] -> Annex ()
showLog outputter ps = do
	sets <- mapM (getset newref) ps
	previous <- maybe (return genesis) (getset oldref) (lastMaybe ps)
	sequence_ $ compareChanges outputter $ sets ++ [previous]
  where
	genesis = (0, S.empty)
	getset select change = do
		s <- S.fromList <$> get (select change)
		return (changetime change, s)
	get ref = map toUUID . Logs.Presence.getLog . L.unpack <$>
		catObject ref

normalOutput :: (UUID -> String) -> FilePath -> TimeZone -> Outputter
normalOutput lookupdescription file zone present ts us =
	liftIO $ mapM_ (putStrLn . format) us
  where
	time = showTimeStamp zone ts
	addel = if present then "+" else "-"
	format u = unwords [ addel, time, file, "|", 
		fromUUID u ++ " -- " ++ lookupdescription u ]

gourceOutput :: (UUID -> String) -> FilePath -> Outputter
gourceOutput lookupdescription file present ts us =
	liftIO $ mapM_ (putStrLn . intercalate "|" . format) us
  where
	time = takeWhile isDigit $ show ts
	addel = if present then "A" else "M"
	format u = [ time, lookupdescription u, addel, file ]

{- Generates a display of the changes (which are ordered with newest first),
 - by comparing each change with the previous change.
 - Uses a formatter to generate a display of items that are added and
 - removed. -}
compareChanges :: Ord a => (Bool -> POSIXTime -> [a] -> b) -> [(POSIXTime, S.Set a)] -> [b]
compareChanges format changes = concatMap diff $ zip changes (drop 1 changes)
  where
	diff ((ts, new), (_, old)) =
		[format True ts added, format False ts removed]
	  where
		added = S.toList $ S.difference new old
		removed = S.toList $ S.difference old new

{- Gets the git log for a given location log file.
 -
 - This is complicated by git log using paths relative to the current
 - directory, even when looking at files in a different branch. A wacky
 - relative path to the log file has to be used.
 -
 - The --remove-empty is a significant optimisation. It relies on location
 - log files never being deleted in normal operation. Letting git stop
 - once the location log file is gone avoids it checking all the way back
 - to commit 0 to see if it used to exist, so generally speeds things up a
 - *lot* for newish files. -}
getLog :: Key -> [CommandParam] -> Annex [String]
getLog key os = do
	top <- fromRepo Git.repoPath
	p <- liftIO $ relPathCwdToFile top
	let logfile = p </> locationLogFile key
	inRepo $ pipeNullSplitZombie $
		[ Params "log -z --pretty=format:%ct --raw --abbrev=40"
		, Param "--remove-empty"
		] ++ os ++
		[ Param $ Git.fromRef Annex.Branch.fullname
		, Param "--"
		, Param logfile
		]

readLog :: [String] -> [RefChange]
readLog = mapMaybe (parse . lines)
  where
	parse (ts:raw:[]) = let (old, new) = parseRaw raw in
		Just RefChange
			{ changetime = parseTimeStamp ts
			, oldref = old
			, newref = new
			}
	parse _ = Nothing

-- Parses something like ":100644 100644 oldsha newsha M"
parseRaw :: String -> (Git.Ref, Git.Ref)
parseRaw l = go $ words l
  where
	go (_:_:oldsha:newsha:_) = (Git.Ref oldsha, Git.Ref newsha)
	go _ = error $ "unable to parse git log output: " ++ l

parseTimeStamp :: String -> POSIXTime
parseTimeStamp = utcTimeToPOSIXSeconds . fromMaybe (error "bad timestamp") .
	parseTime defaultTimeLocale "%s"

showTimeStamp :: TimeZone -> POSIXTime -> String
showTimeStamp zone = show . utcToLocalTime zone . posixSecondsToUTCTime
