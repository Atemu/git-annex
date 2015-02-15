{- git-annex presence log
 -
 - This is used to store presence information in the git-annex branch in
 - a way that can be union merged.
 -
 - A line of the log will look like: "date N INFO"
 - Where N=1 when the INFO is present, and 0 otherwise.
 - 
 - Copyright 2010-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Presence (
	module X,
	addLog,
	readLog,
	logNow,
	currentLog,
	historicalLog
) where

import Data.Time.Clock.POSIX

import Logs.Presence.Pure as X
import Common.Annex
import qualified Annex.Branch
import Git.Types (RefDate)

addLog :: FilePath -> LogLine -> Annex ()
addLog file line = Annex.Branch.change file $ \s -> 
	showLog $ compactLog (line : parseLog s)

{- Reads a log file.
 - Note that the LogLines returned may be in any order. -}
readLog :: FilePath -> Annex [LogLine]
readLog = parseLog <$$> Annex.Branch.get

{- Generates a new LogLine with the current date. -}
logNow :: LogStatus -> String -> Annex LogLine
logNow s i = do
	now <- liftIO getPOSIXTime
	return $ LogLine now s i

{- Reads a log and returns only the info that is still in effect. -}
currentLog :: FilePath -> Annex [String]
currentLog file = map info . filterPresent <$> readLog file

{- Reads a historical version of a log and returns the info that was in
 - effect at that time. 
 -
 - The date is formatted as shown in gitrevisions man page.
 -}
historicalLog :: RefDate -> FilePath -> Annex [String]
historicalLog refdate file = map info . filterPresent . parseLog
	<$> Annex.Branch.getHistorical refdate file
