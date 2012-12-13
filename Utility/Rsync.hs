{- various rsync stuff
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Rsync where

import Common

import Data.Char

{- Generates parameters to make rsync use a specified command as its remote
 - shell. -}
rsyncShell :: [CommandParam] -> [CommandParam]
rsyncShell command = [Param "-e", Param $ unwords $ map escape (toCommand command)]
  where
	{- rsync requires some weird, non-shell like quoting in
	- here. A doubled single quote inside the single quoted
	- string is a single quote. -}
	escape s = "'" ++  join "''" (split "'" s) ++ "'"

{- Runs rsync in server mode to send a file. -}
rsyncServerSend :: FilePath -> IO Bool
rsyncServerSend file = rsync $
	rsyncServerParams ++ [Param "--sender", File file]

{- Runs rsync in server mode to receive a file. -}
rsyncServerReceive :: FilePath -> IO Bool
rsyncServerReceive file = rsync $ rsyncServerParams ++ [File file]

rsyncServerParams :: [CommandParam]
rsyncServerParams =
	[ Param "--server"
	-- preserve permissions
	, Param "-p"
	-- preserve timestamps
	, Param "-t"
	-- allow resuming of transfers of big files
	, Param "--inplace"
	-- other options rsync normally uses in server mode
	, Params "-e.Lsf ."
	]

rsync :: [CommandParam] -> IO Bool
rsync = boolSystem "rsync"

{- Runs rsync, but intercepts its progress output and feeds bytes
 - complete values into the callback. The progress output is also output
 - to stdout. 
 -
 - The params must enable rsync's --progress mode for this to work.
 -}
rsyncProgress :: (Integer -> IO ()) -> [CommandParam] -> IO Bool
rsyncProgress callback params = do
	r <- withHandle StdoutHandle createProcessSuccess p (feedprogress 0 [])
	{- For an unknown reason, piping rsync's output like this does
	 - causes it to run a second ssh process, which it neglects to wait
	 - on. Reap the resulting zombie. -}
	reapZombies
	return r
  where
	p = proc "rsync" (toCommand params)
	feedprogress prev buf h = do
		s <- hGetSomeString h 80
		if null s
			then return True
			else do
				putStr s
				hFlush stdout
				let (mbytes, buf') = parseRsyncProgress (buf++s)
				case mbytes of
					Nothing -> feedprogress prev buf' h
					(Just bytes) -> do
						when (bytes /= prev) $
							callback bytes
						feedprogress bytes buf' h

{- Checks if an rsync url involves the remote shell (ssh or rsh).
 - Use of such urls with rsync requires additional shell
 - escaping. -}
rsyncUrlIsShell :: String -> Bool
rsyncUrlIsShell s
	| "rsync://" `isPrefixOf` s = False
	| otherwise = go s
  where
	-- host::dir is rsync protocol, while host:dir is ssh/rsh
	go [] = False
	go (c:cs)
		| c == '/' = False -- got to directory with no colon
		| c == ':' = not $ ":" `isPrefixOf` cs
		| otherwise = go cs

{- Checks if a rsync url is really just a local path. -}
rsyncUrlIsPath :: String -> Bool
rsyncUrlIsPath s
	| rsyncUrlIsShell s = False
	| otherwise = ':' `notElem` s

{- Parses the String looking for rsync progress output, and returns
 - Maybe the number of bytes rsynced so far, and any any remainder of the
 - string that could be an incomplete progress output. That remainder
 - should be prepended to future output, and fed back in. This interface
 - allows the output to be read in any desired size chunk, or even one
 - character at a time.
 -
 - Strategy: Look for chunks prefixed with \r (rsync writes a \r before
 - the first progress output, and each thereafter). The first number
 - after the \r is the number of bytes processed. After the number,
 - there must appear some whitespace, or we didn't get the whole number,
 - and return the \r and part we did get, for later processing.
 -}
parseRsyncProgress :: String -> (Maybe Integer, String)
parseRsyncProgress = go [] . reverse . progresschunks
  where
	go remainder [] = (Nothing, remainder)
	go remainder (x:xs) = case parsebytes (findbytesstart x) of
		Nothing -> go (delim:x++remainder) xs
		Just b -> (Just b, remainder)

	delim = '\r'
	{- Find chunks that each start with delim.
	 - The first chunk doesn't start with it
	 - (it's empty when delim is at the start of the string). -}
	progresschunks = drop 1 . split [delim]
	findbytesstart s = dropWhile isSpace s
	parsebytes s = case break isSpace s of
		([], _) -> Nothing
		(_, []) -> Nothing
		(b, _) -> readish b
