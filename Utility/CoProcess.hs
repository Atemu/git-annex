{- Interface for running a shell command as a coprocess,
 - sending it queries and getting back results.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.CoProcess (
	CoProcessHandle,
	start,
	stop,
	query
) where

import Common

type CoProcessHandle = (ProcessHandle, Handle, Handle, CreateProcess)

start :: FilePath -> [String] -> Maybe [(String, String)] -> IO CoProcessHandle
start command params env = do
	(from, to, _err, pid) <- runInteractiveProcess command params Nothing env
	return (pid, to, from, proc command params)

stop :: CoProcessHandle -> IO ()
stop (pid, from, to, p) = do
	hClose to
	hClose from
	forceSuccessProcess p pid

query :: CoProcessHandle -> (Handle -> IO a) -> (Handle -> IO b) -> IO b
query (_, from, to, _) send receive = do
	_ <- send to
	hFlush to
	receive from
