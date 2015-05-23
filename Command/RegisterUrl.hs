{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Command.RegisterUrl where

import Common.Annex
import Command
import Logs.Web
import Annex.UUID
import Command.FromKey (mkKey)

cmd :: [Command]
cmd = [notDirect $ notBareRepo $
	command "registerurl" (paramPair paramKey paramUrl) seek
		SectionPlumbing "registers an url for a key"]

seek :: CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start (keyname:url:[]) = do
	let key = mkKey keyname
	showStart "registerurl" url
	next $ perform key url
start [] = do
	showStart "registerurl" "stdin"
	next massAdd
start _ = error "specify a key and an url"

massAdd :: CommandPerform
massAdd = go True =<< map (separate (== ' ')) . lines <$> liftIO getContents
  where
	go status [] = next $ return status
	go status ((keyname,u):rest) | not (null keyname) && not (null u) = do
		let key = mkKey keyname
		ok <- perform' key u
		let !status' = status && ok
		go status' rest
	go _ _ = error "Expected pairs of key and url on stdin, but got something else."

perform :: Key -> URLString -> CommandPerform
perform key url = do
	ok <- perform' key url
	next $ return ok

perform' :: Key -> URLString -> Annex Bool
perform' key url = do
	setUrlPresent webUUID key url
	return True
