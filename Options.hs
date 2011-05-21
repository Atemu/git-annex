{- git-annex dashed options
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Options where

import System.Console.GetOpt
import System.Log.Logger
import Control.Monad.State (liftIO)

import qualified Annex
import Types
import Command

{- Each dashed command-line option results in generation of an action
 - in the Annex monad that performs the necessary setting.
 -}
type Option = OptDescr (Annex ())

commonOptions :: [Option]
commonOptions =
	[ Option [] ["force"] (NoArg (setforce True))
		"allow actions that may lose annexed data"
	, Option ['F'] ["fast"] (NoArg (setfast True))
		"avoid slow operations"
	, Option ['q'] ["quiet"] (NoArg (setquiet True))
		"avoid verbose output"
	, Option ['v'] ["verbose"] (NoArg (setquiet False))
		"allow verbose output (default)"
	, Option ['d'] ["debug"] (NoArg (setdebug))
		"show debug messages"
	, Option ['b'] ["backend"] (ReqArg setforcebackend paramName)
		"specify key-value backend to use"
	]
	where
		setforce v = Annex.changeState $ \s -> s { Annex.force = v }
		setfast v = Annex.changeState $ \s -> s { Annex.fast = v }
		setquiet v = Annex.changeState $ \s -> s { Annex.quiet = v }
		setforcebackend v = Annex.changeState $ \s -> s { Annex.forcebackend = Just v }
		setdebug = liftIO $ updateGlobalLogger "" $ setLevel DEBUG
