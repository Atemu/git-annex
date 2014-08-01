{- git-annex command data types
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Command where

import Data.Ord

import Types

{- A command runs in these stages.
 -
 - a. The check stage runs checks, that error out if
 -    anything prevents the command from running. -}
data CommandCheck = CommandCheck { idCheck :: Int, runCheck :: Annex () }
{- b. The seek stage takes the parameters passed to the command,
 -    looks through the repo to find the ones that are relevant
 -    to that command (ie, new files to add), and runs commandAction
 -    to handle all necessary actions. -}
type CommandSeek = [String] -> Annex ()
{- c. The start stage is run before anything is printed about the
 -    command, is passed some input, and can early abort it
 -    if the input does not make sense. It should run quickly and
 -    should not modify Annex state. -}
type CommandStart = Annex (Maybe CommandPerform)
{- d. The perform stage is run after a message is printed about the command
 -    being run, and it should be where the bulk of the work happens. -}
type CommandPerform = Annex (Maybe CommandCleanup)
{- e. The cleanup stage is run only if the perform stage succeeds, and it
 -    returns the overall success/fail of the command. -}
type CommandCleanup = Annex Bool

{- A command is defined by specifying these things. -}
data Command = Command
	{ cmdoptions :: [Option]     -- command-specific options
	, cmdnorepo :: Maybe (CmdParams -> IO ()) -- an action to run when not in a repo
	, cmdcheck :: [CommandCheck] -- check stage
	, cmdnocommit :: Bool        -- don't commit journalled state changes
	, cmdnomessages :: Bool      -- don't output normal messages
	, cmdname :: String
	, cmdparamdesc :: String     -- description of params for usage
	, cmdseek :: CommandSeek
	, cmdsection :: CommandSection
	, cmddesc :: String          -- description of command for usage
	}

type CmdParams = [String]

{- CommandCheck functions can be compared using their unique id. -}
instance Eq CommandCheck where
	a == b = idCheck a == idCheck b

instance Eq Command where
	a == b = cmdname a == cmdname b

{- Order commands by name. -}
instance Ord Command where
	compare = comparing cmdname

{- The same sections are listed in doc/git-annex.mdwn -}
data CommandSection 
	= SectionCommon
	| SectionSetup
	| SectionMaintenance
	| SectionQuery
	| SectionMetaData
	| SectionUtility
	| SectionPlumbing
	| SectionTesting
	deriving (Eq, Ord, Enum, Bounded)

descSection :: CommandSection -> String
descSection SectionCommon = "Commonly used commands"
descSection SectionSetup = "Repository setup commands"
descSection SectionMaintenance = "Repository maintenance commands"
descSection SectionQuery = "Query commands"
descSection SectionMetaData = "Metadata commands"
descSection SectionUtility = "Utility commands"
descSection SectionPlumbing = "Plumbing commands"
descSection SectionTesting = "Testing commands"
