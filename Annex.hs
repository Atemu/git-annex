{- git-annex monad
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses #-}

module Annex (
	Annex,
	AnnexState(..),
	OutputType(..),
	new,
	newState,
	run,
	eval,
	getState,
	changeState,
	setFlag,
	setField,
	getFlag,
	getField,
	gitRepo,
	inRepo,
	fromRepo,
) where

import Control.Monad.State
import Control.Monad.Trans.Control (StM, MonadBaseControl, liftBaseWith, restoreM)
import Control.Monad.Base (liftBase, MonadBase)

import Common
import qualified Git
import qualified Git.Config
import Git.CatFile
import qualified Git.Queue
import Types.Backend
import qualified Types.Remote
import Types.Crypto
import Types.BranchState
import Types.TrustLevel
import Types.UUID
import qualified Utility.Matcher
import qualified Data.Map as M

-- git-annex's monad
newtype Annex a = Annex { runAnnex :: StateT AnnexState IO a }
	deriving (
		Monad,
		MonadIO,
		MonadState AnnexState,
		Functor,
		Applicative
	)

instance MonadBase IO Annex where
	liftBase = Annex . liftBase

instance MonadBaseControl IO Annex where
	newtype StM Annex a = StAnnex (StM (StateT AnnexState IO) a)
	liftBaseWith f = Annex $ liftBaseWith $ \runInIO ->
		f $ liftM StAnnex . runInIO . runAnnex
	restoreM = Annex . restoreM . unStAnnex
		where
			unStAnnex (StAnnex st) = st

data OutputType = NormalOutput | QuietOutput | JSONOutput

type Matcher a = Either [Utility.Matcher.Token a] (Utility.Matcher.Matcher a)

-- internal state storage
data AnnexState = AnnexState
	{ repo :: Git.Repo
	, backends :: [BackendA Annex]
	, remotes :: [Types.Remote.RemoteA Annex]
	, repoqueue :: Git.Queue.Queue
	, output :: OutputType
	, force :: Bool
	, fast :: Bool
	, auto :: Bool
	, branchstate :: BranchState
	, catfilehandle :: Maybe CatFileHandle
	, forcebackend :: Maybe String
	, forcenumcopies :: Maybe Int
	, limit :: Matcher (FilePath -> Annex Bool)
	, forcetrust :: [(UUID, TrustLevel)]
	, trustmap :: Maybe TrustMap
	, ciphers :: M.Map EncryptedCipher Cipher
	, flags :: M.Map String Bool
	, fields :: M.Map String String
	}

newState :: Git.Repo -> AnnexState
newState gitrepo = AnnexState
	{ repo = gitrepo
	, backends = []
	, remotes = []
	, repoqueue = Git.Queue.new
	, output = NormalOutput
	, force = False
	, fast = False
	, auto = False
	, branchstate = startBranchState
	, catfilehandle = Nothing
	, forcebackend = Nothing
	, forcenumcopies = Nothing
	, limit = Left []
	, forcetrust = []
	, trustmap = Nothing
	, ciphers = M.empty
	, flags = M.empty
	, fields = M.empty
	}

{- Create and returns an Annex state object for the specified git repo. -}
new :: Git.Repo -> IO AnnexState
new gitrepo = newState <$> Git.Config.read gitrepo

{- performs an action in the Annex monad -}
run :: AnnexState -> Annex a -> IO (a, AnnexState)
run s a = runStateT (runAnnex a) s
eval :: AnnexState -> Annex a -> IO a
eval s a = evalStateT (runAnnex a) s

{- Gets a value from the internal state, selected by the passed value
 - constructor. -}
getState :: (AnnexState -> a) -> Annex a
getState = gets

{- Applies a state mutation function to change the internal state. 
 -
 - Example: changeState $ \s -> s { output = QuietOutput }
 -}
changeState :: (AnnexState -> AnnexState) -> Annex ()
changeState = modify

{- Sets a flag to True -}
setFlag :: String -> Annex ()
setFlag flag = changeState $ \s ->
	s { flags = M.insert flag True $ flags s }

{- Sets a field to a value -}
setField :: String -> String -> Annex ()
setField field value = changeState $ \s ->
	s { fields = M.insert field value $ fields s }

{- Checks if a flag was set. -}
getFlag :: String -> Annex Bool
getFlag flag = fromMaybe False . M.lookup flag <$> getState flags

{- Gets the value of a field. -}
getField :: String -> Annex (Maybe String)
getField field = M.lookup field <$> getState fields

{- Returns the annex's git repository. -}
gitRepo :: Annex Git.Repo
gitRepo = getState repo

{- Runs an IO action in the annex's git repository. -}
inRepo :: (Git.Repo -> IO a) -> Annex a
inRepo a = liftIO . a =<< gitRepo

{- Extracts a value from the annex's git repisitory. -}
fromRepo :: (Git.Repo -> a) -> Annex a
fromRepo a = a <$> gitRepo
