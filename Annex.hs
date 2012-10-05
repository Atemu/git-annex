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
	PreferredContentMap,
	new,
	newState,
	run,
	eval,
	exec,
	getState,
	changeState,
	setFlag,
	setField,
	setOutput,
	getFlag,
	getField,
	addCleanup,
	gitRepo,
	inRepo,
	fromRepo,
) where

import Control.Monad.State.Strict
import Control.Monad.Trans.Control (StM, MonadBaseControl, liftBaseWith, restoreM)
import Control.Monad.Base (liftBase, MonadBase)
import System.Posix.Types (Fd)

import Common
import qualified Git
import qualified Git.Config
import Git.CatFile
import Git.CheckAttr
import Git.SharedRepository
import qualified Git.Queue
import Types.Backend
import qualified Types.Remote
import Types.Crypto
import Types.BranchState
import Types.TrustLevel
import Types.Group
import Types.Messages
import Types.UUID
import Utility.State
import qualified Utility.Matcher
import qualified Data.Map as M
import qualified Data.Set as S

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

type Matcher a = Either [Utility.Matcher.Token a] (Utility.Matcher.Matcher a)

type PreferredContentMap = M.Map UUID (Utility.Matcher.Matcher (S.Set UUID -> FilePath -> Annex Bool))

-- internal state storage
data AnnexState = AnnexState
	{ repo :: Git.Repo
	, backends :: [BackendA Annex]
	, remotes :: [Types.Remote.RemoteA Annex]
	, output :: MessageState
	, force :: Bool
	, fast :: Bool
	, auto :: Bool
	, branchstate :: BranchState
	, repoqueue :: Maybe Git.Queue.Queue
	, catfilehandle :: Maybe CatFileHandle
	, checkattrhandle :: Maybe CheckAttrHandle
	, forcebackend :: Maybe String
	, forcenumcopies :: Maybe Int
	, limit :: Matcher (FilePath -> Annex Bool)
	, preferredcontentmap :: Maybe PreferredContentMap
	, shared :: Maybe SharedRepository
	, forcetrust :: TrustMap
	, trustmap :: Maybe TrustMap
	, groupmap :: Maybe GroupMap
	, ciphers :: M.Map StorableCipher Cipher
	, lockpool :: M.Map FilePath Fd
	, flags :: M.Map String Bool
	, fields :: M.Map String String
	, cleanup :: M.Map String (Annex ())
	}

newState :: Git.Repo -> AnnexState
newState gitrepo = AnnexState
	{ repo = gitrepo
	, backends = []
	, remotes = []
	, output = defaultMessageState
	, force = False
	, fast = False
	, auto = False
	, branchstate = startBranchState
	, repoqueue = Nothing
	, catfilehandle = Nothing
	, checkattrhandle = Nothing
	, forcebackend = Nothing
	, forcenumcopies = Nothing
	, limit = Left []
	, preferredcontentmap = Nothing
	, shared = Nothing
	, forcetrust = M.empty
	, trustmap = Nothing
	, groupmap = Nothing
	, ciphers = M.empty
	, lockpool = M.empty
	, flags = M.empty
	, fields = M.empty
	, cleanup = M.empty
	}

{- Makes an Annex state object for the specified git repo.
 - Ensures the config is read, if it was not already. -}
new :: Git.Repo -> IO AnnexState
new = newState <$$> Git.Config.read

{- performs an action in the Annex monad -}
run :: AnnexState -> Annex a -> IO (a, AnnexState)
run s a = runStateT (runAnnex a) s
eval :: AnnexState -> Annex a -> IO a
eval s a = evalStateT (runAnnex a) s
exec :: AnnexState -> Annex a -> IO AnnexState
exec s a = execStateT (runAnnex a) s

{- Sets a flag to True -}
setFlag :: String -> Annex ()
setFlag flag = changeState $ \s ->
	s { flags = M.insertWith' const flag True $ flags s }

{- Sets a field to a value -}
setField :: String -> String -> Annex ()
setField field value = changeState $ \s ->
	s { fields = M.insertWith' const field value $ fields s }

{- Adds a cleanup action to perform. -}
addCleanup :: String -> Annex () -> Annex ()
addCleanup uid a = changeState $ \s ->
	s { cleanup = M.insertWith' const uid a $ cleanup s }

{- Sets the type of output to emit. -}
setOutput :: OutputType -> Annex ()
setOutput o = changeState $ \s ->
	s { output = (output s) { outputType = o } }

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
