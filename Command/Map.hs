{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Map where

import Control.Exception.Extensible
import qualified Data.Map as M

import Common.Annex
import Command
import qualified Git
import qualified Git.Url
import qualified Git.Config
import qualified Git.Construct
import qualified Annex
import Annex.UUID
import Logs.UUID
import Logs.Trust
import Remote.Helper.Ssh
import qualified Utility.Dot as Dot

-- a link from the first repository to the second (its remote)
data Link = Link Git.Repo Git.Repo

def :: [Command]
def = [dontCheck repoExists $
	command "map" paramNothing seek "generate map of repositories"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = do
	rs <- spider =<< gitRepo

	umap <- uuidMap
	trusted <- trustGet Trusted
		
	file <- (</>) <$> fromRepo gitAnnexDir <*> pure "map.dot"

	liftIO $ writeFile file (drawMap rs umap trusted)
	next $ next $
		ifM (Annex.getState Annex.fast)
			( return True
			, do
				showLongNote $ "running: dot -Tx11 " ++ file
				showOutput
				liftIO $ boolSystem "dot" [Param "-Tx11", File file]
			)

{- Generates a graph for dot(1). Each repository, and any other uuids, are
 - displayed as a node, and each of its remotes is represented as an edge
 - pointing at the node for the remote.
 -
 - The order nodes are added to the graph matters, since dot will draw
 - the first ones near to the top and left. So it looks better to put
 - the repositories first, followed by uuids that were not matched
 - to a repository.
 -}
drawMap :: [Git.Repo] -> M.Map UUID String -> [UUID] -> String
drawMap rs umap ts = Dot.graph $ repos ++ trusted ++ others
  where
	repos = map (node umap rs) rs
	ruuids = ts ++ map getUncachedUUID rs
	others = map (unreachable . uuidnode) $
		filter (`notElem` ruuids) (M.keys umap)
	trusted = map (trustworthy . uuidnode) ts
	uuidnode u = Dot.graphNode (fromUUID u) $ M.findWithDefault "" u umap

hostname :: Git.Repo -> String
hostname r
	| Git.repoIsUrl r = Git.Url.host r
	| otherwise = "localhost"

basehostname :: Git.Repo -> String
basehostname r = Prelude.head $ split "." $ hostname r

{- A name to display for a repo. Uses the name from uuid.log if available,
 - or the remote name if not. -}
repoName :: M.Map UUID String -> Git.Repo -> String
repoName umap r
	| repouuid == NoUUID = fallback
	| otherwise = M.findWithDefault fallback repouuid umap
  where
	repouuid = getUncachedUUID r
	fallback = fromMaybe "unknown" $ Git.remoteName r

{- A unique id for the node for a repo. Uses the annex.uuid if available. -}
nodeId :: Git.Repo -> String
nodeId r =
	case getUncachedUUID r of
		NoUUID -> Git.repoLocation r
		UUID u -> u

{- A node representing a repo. -}
node :: M.Map UUID String -> [Git.Repo] -> Git.Repo -> String
node umap fullinfo r = unlines $ n:edges
  where
	n = Dot.subGraph (hostname r) (basehostname r) "lightblue" $
		decorate $ Dot.graphNode (nodeId r) (repoName umap r)
	edges = map (edge umap fullinfo r) (Git.remotes r)
	decorate
		| Git.config r == M.empty = unreachable
		| otherwise = reachable

{- An edge between two repos. The second repo is a remote of the first. -}
edge :: M.Map UUID String -> [Git.Repo] -> Git.Repo -> Git.Repo -> String	
edge umap fullinfo from to =
	Dot.graphEdge (nodeId from) (nodeId fullto) edgename
  where
	-- get the full info for the remote, to get its UUID
	fullto = findfullinfo to
	findfullinfo n =
		case filter (same n) fullinfo of
			[] -> n
			(n':_) -> n'
	{- Only name an edge if the name is different than the name
	 - that will be used for the destination node, and is
	 - different from its hostname. (This reduces visual clutter.) -}
	edgename = maybe Nothing calcname $ Git.remoteName to
	calcname n
		| n `elem` [repoName umap fullto, hostname fullto] = Nothing
		| otherwise = Just n

unreachable :: String -> String
unreachable = Dot.fillColor "red"
reachable :: String -> String
reachable = Dot.fillColor "white"
trustworthy :: String -> String
trustworthy = Dot.fillColor "green"

{- Recursively searches out remotes starting with the specified repo. -}
spider :: Git.Repo -> Annex [Git.Repo]
spider r = spider' [r] []
spider' :: [Git.Repo] -> [Git.Repo] -> Annex [Git.Repo]
spider' [] known = return known
spider' (r:rs) known
	| any (same r) known = spider' rs known
	| otherwise = do
		r' <- scan r

		-- The remotes will be relative to r', and need to be
		-- made absolute for later use.
		remotes <- mapM (absRepo r') (Git.remotes r')
		let r'' = r' { Git.remotes = remotes }

		spider' (rs ++ remotes) (r'':known)

{- Converts repos to a common absolute form. -}
absRepo :: Git.Repo -> Git.Repo -> Annex Git.Repo
absRepo reference r
	| Git.repoIsUrl reference = return $ Git.Construct.localToUrl reference r
	| Git.repoIsUrl r = return r
	| otherwise = liftIO $ Git.Construct.fromAbsPath =<< absPath (Git.repoPath r)

{- Checks if two repos are the same. -}
same :: Git.Repo -> Git.Repo -> Bool
same a b
	| both Git.repoIsSsh = matching Git.Url.authority && matching Git.repoPath
	| both Git.repoIsUrl && neither Git.repoIsSsh = matching show
	| neither Git.repoIsSsh = matching Git.repoPath
	| otherwise = False
  where
	matching t = t a == t b
	both t = t a && t b
	neither t = not (t a) && not (t b)

{- reads the config of a remote, with progress display -}
scan :: Git.Repo -> Annex Git.Repo
scan r = do
	showStart "map" $ Git.repoDescribe r
	v <- tryScan r
	case v of
		Just r' -> do
			showEndOk
			return r'
		Nothing -> do
			showOutput
			showEndFail
			return r

{- tries to read the config of a remote, returning it only if it can
 - be accessed -}
tryScan :: Git.Repo -> Annex (Maybe Git.Repo)
tryScan r
	| Git.repoIsSsh r = sshscan
	| Git.repoIsUrl r = return Nothing
	| otherwise = safely $ Git.Config.read r
  where
	safely a = do
		result <- liftIO (try a :: IO (Either SomeException Git.Repo))
		case result of
			Left _ -> return Nothing
			Right r' -> return $ Just r'
	pipedconfig cmd params = safely $
		withHandle StdoutHandle createProcessSuccess p $
			Git.Config.hRead r
	  where
		p = proc cmd $ toCommand params

	configlist = onRemote r (pipedconfig, Nothing) "configlist" [] []
	manualconfiglist = do
		sshparams <- sshToRepo r [Param sshcmd]
		liftIO $ pipedconfig "ssh" sshparams
	  where
		sshcmd = cddir ++ " && " ++
			"git config --null --list"
		dir = Git.repoPath r
		cddir
			| "/~" `isPrefixOf` dir =
				let (userhome, reldir) = span (/= '/') (drop 1 dir)
				in "cd " ++ userhome ++ " && cd " ++ shellEscape (drop 1 reldir)
			| otherwise = "cd " ++ shellEscape dir

	-- First, try sshing and running git config manually,
	-- only fall back to git-annex-shell configlist if that
	-- fails.
	-- 
	-- This is done for two reasons, first I'd like this
	-- subcommand to be usable on non-git-annex repos.
	-- Secondly, configlist doesn't include information about
	-- the remote's remotes.
	sshscan = do
		sshnote
		v <- manualconfiglist
		case v of
			Nothing -> do
				sshnote
				configlist
			ok -> return ok

	sshnote = do
		showAction "sshing"
		showOutput
