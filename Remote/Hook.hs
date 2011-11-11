{- A remote that provides hooks to run shell commands.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Hook (remote) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import System.Exit

import Common.Annex
import Types.Remote
import qualified Git
import Config
import Annex.Content
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto

remote :: RemoteType Annex
remote = RemoteType {
	typename = "hook",
	enumerate = findSpecialRemotes "hooktype",
	generate = gen,
	setup = hookSetup
}

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex (Remote Annex)
gen r u c = do
	hooktype <- getConfig r "hooktype" (error "missing hooktype")
	cst <- remoteCost r expensiveRemoteCost
	return $ encryptableRemote c
		(storeEncrypted hooktype)
		(retrieveEncrypted hooktype)
		Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
 			storeKey = store hooktype,
			retrieveKeyFile = retrieve hooktype,
			removeKey = remove hooktype,
			hasKey = checkPresent r hooktype,
			hasKeyCheap = False,
			config = Nothing,
			repo = r
		}

hookSetup :: UUID -> RemoteConfig -> Annex RemoteConfig
hookSetup u c = do
	let hooktype = fromMaybe (error "Specify hooktype=") $
		M.lookup "hooktype" c
	c' <- encryptionSetup c
	gitConfigSpecialRemote u c' "hooktype" hooktype
	return c'

hookEnv :: Key -> Maybe FilePath -> Maybe [(String, String)]
hookEnv k f = Just $ fileenv f ++ keyenv
	where
		env s v = ("ANNEX_" ++ s, v)
		keyenv =
			[ env "KEY" (show k)
			, env "HASH_1" hash_1
			, env "HASH_2" hash_2
			]
		fileenv Nothing = []
		fileenv (Just file) =  [env "FILE" file]
		[hash_1, hash_2, _rest] =
			map takeDirectory $ splitPath $ hashDirMixed k

lookupHook :: String -> String -> Annex (Maybe String)
lookupHook hooktype hook =do
	g <- gitRepo
	command <- getConfig g hookname ""
	if null command
		then do
			warning $ "missing configuration for " ++ hookname
			return Nothing
		else return $ Just command
	where
		hookname =  hooktype ++ "-" ++ hook ++ "-hook"

runHook :: String -> String -> Key -> Maybe FilePath -> Annex Bool -> Annex Bool
runHook hooktype hook k f a = maybe (return False) run =<< lookupHook hooktype hook
	where
		run command = do
			showOutput -- make way for hook output
			res <- liftIO $ boolSystemEnv
				"sh" [Param "-c", Param command] $ hookEnv k f
			if res
				then a
				else do
					warning $ hook ++ " hook exited nonzero!"
					return res

store :: String -> Key -> Annex Bool
store h k = do
	src <- fromRepo $ gitAnnexLocation k
	runHook h "store" k (Just src) $ return True

storeEncrypted :: String -> (Cipher, Key) -> Key -> Annex Bool
storeEncrypted h (cipher, enck) k = withTmp enck $ \tmp -> do
	src <- fromRepo $ gitAnnexLocation k
	liftIO $ withEncryptedContent cipher (L.readFile src) $ L.writeFile tmp
	runHook h "store" enck (Just tmp) $ return True

retrieve :: String -> Key -> FilePath -> Annex Bool
retrieve h k f = runHook h "retrieve" k (Just f) $ return True

retrieveEncrypted :: String -> (Cipher, Key) -> FilePath -> Annex Bool
retrieveEncrypted h (cipher, enck) f = withTmp enck $ \tmp ->
	runHook h "retrieve" enck (Just tmp) $ liftIO $ catchBoolIO $ do
		withDecryptedContent cipher (L.readFile tmp) $ L.writeFile f
		return True

remove :: String -> Key -> Annex Bool
remove h k = runHook h "remove" k Nothing $ return True

checkPresent :: Git.Repo -> String -> Key -> Annex (Either String Bool)
checkPresent r h k = do
	showAction $ "checking " ++ Git.repoDescribe r
	v <- lookupHook h "checkpresent"
	liftIO $ catchMsgIO $ check v
	where
		findkey s = show k `elem` lines s
		env = hookEnv k Nothing
		check Nothing = error "checkpresent hook misconfigured"
		check (Just hook) = do
			(frompipe, topipe) <- createPipe
			pid <- forkProcess $ do
				_ <- dupTo topipe stdOutput
				closeFd frompipe
				executeFile "sh" True ["-c", hook] env
			closeFd topipe
			fromh <- fdToHandle frompipe
			reply <- hGetContentsStrict fromh
			hClose fromh
			s <- getProcessStatus True False pid
			case s of
				Just (Exited ExitSuccess) -> return $ findkey reply
				_ -> error "checkpresent hook failed"
