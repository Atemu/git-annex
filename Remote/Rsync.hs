{- A remote that is only accessible by rsync.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Rsync (remote) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

import Common.Annex
import Types.Remote
import qualified Git
import Config
import Annex.Content
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto
import Utility.RsyncFile

type RsyncUrl = String

data RsyncOpts = RsyncOpts {
	rsyncUrl :: RsyncUrl,
	rsyncOptions :: [CommandParam]
}

remote :: RemoteType Annex
remote = RemoteType {
	typename = "rsync",
	enumerate = findSpecialRemotes "rsyncurl",
	generate = gen,
	setup = rsyncSetup
}

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex (Remote Annex)
gen r u c = do
	o <- genRsyncOpts r
	cst <- remoteCost r expensiveRemoteCost
	return $ encryptableRemote c
		(storeEncrypted o)
		(retrieveEncrypted o)
		Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
 			storeKey = store o,
			retrieveKeyFile = retrieve o,
			removeKey = remove o,
			hasKey = checkPresent r o,
			hasKeyCheap = False,
			config = Nothing,
			repo = r
		}

genRsyncOpts :: Git.Repo -> Annex RsyncOpts
genRsyncOpts r = do
	url <- getConfig r "rsyncurl" (error "missing rsyncurl")
	opts <- getConfig r "rsync-options" ""
	return $ RsyncOpts url $ map Param $ filter safe $ words opts
	where
		safe o
			-- Don't allow user to pass --delete to rsync;
			-- that could cause it to delete other keys
			-- in the same hash bucket as a key it sends.
			| o == "--delete" = False
			| o == "--delete-excluded" = False
			| otherwise = True

rsyncSetup :: UUID -> RemoteConfig -> Annex RemoteConfig
rsyncSetup u c = do
	-- verify configuration is sane
	let url = fromMaybe (error "Specify rsyncurl=") $
		M.lookup "rsyncurl" c
	c' <- encryptionSetup c

	-- The rsyncurl is stored in git config, not only in this remote's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' "rsyncurl" url
	return c'

rsyncKey :: RsyncOpts -> Key -> String
rsyncKey o k = rsyncUrl o </> hashDirMixed k </> shellEscape (f </> f)
        where
                f = keyFile k

rsyncKeyDir :: RsyncOpts -> Key -> String
rsyncKeyDir o k = rsyncUrl o </> hashDirMixed k </> shellEscape (keyFile k)

store :: RsyncOpts -> Key -> Annex Bool
store o k = rsyncSend o k =<< fromRepo (gitAnnexLocation k)

storeEncrypted :: RsyncOpts -> (Cipher, Key) -> Key -> Annex Bool
storeEncrypted o (cipher, enck) k = withTmp enck $ \tmp -> do
	src <- fromRepo $ gitAnnexLocation k
	liftIO $ withEncryptedContent cipher (L.readFile src) $ L.writeFile tmp
	rsyncSend o enck tmp

retrieve :: RsyncOpts -> Key -> FilePath -> Annex Bool
retrieve o k f = rsyncRemote o
	-- use inplace when retrieving to support resuming
	[ Param "--inplace"
	, Param $ rsyncKey o k
	, Param f
	]

retrieveEncrypted :: RsyncOpts -> (Cipher, Key) -> FilePath -> Annex Bool
retrieveEncrypted o (cipher, enck) f = withTmp enck $ \tmp -> do
	res <- retrieve o enck tmp
	if res
		then liftIO $ catchBoolIO $ do
			withDecryptedContent cipher (L.readFile tmp) $ L.writeFile f
			return True
		else return res

remove :: RsyncOpts -> Key -> Annex Bool
remove o k = withRsyncScratchDir $ \tmp -> do
	{- Send an empty directory to rysnc as the parent directory
         - of the file to remove. -}
	let dummy = tmp </> keyFile k
	liftIO $ createDirectoryIfMissing True dummy
	liftIO $ rsync $ rsyncOptions o ++
		[ Params "--delete --recursive"
		, partialParams
		, Param $ addTrailingPathSeparator dummy
		, Param $ rsyncKeyDir o k
		]

checkPresent :: Git.Repo -> RsyncOpts -> Key -> Annex (Either String Bool)
checkPresent r o k = do
	showAction $ "checking " ++ Git.repoDescribe r
	-- note: Does not currently differnetiate between rsync failing
	-- to connect, and the file not being present.
	res <- liftIO $ boolSystem "sh" [Param "-c", Param cmd]
	return $ Right res
	where
		cmd = "rsync --quiet " ++ shellEscape (rsyncKey o k) ++ " 2>/dev/null"

{- Rsync params to enable resumes of sending files safely,
 - ensure that files are only moved into place once complete
 -}
partialParams :: CommandParam
partialParams = Params "--no-inplace --partial --partial-dir=.rsync-partial"

{- Runs an action in an empty scratch directory that can be used to build
 - up trees for rsync. -}
withRsyncScratchDir :: (FilePath -> Annex Bool) -> Annex Bool
withRsyncScratchDir a = do
	pid <- liftIO getProcessID
	t <- fromRepo gitAnnexTmpDir
	let tmp = t </> "rsynctmp" </> show pid
	nuke tmp
	liftIO $ createDirectoryIfMissing True tmp
	res <- a tmp
	nuke tmp
	return res
	where
		nuke d = liftIO $ 
			doesDirectoryExist d >>? removeDirectoryRecursive d

rsyncRemote :: RsyncOpts -> [CommandParam] -> Annex Bool
rsyncRemote o params = do
	showOutput -- make way for progress bar
	res <- liftIO $ rsync $ rsyncOptions o ++ defaultParams ++ params
	if res
		then return res
		else do
			showLongNote "rsync failed -- run git annex again to resume file transfer"
			return res
	where
		defaultParams = [Params "--progress"]

{- To send a single key is slightly tricky; need to build up a temporary
   directory structure to pass to rsync so it can create the hash
   directories. -}
rsyncSend :: RsyncOpts -> Key -> FilePath -> Annex Bool
rsyncSend o k src = withRsyncScratchDir $ \tmp -> do
	let dest = tmp </> hashDirMixed k </> f </> f
	liftIO $ createDirectoryIfMissing True $ parentDir dest
	liftIO $ createLink src dest
	rsyncRemote o
		[ Param "--recursive"
		, partialParams
 		  -- tmp/ to send contents of tmp dir
		, Param $ addTrailingPathSeparator tmp
		, Param $ rsyncUrl o
		]
	where
		f = keyFile k
