{- git-annex remotes types
 -
 - Most things should not need this, using Types instead
 -
 - Copyright 2011-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Remote
	( RemoteConfigKey
	, RemoteConfig
	, RemoteTypeA(..)
	, RemoteA(..)
	, Availability(..)
	)
	where

import Data.Map as M
import Data.Ord

import qualified Git
import Types.Key
import Types.UUID
import Types.GitConfig
import Types.Availability
import Types.Creds
import Config.Cost
import Utility.Metered
import Git.Types
import Utility.SafeCommand

type RemoteConfigKey = String
type RemoteConfig = M.Map RemoteConfigKey String

{- There are different types of remotes. -}
data RemoteTypeA a = RemoteType {
	-- human visible type name
	typename :: String,
	-- enumerates remotes of this type
	enumerate :: a [Git.Repo],
	-- generates a remote of this type
	generate :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> a (Maybe (RemoteA a)),
	-- initializes or changes a remote
	setup :: Maybe UUID -> Maybe CredPair -> RemoteConfig -> a (RemoteConfig, UUID)
}

instance Eq (RemoteTypeA a) where
	x == y = typename x == typename y

{- An individual remote. -}
data RemoteA a = Remote {
	-- each Remote has a unique uuid
	uuid :: UUID,
	-- each Remote has a human visible name
	name :: RemoteName,
	-- Remotes have a use cost; higher is more expensive
	cost :: Cost,
	-- Transfers a key's contents from disk to the remote.
	-- The key should not appear to be present on the remote until
	-- all of its contents have been transferred.
	storeKey :: Key -> AssociatedFile -> MeterUpdate -> a Bool,
	-- Retrieves a key's contents to a file.
	-- (The MeterUpdate does not need to be used if it retrieves
	-- directly to the file, and not to an intermediate file.)
	retrieveKeyFile :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> a Bool,
	-- retrieves a key's contents to a tmp file, if it can be done cheaply
	retrieveKeyFileCheap :: Key -> FilePath -> a Bool,
	-- removes a key's contents (succeeds if the contents are not present)
	removeKey :: Key -> a Bool,
	-- Checks if a key is present in the remote.
	-- Throws an exception if the remote cannot be accessed.
	checkPresent :: Key -> a Bool,
	-- Some remotes can checkPresent without an expensive network
	-- operation.
	checkPresentCheap :: Bool,
	-- Some remotes can provide additional details for whereis.
	whereisKey :: Maybe (Key -> a [String]),
	-- Some remotes can run a fsck operation on the remote,
	-- without transferring all the data to the local repo
	-- The parameters are passed to the fsck command on the remote.
	remoteFsck :: Maybe ([CommandParam] -> a (IO Bool)),
	-- Runs an action to repair the remote's git repository.
	repairRepo :: Maybe (a Bool -> a (IO Bool)),
	-- a Remote has a persistent configuration store
	config :: RemoteConfig,
	-- git repo for the Remote
	repo :: Git.Repo,
	-- a Remote's configuration from git
	gitconfig :: RemoteGitConfig,
	-- a Remote can be assocated with a specific local filesystem path
	localpath :: Maybe FilePath,
	-- a Remote can be known to be readonly
	readonly :: Bool,
	-- a Remote can be globally available. (Ie, "in the cloud".)
	availability :: Availability,
	-- the type of the remote
	remotetype :: RemoteTypeA a,
	-- For testing, makes a version of this remote that is not
	-- available for use. All its actions should fail.
	mkUnavailable :: a (Maybe (RemoteA a))
}

instance Show (RemoteA a) where
	show remote = "Remote { name =\"" ++ name remote ++ "\" }"

-- two remotes are the same if they have the same uuid
instance Eq (RemoteA a) where
	x == y = uuid x == uuid y

instance Ord (RemoteA a) where
	compare = comparing uuid
