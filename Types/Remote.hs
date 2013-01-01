{- git-annex remotes types
 -
 - Most things should not need this, using Types instead
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Remote where

import Data.Map as M
import Data.Ord

import qualified Git
import Types.Key
import Types.UUID
import Types.Meters
import Types.GitConfig

type RemoteConfigKey = String
type RemoteConfig = M.Map RemoteConfigKey String

{- There are different types of remotes. -}
data RemoteTypeA a = RemoteType {
	-- human visible type name
	typename :: String,
	-- enumerates remotes of this type
	enumerate :: a [Git.Repo],
	-- generates a remote of this type
	generate :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> a (RemoteA a),
	-- initializes or changes a remote
	setup :: UUID -> RemoteConfig -> a RemoteConfig
}

instance Eq (RemoteTypeA a) where
	x == y = typename x == typename y

{- A filename associated with a Key, for display to user. -}
type AssociatedFile = Maybe FilePath

{- An individual remote. -}
data RemoteA a = Remote {
	-- each Remote has a unique uuid
	uuid :: UUID,
	-- each Remote has a human visible name
	name :: String,
	-- Remotes have a use cost; higher is more expensive
	cost :: Int,
	-- Transfers a key to the remote.
	storeKey :: Key -> AssociatedFile -> MeterUpdate -> a Bool,
	-- retrieves a key's contents to a file
	retrieveKeyFile :: Key -> AssociatedFile -> FilePath -> a Bool,
	-- retrieves a key's contents to a tmp file, if it can be done cheaply
	retrieveKeyFileCheap :: Key -> FilePath -> a Bool,
	-- removes a key's contents
	removeKey :: Key -> a Bool,
	-- Checks if a key is present in the remote; if the remote
	-- cannot be accessed returns a Left error message.
	hasKey :: Key -> a (Either String Bool),
	-- Some remotes can check hasKey without an expensive network
	-- operation.
	hasKeyCheap :: Bool,
	-- Some remotes can provide additional details for whereis.
	whereisKey :: Maybe (Key -> a [String]),
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
	-- the type of the remote
	remotetype :: RemoteTypeA a
}

instance Show (RemoteA a) where
	show remote = "Remote { name =\"" ++ name remote ++ "\" }"

-- two remotes are the same if they have the same uuid
instance Eq (RemoteA a) where
	x == y = uuid x == uuid y

-- order remotes by cost
instance Ord (RemoteA a) where
	compare = comparing cost
