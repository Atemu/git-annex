{- git-annex remotes class
 -
 - Most things should not need this, using Remote instead
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RemoteClass where

import Control.Exception
import Data.Map as M

import qualified GitRepo as Git
import Key

{- There are different types of remotes. -}
data RemoteType a = RemoteType {
	-- human visible type name
	typename :: String,
	-- enumerates remotes of this type
	enumerate :: a [Git.Repo],
	-- generates a remote of this type
	generate :: Git.Repo -> Maybe (M.Map String String) -> a (Remote a),
	-- initializes or changes a remote
	setup :: String -> M.Map String String -> a (M.Map String String)
}

{- An individual remote. -}
data Remote a = Remote {
	-- each Remote has a unique uuid
	uuid :: String,
	-- each Remote has a human visible name
	name :: String,
	-- Remotes have a use cost; higher is more expensive
	cost :: Int,
	-- Transfers a key to the remote.
	storeKey :: Key -> a Bool,
	-- retrieves a key's contents to a file
	retrieveKeyFile :: Key -> FilePath -> a Bool,
	-- removes a key's contents
	removeKey :: Key -> a Bool,
	-- Checks if a key is present in the remote; if the remote
	-- cannot be accessed returns a Left error.
	hasKey :: Key -> a (Either IOException Bool),
	-- Some remotes can check hasKey without an expensive network
	-- operation.
	hasKeyCheap :: Bool,
	-- a Remote can have a persistent configuration store
	config :: Maybe (M.Map String String)
}

instance Show (Remote a) where
	show remote = "Remote { uuid =\"" ++ uuid remote ++ "\" }"

-- two remotes are the same if they have the same uuid
instance Eq (Remote a) where
	x == y = uuid x == uuid y

-- order remotes by cost
instance Ord (Remote a) where
	compare x y = compare (cost x) (cost y)
