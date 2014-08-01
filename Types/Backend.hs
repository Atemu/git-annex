{- git-annex key/value backend data type
 -
 - Most things should not need this, using Types instead
 -
 - Copyright 2010,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Backend where

import Types.Key
import Types.KeySource

data BackendA a = Backend
	{ name :: String
	, getKey :: KeySource -> a (Maybe Key) 
	-- Checks the content of a key.
	, fsckKey :: Maybe (Key -> FilePath -> a Bool)
	-- Checks if a key can be upgraded to a better form.
	, canUpgradeKey :: Maybe (Key -> Bool)
	-- Checks if there is a fast way to migrate a key to a different
	-- backend (ie, without re-hashing).
	, fastMigrate :: Maybe (Key -> BackendA a -> Maybe Key)
	-- Checks if a key is known (or assumed) to always refer to the
	-- same data.
	, isStableKey :: Key -> Bool
	}

instance Show (BackendA a) where
	show backend = "Backend { name =\"" ++ name backend ++ "\" }"

instance Eq (BackendA a) where
	a == b = name a == name b
