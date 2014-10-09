{- git-annex assistant gpg stuff
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.Gpg where

import Utility.Gpg
import Utility.UserInfo
import Types.Remote (RemoteConfigKey)

import qualified Data.Map as M

{- Generates a gpg user id that is not used by any existing secret key -}
newUserId :: IO UserId
newUserId = do
	oldkeys <- secretKeys
	username <- myUserName
	let basekeyname = username ++ "'s git-annex encryption key"
	return $ Prelude.head $ filter (\n -> M.null $ M.filter (== n) oldkeys)
		( basekeyname
		: map (\n -> basekeyname ++ show n) ([2..] :: [Int])
		)

data EnableEncryption = HybridEncryption | SharedEncryption | NoEncryption
	deriving (Eq)

{- Generates Remote configuration for encryption. -}
configureEncryption :: EnableEncryption -> (RemoteConfigKey, String)
configureEncryption SharedEncryption = ("encryption", "shared")
configureEncryption NoEncryption = ("encryption", "none")
configureEncryption HybridEncryption = ("encryption", "hybrid")
