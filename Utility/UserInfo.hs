{- user info
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.UserInfo (
	myHomeDir,
	myUserName,
	myUserGecos,
) where

import Control.Applicative
import System.Posix.User
import System.Posix.Env

{- Current user's home directory.
 -
 - getpwent will fail on LDAP or NIS, so use HOME if set. -}
myHomeDir :: IO FilePath
myHomeDir = myVal ["HOME"] homeDirectory

{- Current user's user name. -}
myUserName :: IO String
myUserName = myVal ["USER", "LOGNAME"] userName

myUserGecos :: IO String
myUserGecos = myVal [] userGecos

myVal :: [String] -> (UserEntry -> String) -> IO String
myVal envvars extract = maybe (extract <$> getpwent) return =<< check envvars
  where
	check [] = return Nothing
	check (v:vs) = maybe (check vs) (return . Just) =<< getEnv v
	getpwent = getUserEntryForID =<< getEffectiveUserID
