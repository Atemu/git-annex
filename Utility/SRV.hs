{- SRV record lookup
 -
 - Uses either the ADNS Haskell library, or the standalone Haskell DNS
 - package, or the host command.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.SRV (
	mkSRVTcp,
	mkSRV,
	lookupSRV,
	lookupSRVHost,
) where

import Utility.Process
import Utility.Exception
import Utility.PartialPrelude

import Network
import Data.Function
import Data.List
import Control.Applicative
import Data.Maybe

#ifdef WITH_ADNS
import ADNS.Resolver
import Data.Either
#else
#ifndef WITH_HOST
#ifdef WITH_DNS
import qualified Network.DNS.Lookup as DNS
import Network.DNS.Resolver
import qualified Data.ByteString.UTF8 as B8
#endif
#endif
#endif

newtype SRV = SRV String
	deriving (Show, Eq)

type HostPort = (HostName, PortID)

type PriorityWeight = (Int, Int) -- sort by priority first, then weight

mkSRV :: String -> String -> HostName -> SRV
mkSRV transport protocol host = SRV $ concat
	["_", protocol, "._", transport, ".", host]

mkSRVTcp :: String -> HostName -> SRV
mkSRVTcp = mkSRV "tcp"

{- Returns an ordered list, with highest priority hosts first.
 -
 - On error, returns an empty list. -}
lookupSRV :: SRV -> IO [HostPort]
#ifdef WITH_ADNS
lookupSRV (SRV srv) = initResolver [] $ \resolver -> do
	r <- catchDefaultIO (Right []) $
		resolveSRV resolver srv
	return $ either (\_ -> []) id r
#else
#ifdef WITH_HOST
lookupSRV = lookupSRVHost
#else
#ifdef WITH_DNS
lookupSRV (SRV srv) = do
	seed <- makeResolvSeed defaultResolvConf
	print srv
	r <- withResolver seed $ flip DNS.lookupSRV $ B8.fromString srv
	print r
	return $ maybe [] (orderHosts . map tohosts) r
  where
	tohosts (priority, weight, port, hostname) =
		( (priority, weight)
		, (B8.toString hostname, PortNumber $ fromIntegral port)
		)
#else
lookupSRV = lookupSRVHost
#endif
#endif
#endif

lookupSRVHost :: SRV -> IO [HostPort]
lookupSRVHost (SRV srv) = catchDefaultIO [] $ 
	parseSrvHost <$> readProcessEnv "host" ["-t", "SRV", "--", srv]
		-- clear environment, to avoid LANG affecting output
		(Just [])

parseSrvHost :: String -> [HostPort]
parseSrvHost = orderHosts . catMaybes . map parse . lines
  where
	parse l = case words l of
		[_, _, _, _, spriority, sweight, sport, hostname] -> do
			let v = 
				( readish sport :: Maybe Int
				, readish spriority :: Maybe Int
				, readish sweight :: Maybe Int
				)
			case v of
				(Just port, Just priority, Just weight) -> Just
					( (priority, weight)
					, (hostname, PortNumber $ fromIntegral port)
					)
				_ -> Nothing
		_ -> Nothing

orderHosts :: [(PriorityWeight, HostPort)] -> [HostPort]
orderHosts = map snd . sortBy (compare `on` fst)
