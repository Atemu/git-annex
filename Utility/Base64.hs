{- Simple Base64 access
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Base64 (toB64, fromB64Maybe, fromB64) where

import "dataenc" Codec.Binary.Base64
import Data.Bits.Utils
import Control.Applicative
import Data.Maybe

toB64 :: String -> String		
toB64 = encode . s2w8

fromB64Maybe :: String -> Maybe String
fromB64Maybe s = w82s <$> decode s

fromB64 :: String -> String
fromB64 = fromMaybe bad . fromB64Maybe
  where
	bad = error "bad base64 encoded data"
